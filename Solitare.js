// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=0,_1=function(_2,_3){while(1){var _4=E(_2);if(!_4[0]){return E(_3);}else{_2=_4[2];var _5=_3+1|0;_3=_5;continue;}}},_6=[0],_7=function(_8,_9){var _a=E(_8);return _a[0]==0?E(_9):[1,_a[1],new T(function(){return B(_7(_a[2],_9));})];},_b=new T(function(){return B(unCStr(": empty list"));}),_c=new T(function(){return B(unCStr("Prelude."));}),_d=function(_e){return new F(function(){return err(B(_7(_c,new T(function(){return B(_7(_e,_b));}))));});},_f=new T(function(){return B(unCStr("head"));}),_g=new T(function(){return B(_d(_f));}),_h=function(_i,_j){while(1){var _k=E(_i);if(!_k){return E(_j);}else{var _l=E(_j);if(!_l[0]){return [0];}else{_i=_k-1|0;_j=_l[2];continue;}}}},_m=function(_n,_o,_p){var _q=E(_n);if(!_q[0]){return [0];}else{var _r=E(_o);if(!_r[0]){return [0];}else{var _s=E(_p);return _s[0]==0?[0]:[1,[0,[1,_r[1],new T(function(){return E(E(_q[1])[1]);})],_s[1]],new T(function(){return B(_m(_q[2],_r[2],_s[2]));})];}}},_t=function(_u,_v){var _w=E(_v);return _w[0]==0?[0]:[1,new T(function(){return B(A(_u,[_w[1]]));}),new T(function(){return B(_t(_u,_w[2]));})];},_x=new T(function(){return B(unCStr("tail"));}),_y=new T(function(){return B(_d(_x));}),_z=function(_A){return E(E(_A)[2]);},_B=function(_C,_D){var _E=E(_C);if(!_E[0]){return [0,_6,_D];}else{var _F=_E[1],_G=_E[2],_H=new T(function(){var _I=E(_D);if(!_I[0]){var _J=E(_y);}else{var _K=B(_B(B(_m(_G,_I[2],new T(function(){return B(_t(_z,_G));}))),new T(function(){var _L=B(_1(_E,0));return _L>=0?B(_h(_L,_I)):E(_I);}))),_J=[0,_K[1],_K[2]];}return _J;});return [0,[1,[0,new T(function(){return E(E(_F)[1]);}),[1,new T(function(){var _M=E(_D);return _M[0]==0?E(_g):E(_M[1]);}),new T(function(){return E(E(_F)[2]);})]],new T(function(){return E(E(_H)[1]);})],new T(function(){return E(E(_H)[2]);})];}},_N=[0],_O=new T(function(){return [0,"(function(){return md51(jsRand().toString());})"];}),_P=function(_Q){var _R=B(A(_Q,[_])),_S=_R;return E(_S);},_T=function(_U){return new F(function(){return _P(function(_){var _=0;return new F(function(){return eval(E(_U)[1]);});});});},_V=function(_){return new F(function(){return A(_T,[_O,_]);});},_W=function(_){return new F(function(){return _V(_);});},_X=[1,_6,_6],_Y=function(_Z){return _Z>1?[1,_6,new T(function(){return B(_Y(_Z-1|0));})]:E(_X);},_10=new T(function(){return B(_Y(4));}),_11=function(_12,_13){var _14=jsShowI(_12),_15=_14;return new F(function(){return _7(fromJSStr(_15),_13);});},_16=[0,41],_17=[0,40],_18=function(_19,_1a,_1b){return _1a>=0?B(_11(_1a,_1b)):_19<=6?B(_11(_1a,_1b)):[1,_17,new T(function(){var _1c=jsShowI(_1a),_1d=_1c;return B(_7(fromJSStr(_1d),[1,_16,_1b]));})];},_1e=new T(function(){return B(unCStr("base_only"));}),_1f=new T(function(){return [0,toJSStr(E(_1e))];}),_1g=function(_1h,_1i,_){var _1j=E(_1i);if(!_1j[0]){return _0;}else{var _1k=E(_1f)[1],_1l=placeCard_ffi(_1k,_1k,toJSStr(B(unAppCStr("emptyFoundation",new T(function(){return B(_18(0,_1h,_6));})))),310+(imul(90,_1h)|0)|0,20),_1m=E(_1h);if(_1m==2147483647){return _0;}else{return new F(function(){return (function(_1n,_1o,_){while(1){var _1p=(function(_1q,_1r,_){var _1s=E(_1r);if(!_1s[0]){return _0;}else{var _1t=placeCard_ffi(_1k,_1k,toJSStr(B(unAppCStr("emptyFoundation",new T(function(){return B(_18(0,_1q,_6));})))),310+(imul(90,_1q)|0)|0,20),_1u=E(_1q);if(_1u==2147483647){return _0;}else{_1n=_1u+1|0;_1o=_1s[2];return null;}}})(_1n,_1o,_);if(_1p!=null){return _1p;}}})(_1m+1|0,_1j[2],_);});}}},_1v=function(_1w,_1x,_1y,_1z,_){var _1A=toJSStr(E(_1w)),_1B=placeCard_ffi(_1A,_1A,toJSStr(E(_1x)),40+(imul(90,_1y)|0)|0,160+(imul(20,_1z)|0)|0);return _0;},_1C=function(_1D,_1E){while(1){var _1F=E(_1D);if(!_1F[0]){return E(_1E);}else{_1D=_1F[2];var _1G=[1,_1F[1],_1E];_1E=_1G;continue;}}},_1H=function(_1I){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_18(9,_1I,_6));}))));});},_1J=new T(function(){return B(unCStr("10"));}),_1K=new T(function(){return B(unCStr("jack"));}),_1L=new T(function(){return B(unCStr("queen"));}),_1M=new T(function(){return B(unCStr("king"));}),_1N=new T(function(){return B(unCStr("heart"));}),_1O=new T(function(){return B(unCStr("diamond"));}),_1P=new T(function(){return B(unCStr("spade"));}),_1Q=new T(function(){return B(unCStr("club"));}),_1R=[0,95],_1S=function(_1T,_1U){var _1V=E(_1T);switch(_1V){case 9:return new F(function(){return _7(_1J,[1,_1R,new T(function(){switch(E(_1U)){case 0:var _1W=E(_1N);break;case 1:var _1W=E(_1O);break;case 2:var _1W=E(_1P);break;default:var _1W=E(_1Q);}return _1W;})]);});break;case 10:return new F(function(){return _7(_1K,[1,_1R,new T(function(){switch(E(_1U)){case 0:var _1X=E(_1N);break;case 1:var _1X=E(_1O);break;case 2:var _1X=E(_1P);break;default:var _1X=E(_1Q);}return _1X;})]);});break;case 11:return new F(function(){return _7(_1L,[1,_1R,new T(function(){switch(E(_1U)){case 0:var _1Y=E(_1N);break;case 1:var _1Y=E(_1O);break;case 2:var _1Y=E(_1P);break;default:var _1Y=E(_1Q);}return _1Y;})]);});break;case 12:return new F(function(){return _7(_1M,[1,_1R,new T(function(){switch(E(_1U)){case 0:var _1Z=E(_1N);break;case 1:var _1Z=E(_1O);break;case 2:var _1Z=E(_1P);break;default:var _1Z=E(_1Q);}return _1Z;})]);});break;default:return [1,new T(function(){var _20=dataToTag(E(_1V))+49|0;if(_20>>>0>1114111){var _21=B(_1H(_20));}else{var _21=[0,_20];}var _22=_21,_23=_22,_24=_23;return _24;}),[1,_1R,new T(function(){switch(E(_1U)){case 0:var _25=E(_1N);break;case 1:var _25=E(_1O);break;case 2:var _25=E(_1P);break;default:var _25=E(_1Q);}return _25;})]];}},_26=function(_27){var _28=E(_27);return new F(function(){return _1S(_28[1],_28[2]);});},_29=function(_2a,_2b,_2c,_){var _2d=B(_1(_2b,0));if(_2d<=2147483647){var _2e=new T(function(){return B(unAppCStr("visibleColumn",new T(function(){return B(_18(0,E(_2a)[1],_6));})));}),_2f=B(_1C(_2c,_6));if(!_2f[0]){return _0;}else{var _2g=E(_2a)[1],_2h=B(_1v(B(_26(_2f[1])),_2e,_2g,_2d,_)),_2i=_2h,_2j=E(_2d);if(_2j==2147483647){return _0;}else{return new F(function(){return (function(_2k,_2l,_){while(1){var _2m=E(_2l);if(!_2m[0]){return _0;}else{var _2n=B(_1v(B(_26(_2m[1])),_2e,_2g,_2k,_)),_2o=_2n,_2p=E(_2k);if(_2p==2147483647){return _0;}else{_2k=_2p+1|0;_2l=_2m[2];continue;}}}})(_2j+1|0,_2f[2],_);});}}}else{return _0;}},_2q=function(_2r,_2s,_2t,_2u){var _2v=E(_2t);if(!_2v[0]){return E(_2s);}else{var _2w=E(_2u);if(!_2w[0]){return E(_2s);}else{return new F(function(){return A(_2r,[_2v[1],_2w[1],new T(function(){return B(_2q(_2r,_2s,_2v[2],_2w[2]));})]);});}}},_2x=new T(function(){return B(unCStr("emptyColumn"));}),_2y=new T(function(){return B(unCStr("back"));}),_2z=function(_2A,_2B){if(_2A<=_2B){var _2C=function(_2D){return [1,[0,_2D],new T(function(){if(_2D!=_2B){var _2E=B(_2C(_2D+1|0));}else{var _2E=[0];}return _2E;})];};return new F(function(){return _2C(_2A);});}else{return [0];}},_2F=new T(function(){return B(_2z(0,2147483647));}),_2G=function(_){return _0;},_2H=function(_2I,_2J,_){var _2K=B(_1v(_1e,_2x,_2I,0,_)),_2L=_2K,_2M=E(_2J),_2N=_2M[1],_2O=B(A(_2q,[function(_2P,_2Q,_2R,_){var _2S=B(_1v(_2y,new T(function(){return B(unAppCStr("hiddenColumn",new T(function(){return B(_18(0,_2I,_6));})));}),_2I,E(_2P)[1],_)),_2T=_2S;return new F(function(){return A(_2R,[_]);});},_2G,_2F,_2N,_])),_2U=_2O;return new F(function(){return _29([0,_2I],_2N,_2M[2],_);});},_2V=function(_2W,_2X,_){while(1){var _2Y=E(_2X);if(!_2Y[0]){return _0;}else{var _2Z=B(_2H(_2W,_2Y[1],_)),_30=_2Z,_31=E(_2W);if(_31==2147483647){return _0;}else{_2W=_31+1|0;_2X=_2Y[2];continue;}}}},_32=[0,41],_33=[1,_32,_6],_34=new T(function(){return B(_18(0,12,_33));}),_35=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_34));}),_36=function(_37){return new F(function(){return err(B(unAppCStr("toEnum{Rank}: tag (",new T(function(){return B(_18(0,_37,_35));}))));});},_38=function(_39,_3a){var _3b=dataToTag(E(E(_3a))),_3c=dataToTag(E(E(_39)));if(_3c<=_3b){var _3d=function(_3e){return [1,new T(function(){return _3e<0?B(_36(_3e)):_3e>12?B(_36(_3e)):_3e;}),new T(function(){if(_3e!=_3b){var _3f=B(_3d(_3e+1|0));}else{var _3f=[0];}return _3f;})];};return new F(function(){return _3d(_3c);});}else{return [0];}},_3g=0,_3h=12,_3i=new T(function(){return B(_38(_3g,_3h));}),_3j=[1,_32,_6],_3k=new T(function(){return B(_18(0,3,_3j));}),_3l=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_3k));}),_3m=function(_3n){return new F(function(){return err(B(unAppCStr("toEnum{Suit}: tag (",new T(function(){return B(_18(0,_3n,_3l));}))));});},_3o=function(_3p){return [1,new T(function(){return _3p<0?B(_3m(_3p)):_3p>3?B(_3m(_3p)):_3p;}),new T(function(){var _3q=E(_3p);if(_3q==3){var _3r=[0];}else{var _3r=B(_3o(_3q+1|0));}return _3r;})];},_3s=new T(function(){return B(_3o(0));}),_3t=function(_3u){var _3v=E(_3u);if(!_3v[0]){return [0];}else{var _3w=function(_3x){var _3y=E(_3x);return _3y[0]==0?E(new T(function(){return B(_3t(_3v[2]));})):[1,[0,_3v[1],_3y[1]],new T(function(){return B(_3w(_3y[2]));})];};return new F(function(){return _3w(_3s);});}},_3z=new T(function(){return B(_3t(_3i));}),_3A=[0,_6,_6],_3B=[1,_3A,_6],_3C=function(_3D){return _3D>1?[1,_3A,new T(function(){return B(_3C(_3D-1|0));})]:E(_3B);},_3E=new T(function(){return B(_3C(7));}),_3F=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_3G=new T(function(){return B(err(_3F));}),_3H=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_3I=new T(function(){return B(err(_3H));}),_3J=function(_3K,_3L){while(1){var _3M=E(_3K);if(!_3M[0]){return E(_3I);}else{var _3N=E(_3L);if(!_3N){return E(_3M[1]);}else{_3K=_3M[2];_3L=_3N-1|0;continue;}}}},_3O=function(_3P,_3Q){return E(_3P)[1]!=E(_3Q)[1];},_3R=function(_3S,_3T){return E(_3S)[1]==E(_3T)[1];},_3U=[0,_3R,_3O],_3V=function(_3W,_3X,_3Y,_){var _3Z=B(_1(_3X,0));if(_3Z<=2147483647){var _40=B(_1C(_3Y,_6));if(!_40[0]){return _0;}else{var _41=E(_40[1]),_42=toJSStr(B(unAppCStr("visibleColumn",new T(function(){return B(_18(0,E(_3W)[1],_6));})))),_43=40+(imul(90,E(_3W)[1])|0)|0,_44=alignCard_ffi(toJSStr(B(_1S(_41[1],_41[2]))),_42,_43,160+(imul(20,_3Z)|0)|0),_45=E(_3Z);if(_45==2147483647){return _0;}else{return new F(function(){return (function(_46,_47,_){while(1){var _48=E(_47);if(!_48[0]){return _0;}else{var _49=E(_48[1]),_4a=alignCard_ffi(toJSStr(B(_1S(_49[1],_49[2]))),_42,_43,160+(imul(20,_46)|0)|0),_4b=E(_46);if(_4b==2147483647){return _0;}else{_46=_4b+1|0;_47=_48[2];continue;}}}})(_45+1|0,_40[2],_);});}}}else{return _0;}},_4c=new T(function(){return [0,"hiddenReserves"];}),_4d=function(_4e,_4f){if(_4e<=0){if(_4e>=0){return new F(function(){return quot(_4e,_4f);});}else{if(_4f<=0){return new F(function(){return quot(_4e,_4f);});}else{return quot(_4e+1|0,_4f)-1|0;}}}else{if(_4f>=0){if(_4e>=0){return new F(function(){return quot(_4e,_4f);});}else{if(_4f<=0){return new F(function(){return quot(_4e,_4f);});}else{return quot(_4e+1|0,_4f)-1|0;}}}else{return quot(_4e-1|0,_4f)-1|0;}}},_4g=new T(function(){return [0,40+B(_4d(90,2))|0];}),_4h=function(_4i,_4j,_){var _4k=alignCard_ffi(toJSStr(B(_1S(_4i,_4j))),E(_4c)[1],E(_4g)[1],20);return _0;},_4l=function(_4m,_4n,_4o){var _4p=[1,[1,_4o,new T(function(){return _4n>=0?B(_3J(_4m,_4n)):E(_3G);})],new T(function(){var _4q=_4n+1|0;return _4q>=0?B(_h(_4q,_4m)):E(_4m);})];if(_4n>0){var _4r=function(_4s,_4t){var _4u=E(_4s);if(!_4u[0]){return E(_4p);}else{var _4v=_4u[1];return _4t>1?[1,_4v,new T(function(){return B(_4r(_4u[2],_4t-1|0));})]:[1,_4v,_4p];}};return new F(function(){return _4r(_4m,_4n);});}else{return E(_4p);}},_4w=[0,_6,_6],_4x=function(_4y,_4z,_4A,_4B,_4C,_4D){return [0,_4y,new T(function(){var _4E=E(_4D)[1],_4F=new T(function(){var _4G=E(_4C)[1];return _4G>=0?B(_3J(_4z,_4G)):E(_3G);}),_4H=new T(function(){var _4I=E(_4C)[1],_4J=[1,new T(function(){var _4K=E(E(_4F)[1]);return _4K[0]==0?E(_4w):[0,_4K[2],[1,_4K[1],_6]];}),new T(function(){var _4L=_4I+1|0;return _4L>=0?B(_h(_4L,_4z)):E(_4z);})];if(_4I>0){var _4M=function(_4N,_4O){var _4P=E(_4N);if(!_4P[0]){return E(_4J);}else{var _4Q=_4P[1];return _4O>1?[1,_4Q,new T(function(){return B(_4M(_4P[2],_4O-1|0));})]:[1,_4Q,_4J];}},_4R=B(_4M(_4z,_4I));}else{var _4R=E(_4J);}var _4S=_4R;return _4S;}),_4T=new T(function(){return _4E>=0?B(_3J(_4H,_4E)):E(_3G);}),_4U=[1,[0,new T(function(){return E(E(_4T)[1]);}),new T(function(){return B(_7(E(_4F)[2],new T(function(){return E(E(_4T)[2]);})));})],new T(function(){var _4V=_4E+1|0;return _4V>=0?B(_h(_4V,_4H)):E(_4H);})];if(_4E>0){var _4W=function(_4X,_4Y){var _4Z=E(_4X);if(!_4Z[0]){return E(_4U);}else{var _50=_4Z[1];return _4Y>1?[1,_50,new T(function(){return B(_4W(_4Z[2],_4Y-1|0));})]:[1,_50,_4U];}},_51=B(_4W(_4H,_4E));}else{var _51=E(_4U);}var _52=_51;return _52;}),_4A,_4B];},_53=function(_54,_55,_56,_57,_58,_59){var _5a=new T(function(){var _5b=E(_58)[1];return _5b>=0?B(_3J(_55,_5b)):E(_3G);}),_5c=new T(function(){return E(E(_5a)[2]);});return [0,new T(function(){return B(_4l(_54,E(_59)[1],new T(function(){var _5d=E(_5c);return _5d[0]==0?E(_g):E(_5d[1]);})));}),new T(function(){var _5e=E(_58)[1],_5f=[1,new T(function(){var _5g=E(E(_5a)[1]);if(!_5g[0]){var _5h=[0,_6,new T(function(){var _5i=E(_5c);return _5i[0]==0?E(_y):E(_5i[2]);})];}else{var _5j=E(_5c);if(!_5j[0]){var _5k=E(_y);}else{var _5l=E(_5j[2]),_5k=_5l[0]==0?[0,_5g[2],[1,_5g[1],_6]]:[0,_5g,_5l];}var _5h=_5k;}var _5m=_5h;return _5m;}),new T(function(){var _5n=_5e+1|0;return _5n>=0?B(_h(_5n,_55)):E(_55);})];if(_5e>0){var _5o=function(_5p,_5q){var _5r=E(_5p);if(!_5r[0]){return E(_5f);}else{var _5s=_5r[1];return _5q>1?[1,_5s,new T(function(){return B(_5o(_5r[2],_5q-1|0));})]:[1,_5s,_5f];}},_5t=B(_5o(_55,_5e));}else{var _5t=E(_5f);}var _5u=_5t;return _5u;}),_56,_57];},_5v=function(_5w,_5x,_5y,_5z,_5A){return [0,_5w,new T(function(){var _5B=E(_5A)[1],_5C=new T(function(){return _5B>=0?B(_3J(_5x,_5B)):E(_3G);}),_5D=[1,[0,new T(function(){return E(E(_5C)[1]);}),[1,new T(function(){var _5E=E(_5y);return _5E[0]==0?E(_g):E(_5E[1]);}),new T(function(){return E(E(_5C)[2]);})]],new T(function(){var _5F=_5B+1|0;return _5F>=0?B(_h(_5F,_5x)):E(_5x);})];if(_5B>0){var _5G=function(_5H,_5I){var _5J=E(_5H);if(!_5J[0]){return E(_5D);}else{var _5K=_5J[1];return _5I>1?[1,_5K,new T(function(){return B(_5G(_5J[2],_5I-1|0));})]:[1,_5K,_5D];}},_5L=B(_5G(_5x,_5B));}else{var _5L=E(_5D);}var _5M=_5L;return _5M;}),new T(function(){var _5N=E(_5y);return _5N[0]==0?E(_y):E(_5N[2]);}),_5z];},_5O=new T(function(){return B(unCStr("Control.Exception.Base"));}),_5P=new T(function(){return B(unCStr("base"));}),_5Q=new T(function(){return B(unCStr("PatternMatchFail"));}),_5R=new T(function(){var _5S=hs_wordToWord64(18445595),_5T=_5S,_5U=hs_wordToWord64(52003073),_5V=_5U;return [0,_5T,_5V,[0,_5T,_5V,_5P,_5O,_5Q],_6];}),_5W=function(_5X){return E(_5R);},_5Y=function(_5Z){return E(E(_5Z)[1]);},_60=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_61=new T(function(){return B(err(_60));}),_62=function(_63,_64,_65){var _66=new T(function(){var _67=B(A(_63,[_65])),_68=B(A(_64,[new T(function(){var _69=E(_66);return _69[0]==0?E(_61):E(_69[1]);})])),_6a=hs_eqWord64(_67[1],_68[1]),_6b=_6a;if(!E(_6b)){var _6c=[0];}else{var _6d=hs_eqWord64(_67[2],_68[2]),_6e=_6d,_6c=E(_6e)==0?[0]:[1,_65];}var _6f=_6c,_6g=_6f;return _6g;});return E(_66);},_6h=function(_6i){var _6j=E(_6i);return new F(function(){return _62(B(_5Y(_6j[1])),_5W,_6j[2]);});},_6k=function(_6l){return E(E(_6l)[1]);},_6m=function(_6n,_6o){return new F(function(){return _7(E(_6n)[1],_6o);});},_6p=[0,44],_6q=[0,93],_6r=[0,91],_6s=function(_6t,_6u,_6v){var _6w=E(_6u);return _6w[0]==0?B(unAppCStr("[]",_6v)):[1,_6r,new T(function(){return B(A(_6t,[_6w[1],new T(function(){var _6x=function(_6y){var _6z=E(_6y);return _6z[0]==0?E([1,_6q,_6v]):[1,_6p,new T(function(){return B(A(_6t,[_6z[1],new T(function(){return B(_6x(_6z[2]));})]));})];};return B(_6x(_6w[2]));})]));})];},_6A=function(_6B,_6C){return new F(function(){return _6s(_6m,_6B,_6C);});},_6D=function(_6E,_6F,_6G){return new F(function(){return _7(E(_6F)[1],_6G);});},_6H=[0,_6D,_6k,_6A],_6I=new T(function(){return [0,_5W,_6H,_6J,_6h];}),_6J=function(_6K){return [0,_6I,_6K];},_6L=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_6M=function(_6N,_6O){return new F(function(){return die(new T(function(){return B(A(_6O,[_6N]));}));});},_6P=function(_6Q,_6R){var _6S=E(_6R);if(!_6S[0]){return [0,_6,_6];}else{var _6T=_6S[1];if(!B(A(_6Q,[_6T]))){return [0,_6,_6S];}else{var _6U=new T(function(){var _6V=B(_6P(_6Q,_6S[2]));return [0,_6V[1],_6V[2]];});return [0,[1,_6T,new T(function(){return E(E(_6U)[1]);})],new T(function(){return E(E(_6U)[2]);})];}}},_6W=[0,32],_6X=[0,10],_6Y=[1,_6X,_6],_6Z=function(_70){return E(E(_70)[1])==124?false:true;},_71=function(_72,_73){var _74=B(_6P(_6Z,B(unCStr(_72)))),_75=_74[1],_76=function(_77,_78){return new F(function(){return _7(_77,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_7(_73,new T(function(){return B(_7(_78,_6Y));})));})));}));});},_79=E(_74[2]);if(!_79[0]){return new F(function(){return _76(_75,_6);});}else{return E(E(_79[1])[1])==124?B(_76(_75,[1,_6W,_79[2]])):B(_76(_75,_6));}},_7a=function(_7b){return new F(function(){return _6M([0,new T(function(){return B(_71(_7b,_6L));})],_6J);});},_7c=new T(function(){return B(_7a("Game.hs:(47,1)-(49,86)|function goesOnColumn"));}),_7d=function(_7e,_7f,_7g,_7h){var _7i=new T(function(){var _7j=E(_7h);if(!_7j[0]){var _7k=E(_7c);}else{var _7l=_7j[1];switch(E(_7f)){case 2:var _7m=E(_7l);switch(E(_7m[2])){case 2:var _7n=false;break;case 3:var _7n=false;break;default:var _7n=(dataToTag(E(E(_7e)))+1|0)==dataToTag(E(E(_7m[1])));}var _7o=_7n,_7p=_7o;break;case 3:var _7q=E(_7l);switch(E(_7q[2])){case 2:var _7r=false;break;case 3:var _7r=false;break;default:var _7r=(dataToTag(E(E(_7e)))+1|0)==dataToTag(E(E(_7q[1])));}var _7s=_7r,_7p=_7s;break;default:var _7t=E(_7l),_7u=_7t[1];switch(E(_7t[2])){case 2:var _7v=(dataToTag(E(E(_7e)))+1|0)==dataToTag(E(E(_7u)));break;case 3:var _7v=(dataToTag(E(E(_7e)))+1|0)==dataToTag(E(E(_7u)));break;default:var _7v=false;}var _7w=_7v,_7p=_7w;}var _7k=_7p;}return _7k;});return E(_7g)[0]==0?E(_7h)[0]==0?dataToTag(E(_7e))==12?true:false:E(_7i):E(_7i);},_7x=function(_7y,_7z,_7A){var _7B=E(_7A);if(!_7B[0]){return dataToTag(E(_7y))==0?true:false;}else{var _7C=_7B[1];switch(E(_7z)){case 0:var _7D=E(_7C);switch(E(_7D[2])){case 0:return dataToTag(E(E(_7y)))==(dataToTag(E(E(_7D[1])))+1|0);case 1:return false;case 2:return false;default:return false;}break;case 1:var _7E=E(_7C);return E(_7E[2])==1?dataToTag(E(E(_7y)))==(dataToTag(E(E(_7E[1])))+1|0):false;case 2:var _7F=E(_7C);return E(_7F[2])==2?dataToTag(E(E(_7y)))==(dataToTag(E(E(_7F[1])))+1|0):false;default:var _7G=E(_7C);return E(_7G[2])==3?dataToTag(E(E(_7y)))==(dataToTag(E(E(_7G[1])))+1|0):false;}}},_7H=new T(function(){return B(unCStr(".emptyDeck"));}),_7I=new T(function(){return B(unCStr(".solitareDeck"));}),_7J=function(_){var _7K=deleteBySelectionString_ffi(toJSStr(E(_7H))),_7L=deleteBySelectionString_ffi(toJSStr(E(_7I)));return _0;},_7M=function(_7N,_){var _7O=deleteBySelectionString_ffi(toJSStr(B(unAppCStr(".hiddenColumn",new T(function(){return B(_18(0,E(_7N)[1],_6));}))))),_7P=deleteBySelectionString_ffi(toJSStr(B(unAppCStr(".visibleColumn",new T(function(){return B(_18(0,E(_7N)[1],_6));})))));return _0;},_7Q=function(_7R,_7S,_){var _7T=B(_1C(_7S,_6));if(!_7T[0]){return _0;}else{var _7U=E(_7T[1]),_7V=toJSStr(B(unAppCStr("foundation",new T(function(){return B(_18(0,E(_7R)[1],_6));})))),_7W=310+(imul(90,E(_7R)[1])|0)|0,_7X=alignCard_ffi(toJSStr(B(_1S(_7U[1],_7U[2]))),_7V,_7W,20);return new F(function(){return (function(_7Y,_){while(1){var _7Z=E(_7Y);if(!_7Z[0]){return _0;}else{var _80=E(_7Z[1]),_81=alignCard_ffi(toJSStr(B(_1S(_80[1],_80[2]))),_7V,_7W,20);_7Y=_7Z[2];continue;}}})(_7T[2],_);});}},_82=new T(function(){return B(unCStr(".hiddenReserves"));}),_83=new T(function(){return B(unCStr(".emptyReserves"));}),_84=function(_){var _85=deleteBySelectionString_ffi(toJSStr(E(_83))),_86=deleteBySelectionString_ffi(toJSStr(E(_82)));return _0;},_87=[0,46],_88=function(_89,_){while(1){var _8a=E(_89);if(!_8a[0]){return _0;}else{var _8b=E(_8a[1]),_8c=B(_4h(_8b[1],_8b[2],_)),_8d=_8c;_89=_8a[2];continue;}}},_8e=new T(function(){return [0,"solitareDeck"];}),_8f=new T(function(){return [0,E(_4g)[1]+90|0];}),_8g=function(_8h,_8i,_){var _8j=alignCard_ffi(toJSStr(B(_1S(_8h,_8i))),E(_8e)[1],E(_8f)[1],20);return _0;},_8k=function(_8l,_){while(1){var _8m=E(_8l);if(!_8m[0]){return _0;}else{var _8n=E(_8m[1]),_8o=B(_8g(_8n[1],_8n[2],_)),_8p=_8o;_8l=_8m[2];continue;}}},_8q=function(_8r,_){while(1){var _8s=E(_8r);if(!_8s[0]){return _0;}else{var _8t=E(_8s[1]),_8u=B(_8g(_8t[1],_8t[2],_)),_8v=_8u;_8r=_8s[2];continue;}}},_8w=function(_8x,_){while(1){var _8y=E(_8x);if(!_8y[0]){return _0;}else{var _8z=E(_8y[1]),_8A=B(_8g(_8z[1],_8z[2],_)),_8B=_8A;_8x=_8y[2];continue;}}},_8C=function(_8D,_){while(1){var _8E=E(_8D);if(!_8E[0]){return _0;}else{var _8F=E(_8E[1]),_8G=B(_8g(_8F[1],_8F[2],_)),_8H=_8G;_8D=_8E[2];continue;}}},_8I=function(_8J,_8K){while(1){var _8L=E(_8J);if(!_8L[0]){return E(_8K)[0]==0?true:false;}else{var _8M=E(_8K);if(!_8M[0]){return false;}else{if(E(_8L[1])[1]!=E(_8M[1])[1]){return false;}else{_8J=_8L[2];_8K=_8M[2];continue;}}}}},_8N=function(_8O){return E(E(_8O)[1]);},_8P=function(_8Q,_8R,_8S){while(1){var _8T=E(_8R);if(!_8T[0]){return true;}else{var _8U=E(_8S);if(!_8U[0]){return false;}else{if(!B(A(_8N,[_8Q,_8T[1],_8U[1]]))){return false;}else{_8R=_8T[2];_8S=_8U[2];continue;}}}}},_8V=function(_8W,_8X){while(1){var _8Y=E(_8X);if(!_8Y[0]){return E(_8W);}else{_8W=_8Y[1];_8X=_8Y[2];continue;}}},_8Z=new T(function(){return B(unCStr("last"));}),_90=new T(function(){return B(_d(_8Z));}),_91=new T(function(){return B(unCStr("solitareDeck"));}),_92=new T(function(){return B(unCStr("hiddenReserves"));}),_93=new T(function(){return B(unCStr("visibleColumn"));}),_94=[0,6],_95=[0,3],_96=new T(function(){return [0,E(_8f)[1]+90|0];}),_97=[1,_7I],_98=[1,_82],_99=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_9a=new T(function(){return B(err(_99));}),_9b=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_9c=new T(function(){return B(err(_9b));}),_9d=new T(function(){return B(_7a("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_9e=function(_9f,_9g){while(1){var _9h=(function(_9i,_9j){var _9k=E(_9i);switch(_9k[0]){case 0:var _9l=E(_9j);if(!_9l[0]){return [0];}else{_9f=B(A(_9k[1],[_9l[1]]));_9g=_9l[2];return null;}break;case 1:var _9m=B(A(_9k[1],[_9j])),_9n=_9j;_9f=_9m;_9g=_9n;return null;case 2:return [0];case 3:return [1,[0,_9k[1],_9j],new T(function(){return B(_9e(_9k[2],_9j));})];default:return E(_9k[1]);}})(_9f,_9g);if(_9h!=null){return _9h;}}},_9o=function(_9p,_9q){var _9r=new T(function(){var _9s=E(_9q);if(_9s[0]==3){var _9t=[3,_9s[1],new T(function(){return B(_9o(_9p,_9s[2]));})];}else{var _9u=E(_9p);if(_9u[0]==2){var _9v=E(_9s);}else{var _9w=E(_9s);if(_9w[0]==2){var _9x=E(_9u);}else{var _9y=new T(function(){var _9z=E(_9w);if(_9z[0]==4){var _9A=[1,function(_9B){return [4,new T(function(){return B(_7(B(_9e(_9u,_9B)),_9z[1]));})];}];}else{var _9C=E(_9u);if(_9C[0]==1){var _9D=_9C[1],_9E=E(_9z);if(!_9E[0]){var _9F=[1,function(_9G){return new F(function(){return _9o(B(A(_9D,[_9G])),_9E);});}];}else{var _9F=[1,function(_9H){return new F(function(){return _9o(B(A(_9D,[_9H])),new T(function(){return B(A(_9E[1],[_9H]));}));});}];}var _9I=_9F;}else{var _9J=E(_9z);if(!_9J[0]){var _9K=E(_9d);}else{var _9K=[1,function(_9L){return new F(function(){return _9o(_9C,new T(function(){return B(A(_9J[1],[_9L]));}));});}];}var _9I=_9K;}var _9A=_9I;}return _9A;}),_9M=E(_9u);switch(_9M[0]){case 1:var _9N=E(_9w);if(_9N[0]==4){var _9O=[1,function(_9P){return [4,new T(function(){return B(_7(B(_9e(B(A(_9M[1],[_9P])),_9P)),_9N[1]));})];}];}else{var _9O=E(_9y);}var _9Q=_9O;break;case 4:var _9R=_9M[1],_9S=E(_9w);switch(_9S[0]){case 0:var _9T=[1,function(_9U){return [4,new T(function(){return B(_7(_9R,new T(function(){return B(_9e(_9S,_9U));})));})];}];break;case 1:var _9T=[1,function(_9V){return [4,new T(function(){return B(_7(_9R,new T(function(){return B(_9e(B(A(_9S[1],[_9V])),_9V));})));})];}];break;default:var _9T=[4,new T(function(){return B(_7(_9R,_9S[1]));})];}var _9Q=_9T;break;default:var _9Q=E(_9y);}var _9x=_9Q;}var _9v=_9x;}var _9t=_9v;}return _9t;}),_9W=E(_9p);switch(_9W[0]){case 0:var _9X=E(_9q);return _9X[0]==0?[0,function(_9Y){return new F(function(){return _9o(B(A(_9W[1],[_9Y])),new T(function(){return B(A(_9X[1],[_9Y]));}));});}]:E(_9r);case 3:return [3,_9W[1],new T(function(){return B(_9o(_9W[2],_9q));})];default:return E(_9r);}},_9Z=function(_a0,_a1,_a2){while(1){var _a3=E(_a1);if(!_a3[0]){return E(_a2)[0]==0?true:false;}else{var _a4=E(_a2);if(!_a4[0]){return false;}else{if(!B(A(_8N,[_a0,_a3[1],_a4[1]]))){return false;}else{_a1=_a3[2];_a2=_a4[2];continue;}}}}},_a5=function(_a6,_a7,_a8){return !B(_9Z(_a6,_a7,_a8))?true:false;},_a9=function(_aa){return [0,function(_ab,_ac){return new F(function(){return _9Z(_aa,_ab,_ac);});},function(_ab,_ac){return new F(function(){return _a5(_aa,_ab,_ac);});}];},_ad=new T(function(){return B(_a9(_3U));}),_ae=function(_af,_ag){var _ah=E(_af);switch(_ah[0]){case 0:return [0,function(_ai){return new F(function(){return _ae(B(A(_ah[1],[_ai])),_ag);});}];case 1:return [1,function(_aj){return new F(function(){return _ae(B(A(_ah[1],[_aj])),_ag);});}];case 2:return [2];case 3:return new F(function(){return _9o(B(A(_ag,[_ah[1]])),new T(function(){return B(_ae(_ah[2],_ag));}));});break;default:var _ak=function(_al){var _am=E(_al);if(!_am[0]){return [0];}else{var _an=E(_am[1]);return new F(function(){return _7(B(_9e(B(A(_ag,[_an[1]])),_an[2])),new T(function(){return B(_ak(_am[2]));}));});}},_ao=B(_ak(_ah[1]));return _ao[0]==0?[2]:[4,_ao];}},_ap=[2],_aq=function(_ar){return [3,_ar,_ap];},_as=function(_at,_au){var _av=E(_at);if(!_av){return new F(function(){return A(_au,[_0]);});}else{return [0,function(_aw){return E(new T(function(){return B(_as(_av-1|0,_au));}));}];}},_ax=function(_ay,_az,_aA){return [1,function(_aB){return new F(function(){return A(function(_aC,_aD,_aE){while(1){var _aF=(function(_aG,_aH,_aI){var _aJ=E(_aG);switch(_aJ[0]){case 0:var _aK=E(_aH);if(!_aK[0]){return E(_az);}else{_aC=B(A(_aJ[1],[_aK[1]]));_aD=_aK[2];var _aL=_aI+1|0;_aE=_aL;return null;}break;case 1:var _aM=B(A(_aJ[1],[_aH])),_aN=_aH,_aL=_aI;_aC=_aM;_aD=_aN;_aE=_aL;return null;case 2:return E(_az);case 3:return function(_aO){return new F(function(){return _as(_aI,function(_aP){return E(new T(function(){return B(_ae(_aJ,_aO));}));});});};default:return function(_aQ){return new F(function(){return _ae(_aJ,_aQ);});};}})(_aC,_aD,_aE);if(_aF!=null){return _aF;}}},[new T(function(){return B(A(_ay,[_aq]));}),_aB,0,_aA]);});}];},_aR=[6],_aS=function(_aT){return E(_aT);},_aU=new T(function(){return B(unCStr("valDig: Bad base"));}),_aV=new T(function(){return B(err(_aU));}),_aW=function(_aX,_aY){var _aZ=function(_b0,_b1){var _b2=E(_b0);if(!_b2[0]){return function(_b3){return new F(function(){return A(_b3,[new T(function(){return B(A(_b1,[_6]));})]);});};}else{var _b4=E(_b2[1])[1],_b5=function(_b6){return function(_b7){return [0,function(_b8){return E(new T(function(){return B(A(new T(function(){return B(_aZ(_b2[2],function(_b9){return new F(function(){return A(_b1,[[1,_b6,_b9]]);});}));}),[_b7]));}));}];};};switch(E(E(_aX)[1])){case 8:if(48>_b4){return function(_ba){return new F(function(){return A(_ba,[new T(function(){return B(A(_b1,[_6]));})]);});};}else{if(_b4>55){return function(_bb){return new F(function(){return A(_bb,[new T(function(){return B(A(_b1,[_6]));})]);});};}else{return new F(function(){return _b5([0,_b4-48|0]);});}}break;case 10:if(48>_b4){return function(_bc){return new F(function(){return A(_bc,[new T(function(){return B(A(_b1,[_6]));})]);});};}else{if(_b4>57){return function(_bd){return new F(function(){return A(_bd,[new T(function(){return B(A(_b1,[_6]));})]);});};}else{return new F(function(){return _b5([0,_b4-48|0]);});}}break;case 16:var _be=new T(function(){if(97>_b4){if(65>_b4){var _bf=[0];}else{if(_b4>70){var _bg=[0];}else{var _bg=[1,[0,(_b4-65|0)+10|0]];}var _bf=_bg;}var _bh=_bf;}else{if(_b4>102){if(65>_b4){var _bi=[0];}else{if(_b4>70){var _bj=[0];}else{var _bj=[1,[0,(_b4-65|0)+10|0]];}var _bi=_bj;}var _bk=_bi;}else{var _bk=[1,[0,(_b4-97|0)+10|0]];}var _bh=_bk;}return _bh;});if(48>_b4){var _bl=E(_be);if(!_bl[0]){return function(_bm){return new F(function(){return A(_bm,[new T(function(){return B(A(_b1,[_6]));})]);});};}else{return new F(function(){return _b5(_bl[1]);});}}else{if(_b4>57){var _bn=E(_be);if(!_bn[0]){return function(_bo){return new F(function(){return A(_bo,[new T(function(){return B(A(_b1,[_6]));})]);});};}else{return new F(function(){return _b5(_bn[1]);});}}else{return new F(function(){return _b5([0,_b4-48|0]);});}}break;default:return E(_aV);}}};return [1,function(_bp){return new F(function(){return A(_aZ,[_bp,_aS,function(_bq){var _br=E(_bq);return _br[0]==0?[2]:B(A(_aY,[_br]));}]);});}];},_bs=[0,10],_bt=[0,1],_bu=[0,2147483647],_bv=function(_bw,_bx){while(1){var _by=E(_bw);if(!_by[0]){var _bz=_by[1],_bA=E(_bx);if(!_bA[0]){var _bB=_bA[1],_bC=addC(_bz,_bB);if(!E(_bC[2])){return [0,_bC[1]];}else{_bw=[1,I_fromInt(_bz)];_bx=[1,I_fromInt(_bB)];continue;}}else{_bw=[1,I_fromInt(_bz)];_bx=_bA;continue;}}else{var _bD=E(_bx);if(!_bD[0]){_bw=_by;_bx=[1,I_fromInt(_bD[1])];continue;}else{return [1,I_add(_by[1],_bD[1])];}}}},_bE=new T(function(){return B(_bv(_bu,_bt));}),_bF=function(_bG){var _bH=E(_bG);if(!_bH[0]){var _bI=E(_bH[1]);return _bI==(-2147483648)?E(_bE):[0, -_bI];}else{return [1,I_negate(_bH[1])];}},_bJ=[0,10],_bK=[0,0],_bL=function(_bM){return [0,_bM];},_bN=function(_bO,_bP){while(1){var _bQ=E(_bO);if(!_bQ[0]){var _bR=_bQ[1],_bS=E(_bP);if(!_bS[0]){var _bT=_bS[1];if(!(imul(_bR,_bT)|0)){return [0,imul(_bR,_bT)|0];}else{_bO=[1,I_fromInt(_bR)];_bP=[1,I_fromInt(_bT)];continue;}}else{_bO=[1,I_fromInt(_bR)];_bP=_bS;continue;}}else{var _bU=E(_bP);if(!_bU[0]){_bO=_bQ;_bP=[1,I_fromInt(_bU[1])];continue;}else{return [1,I_mul(_bQ[1],_bU[1])];}}}},_bV=function(_bW,_bX,_bY){while(1){var _bZ=E(_bY);if(!_bZ[0]){return E(_bX);}else{var _c0=B(_bv(B(_bN(_bX,_bW)),B(_bL(E(_bZ[1])[1]))));_bY=_bZ[2];_bX=_c0;continue;}}},_c1=function(_c2){var _c3=new T(function(){return B(_9o(B(_9o([0,function(_c4){if(E(E(_c4)[1])==45){return new F(function(){return _aW(_bs,function(_c5){return new F(function(){return A(_c2,[[1,new T(function(){return B(_bF(B(_bV(_bJ,_bK,_c5))));})]]);});});});}else{return [2];}}],[0,function(_c6){if(E(E(_c6)[1])==43){return new F(function(){return _aW(_bs,function(_c7){return new F(function(){return A(_c2,[[1,new T(function(){return B(_bV(_bJ,_bK,_c7));})]]);});});});}else{return [2];}}])),new T(function(){return B(_aW(_bs,function(_c8){return new F(function(){return A(_c2,[[1,new T(function(){return B(_bV(_bJ,_bK,_c8));})]]);});}));})));});return new F(function(){return _9o([0,function(_c9){return E(E(_c9)[1])==101?E(_c3):[2];}],[0,function(_ca){return E(E(_ca)[1])==69?E(_c3):[2];}]);});},_cb=function(_cc){return new F(function(){return A(_cc,[_N]);});},_cd=function(_ce){return new F(function(){return A(_ce,[_N]);});},_cf=function(_cg){return [0,function(_ch){return E(E(_ch)[1])==46?E(new T(function(){return B(_aW(_bs,function(_ci){return new F(function(){return A(_cg,[[1,_ci]]);});}));})):[2];}];},_cj=function(_ck){return new F(function(){return _aW(_bs,function(_cl){return new F(function(){return _ax(_cf,_cb,function(_cm){return new F(function(){return _ax(_c1,_cd,function(_cn){return new F(function(){return A(_ck,[[5,[1,_cl,_cm,_cn]]]);});});});});});});});},_co=function(_cp,_cq,_cr){while(1){var _cs=E(_cr);if(!_cs[0]){return false;}else{if(!B(A(_8N,[_cp,_cq,_cs[1]]))){_cr=_cs[2];continue;}else{return true;}}}},_ct=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_cu=function(_cv){return new F(function(){return _co(_3U,_cv,_ct);});},_cw=[0,8],_cx=[0,16],_cy=function(_cz){return [0,function(_cA){return E(E(_cA)[1])==48?E([0,function(_cB){switch(E(E(_cB)[1])){case 79:return E(new T(function(){return B(_aW(_cw,function(_cC){return new F(function(){return A(_cz,[[5,[0,_cw,_cC]]]);});}));}));case 88:return E(new T(function(){return B(_aW(_cx,function(_cD){return new F(function(){return A(_cz,[[5,[0,_cx,_cD]]]);});}));}));case 111:return E(new T(function(){return B(_aW(_cw,function(_cE){return new F(function(){return A(_cz,[[5,[0,_cw,_cE]]]);});}));}));case 120:return E(new T(function(){return B(_aW(_cx,function(_cF){return new F(function(){return A(_cz,[[5,[0,_cx,_cF]]]);});}));}));default:return [2];}}]):[2];}];},_cG=false,_cH=true,_cI=function(_cJ){return [0,function(_cK){switch(E(E(_cK)[1])){case 79:return E(new T(function(){return B(A(_cJ,[_cw]));}));case 88:return E(new T(function(){return B(A(_cJ,[_cx]));}));case 111:return E(new T(function(){return B(A(_cJ,[_cw]));}));case 120:return E(new T(function(){return B(A(_cJ,[_cx]));}));default:return [2];}}];},_cL=function(_cM){return new F(function(){return A(_cM,[_bs]);});},_cN=function(_cO){var _cP=E(_cO);return _cP[0]==0?E(_cP[1]):I_toInt(_cP[1]);},_cQ=function(_cR,_cS){var _cT=E(_cR);if(!_cT[0]){var _cU=_cT[1],_cV=E(_cS);return _cV[0]==0?_cU<=_cV[1]:I_compareInt(_cV[1],_cU)>=0;}else{var _cW=_cT[1],_cX=E(_cS);return _cX[0]==0?I_compareInt(_cW,_cX[1])<=0:I_compare(_cW,_cX[1])<=0;}},_cY=function(_cZ){return [2];},_d0=function(_d1){var _d2=E(_d1);if(!_d2[0]){return E(_cY);}else{var _d3=_d2[1],_d4=E(_d2[2]);return _d4[0]==0?E(_d3):function(_d5){return new F(function(){return _9o(B(A(_d3,[_d5])),new T(function(){return B(A(new T(function(){return B(_d0(_d4));}),[_d5]));}));});};}},_d6=new T(function(){return B(unCStr("NUL"));}),_d7=function(_d8){return [2];},_d9=function(_da){return new F(function(){return _d7(_da);});},_db=function(_dc,_dd){var _de=function(_df,_dg){var _dh=E(_df);if(!_dh[0]){return function(_di){return new F(function(){return A(_di,[_dc]);});};}else{var _dj=E(_dg);return _dj[0]==0?E(_d7):E(_dh[1])[1]!=E(_dj[1])[1]?E(_d9):function(_dk){return [0,function(_dl){return E(new T(function(){return B(A(new T(function(){return B(_de(_dh[2],_dj[2]));}),[_dk]));}));}];};}};return [1,function(_dm){return new F(function(){return A(_de,[_dc,_dm,_dd]);});}];},_dn=[0,0],_do=function(_dp){return new F(function(){return _db(_d6,function(_dq){return E(new T(function(){return B(A(_dp,[_dn]));}));});});},_dr=new T(function(){return B(unCStr("STX"));}),_ds=[0,2],_dt=function(_du){return new F(function(){return _db(_dr,function(_dv){return E(new T(function(){return B(A(_du,[_ds]));}));});});},_dw=new T(function(){return B(unCStr("ETX"));}),_dx=[0,3],_dy=function(_dz){return new F(function(){return _db(_dw,function(_dA){return E(new T(function(){return B(A(_dz,[_dx]));}));});});},_dB=new T(function(){return B(unCStr("EOT"));}),_dC=[0,4],_dD=function(_dE){return new F(function(){return _db(_dB,function(_dF){return E(new T(function(){return B(A(_dE,[_dC]));}));});});},_dG=new T(function(){return B(unCStr("ENQ"));}),_dH=[0,5],_dI=function(_dJ){return new F(function(){return _db(_dG,function(_dK){return E(new T(function(){return B(A(_dJ,[_dH]));}));});});},_dL=new T(function(){return B(unCStr("ACK"));}),_dM=[0,6],_dN=function(_dO){return new F(function(){return _db(_dL,function(_dP){return E(new T(function(){return B(A(_dO,[_dM]));}));});});},_dQ=new T(function(){return B(unCStr("BEL"));}),_dR=[0,7],_dS=function(_dT){return new F(function(){return _db(_dQ,function(_dU){return E(new T(function(){return B(A(_dT,[_dR]));}));});});},_dV=new T(function(){return B(unCStr("BS"));}),_dW=[0,8],_dX=function(_dY){return new F(function(){return _db(_dV,function(_dZ){return E(new T(function(){return B(A(_dY,[_dW]));}));});});},_e0=new T(function(){return B(unCStr("HT"));}),_e1=[0,9],_e2=function(_e3){return new F(function(){return _db(_e0,function(_e4){return E(new T(function(){return B(A(_e3,[_e1]));}));});});},_e5=new T(function(){return B(unCStr("LF"));}),_e6=[0,10],_e7=function(_e8){return new F(function(){return _db(_e5,function(_e9){return E(new T(function(){return B(A(_e8,[_e6]));}));});});},_ea=new T(function(){return B(unCStr("VT"));}),_eb=[0,11],_ec=function(_ed){return new F(function(){return _db(_ea,function(_ee){return E(new T(function(){return B(A(_ed,[_eb]));}));});});},_ef=new T(function(){return B(unCStr("FF"));}),_eg=[0,12],_eh=function(_ei){return new F(function(){return _db(_ef,function(_ej){return E(new T(function(){return B(A(_ei,[_eg]));}));});});},_ek=new T(function(){return B(unCStr("CR"));}),_el=[0,13],_em=function(_en){return new F(function(){return _db(_ek,function(_eo){return E(new T(function(){return B(A(_en,[_el]));}));});});},_ep=new T(function(){return B(unCStr("SI"));}),_eq=[0,15],_er=function(_es){return new F(function(){return _db(_ep,function(_et){return E(new T(function(){return B(A(_es,[_eq]));}));});});},_eu=new T(function(){return B(unCStr("DLE"));}),_ev=[0,16],_ew=function(_ex){return new F(function(){return _db(_eu,function(_ey){return E(new T(function(){return B(A(_ex,[_ev]));}));});});},_ez=new T(function(){return B(unCStr("DC1"));}),_eA=[0,17],_eB=function(_eC){return new F(function(){return _db(_ez,function(_eD){return E(new T(function(){return B(A(_eC,[_eA]));}));});});},_eE=new T(function(){return B(unCStr("DC2"));}),_eF=[0,18],_eG=function(_eH){return new F(function(){return _db(_eE,function(_eI){return E(new T(function(){return B(A(_eH,[_eF]));}));});});},_eJ=new T(function(){return B(unCStr("DC3"));}),_eK=[0,19],_eL=function(_eM){return new F(function(){return _db(_eJ,function(_eN){return E(new T(function(){return B(A(_eM,[_eK]));}));});});},_eO=new T(function(){return B(unCStr("DC4"));}),_eP=[0,20],_eQ=function(_eR){return new F(function(){return _db(_eO,function(_eS){return E(new T(function(){return B(A(_eR,[_eP]));}));});});},_eT=new T(function(){return B(unCStr("NAK"));}),_eU=[0,21],_eV=function(_eW){return new F(function(){return _db(_eT,function(_eX){return E(new T(function(){return B(A(_eW,[_eU]));}));});});},_eY=new T(function(){return B(unCStr("SYN"));}),_eZ=[0,22],_f0=function(_f1){return new F(function(){return _db(_eY,function(_f2){return E(new T(function(){return B(A(_f1,[_eZ]));}));});});},_f3=new T(function(){return B(unCStr("ETB"));}),_f4=[0,23],_f5=function(_f6){return new F(function(){return _db(_f3,function(_f7){return E(new T(function(){return B(A(_f6,[_f4]));}));});});},_f8=new T(function(){return B(unCStr("CAN"));}),_f9=[0,24],_fa=function(_fb){return new F(function(){return _db(_f8,function(_fc){return E(new T(function(){return B(A(_fb,[_f9]));}));});});},_fd=new T(function(){return B(unCStr("EM"));}),_fe=[0,25],_ff=function(_fg){return new F(function(){return _db(_fd,function(_fh){return E(new T(function(){return B(A(_fg,[_fe]));}));});});},_fi=new T(function(){return B(unCStr("SUB"));}),_fj=[0,26],_fk=function(_fl){return new F(function(){return _db(_fi,function(_fm){return E(new T(function(){return B(A(_fl,[_fj]));}));});});},_fn=new T(function(){return B(unCStr("ESC"));}),_fo=[0,27],_fp=function(_fq){return new F(function(){return _db(_fn,function(_fr){return E(new T(function(){return B(A(_fq,[_fo]));}));});});},_fs=new T(function(){return B(unCStr("FS"));}),_ft=[0,28],_fu=function(_fv){return new F(function(){return _db(_fs,function(_fw){return E(new T(function(){return B(A(_fv,[_ft]));}));});});},_fx=new T(function(){return B(unCStr("GS"));}),_fy=[0,29],_fz=function(_fA){return new F(function(){return _db(_fx,function(_fB){return E(new T(function(){return B(A(_fA,[_fy]));}));});});},_fC=new T(function(){return B(unCStr("RS"));}),_fD=[0,30],_fE=function(_fF){return new F(function(){return _db(_fC,function(_fG){return E(new T(function(){return B(A(_fF,[_fD]));}));});});},_fH=new T(function(){return B(unCStr("US"));}),_fI=[0,31],_fJ=function(_fK){return new F(function(){return _db(_fH,function(_fL){return E(new T(function(){return B(A(_fK,[_fI]));}));});});},_fM=new T(function(){return B(unCStr("SP"));}),_fN=[0,32],_fO=function(_fP){return new F(function(){return _db(_fM,function(_fQ){return E(new T(function(){return B(A(_fP,[_fN]));}));});});},_fR=new T(function(){return B(unCStr("DEL"));}),_fS=[0,127],_fT=function(_fU){return new F(function(){return _db(_fR,function(_fV){return E(new T(function(){return B(A(_fU,[_fS]));}));});});},_fW=[1,_fT,_6],_fX=[1,_fO,_fW],_fY=[1,_fJ,_fX],_fZ=[1,_fE,_fY],_g0=[1,_fz,_fZ],_g1=[1,_fu,_g0],_g2=[1,_fp,_g1],_g3=[1,_fk,_g2],_g4=[1,_ff,_g3],_g5=[1,_fa,_g4],_g6=[1,_f5,_g5],_g7=[1,_f0,_g6],_g8=[1,_eV,_g7],_g9=[1,_eQ,_g8],_ga=[1,_eL,_g9],_gb=[1,_eG,_ga],_gc=[1,_eB,_gb],_gd=[1,_ew,_gc],_ge=[1,_er,_gd],_gf=[1,_em,_ge],_gg=[1,_eh,_gf],_gh=[1,_ec,_gg],_gi=[1,_e7,_gh],_gj=[1,_e2,_gi],_gk=[1,_dX,_gj],_gl=[1,_dS,_gk],_gm=[1,_dN,_gl],_gn=[1,_dI,_gm],_go=[1,_dD,_gn],_gp=[1,_dy,_go],_gq=[1,_dt,_gp],_gr=[1,_do,_gq],_gs=new T(function(){return B(unCStr("SOH"));}),_gt=[0,1],_gu=function(_gv){return new F(function(){return _db(_gs,function(_gw){return E(new T(function(){return B(A(_gv,[_gt]));}));});});},_gx=new T(function(){return B(unCStr("SO"));}),_gy=[0,14],_gz=function(_gA){return new F(function(){return _db(_gx,function(_gB){return E(new T(function(){return B(A(_gA,[_gy]));}));});});},_gC=function(_gD){return new F(function(){return _ax(_gu,_gz,_gD);});},_gE=[1,_gC,_gr],_gF=new T(function(){return B(_d0(_gE));}),_gG=[0,1114111],_gH=[0,34],_gI=[0,_gH,_cH],_gJ=[0,39],_gK=[0,_gJ,_cH],_gL=[0,92],_gM=[0,_gL,_cH],_gN=[0,_dR,_cH],_gO=[0,_dW,_cH],_gP=[0,_eg,_cH],_gQ=[0,_e6,_cH],_gR=[0,_el,_cH],_gS=[0,_e1,_cH],_gT=[0,_eb,_cH],_gU=[0,_dn,_cH],_gV=[0,_gt,_cH],_gW=[0,_ds,_cH],_gX=[0,_dx,_cH],_gY=[0,_dC,_cH],_gZ=[0,_dH,_cH],_h0=[0,_dM,_cH],_h1=[0,_dR,_cH],_h2=[0,_dW,_cH],_h3=[0,_e1,_cH],_h4=[0,_e6,_cH],_h5=[0,_eb,_cH],_h6=[0,_eg,_cH],_h7=[0,_el,_cH],_h8=[0,_gy,_cH],_h9=[0,_eq,_cH],_ha=[0,_ev,_cH],_hb=[0,_eA,_cH],_hc=[0,_eF,_cH],_hd=[0,_eK,_cH],_he=[0,_eP,_cH],_hf=[0,_eU,_cH],_hg=[0,_eZ,_cH],_hh=[0,_f4,_cH],_hi=[0,_f9,_cH],_hj=[0,_fe,_cH],_hk=[0,_fj,_cH],_hl=[0,_fo,_cH],_hm=[0,_ft,_cH],_hn=[0,_fy,_cH],_ho=[0,_fD,_cH],_hp=[0,_fI,_cH],_hq=function(_hr){return new F(function(){return _9o([0,function(_hs){switch(E(E(_hs)[1])){case 34:return E(new T(function(){return B(A(_hr,[_gI]));}));case 39:return E(new T(function(){return B(A(_hr,[_gK]));}));case 92:return E(new T(function(){return B(A(_hr,[_gM]));}));case 97:return E(new T(function(){return B(A(_hr,[_gN]));}));case 98:return E(new T(function(){return B(A(_hr,[_gO]));}));case 102:return E(new T(function(){return B(A(_hr,[_gP]));}));case 110:return E(new T(function(){return B(A(_hr,[_gQ]));}));case 114:return E(new T(function(){return B(A(_hr,[_gR]));}));case 116:return E(new T(function(){return B(A(_hr,[_gS]));}));case 118:return E(new T(function(){return B(A(_hr,[_gT]));}));default:return [2];}}],new T(function(){return B(_9o(B(_ax(_cI,_cL,function(_ht){return new F(function(){return _aW(_ht,function(_hu){var _hv=B(_bV(new T(function(){return B(_bL(E(_ht)[1]));}),_bK,_hu));return !B(_cQ(_hv,_gG))?[2]:B(A(_hr,[[0,new T(function(){var _hw=B(_cN(_hv));if(_hw>>>0>1114111){var _hx=B(_1H(_hw));}else{var _hx=[0,_hw];}var _hy=_hx,_hz=_hy;return _hz;}),_cH]]));});});})),new T(function(){return B(_9o([0,function(_hA){return E(E(_hA)[1])==94?E([0,function(_hB){switch(E(E(_hB)[1])){case 64:return E(new T(function(){return B(A(_hr,[_gU]));}));case 65:return E(new T(function(){return B(A(_hr,[_gV]));}));case 66:return E(new T(function(){return B(A(_hr,[_gW]));}));case 67:return E(new T(function(){return B(A(_hr,[_gX]));}));case 68:return E(new T(function(){return B(A(_hr,[_gY]));}));case 69:return E(new T(function(){return B(A(_hr,[_gZ]));}));case 70:return E(new T(function(){return B(A(_hr,[_h0]));}));case 71:return E(new T(function(){return B(A(_hr,[_h1]));}));case 72:return E(new T(function(){return B(A(_hr,[_h2]));}));case 73:return E(new T(function(){return B(A(_hr,[_h3]));}));case 74:return E(new T(function(){return B(A(_hr,[_h4]));}));case 75:return E(new T(function(){return B(A(_hr,[_h5]));}));case 76:return E(new T(function(){return B(A(_hr,[_h6]));}));case 77:return E(new T(function(){return B(A(_hr,[_h7]));}));case 78:return E(new T(function(){return B(A(_hr,[_h8]));}));case 79:return E(new T(function(){return B(A(_hr,[_h9]));}));case 80:return E(new T(function(){return B(A(_hr,[_ha]));}));case 81:return E(new T(function(){return B(A(_hr,[_hb]));}));case 82:return E(new T(function(){return B(A(_hr,[_hc]));}));case 83:return E(new T(function(){return B(A(_hr,[_hd]));}));case 84:return E(new T(function(){return B(A(_hr,[_he]));}));case 85:return E(new T(function(){return B(A(_hr,[_hf]));}));case 86:return E(new T(function(){return B(A(_hr,[_hg]));}));case 87:return E(new T(function(){return B(A(_hr,[_hh]));}));case 88:return E(new T(function(){return B(A(_hr,[_hi]));}));case 89:return E(new T(function(){return B(A(_hr,[_hj]));}));case 90:return E(new T(function(){return B(A(_hr,[_hk]));}));case 91:return E(new T(function(){return B(A(_hr,[_hl]));}));case 92:return E(new T(function(){return B(A(_hr,[_hm]));}));case 93:return E(new T(function(){return B(A(_hr,[_hn]));}));case 94:return E(new T(function(){return B(A(_hr,[_ho]));}));case 95:return E(new T(function(){return B(A(_hr,[_hp]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_gF,[function(_hC){return new F(function(){return A(_hr,[[0,_hC,_cH]]);});}]));})));})));}));});},_hD=function(_hE){return new F(function(){return A(_hE,[_0]);});},_hF=function(_hG){var _hH=E(_hG);if(!_hH[0]){return E(_hD);}else{var _hI=_hH[2],_hJ=E(E(_hH[1])[1]);switch(_hJ){case 9:return function(_hK){return [0,function(_hL){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hK]));}));}];};case 10:return function(_hM){return [0,function(_hN){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hM]));}));}];};case 11:return function(_hO){return [0,function(_hP){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hO]));}));}];};case 12:return function(_hQ){return [0,function(_hR){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hQ]));}));}];};case 13:return function(_hS){return [0,function(_hT){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hS]));}));}];};case 32:return function(_hU){return [0,function(_hV){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hU]));}));}];};case 160:return function(_hW){return [0,function(_hX){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hW]));}));}];};default:var _hY=u_iswspace(_hJ),_hZ=_hY;return E(_hZ)==0?E(_hD):function(_i0){return [0,function(_i1){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_i0]));}));}];};}}},_i2=function(_i3){var _i4=new T(function(){return B(_i2(_i3));}),_i5=[1,function(_i6){return new F(function(){return A(_hF,[_i6,function(_i7){return E([0,function(_i8){return E(E(_i8)[1])==92?E(_i4):[2];}]);}]);});}];return new F(function(){return _9o([0,function(_i9){return E(E(_i9)[1])==92?E([0,function(_ia){var _ib=E(E(_ia)[1]);switch(_ib){case 9:return E(_i5);case 10:return E(_i5);case 11:return E(_i5);case 12:return E(_i5);case 13:return E(_i5);case 32:return E(_i5);case 38:return E(_i4);case 160:return E(_i5);default:var _ic=u_iswspace(_ib),_id=_ic;return E(_id)==0?[2]:E(_i5);}}]):[2];}],[0,function(_ie){var _if=E(_ie);return E(_if[1])==92?E(new T(function(){return B(_hq(_i3));})):B(A(_i3,[[0,_if,_cG]]));}]);});},_ig=function(_ih,_ii){return new F(function(){return _i2(function(_ij){var _ik=E(_ij),_il=E(_ik[1]);if(E(_il[1])==34){if(!E(_ik[2])){return E(new T(function(){return B(A(_ii,[[1,new T(function(){return B(A(_ih,[_6]));})]]));}));}else{return new F(function(){return _ig(function(_im){return new F(function(){return A(_ih,[[1,_il,_im]]);});},_ii);});}}else{return new F(function(){return _ig(function(_in){return new F(function(){return A(_ih,[[1,_il,_in]]);});},_ii);});}});});},_io=new T(function(){return B(unCStr("_\'"));}),_ip=function(_iq){var _ir=u_iswalnum(_iq),_is=_ir;return E(_is)==0?B(_co(_3U,[0,_iq],_io)):true;},_it=function(_iu){return new F(function(){return _ip(E(_iu)[1]);});},_iv=new T(function(){return B(unCStr(",;()[]{}`"));}),_iw=function(_ix){return new F(function(){return A(_ix,[_6]);});},_iy=function(_iz,_iA){var _iB=function(_iC){var _iD=E(_iC);if(!_iD[0]){return E(_iw);}else{var _iE=_iD[1];return !B(A(_iz,[_iE]))?E(_iw):function(_iF){return [0,function(_iG){return E(new T(function(){return B(A(new T(function(){return B(_iB(_iD[2]));}),[function(_iH){return new F(function(){return A(_iF,[[1,_iE,_iH]]);});}]));}));}];};}};return [1,function(_iI){return new F(function(){return A(_iB,[_iI,_iA]);});}];},_iJ=new T(function(){return B(unCStr(".."));}),_iK=new T(function(){return B(unCStr("::"));}),_iL=new T(function(){return B(unCStr("->"));}),_iM=[0,64],_iN=[1,_iM,_6],_iO=[0,126],_iP=[1,_iO,_6],_iQ=new T(function(){return B(unCStr("=>"));}),_iR=[1,_iQ,_6],_iS=[1,_iP,_iR],_iT=[1,_iN,_iS],_iU=[1,_iL,_iT],_iV=new T(function(){return B(unCStr("<-"));}),_iW=[1,_iV,_iU],_iX=[0,124],_iY=[1,_iX,_6],_iZ=[1,_iY,_iW],_j0=[1,_gL,_6],_j1=[1,_j0,_iZ],_j2=[0,61],_j3=[1,_j2,_6],_j4=[1,_j3,_j1],_j5=[1,_iK,_j4],_j6=[1,_iJ,_j5],_j7=function(_j8){return new F(function(){return _9o([1,function(_j9){return E(_j9)[0]==0?E(new T(function(){return B(A(_j8,[_aR]));})):[2];}],new T(function(){return B(_9o([0,function(_ja){return E(E(_ja)[1])==39?E([0,function(_jb){var _jc=E(_jb);switch(E(_jc[1])){case 39:return [2];case 92:return E(new T(function(){return B(_hq(function(_jd){var _je=E(_jd);return new F(function(){return (function(_jf,_jg){var _jh=new T(function(){return B(A(_j8,[[0,_jf]]));});return !E(_jg)?E(E(_jf)[1])==39?[2]:[0,function(_ji){return E(E(_ji)[1])==39?E(_jh):[2];}]:[0,function(_jj){return E(E(_jj)[1])==39?E(_jh):[2];}];})(_je[1],_je[2]);});}));}));default:return [0,function(_jk){return E(E(_jk)[1])==39?E(new T(function(){return B(A(_j8,[[0,_jc]]));})):[2];}];}}]):[2];}],new T(function(){return B(_9o([0,function(_jl){return E(E(_jl)[1])==34?E(new T(function(){return B(_ig(_aS,_j8));})):[2];}],new T(function(){return B(_9o([0,function(_jm){return !B(_co(_3U,_jm,_iv))?[2]:B(A(_j8,[[2,[1,_jm,_6]]]));}],new T(function(){return B(_9o([0,function(_jn){if(!B(_co(_3U,_jn,_ct))){return [2];}else{return new F(function(){return _iy(_cu,function(_jo){var _jp=[1,_jn,_jo];return !B(_co(_ad,_jp,_j6))?B(A(_j8,[[4,_jp]])):B(A(_j8,[[2,_jp]]));});});}}],new T(function(){return B(_9o([0,function(_jq){var _jr=E(_jq),_js=_jr[1],_jt=u_iswalpha(_js),_ju=_jt;if(!E(_ju)){if(E(_js)==95){return new F(function(){return _iy(_it,function(_jv){return new F(function(){return A(_j8,[[3,[1,_jr,_jv]]]);});});});}else{return [2];}}else{return new F(function(){return _iy(_it,function(_jw){return new F(function(){return A(_j8,[[3,[1,_jr,_jw]]]);});});});}}],new T(function(){return B(_ax(_cy,_cj,_j8));})));})));})));})));})));}));});},_jx=function(_jy){return [1,function(_jz){return new F(function(){return A(_hF,[_jz,function(_jA){return E(new T(function(){return B(_j7(_jy));}));}]);});}];},_jB=[0,0],_jC=function(_jD,_jE){return new F(function(){return _jx(function(_jF){var _jG=E(_jF);if(_jG[0]==2){var _jH=E(_jG[1]);return _jH[0]==0?[2]:E(E(_jH[1])[1])==40?E(_jH[2])[0]==0?E(new T(function(){return B(A(_jD,[_jB,function(_jI){return new F(function(){return _jx(function(_jJ){var _jK=E(_jJ);if(_jK[0]==2){var _jL=E(_jK[1]);return _jL[0]==0?[2]:E(E(_jL[1])[1])==41?E(_jL[2])[0]==0?E(new T(function(){return B(A(_jE,[_jI]));})):[2]:[2];}else{return [2];}});});}]));})):[2]:[2];}else{return [2];}});});},_jM=function(_jN,_jO,_jP){var _jQ=function(_jR,_jS){return new F(function(){return _9o(B(_jx(function(_jT){var _jU=E(_jT);if(_jU[0]==4){var _jV=E(_jU[1]);if(!_jV[0]){return new F(function(){return A(_jN,[_jU,_jR,_jS]);});}else{return E(E(_jV[1])[1])==45?E(_jV[2])[0]==0?E([1,function(_jW){return new F(function(){return A(_hF,[_jW,function(_jX){return E(new T(function(){return B(_j7(function(_jY){return new F(function(){return A(_jN,[_jY,_jR,function(_jZ){return new F(function(){return A(_jS,[new T(function(){return [0, -E(_jZ)[1]];})]);});}]);});}));}));}]);});}]):B(A(_jN,[_jU,_jR,_jS])):B(A(_jN,[_jU,_jR,_jS]));}}else{return new F(function(){return A(_jN,[_jU,_jR,_jS]);});}})),new T(function(){return B(_jC(_jQ,_jS));}));});};return new F(function(){return _jQ(_jO,_jP);});},_k0=function(_k1,_k2){return [2];},_k3=function(_k4,_k5){return new F(function(){return _k0(_k4,_k5);});},_k6=function(_k7){var _k8=E(_k7);return _k8[0]==0?[1,new T(function(){return B(_bV(new T(function(){return B(_bL(E(_k8[1])[1]));}),_bK,_k8[2]));})]:E(_k8[2])[0]==0?E(_k8[3])[0]==0?[1,new T(function(){return B(_bV(_bJ,_bK,_k8[1]));})]:[0]:[0];},_k9=function(_ka){var _kb=E(_ka);if(_kb[0]==5){var _kc=B(_k6(_kb[1]));return _kc[0]==0?E(_k0):function(_kd,_ke){return new F(function(){return A(_ke,[new T(function(){return [0,B(_cN(_kc[1]))];})]);});};}else{return E(_k3);}},_kf=function(_kg){return [1,function(_kh){return new F(function(){return A(_hF,[_kh,function(_ki){return E([3,_kg,_ap]);}]);});}];},_kj=new T(function(){return B(_jM(_k9,_jB,_kf));}),_kk=[0,47],_kl=function(_km,_kn,_ko,_){var _kp=placeCard_ffi(toJSStr(E(_km)),toJSStr(E(_kn)),toJSStr(E(_ko)),E(_4g)[1],20);return _0;},_kq=function(_kr,_){while(1){var _ks=E(_kr);if(!_ks[0]){return _0;}else{var _kt=B(_kl(_2y,B(_26(_ks[1])),_92,_)),_ku=_kt;_kr=_ks[2];continue;}}},_kv=new T(function(){return B(unCStr("emptyReserves"));}),_kw=new T(function(){return B(unCStr("emptyDeck"));}),_kx=function(_ky,_kz,_kA,_){var _kB=placeCard_ffi(toJSStr(E(_ky)),toJSStr(E(_kz)),toJSStr(E(_kA)),E(_8f)[1],20);return _0;},_kC=function(_kD,_){while(1){var _kE=E(_kD);if(!_kE[0]){return _0;}else{var _kF=_kE[1],_kG=B(_kx(B(_26(_kF)),B(_26(_kF)),_91,_)),_kH=_kG;_kD=_kE[2];continue;}}},_kI=function(_kJ,_){var _kK=B(_kx(_1e,_1e,_kw,_)),_kL=_kK;return new F(function(){return _kC(B(_1C(_kJ,_6)),_);});},_kM=function(_kN){while(1){var _kO=(function(_kP){var _kQ=E(_kP);if(!_kQ[0]){return [0];}else{var _kR=_kQ[2],_kS=E(_kQ[1]);if(!E(_kS[2])[0]){return [1,_kS[1],new T(function(){return B(_kM(_kR));})];}else{_kN=_kR;return null;}}})(_kN);if(_kO!=null){return _kO;}}},_kT=function(_kU,_kV,_kW){while(1){var _kX=E(_kV);if(!_kX[0]){return [1,_kW];}else{var _kY=E(_kW);if(!_kY[0]){return [0];}else{if(!B(A(_8N,[_kU,_kX[1],_kY[1]]))){return [0];}else{_kV=_kX[2];_kW=_kY[2];continue;}}}}},_kZ=function(_l0,_l1){var _l2=E(_l0);if(!_l2){return [0];}else{var _l3=E(_l1);return _l3[0]==0?[0]:[1,_l3[1],new T(function(){return B(_kZ(_l2-1|0,_l3[2]));})];}},_l4=function(_l5,_l6,_){var _l7=setDragEndCallback_ffi(function(_l8,_l9,_la,_lb){var _lc=new T(function(){return fromJSStr(E(_l9)[1]);});if(!B(_8P(_3U,_93,_lc))){if(!B(_8I(_92,_lc))){if(!B(_8I(_91,_lc))){return function(_){var _ld=consoleLog_ffi(toJSStr(B(unAppCStr("In onDragEnd - Unhandled id/class: ",new T(function(){return B(_7(fromJSStr(E(_l8)[1]),[1,_kk,_lc]));})))));return _0;};}else{var _le=E(_l5),_lf=_le[1],_lg=_le[2],_lh=_le[3],_li=_le[4],_lj=E(_lb)[1],_lk=new T(function(){return _lj<160;}),_ll=function(_){var _lm=function(_){if(!E(_lk)){return new F(function(){return _8C(_lh,_);});}else{var _ln=E(_la)[1];if(_ln<310){return new F(function(){return _8w(_lh,_);});}else{var _lo=E(_lh);if(!_lo[0]){return E(_g);}else{var _lp=E(_lo[1]),_lq=B(_4d(_ln-310|0,90)),_lr=function(_ls,_lt){if(_ls>=0){if(!B(_7x(_lp[1],_lp[2],B(_3J(_lf,_ls))))){return new F(function(){return _8q(_lo,_);});}else{var _lu=B(_4l(_lf,_ls,_lp)),_lv=B(_7Q(_lt,B(_3J(_lu,_ls)),_)),_lw=_lv;return new F(function(){return _l4([0,_lu,_lg,_lo[2],_li],_l6,_);});}}else{return E(_3G);}};return 3>_lq?B(_lr(_lq,[0,_lq])):B(_lr(3,_95));}}}};if(_lj<160){return new F(function(){return _lm(_);});}else{var _lx=E(_la)[1];if(_lx<40){return new F(function(){return _lm(_);});}else{var _ly=E(_lh);if(!_ly[0]){return E(_g);}else{var _lz=E(_ly[1]),_lA=B(_4d(_lx-40|0,90)),_lB=function(_lC,_lD){if(_lC>=0){var _lE=B(_3J(_lg,_lC));if(!B(_7d(_lz[1],_lz[2],_lE[1],_lE[2]))){return new F(function(){return _8k(_ly,_);});}else{var _lF=B(_5v(_lf,_lg,_ly,_li,_lD)),_lG=_lF[2],_lH=B(_3J(_lG,_lC)),_lI=B(_3V(_lD,_lH[1],_lH[2],_)),_lJ=_lI;return new F(function(){return _l4([0,_lF[1],_lG,_lF[3],_lF[4]],_l6,_);});}}else{return E(_3G);}};return 6>_lA?B(_lB(_lA,[0,_lA])):B(_lB(6,_94));}}}};if(!E(_lk)){return E(_ll);}else{var _lK=E(_la)[1];return _lK<E(_4g)[1]?E(_ll):_lK>=E(_8f)[1]?E(_ll):function(_){var _lL=B(_7J(_)),_lM=_lL,_lN=B(_84(_)),_lO=_lN,_lP=B(_kl(_1e,_1e,_kv,_)),_lQ=_lP,_lR=E(_l5),_lS=new T(function(){return B(_1C(_lR[3],_6));}),_lT=B(_kq(_lS,_)),_lU=_lT;return new F(function(){return _l4([0,_lR[1],_lR[2],_6,_lS],_98,_);});};}}}else{var _lV=E(_l5)[4];if(E(_lb)[1]>=160){return function(_aQ){return new F(function(){return _88(_lV,_aQ);});};}else{var _lW=E(_la)[1];return _lW<E(_8f)[1]?function(_aQ){return new F(function(){return (function(_lX,_){while(1){var _lY=E(_lX);if(!_lY[0]){return _0;}else{var _lZ=E(_lY[1]),_m0=B(_4h(_lZ[1],_lZ[2],_)),_m1=_m0;_lX=_lY[2];continue;}}})(_lV,_aQ);});}:_lW>=E(_96)[1]?function(_aQ){return new F(function(){return (function(_m2,_){while(1){var _m3=E(_m2);if(!_m3[0]){return _0;}else{var _m4=E(_m3[1]),_m5=B(_4h(_m4[1],_m4[2],_)),_m6=_m5;_m2=_m3[2];continue;}}})(_lV,_aQ);});}:function(_){var _m7=B(_84(_)),_m8=_m7,_m9=B(_kl(_1e,_1e,_kv,_)),_ma=_m9,_mb=E(_l5),_mc=_mb[4],_md=new T(function(){return B(_h(3,_mc));}),_me=new T(function(){return B(_7(B(_1C(B(_kZ(3,_mc)),_6)),_mb[3]));}),_mf=B(_kq(_md,_)),_mg=_mf,_mh=B(_7J(_)),_mi=_mh,_mj=B(_kI(_me,_)),_mk=_mj;return new F(function(){return _l4([0,_mb[1],_mb[2],_me,_md],_97,_);});};}}}else{var _ml=E(_l5),_mm=_ml[1],_mn=_ml[2],_mo=_ml[3],_mp=_ml[4],_mq=E(_lb)[1],_mr=new T(function(){var _ms=B(_kM(B(_9e(_kj,new T(function(){var _mt=B(_kT(_3U,_93,_lc));return _mt[0]==0?E(_61):E(_mt[1]);})))));return _ms[0]==0?E(_9a):E(_ms[2])[0]==0?E(_ms[1]):E(_9c);}),_mu=function(_){if(_mq>=160){var _mv=E(_mr),_mw=_mv[1];if(_mw>=0){var _mx=B(_3J(_mn,_mw));return new F(function(){return _3V(_mv,_mx[1],_mx[2],_);});}else{return E(_3G);}}else{var _my=E(_la)[1];if(_my<310){var _mz=E(_mr),_mA=_mz[1];if(_mA>=0){var _mB=B(_3J(_mn,_mA));return new F(function(){return _3V(_mz,_mB[1],_mB[2],_);});}else{return E(_3G);}}else{var _mC=E(_mr),_mD=_mC[1];if(_mD>=0){var _mE=B(_3J(_mn,_mD)),_mF=E(_mE[2]);if(!_mF[0]){return E(_g);}else{var _mG=E(_mF[1]),_mH=B(_4d(_my-310|0,90)),_mI=function(_mJ,_mK){if(_mJ>=0){if(!B(_7x(_mG[1],_mG[2],B(_3J(_mm,_mJ))))){return new F(function(){return _3V(_mC,_mE[1],_mF,_);});}else{var _mL=B(_53(_mm,_mn,_mo,_mp,_mC,_mK)),_mM=_mL[1],_mN=_mL[2],_mO=B(_7Q(_mK,B(_3J(_mM,_mJ)),_)),_mP=_mO,_mQ=B(_7M(_mC,_)),_mR=_mQ,_mS=B(_2H(_mD,new T(function(){return B(_3J(_mN,_mD));}),_)),_mT=_mS;return new F(function(){return _l4([0,_mM,_mN,_mL[3],_mL[4]],_l6,_);});}}else{return E(_3G);}};return 3>_mH?B(_mI(_mH,[0,_mH])):B(_mI(3,_95));}}else{return E(_3G);}}}};if(_mq<160){return E(_mu);}else{var _mU=E(_la)[1];if(_mU<40){return E(_mu);}else{var _mV=E(_mr),_mW=_mV[1];if(_mW>=0){var _mX=B(_3J(_mn,_mW)),_mY=E(_mX[2]);if(!_mY[0]){return E(_90);}else{var _mZ=B(_8V(_mY[1],_mY[2])),_n0=B(_4d(_mU-40|0,90)),_n1=function(_n2,_n3,_){if(_n2>=0){var _n4=B(_3J(_mn,_n2));if(!B(_7d(_mZ[1],_mZ[2],_n4[1],_n4[2]))){return new F(function(){return _3V(_mV,_mX[1],_mY,_);});}else{var _n5=B(_4x(_mm,_mn,_mo,_mp,_mV,_n3)),_n6=_n5[2],_n7=B(_3J(_n6,_n2)),_n8=B(_3V(_n3,_n7[1],_n7[2],_)),_n9=_n8,_na=B(_7M(_mV,_)),_nb=_na,_nc=B(_2H(_mW,new T(function(){return B(_3J(_n6,_mW));}),_)),_nd=_nc;return new F(function(){return _l4([0,_n5[1],_n6,_n5[3],_n5[4]],[1,new T(function(){return B(unAppCStr(".visibleColumn",new T(function(){return B(_18(0,_mW,_6));})));})],_);});}}else{return E(_3G);}};return 6>_n0?function(_aQ){return new F(function(){return _n1(_n0,[0,_n0],_aQ);});}:function(_aQ){return new F(function(){return _n1(6,_94,_aQ);});};}}else{return E(_3G);}}}}}),_ne=setMouseoverCallback_ffi(function(_nf,_ng,_nh,_ni){var _nj=E(_l5),_nk=new T(function(){return fromJSStr(E(_ng)[1]);}),_nl=[1,_87,_nk],_nm=function(_){var _nn=new T(function(){return B(_8P(_3U,_93,_nk));}),_no=new T(function(){return B(_8I(_92,_nk));}),_np=new T(function(){return B(_8I(_91,_nk));}),_nq=function(_){var _nr=deleteBySelectionString_ffi(toJSStr(_nl)),_ns=B(_l4(_nj,[1,_nl],_)),_nt=_ns;if(!E(_nn)){if(!E(_no)){if(!E(_np)){var _nu=consoleLog_ffi(toJSStr(B(unAppCStr("In onMouseover - Unhandled id/class: ",new T(function(){return B(_7(fromJSStr(E(_nf)[1]),[1,_kk,new T(function(){return fromJSStr(E(_ng)[1]);})]));})))));return _0;}else{return new F(function(){return _kI(_nj[3],_);});}}else{var _nv=B(_kl(_1e,_1e,_kv,_)),_nw=_nv;return new F(function(){return _kq(_nj[4],_);});}}else{var _nx=B(_kM(B(_9e(_kj,new T(function(){var _ny=B(_kT(_3U,_93,_nk));return _ny[0]==0?E(_61):E(_ny[1]);})))));if(!_nx[0]){return E(_9a);}else{if(!E(_nx[2])[0]){var _nz=E(_nx[1]),_nA=_nz[1];if(_nA>=0){var _nB=B(_3J(_nj[2],_nA));return new F(function(){return _29(_nz,_nB[1],_nB[2],_);});}else{return E(_3G);}}else{return E(_9c);}}}};return !E(_nn)?!E(_no)?!E(_np)?_0:B(_nq(_)):B(_nq(_)):B(_nq(_));},_nC=E(_l6);return _nC[0]==0?E(_nm):!B(_8I(_nl,_nC[1]))?E(_nm):E(_2G);});return _0;},_nD=new T(function(){return B(unCStr("ArithException"));}),_nE=new T(function(){return B(unCStr("GHC.Exception"));}),_nF=new T(function(){return B(unCStr("base"));}),_nG=new T(function(){var _nH=hs_wordToWord64(4194982440),_nI=_nH,_nJ=hs_wordToWord64(3110813675),_nK=_nJ;return [0,_nI,_nK,[0,_nI,_nK,_nF,_nE,_nD],_6];}),_nL=function(_nM){return E(_nG);},_nN=function(_nO){var _nP=E(_nO);return new F(function(){return _62(B(_5Y(_nP[1])),_nL,_nP[2]);});},_nQ=new T(function(){return B(unCStr("arithmetic underflow"));}),_nR=new T(function(){return B(unCStr("arithmetic overflow"));}),_nS=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_nT=new T(function(){return B(unCStr("denormal"));}),_nU=new T(function(){return B(unCStr("divide by zero"));}),_nV=new T(function(){return B(unCStr("loss of precision"));}),_nW=function(_nX){switch(E(_nX)){case 0:return E(_nR);case 1:return E(_nQ);case 2:return E(_nV);case 3:return E(_nU);case 4:return E(_nT);default:return E(_nS);}},_nY=function(_nZ){return new F(function(){return _7(_nQ,_nZ);});},_o0=function(_nZ){return new F(function(){return _7(_nR,_nZ);});},_o1=function(_nZ){return new F(function(){return _7(_nS,_nZ);});},_o2=function(_nZ){return new F(function(){return _7(_nT,_nZ);});},_o3=function(_nZ){return new F(function(){return _7(_nU,_nZ);});},_o4=function(_nZ){return new F(function(){return _7(_nV,_nZ);});},_o5=function(_o6){switch(E(_o6)){case 0:return E(_o0);case 1:return E(_nY);case 2:return E(_o4);case 3:return E(_o3);case 4:return E(_o2);default:return E(_o1);}},_o7=function(_o8,_o9){return new F(function(){return _6s(_o5,_o8,_o9);});},_oa=function(_ob,_oc){switch(E(_oc)){case 0:return E(_o0);case 1:return E(_nY);case 2:return E(_o4);case 3:return E(_o3);case 4:return E(_o2);default:return E(_o1);}},_od=[0,_oa,_nW,_o7],_oe=new T(function(){return [0,_nL,_od,_of,_nN];}),_of=function(_nZ){return [0,_oe,_nZ];},_og=3,_oh=new T(function(){return B(_6M(_og,_of));}),_oi=function(_oj){var _ok=jsTrunc(_oj),_ol=_ok;return [0,_ol];},_om=new T(function(){return [0,"(function(s){return s[0];})"];}),_on=new T(function(){return B(_T(_om));}),_oo=function(_op,_){var _oq=B(A(_on,[E(_op),_])),_or=_oq;return new T(function(){return B(_oi(_or));});},_os=function(_ot,_){return new F(function(){return _oo(_ot,_);});},_ou=function(_ov,_ow){var _ox=_ov%_ow;if(_ov<=0){if(_ov>=0){return E(_ox);}else{if(_ow<=0){return E(_ox);}else{var _oy=E(_ox);return _oy==0?0:_oy+_ow|0;}}}else{if(_ow>=0){if(_ov>=0){return E(_ox);}else{if(_ow<=0){return E(_ox);}else{var _oz=E(_ox);return _oz==0?0:_oz+_ow|0;}}}else{var _oA=E(_ox);return _oA==0?0:_oA+_ow|0;}}},_oB=new T(function(){return [0,"(function(s){return md51(s.join(\',\'));})"];}),_oC=new T(function(){return B(_T(_oB));}),_oD=function(_oE,_){return new F(function(){return A(_oC,[E(_oE),_]);});},_oF=function(_ot,_){return new F(function(){return _oD(_ot,_);});},_oG=function(_oH){return new F(function(){return _P(function(_){var _=0;return new F(function(){return _oF(_oH,_);});});});},_oI=function(_oJ,_oK,_oL){while(1){var _oM=(function(_oN,_oO,_oP){if(_oN>_oO){var _oQ=_oO,_oR=_oN,_oS=_oP;_oJ=_oQ;_oK=_oR;_oL=_oS;return null;}else{return [0,new T(function(){var _oT=(_oO-_oN|0)+1|0;switch(_oT){case -1:var _oU=[0,_oN];break;case 0:var _oU=E(_oh);break;default:var _oU=[0,B(_ou(B(_P(function(_){var _=0;return new F(function(){return _os(_oP,_);});}))[1],_oT))+_oN|0];}var _oV=_oU;return _oV;}),new T(function(){return B(_oG(_oP));})];}})(_oJ,_oK,_oL);if(_oM!=null){return _oM;}}},_oW=function(_oX,_oY){var _oZ=E(_oX);if(!_oZ){return [0,_6,_oY];}else{var _p0=E(_oY);if(!_p0[0]){return [0,_6,_6];}else{var _p1=new T(function(){var _p2=B(_oW(_oZ-1|0,_p0[2]));return [0,_p2[1],_p2[2]];});return [0,[1,_p0[1],new T(function(){return E(E(_p1)[1]);})],new T(function(){return E(E(_p1)[2]);})];}}},_p3=function(_p4,_p5){var _p6=E(_p5);if(!_p6[0]){return [0];}else{var _p7=new T(function(){var _p8=B(_oI(0,B(_1(_p6,0))-1|0,_p4));return [0,_p8[1],_p8[2]];}),_p9=new T(function(){var _pa=E(E(_p7)[1])[1];if(_pa>=0){var _pb=B(_oW(_pa,_p6)),_pc=[0,_pb[1],_pb[2]];}else{var _pc=[0,_6,_p6];}var _pd=_pc,_pe=_pd;return _pe;}),_pf=new T(function(){return E(E(_p9)[2]);});return [1,new T(function(){var _pg=E(_pf);return _pg[0]==0?E(_g):E(_pg[1]);}),new T(function(){return B(_p3(new T(function(){return E(E(_p7)[2]);}),B(_7(E(_p9)[1],new T(function(){var _ph=E(_pf);return _ph[0]==0?E(_y):E(_ph[2]);})))));})];}},_pi=function(_){var _pj=B(_W(_)),_pk=_pj,_pl=B(_B(_3E,new T(function(){return B(_p3(_pk,_3z));}))),_pm=_pl[1],_pn=_pl[2],_po=B(_2V(0,_pm,_)),_pp=_po,_pq=B(_1g(0,_10,_)),_pr=_pq,_ps=B(_kI(_6,_)),_pt=_ps,_pu=B(_kl(_1e,_1e,_kv,_)),_pv=_pu,_pw=B(_kq(_pn,_)),_px=_pw;return new F(function(){return _l4([0,_10,_pm,_6,_pn],_N,_);});},_py=[0,_pi],_pz=function(_){var _pA=loadCards_ffi(E(_py)[1]);return _0;},_pB=function(_){return new F(function(){return _pz(_);});};
var hasteMain = function() {B(A(_pB, [0]));};window.onload = hasteMain;