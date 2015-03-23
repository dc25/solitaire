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

var _0=0,_1=function(_2,_3,_4,_5){var _6=E(_4);if(!_6[0]){return E(_3);}else{var _7=E(_5);if(!_7[0]){return E(_3);}else{return new F(function(){return A(_2,[_6[1],_7[1],new T(function(){return B(_1(_2,_3,_6[2],_7[2]));})]);});}}},_8=[0],_9=new T(function(){return B(unCStr("emptyDeck"));}),_a=new T(function(){return B(unCStr("solitareDeck"));}),_b=function(_c,_d){if(_c<=0){if(_c>=0){return new F(function(){return quot(_c,_d);});}else{if(_d<=0){return new F(function(){return quot(_c,_d);});}else{return quot(_c+1|0,_d)-1|0;}}}else{if(_d>=0){if(_c>=0){return new F(function(){return quot(_c,_d);});}else{if(_d<=0){return new F(function(){return quot(_c,_d);});}else{return quot(_c+1|0,_d)-1|0;}}}else{return quot(_c-1|0,_d)-1|0;}}},_e=new T(function(){return [0,40+B(_b(90,2))|0];}),_f=new T(function(){return [0,E(_e)[1]+90|0];}),_g=function(_h,_i,_j,_){var _k=placeCard_ffi(toJSStr(E(_h)),toJSStr(E(_i)),toJSStr(E(_j)),E(_f)[1],20);return _0;},_l=function(_m,_n){var _o=E(_m);return _o[0]==0?E(_n):[1,_o[1],new T(function(){return B(_l(_o[2],_n));})];},_p=function(_q,_r){var _s=jsShowI(_q),_t=_s;return new F(function(){return _l(fromJSStr(_t),_r);});},_u=[0,41],_v=[0,40],_w=function(_x,_y,_z){return _y>=0?B(_p(_y,_z)):_x<=6?B(_p(_y,_z)):[1,_v,new T(function(){var _A=jsShowI(_y),_B=_A;return B(_l(fromJSStr(_B),[1,_u,_z]));})];},_C=function(_D){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_w(9,_D,_8));}))));});},_E=new T(function(){return B(unCStr("king"));}),_F=new T(function(){return B(unCStr("queen"));}),_G=new T(function(){return B(unCStr("jack"));}),_H=new T(function(){return B(unCStr("10"));}),_I=new T(function(){return B(unCStr("club"));}),_J=new T(function(){return B(unCStr("spade"));}),_K=new T(function(){return B(unCStr("diamond"));}),_L=new T(function(){return B(unCStr("heart"));}),_M=[0,95],_N=function(_O,_P){var _Q=E(_O);switch(_Q){case 9:return new F(function(){return _l(_H,[1,_M,new T(function(){switch(E(_P)){case 0:var _R=E(_L);break;case 1:var _R=E(_K);break;case 2:var _R=E(_J);break;default:var _R=E(_I);}return _R;})]);});break;case 10:return new F(function(){return _l(_G,[1,_M,new T(function(){switch(E(_P)){case 0:var _S=E(_L);break;case 1:var _S=E(_K);break;case 2:var _S=E(_J);break;default:var _S=E(_I);}return _S;})]);});break;case 11:return new F(function(){return _l(_F,[1,_M,new T(function(){switch(E(_P)){case 0:var _T=E(_L);break;case 1:var _T=E(_K);break;case 2:var _T=E(_J);break;default:var _T=E(_I);}return _T;})]);});break;case 12:return new F(function(){return _l(_E,[1,_M,new T(function(){switch(E(_P)){case 0:var _U=E(_L);break;case 1:var _U=E(_K);break;case 2:var _U=E(_J);break;default:var _U=E(_I);}return _U;})]);});break;default:return [1,new T(function(){var _V=dataToTag(E(_Q))+49|0;if(_V>>>0>1114111){var _W=B(_C(_V));}else{var _W=[0,_V];}var _X=_W,_Y=_X,_Z=_Y;return _Z;}),[1,_M,new T(function(){switch(E(_P)){case 0:var _10=E(_L);break;case 1:var _10=E(_K);break;case 2:var _10=E(_J);break;default:var _10=E(_I);}return _10;})]];}},_11=function(_12){var _13=E(_12);return new F(function(){return _N(_13[1],_13[2]);});},_14=function(_15,_){while(1){var _16=E(_15);if(!_16[0]){return _0;}else{var _17=_16[1],_18=B(_g(B(_11(_17)),B(_11(_17)),_a,_)),_19=_18;_15=_16[2];continue;}}},_1a=new T(function(){return B(unCStr("base_only_deck"));}),_1b=new T(function(){return B(unCStr("base_only"));}),_1c=function(_1d,_1e){while(1){var _1f=E(_1d);if(!_1f[0]){return E(_1e);}else{_1d=_1f[2];var _1g=[1,_1f[1],_1e];_1e=_1g;continue;}}},_1h=function(_1i,_){var _1j=B(_g(_1b,_1a,_9,_)),_1k=_1j;return new F(function(){return _14(B(_1c(_1i,_8)),_);});},_1l=function(_1m,_1n,_1o,_1p,_){var _1q=placeCard_ffi(toJSStr(E(_1m)),toJSStr(E(_1n)),toJSStr(E(_1o)),310+(imul(90,_1p)|0)|0,20);return _0;},_1r=new T(function(){return B(unCStr("emptyFoundationClass"));}),_1s=function(_1t,_1u,_){var _1v=B(_1l(_1b,B(_l(_1r,new T(function(){return B(_w(0,_1t,_8));}))),_1r,_1t,_)),_1w=_1v;return new F(function(){return (function(_1x,_){while(1){var _1y=E(_1x);if(!_1y[0]){return _0;}else{var _1z=_1y[1],_1A=B(_1l(B(_11(_1z)),B(_11(_1z)),new T(function(){return B(unAppCStr("foundation",new T(function(){return B(_w(0,_1t,_8));})));}),_1t,_)),_1B=_1A;_1x=_1y[2];continue;}}})(B(_1c(_1u,_8)),_);});},_1C=function(_1D,_1E,_1F,_){var _1G=B(_1s(E(_1D)[1],_1E,_)),_1H=_1G;return new F(function(){return A(_1F,[_]);});},_1I=function(_1J,_1K){if(_1J<=_1K){var _1L=function(_1M){return [1,[0,_1M],new T(function(){if(_1M!=_1K){var _1N=B(_1L(_1M+1|0));}else{var _1N=[0];}return _1N;})];};return new F(function(){return _1L(_1J);});}else{return [0];}},_1O=new T(function(){return B(_1I(0,2147483647));}),_1P=function(_){return _0;},_1Q=function(_1R,_1S,_1T,_1U,_1V,_){var _1W=placeCard_ffi(toJSStr(E(_1R)),toJSStr(E(_1S)),toJSStr(E(_1T)),40+(imul(90,_1U)|0)|0,160+(imul(20,_1V)|0)|0);return _0;},_1X=function(_1Y,_1Z){while(1){var _20=E(_1Y);if(!_20[0]){return E(_1Z);}else{_1Y=_20[2];var _21=_1Z+1|0;_1Z=_21;continue;}}},_22=new T(function(){return B(unCStr("visibleColumn"));}),_23=function(_24,_25,_26,_){var _27=B(_1X(_25,0));if(_27<=2147483647){var _28=new T(function(){return B(_l(_22,new T(function(){return B(_w(0,E(_24)[1],_8));})));}),_29=B(_1c(_26,_8));if(!_29[0]){return _0;}else{var _2a=_29[1],_2b=E(_24)[1],_2c=B(_1Q(B(_11(_2a)),B(_11(_2a)),_28,_2b,_27,_)),_2d=_2c,_2e=E(_27);if(_2e==2147483647){return _0;}else{return new F(function(){return (function(_2f,_2g,_){while(1){var _2h=E(_2g);if(!_2h[0]){return _0;}else{var _2i=_2h[1],_2j=B(_1Q(B(_11(_2i)),B(_11(_2i)),_28,_2b,_2f,_)),_2k=_2j,_2l=E(_2f);if(_2l==2147483647){return _0;}else{_2f=_2l+1|0;_2g=_2h[2];continue;}}}})(_2e+1|0,_29[2],_);});}}}else{return _0;}},_2m=new T(function(){return B(unCStr("emptyColumn"));}),_2n=new T(function(){return B(unCStr("hiddenColumn"));}),_2o=new T(function(){return B(unCStr("back"));}),_2p=function(_2q,_2r,_){var _2s=B(_1Q(_1b,B(_l(_2m,new T(function(){return B(_w(0,_2q,_8));}))),_2m,_2q,0,_)),_2t=_2s,_2u=E(_2r),_2v=_2u[1],_2w=B(A(_1,[function(_2x,_2y,_2z,_){var _2A=B(_1Q(_2o,B(_11(_2y)),new T(function(){return B(_l(_2n,new T(function(){return B(_w(0,_2q,_8));})));}),_2q,E(_2x)[1],_)),_2B=_2A;return new F(function(){return A(_2z,[_]);});},_1P,_1O,_2v,_])),_2C=_2w;return new F(function(){return _23([0,_2q],_2v,_2u[2],_);});},_2D=function(_2E,_2F,_2G,_){var _2H=B(_2p(E(_2E)[1],_2F,_)),_2I=_2H;return new F(function(){return A(_2G,[_]);});},_2J=new T(function(){return B(unCStr("emptyReserves"));}),_2K=function(_2L,_2M,_2N,_){var _2O=placeCard_ffi(toJSStr(E(_2L)),toJSStr(E(_2M)),toJSStr(E(_2N)),E(_e)[1],20);return _0;},_2P=new T(function(){return B(unCStr("hiddenReserves"));}),_2Q=function(_2R,_){while(1){var _2S=E(_2R);if(!_2S[0]){return _0;}else{var _2T=B(_2K(_2o,B(_11(_2S[1])),_2P,_)),_2U=_2T;_2R=_2S[2];continue;}}},_2V=new T(function(){return B(unCStr("base_only_reserves"));}),_2W=function(_2X,_){var _2Y=B(_2K(_1b,_2V,_2J,_)),_2Z=_2Y;return new F(function(){return _2Q(_2X,_);});},_30=function(_31,_32,_33,_34,_){var _35=B(A(_1,[_2D,_1P,_1O,_32,_])),_36=_35,_37=B(A(_1,[_1C,_1P,_1O,_31,_])),_38=_37,_39=B(_1h(_33,_)),_3a=_39;return new F(function(){return _2W(_34,_);});},_3b=new T(function(){return B(unCStr(": empty list"));}),_3c=new T(function(){return B(unCStr("Prelude."));}),_3d=function(_3e){return new F(function(){return err(B(_l(_3c,new T(function(){return B(_l(_3e,_3b));}))));});},_3f=new T(function(){return B(unCStr("head"));}),_3g=new T(function(){return B(_3d(_3f));}),_3h=function(_3i,_3j){while(1){var _3k=E(_3i);if(!_3k){return E(_3j);}else{var _3l=E(_3j);if(!_3l[0]){return [0];}else{_3i=_3k-1|0;_3j=_3l[2];continue;}}}},_3m=function(_3n,_3o,_3p){var _3q=E(_3n);if(!_3q[0]){return [0];}else{var _3r=E(_3o);if(!_3r[0]){return [0];}else{var _3s=E(_3p);return _3s[0]==0?[0]:[1,[0,[1,_3r[1],new T(function(){return E(E(_3q[1])[1]);})],_3s[1]],new T(function(){return B(_3m(_3q[2],_3r[2],_3s[2]));})];}}},_3t=function(_3u,_3v){var _3w=E(_3v);return _3w[0]==0?[0]:[1,new T(function(){return B(A(_3u,[_3w[1]]));}),new T(function(){return B(_3t(_3u,_3w[2]));})];},_3x=new T(function(){return B(unCStr("tail"));}),_3y=new T(function(){return B(_3d(_3x));}),_3z=function(_3A){return E(E(_3A)[2]);},_3B=function(_3C,_3D){var _3E=E(_3C);if(!_3E[0]){return [0,_8,_3D];}else{var _3F=_3E[1],_3G=_3E[2],_3H=new T(function(){var _3I=E(_3D);if(!_3I[0]){var _3J=E(_3y);}else{var _3K=B(_3B(B(_3m(_3G,_3I[2],new T(function(){return B(_3t(_3z,_3G));}))),new T(function(){var _3L=B(_1X(_3E,0));return _3L>=0?B(_3h(_3L,_3I)):E(_3I);}))),_3J=[0,_3K[1],_3K[2]];}return _3J;});return [0,[1,[0,new T(function(){return E(E(_3F)[1]);}),[1,new T(function(){var _3M=E(_3D);return _3M[0]==0?E(_3g):E(_3M[1]);}),new T(function(){return E(E(_3F)[2]);})]],new T(function(){return E(E(_3H)[1]);})],new T(function(){return E(E(_3H)[2]);})];}},_3N=[0],_3O=new T(function(){return [0,"(function(){return md51(jsRand().toString());})"];}),_3P=function(_3Q){var _3R=B(A(_3Q,[_])),_3S=_3R;return E(_3S);},_3T=function(_3U){return new F(function(){return _3P(function(_){var _=0;return new F(function(){return eval(E(_3U)[1]);});});});},_3V=function(_){return new F(function(){return A(_3T,[_3O,_]);});},_3W=function(_){return new F(function(){return _3V(_);});},_3X=[0,41],_3Y=[1,_3X,_8],_3Z=new T(function(){return B(_w(0,12,_3Y));}),_40=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_3Z));}),_41=function(_42){return new F(function(){return err(B(unAppCStr("toEnum{Rank}: tag (",new T(function(){return B(_w(0,_42,_40));}))));});},_43=function(_44,_45){var _46=dataToTag(E(E(_45))),_47=dataToTag(E(E(_44)));if(_47<=_46){var _48=function(_49){return [1,new T(function(){return _49<0?B(_41(_49)):_49>12?B(_41(_49)):_49;}),new T(function(){if(_49!=_46){var _4a=B(_48(_49+1|0));}else{var _4a=[0];}return _4a;})];};return new F(function(){return _48(_47);});}else{return [0];}},_4b=0,_4c=12,_4d=new T(function(){return B(_43(_4b,_4c));}),_4e=[1,_3X,_8],_4f=new T(function(){return B(_w(0,3,_4e));}),_4g=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_4f));}),_4h=function(_4i){return new F(function(){return err(B(unAppCStr("toEnum{Suit}: tag (",new T(function(){return B(_w(0,_4i,_4g));}))));});},_4j=function(_4k){return [1,new T(function(){return _4k<0?B(_4h(_4k)):_4k>3?B(_4h(_4k)):_4k;}),new T(function(){var _4l=E(_4k);if(_4l==3){var _4m=[0];}else{var _4m=B(_4j(_4l+1|0));}return _4m;})];},_4n=new T(function(){return B(_4j(0));}),_4o=function(_4p){var _4q=E(_4p);if(!_4q[0]){return [0];}else{var _4r=function(_4s){var _4t=E(_4s);return _4t[0]==0?E(new T(function(){return B(_4o(_4q[2]));})):[1,[0,_4q[1],_4t[1]],new T(function(){return B(_4r(_4t[2]));})];};return new F(function(){return _4r(_4n);});}},_4u=new T(function(){return B(_4o(_4d));}),_4v=[0,_8,_8],_4w=[1,_4v,_8],_4x=function(_4y){return _4y>1?[1,_4v,new T(function(){return B(_4x(_4y-1|0));})]:E(_4w);},_4z=new T(function(){return B(_4x(7));}),_4A=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_4B=new T(function(){return B(err(_4A));}),_4C=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_4D=new T(function(){return B(err(_4C));}),_4E=function(_4F,_4G){while(1){var _4H=E(_4F);if(!_4H[0]){return E(_4D);}else{var _4I=E(_4G);if(!_4I){return E(_4H[1]);}else{_4F=_4H[2];_4G=_4I-1|0;continue;}}}},_4J=function(_4K,_4L){return E(_4K)[1]!=E(_4L)[1];},_4M=function(_4N,_4O){return E(_4N)[1]==E(_4O)[1];},_4P=[0,_4M,_4J],_4Q=function(_4R,_4S,_4T){var _4U=[1,[1,_4T,new T(function(){return _4S>=0?B(_4E(_4R,_4S)):E(_4B);})],new T(function(){var _4V=_4S+1|0;return _4V>=0?B(_3h(_4V,_4R)):E(_4R);})];if(_4S>0){var _4W=function(_4X,_4Y){var _4Z=E(_4X);if(!_4Z[0]){return E(_4U);}else{var _50=_4Z[1];return _4Y>1?[1,_50,new T(function(){return B(_4W(_4Z[2],_4Y-1|0));})]:[1,_50,_4U];}};return new F(function(){return _4W(_4R,_4S);});}else{return E(_4U);}},_51=[0,_8,_8],_52=function(_53,_54,_55,_56,_57,_58){return [0,_53,new T(function(){var _59=E(_58)[1],_5a=new T(function(){var _5b=E(_57)[1];return _5b>=0?B(_4E(_54,_5b)):E(_4B);}),_5c=new T(function(){var _5d=E(_57)[1],_5e=[1,new T(function(){var _5f=E(E(_5a)[1]);return _5f[0]==0?E(_51):[0,_5f[2],[1,_5f[1],_8]];}),new T(function(){var _5g=_5d+1|0;return _5g>=0?B(_3h(_5g,_54)):E(_54);})];if(_5d>0){var _5h=function(_5i,_5j){var _5k=E(_5i);if(!_5k[0]){return E(_5e);}else{var _5l=_5k[1];return _5j>1?[1,_5l,new T(function(){return B(_5h(_5k[2],_5j-1|0));})]:[1,_5l,_5e];}},_5m=B(_5h(_54,_5d));}else{var _5m=E(_5e);}var _5n=_5m;return _5n;}),_5o=new T(function(){return _59>=0?B(_4E(_5c,_59)):E(_4B);}),_5p=[1,[0,new T(function(){return E(E(_5o)[1]);}),new T(function(){return B(_l(E(_5a)[2],new T(function(){return E(E(_5o)[2]);})));})],new T(function(){var _5q=_59+1|0;return _5q>=0?B(_3h(_5q,_5c)):E(_5c);})];if(_59>0){var _5r=function(_5s,_5t){var _5u=E(_5s);if(!_5u[0]){return E(_5p);}else{var _5v=_5u[1];return _5t>1?[1,_5v,new T(function(){return B(_5r(_5u[2],_5t-1|0));})]:[1,_5v,_5p];}},_5w=B(_5r(_5c,_59));}else{var _5w=E(_5p);}var _5x=_5w;return _5x;}),_55,_56];},_5y=function(_5z,_5A,_5B,_5C,_5D,_5E){var _5F=new T(function(){var _5G=E(_5D)[1];return _5G>=0?B(_4E(_5A,_5G)):E(_4B);}),_5H=new T(function(){return E(E(_5F)[2]);});return [0,new T(function(){return B(_4Q(_5z,E(_5E)[1],new T(function(){var _5I=E(_5H);return _5I[0]==0?E(_3g):E(_5I[1]);})));}),new T(function(){var _5J=E(_5D)[1],_5K=[1,new T(function(){var _5L=E(E(_5F)[1]);if(!_5L[0]){var _5M=[0,_8,new T(function(){var _5N=E(_5H);return _5N[0]==0?E(_3y):E(_5N[2]);})];}else{var _5O=E(_5H);if(!_5O[0]){var _5P=E(_3y);}else{var _5Q=E(_5O[2]),_5P=_5Q[0]==0?[0,_5L[2],[1,_5L[1],_8]]:[0,_5L,_5Q];}var _5M=_5P;}var _5R=_5M;return _5R;}),new T(function(){var _5S=_5J+1|0;return _5S>=0?B(_3h(_5S,_5A)):E(_5A);})];if(_5J>0){var _5T=function(_5U,_5V){var _5W=E(_5U);if(!_5W[0]){return E(_5K);}else{var _5X=_5W[1];return _5V>1?[1,_5X,new T(function(){return B(_5T(_5W[2],_5V-1|0));})]:[1,_5X,_5K];}},_5Y=B(_5T(_5A,_5J));}else{var _5Y=E(_5K);}var _5Z=_5Y;return _5Z;}),_5B,_5C];},_60=function(_61,_62,_63,_64,_65){return [0,_61,new T(function(){var _66=E(_65)[1],_67=new T(function(){return _66>=0?B(_4E(_62,_66)):E(_4B);}),_68=[1,[0,new T(function(){return E(E(_67)[1]);}),[1,new T(function(){var _69=E(_63);return _69[0]==0?E(_3g):E(_69[1]);}),new T(function(){return E(E(_67)[2]);})]],new T(function(){var _6a=_66+1|0;return _6a>=0?B(_3h(_6a,_62)):E(_62);})];if(_66>0){var _6b=function(_6c,_6d){var _6e=E(_6c);if(!_6e[0]){return E(_68);}else{var _6f=_6e[1];return _6d>1?[1,_6f,new T(function(){return B(_6b(_6e[2],_6d-1|0));})]:[1,_6f,_68];}},_6g=B(_6b(_62,_66));}else{var _6g=E(_68);}var _6h=_6g;return _6h;}),new T(function(){var _6i=E(_63);return _6i[0]==0?E(_3y):E(_6i[2]);}),_64];},_6j=new T(function(){return B(unCStr("Control.Exception.Base"));}),_6k=new T(function(){return B(unCStr("base"));}),_6l=new T(function(){return B(unCStr("PatternMatchFail"));}),_6m=new T(function(){var _6n=hs_wordToWord64(18445595),_6o=_6n,_6p=hs_wordToWord64(52003073),_6q=_6p;return [0,_6o,_6q,[0,_6o,_6q,_6k,_6j,_6l],_8];}),_6r=function(_6s){return E(_6m);},_6t=function(_6u){return E(E(_6u)[1]);},_6v=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_6w=new T(function(){return B(err(_6v));}),_6x=function(_6y,_6z,_6A){var _6B=new T(function(){var _6C=B(A(_6y,[_6A])),_6D=B(A(_6z,[new T(function(){var _6E=E(_6B);return _6E[0]==0?E(_6w):E(_6E[1]);})])),_6F=hs_eqWord64(_6C[1],_6D[1]),_6G=_6F;if(!E(_6G)){var _6H=[0];}else{var _6I=hs_eqWord64(_6C[2],_6D[2]),_6J=_6I,_6H=E(_6J)==0?[0]:[1,_6A];}var _6K=_6H,_6L=_6K;return _6L;});return E(_6B);},_6M=function(_6N){var _6O=E(_6N);return new F(function(){return _6x(B(_6t(_6O[1])),_6r,_6O[2]);});},_6P=function(_6Q){return E(E(_6Q)[1]);},_6R=function(_6S,_6T){return new F(function(){return _l(E(_6S)[1],_6T);});},_6U=[0,44],_6V=[0,93],_6W=[0,91],_6X=function(_6Y,_6Z,_70){var _71=E(_6Z);return _71[0]==0?B(unAppCStr("[]",_70)):[1,_6W,new T(function(){return B(A(_6Y,[_71[1],new T(function(){var _72=function(_73){var _74=E(_73);return _74[0]==0?E([1,_6V,_70]):[1,_6U,new T(function(){return B(A(_6Y,[_74[1],new T(function(){return B(_72(_74[2]));})]));})];};return B(_72(_71[2]));})]));})];},_75=function(_76,_77){return new F(function(){return _6X(_6R,_76,_77);});},_78=function(_79,_7a,_7b){return new F(function(){return _l(E(_7a)[1],_7b);});},_7c=[0,_78,_6P,_75],_7d=new T(function(){return [0,_6r,_7c,_7e,_6M];}),_7e=function(_7f){return [0,_7d,_7f];},_7g=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_7h=function(_7i,_7j){return new F(function(){return die(new T(function(){return B(A(_7j,[_7i]));}));});},_7k=function(_7l,_7m){var _7n=E(_7m);if(!_7n[0]){return [0,_8,_8];}else{var _7o=_7n[1];if(!B(A(_7l,[_7o]))){return [0,_8,_7n];}else{var _7p=new T(function(){var _7q=B(_7k(_7l,_7n[2]));return [0,_7q[1],_7q[2]];});return [0,[1,_7o,new T(function(){return E(E(_7p)[1]);})],new T(function(){return E(E(_7p)[2]);})];}}},_7r=[0,32],_7s=[0,10],_7t=[1,_7s,_8],_7u=function(_7v){return E(E(_7v)[1])==124?false:true;},_7w=function(_7x,_7y){var _7z=B(_7k(_7u,B(unCStr(_7x)))),_7A=_7z[1],_7B=function(_7C,_7D){return new F(function(){return _l(_7C,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_l(_7y,new T(function(){return B(_l(_7D,_7t));})));})));}));});},_7E=E(_7z[2]);if(!_7E[0]){return new F(function(){return _7B(_7A,_8);});}else{return E(E(_7E[1])[1])==124?B(_7B(_7A,[1,_7r,_7E[2]])):B(_7B(_7A,_8));}},_7F=function(_7G){return new F(function(){return _7h([0,new T(function(){return B(_7w(_7G,_7g));})],_7e);});},_7H=new T(function(){return B(_7F("Game.hs:(47,1)-(49,86)|function goesOnColumn"));}),_7I=function(_7J,_7K,_7L,_7M){var _7N=new T(function(){var _7O=E(_7M);if(!_7O[0]){var _7P=E(_7H);}else{var _7Q=_7O[1];switch(E(_7K)){case 2:var _7R=E(_7Q);switch(E(_7R[2])){case 2:var _7S=false;break;case 3:var _7S=false;break;default:var _7S=(dataToTag(E(E(_7J)))+1|0)==dataToTag(E(E(_7R[1])));}var _7T=_7S,_7U=_7T;break;case 3:var _7V=E(_7Q);switch(E(_7V[2])){case 2:var _7W=false;break;case 3:var _7W=false;break;default:var _7W=(dataToTag(E(E(_7J)))+1|0)==dataToTag(E(E(_7V[1])));}var _7X=_7W,_7U=_7X;break;default:var _7Y=E(_7Q),_7Z=_7Y[1];switch(E(_7Y[2])){case 2:var _80=(dataToTag(E(E(_7J)))+1|0)==dataToTag(E(E(_7Z)));break;case 3:var _80=(dataToTag(E(E(_7J)))+1|0)==dataToTag(E(E(_7Z)));break;default:var _80=false;}var _81=_80,_7U=_81;}var _7P=_7U;}return _7P;});return E(_7L)[0]==0?E(_7M)[0]==0?dataToTag(E(_7J))==12?true:false:E(_7N):E(_7N);},_82=function(_83,_84,_85){var _86=E(_85);if(!_86[0]){return dataToTag(E(_83))==0?true:false;}else{var _87=_86[1];switch(E(_84)){case 0:var _88=E(_87);switch(E(_88[2])){case 0:return dataToTag(E(E(_83)))==(dataToTag(E(E(_88[1])))+1|0);case 1:return false;case 2:return false;default:return false;}break;case 1:var _89=E(_87);return E(_89[2])==1?dataToTag(E(E(_83)))==(dataToTag(E(E(_89[1])))+1|0):false;case 2:var _8a=E(_87);return E(_8a[2])==2?dataToTag(E(E(_83)))==(dataToTag(E(E(_8a[1])))+1|0):false;default:var _8b=E(_87);return E(_8b[2])==3?dataToTag(E(E(_83)))==(dataToTag(E(E(_8b[1])))+1|0):false;}}},_8c=function(_){var _8d=deleteByClass_ffi(toJSStr(E(_9))),_8e=deleteByClass_ffi(toJSStr(E(_a)));return _0;},_8f=function(_8g,_){var _8h=deleteByClass_ffi(toJSStr(B(_l(_2n,new T(function(){return B(_w(0,E(_8g)[1],_8));}))))),_8i=deleteByClass_ffi(toJSStr(B(_l(_22,new T(function(){return B(_w(0,E(_8g)[1],_8));})))));return _0;},_8j=function(_8k,_){var _8l=E(_8k);return new F(function(){return _30(_8l[1],_8l[2],_8l[3],_8l[4],_);});},_8m=function(_){var _8n=deleteByClass_ffi(toJSStr(E(_2J))),_8o=deleteByClass_ffi(toJSStr(E(_2P)));return _0;},_8p=function(_8q,_8r){while(1){var _8s=E(_8q);if(!_8s[0]){return E(_8r)[0]==0?true:false;}else{var _8t=E(_8r);if(!_8t[0]){return false;}else{if(E(_8s[1])[1]!=E(_8t[1])[1]){return false;}else{_8q=_8s[2];_8r=_8t[2];continue;}}}}},_8u=function(_8v){return E(E(_8v)[1]);},_8w=function(_8x,_8y,_8z){while(1){var _8A=E(_8y);if(!_8A[0]){return true;}else{var _8B=E(_8z);if(!_8B[0]){return false;}else{if(!B(A(_8u,[_8x,_8A[1],_8B[1]]))){return false;}else{_8y=_8A[2];_8z=_8B[2];continue;}}}}},_8C=function(_8D,_8E){while(1){var _8F=E(_8E);if(!_8F[0]){return E(_8D);}else{_8D=_8F[1];_8E=_8F[2];continue;}}},_8G=new T(function(){return B(unCStr("last"));}),_8H=new T(function(){return B(_3d(_8G));}),_8I=new T(function(){return [0,E(_f)[1]+90|0];}),_8J=[1,_a],_8K=[1,_2P],_8L=[0,6],_8M=[0,47],_8N=new T(function(){return B(_7F("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_8O=function(_8P,_8Q){while(1){var _8R=(function(_8S,_8T){var _8U=E(_8S);switch(_8U[0]){case 0:var _8V=E(_8T);if(!_8V[0]){return [0];}else{_8P=B(A(_8U[1],[_8V[1]]));_8Q=_8V[2];return null;}break;case 1:var _8W=B(A(_8U[1],[_8T])),_8X=_8T;_8P=_8W;_8Q=_8X;return null;case 2:return [0];case 3:return [1,[0,_8U[1],_8T],new T(function(){return B(_8O(_8U[2],_8T));})];default:return E(_8U[1]);}})(_8P,_8Q);if(_8R!=null){return _8R;}}},_8Y=function(_8Z,_90){var _91=new T(function(){var _92=E(_90);if(_92[0]==3){var _93=[3,_92[1],new T(function(){return B(_8Y(_8Z,_92[2]));})];}else{var _94=E(_8Z);if(_94[0]==2){var _95=E(_92);}else{var _96=E(_92);if(_96[0]==2){var _97=E(_94);}else{var _98=new T(function(){var _99=E(_96);if(_99[0]==4){var _9a=[1,function(_9b){return [4,new T(function(){return B(_l(B(_8O(_94,_9b)),_99[1]));})];}];}else{var _9c=E(_94);if(_9c[0]==1){var _9d=_9c[1],_9e=E(_99);if(!_9e[0]){var _9f=[1,function(_9g){return new F(function(){return _8Y(B(A(_9d,[_9g])),_9e);});}];}else{var _9f=[1,function(_9h){return new F(function(){return _8Y(B(A(_9d,[_9h])),new T(function(){return B(A(_9e[1],[_9h]));}));});}];}var _9i=_9f;}else{var _9j=E(_99);if(!_9j[0]){var _9k=E(_8N);}else{var _9k=[1,function(_9l){return new F(function(){return _8Y(_9c,new T(function(){return B(A(_9j[1],[_9l]));}));});}];}var _9i=_9k;}var _9a=_9i;}return _9a;}),_9m=E(_94);switch(_9m[0]){case 1:var _9n=E(_96);if(_9n[0]==4){var _9o=[1,function(_9p){return [4,new T(function(){return B(_l(B(_8O(B(A(_9m[1],[_9p])),_9p)),_9n[1]));})];}];}else{var _9o=E(_98);}var _9q=_9o;break;case 4:var _9r=_9m[1],_9s=E(_96);switch(_9s[0]){case 0:var _9t=[1,function(_9u){return [4,new T(function(){return B(_l(_9r,new T(function(){return B(_8O(_9s,_9u));})));})];}];break;case 1:var _9t=[1,function(_9v){return [4,new T(function(){return B(_l(_9r,new T(function(){return B(_8O(B(A(_9s[1],[_9v])),_9v));})));})];}];break;default:var _9t=[4,new T(function(){return B(_l(_9r,_9s[1]));})];}var _9q=_9t;break;default:var _9q=E(_98);}var _97=_9q;}var _95=_97;}var _93=_95;}return _93;}),_9w=E(_8Z);switch(_9w[0]){case 0:var _9x=E(_90);return _9x[0]==0?[0,function(_9y){return new F(function(){return _8Y(B(A(_9w[1],[_9y])),new T(function(){return B(A(_9x[1],[_9y]));}));});}]:E(_91);case 3:return [3,_9w[1],new T(function(){return B(_8Y(_9w[2],_90));})];default:return E(_91);}},_9z=function(_9A,_9B,_9C){while(1){var _9D=E(_9B);if(!_9D[0]){return E(_9C)[0]==0?true:false;}else{var _9E=E(_9C);if(!_9E[0]){return false;}else{if(!B(A(_8u,[_9A,_9D[1],_9E[1]]))){return false;}else{_9B=_9D[2];_9C=_9E[2];continue;}}}}},_9F=function(_9G,_9H,_9I){return !B(_9z(_9G,_9H,_9I))?true:false;},_9J=function(_9K){return [0,function(_9L,_9M){return new F(function(){return _9z(_9K,_9L,_9M);});},function(_9L,_9M){return new F(function(){return _9F(_9K,_9L,_9M);});}];},_9N=new T(function(){return B(_9J(_4P));}),_9O=function(_9P,_9Q){var _9R=E(_9P);switch(_9R[0]){case 0:return [0,function(_9S){return new F(function(){return _9O(B(A(_9R[1],[_9S])),_9Q);});}];case 1:return [1,function(_9T){return new F(function(){return _9O(B(A(_9R[1],[_9T])),_9Q);});}];case 2:return [2];case 3:return new F(function(){return _8Y(B(A(_9Q,[_9R[1]])),new T(function(){return B(_9O(_9R[2],_9Q));}));});break;default:var _9U=function(_9V){var _9W=E(_9V);if(!_9W[0]){return [0];}else{var _9X=E(_9W[1]);return new F(function(){return _l(B(_8O(B(A(_9Q,[_9X[1]])),_9X[2])),new T(function(){return B(_9U(_9W[2]));}));});}},_9Y=B(_9U(_9R[1]));return _9Y[0]==0?[2]:[4,_9Y];}},_9Z=[2],_a0=function(_a1){return [3,_a1,_9Z];},_a2=function(_a3,_a4){var _a5=E(_a3);if(!_a5){return new F(function(){return A(_a4,[_0]);});}else{return [0,function(_a6){return E(new T(function(){return B(_a2(_a5-1|0,_a4));}));}];}},_a7=function(_a8,_a9,_aa){return [1,function(_ab){return new F(function(){return A(function(_ac,_ad,_ae){while(1){var _af=(function(_ag,_ah,_ai){var _aj=E(_ag);switch(_aj[0]){case 0:var _ak=E(_ah);if(!_ak[0]){return E(_a9);}else{_ac=B(A(_aj[1],[_ak[1]]));_ad=_ak[2];var _al=_ai+1|0;_ae=_al;return null;}break;case 1:var _am=B(A(_aj[1],[_ah])),_an=_ah,_al=_ai;_ac=_am;_ad=_an;_ae=_al;return null;case 2:return E(_a9);case 3:return function(_ao){return new F(function(){return _a2(_ai,function(_ap){return E(new T(function(){return B(_9O(_aj,_ao));}));});});};default:return function(_aq){return new F(function(){return _9O(_aj,_aq);});};}})(_ac,_ad,_ae);if(_af!=null){return _af;}}},[new T(function(){return B(A(_a8,[_a0]));}),_ab,0,_aa]);});}];},_ar=[6],_as=function(_at){return E(_at);},_au=new T(function(){return B(unCStr("valDig: Bad base"));}),_av=new T(function(){return B(err(_au));}),_aw=function(_ax,_ay){var _az=function(_aA,_aB){var _aC=E(_aA);if(!_aC[0]){return function(_aD){return new F(function(){return A(_aD,[new T(function(){return B(A(_aB,[_8]));})]);});};}else{var _aE=E(_aC[1])[1],_aF=function(_aG){return function(_aH){return [0,function(_aI){return E(new T(function(){return B(A(new T(function(){return B(_az(_aC[2],function(_aJ){return new F(function(){return A(_aB,[[1,_aG,_aJ]]);});}));}),[_aH]));}));}];};};switch(E(E(_ax)[1])){case 8:if(48>_aE){return function(_aK){return new F(function(){return A(_aK,[new T(function(){return B(A(_aB,[_8]));})]);});};}else{if(_aE>55){return function(_aL){return new F(function(){return A(_aL,[new T(function(){return B(A(_aB,[_8]));})]);});};}else{return new F(function(){return _aF([0,_aE-48|0]);});}}break;case 10:if(48>_aE){return function(_aM){return new F(function(){return A(_aM,[new T(function(){return B(A(_aB,[_8]));})]);});};}else{if(_aE>57){return function(_aN){return new F(function(){return A(_aN,[new T(function(){return B(A(_aB,[_8]));})]);});};}else{return new F(function(){return _aF([0,_aE-48|0]);});}}break;case 16:var _aO=new T(function(){if(97>_aE){if(65>_aE){var _aP=[0];}else{if(_aE>70){var _aQ=[0];}else{var _aQ=[1,[0,(_aE-65|0)+10|0]];}var _aP=_aQ;}var _aR=_aP;}else{if(_aE>102){if(65>_aE){var _aS=[0];}else{if(_aE>70){var _aT=[0];}else{var _aT=[1,[0,(_aE-65|0)+10|0]];}var _aS=_aT;}var _aU=_aS;}else{var _aU=[1,[0,(_aE-97|0)+10|0]];}var _aR=_aU;}return _aR;});if(48>_aE){var _aV=E(_aO);if(!_aV[0]){return function(_aW){return new F(function(){return A(_aW,[new T(function(){return B(A(_aB,[_8]));})]);});};}else{return new F(function(){return _aF(_aV[1]);});}}else{if(_aE>57){var _aX=E(_aO);if(!_aX[0]){return function(_aY){return new F(function(){return A(_aY,[new T(function(){return B(A(_aB,[_8]));})]);});};}else{return new F(function(){return _aF(_aX[1]);});}}else{return new F(function(){return _aF([0,_aE-48|0]);});}}break;default:return E(_av);}}};return [1,function(_aZ){return new F(function(){return A(_az,[_aZ,_as,function(_b0){var _b1=E(_b0);return _b1[0]==0?[2]:B(A(_ay,[_b1]));}]);});}];},_b2=[0,10],_b3=[0,1],_b4=[0,2147483647],_b5=function(_b6,_b7){while(1){var _b8=E(_b6);if(!_b8[0]){var _b9=_b8[1],_ba=E(_b7);if(!_ba[0]){var _bb=_ba[1],_bc=addC(_b9,_bb);if(!E(_bc[2])){return [0,_bc[1]];}else{_b6=[1,I_fromInt(_b9)];_b7=[1,I_fromInt(_bb)];continue;}}else{_b6=[1,I_fromInt(_b9)];_b7=_ba;continue;}}else{var _bd=E(_b7);if(!_bd[0]){_b6=_b8;_b7=[1,I_fromInt(_bd[1])];continue;}else{return [1,I_add(_b8[1],_bd[1])];}}}},_be=new T(function(){return B(_b5(_b4,_b3));}),_bf=function(_bg){var _bh=E(_bg);if(!_bh[0]){var _bi=E(_bh[1]);return _bi==(-2147483648)?E(_be):[0, -_bi];}else{return [1,I_negate(_bh[1])];}},_bj=[0,10],_bk=[0,0],_bl=function(_bm){return [0,_bm];},_bn=function(_bo,_bp){while(1){var _bq=E(_bo);if(!_bq[0]){var _br=_bq[1],_bs=E(_bp);if(!_bs[0]){var _bt=_bs[1];if(!(imul(_br,_bt)|0)){return [0,imul(_br,_bt)|0];}else{_bo=[1,I_fromInt(_br)];_bp=[1,I_fromInt(_bt)];continue;}}else{_bo=[1,I_fromInt(_br)];_bp=_bs;continue;}}else{var _bu=E(_bp);if(!_bu[0]){_bo=_bq;_bp=[1,I_fromInt(_bu[1])];continue;}else{return [1,I_mul(_bq[1],_bu[1])];}}}},_bv=function(_bw,_bx,_by){while(1){var _bz=E(_by);if(!_bz[0]){return E(_bx);}else{var _bA=B(_b5(B(_bn(_bx,_bw)),B(_bl(E(_bz[1])[1]))));_by=_bz[2];_bx=_bA;continue;}}},_bB=function(_bC){var _bD=new T(function(){return B(_8Y(B(_8Y([0,function(_bE){if(E(E(_bE)[1])==45){return new F(function(){return _aw(_b2,function(_bF){return new F(function(){return A(_bC,[[1,new T(function(){return B(_bf(B(_bv(_bj,_bk,_bF))));})]]);});});});}else{return [2];}}],[0,function(_bG){if(E(E(_bG)[1])==43){return new F(function(){return _aw(_b2,function(_bH){return new F(function(){return A(_bC,[[1,new T(function(){return B(_bv(_bj,_bk,_bH));})]]);});});});}else{return [2];}}])),new T(function(){return B(_aw(_b2,function(_bI){return new F(function(){return A(_bC,[[1,new T(function(){return B(_bv(_bj,_bk,_bI));})]]);});}));})));});return new F(function(){return _8Y([0,function(_bJ){return E(E(_bJ)[1])==101?E(_bD):[2];}],[0,function(_bK){return E(E(_bK)[1])==69?E(_bD):[2];}]);});},_bL=function(_bM){return new F(function(){return A(_bM,[_3N]);});},_bN=function(_bO){return new F(function(){return A(_bO,[_3N]);});},_bP=function(_bQ){return [0,function(_bR){return E(E(_bR)[1])==46?E(new T(function(){return B(_aw(_b2,function(_bS){return new F(function(){return A(_bQ,[[1,_bS]]);});}));})):[2];}];},_bT=function(_bU){return new F(function(){return _aw(_b2,function(_bV){return new F(function(){return _a7(_bP,_bL,function(_bW){return new F(function(){return _a7(_bB,_bN,function(_bX){return new F(function(){return A(_bU,[[5,[1,_bV,_bW,_bX]]]);});});});});});});});},_bY=function(_bZ,_c0,_c1){while(1){var _c2=E(_c1);if(!_c2[0]){return false;}else{if(!B(A(_8u,[_bZ,_c0,_c2[1]]))){_c1=_c2[2];continue;}else{return true;}}}},_c3=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_c4=function(_c5){return new F(function(){return _bY(_4P,_c5,_c3);});},_c6=[0,8],_c7=[0,16],_c8=function(_c9){return [0,function(_ca){return E(E(_ca)[1])==48?E([0,function(_cb){switch(E(E(_cb)[1])){case 79:return E(new T(function(){return B(_aw(_c6,function(_cc){return new F(function(){return A(_c9,[[5,[0,_c6,_cc]]]);});}));}));case 88:return E(new T(function(){return B(_aw(_c7,function(_cd){return new F(function(){return A(_c9,[[5,[0,_c7,_cd]]]);});}));}));case 111:return E(new T(function(){return B(_aw(_c6,function(_ce){return new F(function(){return A(_c9,[[5,[0,_c6,_ce]]]);});}));}));case 120:return E(new T(function(){return B(_aw(_c7,function(_cf){return new F(function(){return A(_c9,[[5,[0,_c7,_cf]]]);});}));}));default:return [2];}}]):[2];}];},_cg=false,_ch=true,_ci=function(_cj){return [0,function(_ck){switch(E(E(_ck)[1])){case 79:return E(new T(function(){return B(A(_cj,[_c6]));}));case 88:return E(new T(function(){return B(A(_cj,[_c7]));}));case 111:return E(new T(function(){return B(A(_cj,[_c6]));}));case 120:return E(new T(function(){return B(A(_cj,[_c7]));}));default:return [2];}}];},_cl=function(_cm){return new F(function(){return A(_cm,[_b2]);});},_cn=function(_co){var _cp=E(_co);return _cp[0]==0?E(_cp[1]):I_toInt(_cp[1]);},_cq=function(_cr,_cs){var _ct=E(_cr);if(!_ct[0]){var _cu=_ct[1],_cv=E(_cs);return _cv[0]==0?_cu<=_cv[1]:I_compareInt(_cv[1],_cu)>=0;}else{var _cw=_ct[1],_cx=E(_cs);return _cx[0]==0?I_compareInt(_cw,_cx[1])<=0:I_compare(_cw,_cx[1])<=0;}},_cy=function(_cz){return [2];},_cA=function(_cB){var _cC=E(_cB);if(!_cC[0]){return E(_cy);}else{var _cD=_cC[1],_cE=E(_cC[2]);return _cE[0]==0?E(_cD):function(_cF){return new F(function(){return _8Y(B(A(_cD,[_cF])),new T(function(){return B(A(new T(function(){return B(_cA(_cE));}),[_cF]));}));});};}},_cG=new T(function(){return B(unCStr("NUL"));}),_cH=function(_cI){return [2];},_cJ=function(_cK){return new F(function(){return _cH(_cK);});},_cL=function(_cM,_cN){var _cO=function(_cP,_cQ){var _cR=E(_cP);if(!_cR[0]){return function(_cS){return new F(function(){return A(_cS,[_cM]);});};}else{var _cT=E(_cQ);return _cT[0]==0?E(_cH):E(_cR[1])[1]!=E(_cT[1])[1]?E(_cJ):function(_cU){return [0,function(_cV){return E(new T(function(){return B(A(new T(function(){return B(_cO(_cR[2],_cT[2]));}),[_cU]));}));}];};}};return [1,function(_cW){return new F(function(){return A(_cO,[_cM,_cW,_cN]);});}];},_cX=[0,0],_cY=function(_cZ){return new F(function(){return _cL(_cG,function(_d0){return E(new T(function(){return B(A(_cZ,[_cX]));}));});});},_d1=new T(function(){return B(unCStr("STX"));}),_d2=[0,2],_d3=function(_d4){return new F(function(){return _cL(_d1,function(_d5){return E(new T(function(){return B(A(_d4,[_d2]));}));});});},_d6=new T(function(){return B(unCStr("ETX"));}),_d7=[0,3],_d8=function(_d9){return new F(function(){return _cL(_d6,function(_da){return E(new T(function(){return B(A(_d9,[_d7]));}));});});},_db=new T(function(){return B(unCStr("EOT"));}),_dc=[0,4],_dd=function(_de){return new F(function(){return _cL(_db,function(_df){return E(new T(function(){return B(A(_de,[_dc]));}));});});},_dg=new T(function(){return B(unCStr("ENQ"));}),_dh=[0,5],_di=function(_dj){return new F(function(){return _cL(_dg,function(_dk){return E(new T(function(){return B(A(_dj,[_dh]));}));});});},_dl=new T(function(){return B(unCStr("ACK"));}),_dm=[0,6],_dn=function(_do){return new F(function(){return _cL(_dl,function(_dp){return E(new T(function(){return B(A(_do,[_dm]));}));});});},_dq=new T(function(){return B(unCStr("BEL"));}),_dr=[0,7],_ds=function(_dt){return new F(function(){return _cL(_dq,function(_du){return E(new T(function(){return B(A(_dt,[_dr]));}));});});},_dv=new T(function(){return B(unCStr("BS"));}),_dw=[0,8],_dx=function(_dy){return new F(function(){return _cL(_dv,function(_dz){return E(new T(function(){return B(A(_dy,[_dw]));}));});});},_dA=new T(function(){return B(unCStr("HT"));}),_dB=[0,9],_dC=function(_dD){return new F(function(){return _cL(_dA,function(_dE){return E(new T(function(){return B(A(_dD,[_dB]));}));});});},_dF=new T(function(){return B(unCStr("LF"));}),_dG=[0,10],_dH=function(_dI){return new F(function(){return _cL(_dF,function(_dJ){return E(new T(function(){return B(A(_dI,[_dG]));}));});});},_dK=new T(function(){return B(unCStr("VT"));}),_dL=[0,11],_dM=function(_dN){return new F(function(){return _cL(_dK,function(_dO){return E(new T(function(){return B(A(_dN,[_dL]));}));});});},_dP=new T(function(){return B(unCStr("FF"));}),_dQ=[0,12],_dR=function(_dS){return new F(function(){return _cL(_dP,function(_dT){return E(new T(function(){return B(A(_dS,[_dQ]));}));});});},_dU=new T(function(){return B(unCStr("CR"));}),_dV=[0,13],_dW=function(_dX){return new F(function(){return _cL(_dU,function(_dY){return E(new T(function(){return B(A(_dX,[_dV]));}));});});},_dZ=new T(function(){return B(unCStr("SI"));}),_e0=[0,15],_e1=function(_e2){return new F(function(){return _cL(_dZ,function(_e3){return E(new T(function(){return B(A(_e2,[_e0]));}));});});},_e4=new T(function(){return B(unCStr("DLE"));}),_e5=[0,16],_e6=function(_e7){return new F(function(){return _cL(_e4,function(_e8){return E(new T(function(){return B(A(_e7,[_e5]));}));});});},_e9=new T(function(){return B(unCStr("DC1"));}),_ea=[0,17],_eb=function(_ec){return new F(function(){return _cL(_e9,function(_ed){return E(new T(function(){return B(A(_ec,[_ea]));}));});});},_ee=new T(function(){return B(unCStr("DC2"));}),_ef=[0,18],_eg=function(_eh){return new F(function(){return _cL(_ee,function(_ei){return E(new T(function(){return B(A(_eh,[_ef]));}));});});},_ej=new T(function(){return B(unCStr("DC3"));}),_ek=[0,19],_el=function(_em){return new F(function(){return _cL(_ej,function(_en){return E(new T(function(){return B(A(_em,[_ek]));}));});});},_eo=new T(function(){return B(unCStr("DC4"));}),_ep=[0,20],_eq=function(_er){return new F(function(){return _cL(_eo,function(_es){return E(new T(function(){return B(A(_er,[_ep]));}));});});},_et=new T(function(){return B(unCStr("NAK"));}),_eu=[0,21],_ev=function(_ew){return new F(function(){return _cL(_et,function(_ex){return E(new T(function(){return B(A(_ew,[_eu]));}));});});},_ey=new T(function(){return B(unCStr("SYN"));}),_ez=[0,22],_eA=function(_eB){return new F(function(){return _cL(_ey,function(_eC){return E(new T(function(){return B(A(_eB,[_ez]));}));});});},_eD=new T(function(){return B(unCStr("ETB"));}),_eE=[0,23],_eF=function(_eG){return new F(function(){return _cL(_eD,function(_eH){return E(new T(function(){return B(A(_eG,[_eE]));}));});});},_eI=new T(function(){return B(unCStr("CAN"));}),_eJ=[0,24],_eK=function(_eL){return new F(function(){return _cL(_eI,function(_eM){return E(new T(function(){return B(A(_eL,[_eJ]));}));});});},_eN=new T(function(){return B(unCStr("EM"));}),_eO=[0,25],_eP=function(_eQ){return new F(function(){return _cL(_eN,function(_eR){return E(new T(function(){return B(A(_eQ,[_eO]));}));});});},_eS=new T(function(){return B(unCStr("SUB"));}),_eT=[0,26],_eU=function(_eV){return new F(function(){return _cL(_eS,function(_eW){return E(new T(function(){return B(A(_eV,[_eT]));}));});});},_eX=new T(function(){return B(unCStr("ESC"));}),_eY=[0,27],_eZ=function(_f0){return new F(function(){return _cL(_eX,function(_f1){return E(new T(function(){return B(A(_f0,[_eY]));}));});});},_f2=new T(function(){return B(unCStr("FS"));}),_f3=[0,28],_f4=function(_f5){return new F(function(){return _cL(_f2,function(_f6){return E(new T(function(){return B(A(_f5,[_f3]));}));});});},_f7=new T(function(){return B(unCStr("GS"));}),_f8=[0,29],_f9=function(_fa){return new F(function(){return _cL(_f7,function(_fb){return E(new T(function(){return B(A(_fa,[_f8]));}));});});},_fc=new T(function(){return B(unCStr("RS"));}),_fd=[0,30],_fe=function(_ff){return new F(function(){return _cL(_fc,function(_fg){return E(new T(function(){return B(A(_ff,[_fd]));}));});});},_fh=new T(function(){return B(unCStr("US"));}),_fi=[0,31],_fj=function(_fk){return new F(function(){return _cL(_fh,function(_fl){return E(new T(function(){return B(A(_fk,[_fi]));}));});});},_fm=new T(function(){return B(unCStr("SP"));}),_fn=[0,32],_fo=function(_fp){return new F(function(){return _cL(_fm,function(_fq){return E(new T(function(){return B(A(_fp,[_fn]));}));});});},_fr=new T(function(){return B(unCStr("DEL"));}),_fs=[0,127],_ft=function(_fu){return new F(function(){return _cL(_fr,function(_fv){return E(new T(function(){return B(A(_fu,[_fs]));}));});});},_fw=[1,_ft,_8],_fx=[1,_fo,_fw],_fy=[1,_fj,_fx],_fz=[1,_fe,_fy],_fA=[1,_f9,_fz],_fB=[1,_f4,_fA],_fC=[1,_eZ,_fB],_fD=[1,_eU,_fC],_fE=[1,_eP,_fD],_fF=[1,_eK,_fE],_fG=[1,_eF,_fF],_fH=[1,_eA,_fG],_fI=[1,_ev,_fH],_fJ=[1,_eq,_fI],_fK=[1,_el,_fJ],_fL=[1,_eg,_fK],_fM=[1,_eb,_fL],_fN=[1,_e6,_fM],_fO=[1,_e1,_fN],_fP=[1,_dW,_fO],_fQ=[1,_dR,_fP],_fR=[1,_dM,_fQ],_fS=[1,_dH,_fR],_fT=[1,_dC,_fS],_fU=[1,_dx,_fT],_fV=[1,_ds,_fU],_fW=[1,_dn,_fV],_fX=[1,_di,_fW],_fY=[1,_dd,_fX],_fZ=[1,_d8,_fY],_g0=[1,_d3,_fZ],_g1=[1,_cY,_g0],_g2=new T(function(){return B(unCStr("SOH"));}),_g3=[0,1],_g4=function(_g5){return new F(function(){return _cL(_g2,function(_g6){return E(new T(function(){return B(A(_g5,[_g3]));}));});});},_g7=new T(function(){return B(unCStr("SO"));}),_g8=[0,14],_g9=function(_ga){return new F(function(){return _cL(_g7,function(_gb){return E(new T(function(){return B(A(_ga,[_g8]));}));});});},_gc=function(_gd){return new F(function(){return _a7(_g4,_g9,_gd);});},_ge=[1,_gc,_g1],_gf=new T(function(){return B(_cA(_ge));}),_gg=[0,1114111],_gh=[0,34],_gi=[0,_gh,_ch],_gj=[0,39],_gk=[0,_gj,_ch],_gl=[0,92],_gm=[0,_gl,_ch],_gn=[0,_dr,_ch],_go=[0,_dw,_ch],_gp=[0,_dQ,_ch],_gq=[0,_dG,_ch],_gr=[0,_dV,_ch],_gs=[0,_dB,_ch],_gt=[0,_dL,_ch],_gu=[0,_cX,_ch],_gv=[0,_g3,_ch],_gw=[0,_d2,_ch],_gx=[0,_d7,_ch],_gy=[0,_dc,_ch],_gz=[0,_dh,_ch],_gA=[0,_dm,_ch],_gB=[0,_dr,_ch],_gC=[0,_dw,_ch],_gD=[0,_dB,_ch],_gE=[0,_dG,_ch],_gF=[0,_dL,_ch],_gG=[0,_dQ,_ch],_gH=[0,_dV,_ch],_gI=[0,_g8,_ch],_gJ=[0,_e0,_ch],_gK=[0,_e5,_ch],_gL=[0,_ea,_ch],_gM=[0,_ef,_ch],_gN=[0,_ek,_ch],_gO=[0,_ep,_ch],_gP=[0,_eu,_ch],_gQ=[0,_ez,_ch],_gR=[0,_eE,_ch],_gS=[0,_eJ,_ch],_gT=[0,_eO,_ch],_gU=[0,_eT,_ch],_gV=[0,_eY,_ch],_gW=[0,_f3,_ch],_gX=[0,_f8,_ch],_gY=[0,_fd,_ch],_gZ=[0,_fi,_ch],_h0=function(_h1){return new F(function(){return _8Y([0,function(_h2){switch(E(E(_h2)[1])){case 34:return E(new T(function(){return B(A(_h1,[_gi]));}));case 39:return E(new T(function(){return B(A(_h1,[_gk]));}));case 92:return E(new T(function(){return B(A(_h1,[_gm]));}));case 97:return E(new T(function(){return B(A(_h1,[_gn]));}));case 98:return E(new T(function(){return B(A(_h1,[_go]));}));case 102:return E(new T(function(){return B(A(_h1,[_gp]));}));case 110:return E(new T(function(){return B(A(_h1,[_gq]));}));case 114:return E(new T(function(){return B(A(_h1,[_gr]));}));case 116:return E(new T(function(){return B(A(_h1,[_gs]));}));case 118:return E(new T(function(){return B(A(_h1,[_gt]));}));default:return [2];}}],new T(function(){return B(_8Y(B(_a7(_ci,_cl,function(_h3){return new F(function(){return _aw(_h3,function(_h4){var _h5=B(_bv(new T(function(){return B(_bl(E(_h3)[1]));}),_bk,_h4));return !B(_cq(_h5,_gg))?[2]:B(A(_h1,[[0,new T(function(){var _h6=B(_cn(_h5));if(_h6>>>0>1114111){var _h7=B(_C(_h6));}else{var _h7=[0,_h6];}var _h8=_h7,_h9=_h8;return _h9;}),_ch]]));});});})),new T(function(){return B(_8Y([0,function(_ha){return E(E(_ha)[1])==94?E([0,function(_hb){switch(E(E(_hb)[1])){case 64:return E(new T(function(){return B(A(_h1,[_gu]));}));case 65:return E(new T(function(){return B(A(_h1,[_gv]));}));case 66:return E(new T(function(){return B(A(_h1,[_gw]));}));case 67:return E(new T(function(){return B(A(_h1,[_gx]));}));case 68:return E(new T(function(){return B(A(_h1,[_gy]));}));case 69:return E(new T(function(){return B(A(_h1,[_gz]));}));case 70:return E(new T(function(){return B(A(_h1,[_gA]));}));case 71:return E(new T(function(){return B(A(_h1,[_gB]));}));case 72:return E(new T(function(){return B(A(_h1,[_gC]));}));case 73:return E(new T(function(){return B(A(_h1,[_gD]));}));case 74:return E(new T(function(){return B(A(_h1,[_gE]));}));case 75:return E(new T(function(){return B(A(_h1,[_gF]));}));case 76:return E(new T(function(){return B(A(_h1,[_gG]));}));case 77:return E(new T(function(){return B(A(_h1,[_gH]));}));case 78:return E(new T(function(){return B(A(_h1,[_gI]));}));case 79:return E(new T(function(){return B(A(_h1,[_gJ]));}));case 80:return E(new T(function(){return B(A(_h1,[_gK]));}));case 81:return E(new T(function(){return B(A(_h1,[_gL]));}));case 82:return E(new T(function(){return B(A(_h1,[_gM]));}));case 83:return E(new T(function(){return B(A(_h1,[_gN]));}));case 84:return E(new T(function(){return B(A(_h1,[_gO]));}));case 85:return E(new T(function(){return B(A(_h1,[_gP]));}));case 86:return E(new T(function(){return B(A(_h1,[_gQ]));}));case 87:return E(new T(function(){return B(A(_h1,[_gR]));}));case 88:return E(new T(function(){return B(A(_h1,[_gS]));}));case 89:return E(new T(function(){return B(A(_h1,[_gT]));}));case 90:return E(new T(function(){return B(A(_h1,[_gU]));}));case 91:return E(new T(function(){return B(A(_h1,[_gV]));}));case 92:return E(new T(function(){return B(A(_h1,[_gW]));}));case 93:return E(new T(function(){return B(A(_h1,[_gX]));}));case 94:return E(new T(function(){return B(A(_h1,[_gY]));}));case 95:return E(new T(function(){return B(A(_h1,[_gZ]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_gf,[function(_hc){return new F(function(){return A(_h1,[[0,_hc,_ch]]);});}]));})));})));}));});},_hd=function(_he){return new F(function(){return A(_he,[_0]);});},_hf=function(_hg){var _hh=E(_hg);if(!_hh[0]){return E(_hd);}else{var _hi=_hh[2],_hj=E(E(_hh[1])[1]);switch(_hj){case 9:return function(_hk){return [0,function(_hl){return E(new T(function(){return B(A(new T(function(){return B(_hf(_hi));}),[_hk]));}));}];};case 10:return function(_hm){return [0,function(_hn){return E(new T(function(){return B(A(new T(function(){return B(_hf(_hi));}),[_hm]));}));}];};case 11:return function(_ho){return [0,function(_hp){return E(new T(function(){return B(A(new T(function(){return B(_hf(_hi));}),[_ho]));}));}];};case 12:return function(_hq){return [0,function(_hr){return E(new T(function(){return B(A(new T(function(){return B(_hf(_hi));}),[_hq]));}));}];};case 13:return function(_hs){return [0,function(_ht){return E(new T(function(){return B(A(new T(function(){return B(_hf(_hi));}),[_hs]));}));}];};case 32:return function(_hu){return [0,function(_hv){return E(new T(function(){return B(A(new T(function(){return B(_hf(_hi));}),[_hu]));}));}];};case 160:return function(_hw){return [0,function(_hx){return E(new T(function(){return B(A(new T(function(){return B(_hf(_hi));}),[_hw]));}));}];};default:var _hy=u_iswspace(_hj),_hz=_hy;return E(_hz)==0?E(_hd):function(_hA){return [0,function(_hB){return E(new T(function(){return B(A(new T(function(){return B(_hf(_hi));}),[_hA]));}));}];};}}},_hC=function(_hD){var _hE=new T(function(){return B(_hC(_hD));}),_hF=[1,function(_hG){return new F(function(){return A(_hf,[_hG,function(_hH){return E([0,function(_hI){return E(E(_hI)[1])==92?E(_hE):[2];}]);}]);});}];return new F(function(){return _8Y([0,function(_hJ){return E(E(_hJ)[1])==92?E([0,function(_hK){var _hL=E(E(_hK)[1]);switch(_hL){case 9:return E(_hF);case 10:return E(_hF);case 11:return E(_hF);case 12:return E(_hF);case 13:return E(_hF);case 32:return E(_hF);case 38:return E(_hE);case 160:return E(_hF);default:var _hM=u_iswspace(_hL),_hN=_hM;return E(_hN)==0?[2]:E(_hF);}}]):[2];}],[0,function(_hO){var _hP=E(_hO);return E(_hP[1])==92?E(new T(function(){return B(_h0(_hD));})):B(A(_hD,[[0,_hP,_cg]]));}]);});},_hQ=function(_hR,_hS){return new F(function(){return _hC(function(_hT){var _hU=E(_hT),_hV=E(_hU[1]);if(E(_hV[1])==34){if(!E(_hU[2])){return E(new T(function(){return B(A(_hS,[[1,new T(function(){return B(A(_hR,[_8]));})]]));}));}else{return new F(function(){return _hQ(function(_hW){return new F(function(){return A(_hR,[[1,_hV,_hW]]);});},_hS);});}}else{return new F(function(){return _hQ(function(_hX){return new F(function(){return A(_hR,[[1,_hV,_hX]]);});},_hS);});}});});},_hY=new T(function(){return B(unCStr("_\'"));}),_hZ=function(_i0){var _i1=u_iswalnum(_i0),_i2=_i1;return E(_i2)==0?B(_bY(_4P,[0,_i0],_hY)):true;},_i3=function(_i4){return new F(function(){return _hZ(E(_i4)[1]);});},_i5=new T(function(){return B(unCStr(",;()[]{}`"));}),_i6=function(_i7){return new F(function(){return A(_i7,[_8]);});},_i8=function(_i9,_ia){var _ib=function(_ic){var _id=E(_ic);if(!_id[0]){return E(_i6);}else{var _ie=_id[1];return !B(A(_i9,[_ie]))?E(_i6):function(_if){return [0,function(_ig){return E(new T(function(){return B(A(new T(function(){return B(_ib(_id[2]));}),[function(_ih){return new F(function(){return A(_if,[[1,_ie,_ih]]);});}]));}));}];};}};return [1,function(_ii){return new F(function(){return A(_ib,[_ii,_ia]);});}];},_ij=new T(function(){return B(unCStr(".."));}),_ik=new T(function(){return B(unCStr("::"));}),_il=new T(function(){return B(unCStr("->"));}),_im=[0,64],_in=[1,_im,_8],_io=[0,126],_ip=[1,_io,_8],_iq=new T(function(){return B(unCStr("=>"));}),_ir=[1,_iq,_8],_is=[1,_ip,_ir],_it=[1,_in,_is],_iu=[1,_il,_it],_iv=new T(function(){return B(unCStr("<-"));}),_iw=[1,_iv,_iu],_ix=[0,124],_iy=[1,_ix,_8],_iz=[1,_iy,_iw],_iA=[1,_gl,_8],_iB=[1,_iA,_iz],_iC=[0,61],_iD=[1,_iC,_8],_iE=[1,_iD,_iB],_iF=[1,_ik,_iE],_iG=[1,_ij,_iF],_iH=function(_iI){return new F(function(){return _8Y([1,function(_iJ){return E(_iJ)[0]==0?E(new T(function(){return B(A(_iI,[_ar]));})):[2];}],new T(function(){return B(_8Y([0,function(_iK){return E(E(_iK)[1])==39?E([0,function(_iL){var _iM=E(_iL);switch(E(_iM[1])){case 39:return [2];case 92:return E(new T(function(){return B(_h0(function(_iN){var _iO=E(_iN);return new F(function(){return (function(_iP,_iQ){var _iR=new T(function(){return B(A(_iI,[[0,_iP]]));});return !E(_iQ)?E(E(_iP)[1])==39?[2]:[0,function(_iS){return E(E(_iS)[1])==39?E(_iR):[2];}]:[0,function(_iT){return E(E(_iT)[1])==39?E(_iR):[2];}];})(_iO[1],_iO[2]);});}));}));default:return [0,function(_iU){return E(E(_iU)[1])==39?E(new T(function(){return B(A(_iI,[[0,_iM]]));})):[2];}];}}]):[2];}],new T(function(){return B(_8Y([0,function(_iV){return E(E(_iV)[1])==34?E(new T(function(){return B(_hQ(_as,_iI));})):[2];}],new T(function(){return B(_8Y([0,function(_iW){return !B(_bY(_4P,_iW,_i5))?[2]:B(A(_iI,[[2,[1,_iW,_8]]]));}],new T(function(){return B(_8Y([0,function(_iX){if(!B(_bY(_4P,_iX,_c3))){return [2];}else{return new F(function(){return _i8(_c4,function(_iY){var _iZ=[1,_iX,_iY];return !B(_bY(_9N,_iZ,_iG))?B(A(_iI,[[4,_iZ]])):B(A(_iI,[[2,_iZ]]));});});}}],new T(function(){return B(_8Y([0,function(_j0){var _j1=E(_j0),_j2=_j1[1],_j3=u_iswalpha(_j2),_j4=_j3;if(!E(_j4)){if(E(_j2)==95){return new F(function(){return _i8(_i3,function(_j5){return new F(function(){return A(_iI,[[3,[1,_j1,_j5]]]);});});});}else{return [2];}}else{return new F(function(){return _i8(_i3,function(_j6){return new F(function(){return A(_iI,[[3,[1,_j1,_j6]]]);});});});}}],new T(function(){return B(_a7(_c8,_bT,_iI));})));})));})));})));})));}));});},_j7=function(_j8){return [1,function(_j9){return new F(function(){return A(_hf,[_j9,function(_ja){return E(new T(function(){return B(_iH(_j8));}));}]);});}];},_jb=[0,0],_jc=function(_jd,_je){return new F(function(){return _j7(function(_jf){var _jg=E(_jf);if(_jg[0]==2){var _jh=E(_jg[1]);return _jh[0]==0?[2]:E(E(_jh[1])[1])==40?E(_jh[2])[0]==0?E(new T(function(){return B(A(_jd,[_jb,function(_ji){return new F(function(){return _j7(function(_jj){var _jk=E(_jj);if(_jk[0]==2){var _jl=E(_jk[1]);return _jl[0]==0?[2]:E(E(_jl[1])[1])==41?E(_jl[2])[0]==0?E(new T(function(){return B(A(_je,[_ji]));})):[2]:[2];}else{return [2];}});});}]));})):[2]:[2];}else{return [2];}});});},_jm=function(_jn,_jo,_jp){var _jq=function(_jr,_js){return new F(function(){return _8Y(B(_j7(function(_jt){var _ju=E(_jt);if(_ju[0]==4){var _jv=E(_ju[1]);if(!_jv[0]){return new F(function(){return A(_jn,[_ju,_jr,_js]);});}else{return E(E(_jv[1])[1])==45?E(_jv[2])[0]==0?E([1,function(_jw){return new F(function(){return A(_hf,[_jw,function(_jx){return E(new T(function(){return B(_iH(function(_jy){return new F(function(){return A(_jn,[_jy,_jr,function(_jz){return new F(function(){return A(_js,[new T(function(){return [0, -E(_jz)[1]];})]);});}]);});}));}));}]);});}]):B(A(_jn,[_ju,_jr,_js])):B(A(_jn,[_ju,_jr,_js]));}}else{return new F(function(){return A(_jn,[_ju,_jr,_js]);});}})),new T(function(){return B(_jc(_jq,_js));}));});};return new F(function(){return _jq(_jo,_jp);});},_jA=function(_jB,_jC){return [2];},_jD=function(_jE,_jF){return new F(function(){return _jA(_jE,_jF);});},_jG=function(_jH){var _jI=E(_jH);return _jI[0]==0?[1,new T(function(){return B(_bv(new T(function(){return B(_bl(E(_jI[1])[1]));}),_bk,_jI[2]));})]:E(_jI[2])[0]==0?E(_jI[3])[0]==0?[1,new T(function(){return B(_bv(_bj,_bk,_jI[1]));})]:[0]:[0];},_jJ=function(_jK){var _jL=E(_jK);if(_jL[0]==5){var _jM=B(_jG(_jL[1]));return _jM[0]==0?E(_jA):function(_jN,_jO){return new F(function(){return A(_jO,[new T(function(){return [0,B(_cn(_jM[1]))];})]);});};}else{return E(_jD);}},_jP=function(_jQ){return [1,function(_jR){return new F(function(){return A(_hf,[_jR,function(_jS){return E([3,_jQ,_9Z]);}]);});}];},_jT=new T(function(){return B(_jm(_jJ,_jb,_jP));}),_jU=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_jV=new T(function(){return B(err(_jU));}),_jW=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_jX=new T(function(){return B(err(_jW));}),_jY=function(_jZ){while(1){var _k0=(function(_k1){var _k2=E(_k1);if(!_k2[0]){return [0];}else{var _k3=_k2[2],_k4=E(_k2[1]);if(!E(_k4[2])[0]){return [1,_k4[1],new T(function(){return B(_jY(_k3));})];}else{_jZ=_k3;return null;}}})(_jZ);if(_k0!=null){return _k0;}}},_k5=function(_k6,_k7,_k8){while(1){var _k9=E(_k7);if(!_k9[0]){return [1,_k8];}else{var _ka=E(_k8);if(!_ka[0]){return [0];}else{if(!B(A(_8u,[_k6,_k9[1],_ka[1]]))){return [0];}else{_k7=_k9[2];_k8=_ka[2];continue;}}}}},_kb=function(_kc,_kd){var _ke=E(_kc);if(!_ke){return [0];}else{var _kf=E(_kd);return _kf[0]==0?[0]:[1,_kf[1],new T(function(){return B(_kb(_ke-1|0,_kf[2]));})];}},_kg=function(_kh,_ki,_){var _kj=new T(function(){var _kk=E(_kh),_kl=new T(function(){return B(_1c(_kk[3],_8));});return [0,[0,_kk[1],_kk[2],_8,_kl],_8,_kl];}),_km=new T(function(){var _kn=E(_kh),_ko=_kn[4],_kp=new T(function(){return B(_3h(3,_ko));}),_kq=new T(function(){return B(_l(B(_1c(B(_kb(3,_ko)),_8)),_kn[3]));});return [0,[0,_kn[1],_kn[2],_kq,_kp],_kq,_kp];}),_kr=setDragEndCallback_ffi(function(_ks,_kt,_ku,_kv){var _kw=new T(function(){return fromJSStr(E(_kt)[1]);}),_kx=B(_8w(_4P,_22,_kw)),_ky=new T(function(){if(E(_kv)[1]<160){var _kz=false;}else{var _kz=E(_ku)[1]>=40;}var _kA=_kz;return _kA;}),_kB=function(_){var _kC=new T(function(){if(E(_kv)[1]>=160){var _kD=false;}else{var _kD=E(_ku)[1]>=310;}var _kE=_kD;return _kE;}),_kF=function(_){var _kG=function(_){var _kH=new T(function(){return B(_8p(_a,_kw));}),_kI=function(_){var _kJ=function(_){if(!E(_kH)){return new F(function(){return _8j(_kh,_);});}else{if(!E(_kC)){return new F(function(){return _8j(_kh,_);});}else{var _kK=E(_kh),_kL=_kK[1],_kM=_kK[2],_kN=_kK[4],_kO=E(_kK[3]);if(!_kO[0]){return E(_3g);}else{var _kP=_kO[2],_kQ=E(_kO[1]),_kR=_kQ[1],_kS=_kQ[2],_kT=B(_b(E(_ku)[1]-310|0,90));if(3>_kT){if(_kT>=0){if(!B(_82(_kR,_kS,B(_4E(_kL,_kT))))){return new F(function(){return _1h(_kO,_);});}else{var _kU=new T(function(){return B(_4Q(_kL,_kT,_kQ));}),_kV=B(_1s(_kT,new T(function(){return B(_4E(_kU,_kT));}),_)),_kW=_kV;return new F(function(){return _kg([0,_kU,_kM,_kP,_kN],_ki,_);});}}else{return E(_4B);}}else{if(!B(_82(_kR,_kS,B(_4E(_kL,3))))){return new F(function(){return _1h(_kO,_);});}else{var _kX=new T(function(){return B(_4Q(_kL,3,_kQ));}),_kY=B(_1s(3,new T(function(){return B(_4E(_kX,3));}),_)),_kZ=_kY;return new F(function(){return _kg([0,_kX,_kM,_kP,_kN],_ki,_);});}}}}}};if(!E(_kH)){return new F(function(){return _kJ(_);});}else{if(!E(_ky)){return new F(function(){return _kJ(_);});}else{var _l0=E(_kh),_l1=_l0[2],_l2=E(_l0[3]);if(!_l2[0]){return E(_3g);}else{var _l3=E(_l2[1]),_l4=B(_b(E(_ku)[1]-40|0,90)),_l5=function(_l6,_l7){if(_l6>=0){var _l8=B(_4E(_l1,_l6));if(!B(_7I(_l3[1],_l3[2],_l8[1],_l8[2]))){return new F(function(){return _1h(_l2,_);});}else{var _l9=new T(function(){var _la=B(_60(_l0[1],_l1,_l2,_l0[4],_l7)),_lb=_la[2];return [0,[0,_la[1],_lb,_la[3],_la[4]],_lb];}),_lc=B(_2p(_l6,new T(function(){return B(_4E(E(_l9)[2],_l6));}),_)),_ld=_lc;return new F(function(){return _kg(new T(function(){return E(E(_l9)[1]);}),_ki,_);});}}else{return E(_4B);}};return 6>_l4?B(_l5(_l4,[0,_l4])):B(_l5(6,_8L));}}}};if(!E(_kH)){return new F(function(){return _kI(_);});}else{if(E(_kv)[1]>=160){return new F(function(){return _kI(_);});}else{var _le=E(_ku)[1];if(_le<E(_e)[1]){return new F(function(){return _kI(_);});}else{if(_le>=E(_f)[1]){return new F(function(){return _kI(_);});}else{var _lf=B(_8c(_)),_lg=_lf,_lh=B(_8m(_)),_li=_lh,_lj=B(_2K(_1b,_2V,_2J,_)),_lk=_lj,_ll=B(_2Q(new T(function(){return E(E(_kj)[3]);}),_)),_lm=_ll;return new F(function(){return _kg(new T(function(){return E(E(_kj)[1]);}),_8K,_);});}}}}};if(!B(_8p(_2P,_kw))){return new F(function(){return _kG(_);});}else{if(E(_kv)[1]>=160){return new F(function(){return _kG(_);});}else{var _ln=E(_ku)[1];if(_ln<E(_f)[1]){return new F(function(){return _kG(_);});}else{if(_ln>=E(_8I)[1]){return new F(function(){return _kG(_);});}else{var _lo=B(_8m(_)),_lp=_lo,_lq=B(_2K(_1b,_2V,_2J,_)),_lr=_lq,_ls=B(_2Q(new T(function(){return E(E(_km)[3]);}),_)),_lt=_ls,_lu=B(_8c(_)),_lv=_lu,_lw=B(_1h(new T(function(){return E(E(_km)[2]);}),_)),_lx=_lw;return new F(function(){return _kg(new T(function(){return E(E(_km)[1]);}),_8J,_);});}}}}};if(!E(_kx)){return new F(function(){return _kF(_);});}else{if(!E(_kC)){return new F(function(){return _kF(_);});}else{var _ly=E(_kh),_lz=_ly[1],_lA=_ly[2],_lB=B(_jY(B(_8O(_jT,new T(function(){var _lC=B(_k5(_4P,_22,_kw));return _lC[0]==0?E(_6w):E(_lC[1]);})))));if(!_lB[0]){return E(_jX);}else{if(!E(_lB[2])[0]){var _lD=E(_lB[1]),_lE=_lD[1];if(_lE>=0){var _lF=B(_4E(_lA,_lE)),_lG=E(_lF[2]);if(!_lG[0]){return E(_3g);}else{var _lH=E(_lG[1]),_lI=B(_b(E(_ku)[1]-310|0,90)),_lJ=B(_1X(_lz,0))-1|0,_lK=function(_lL,_lM){if(_lL>=0){if(!B(_82(_lH[1],_lH[2],B(_4E(_lz,_lL))))){return new F(function(){return _2p(_lE,_lF,_);});}else{var _lN=new T(function(){var _lO=B(_5y(_lz,_lA,_ly[3],_ly[4],_lD,_lM)),_lP=_lO[1],_lQ=_lO[2];return [0,[0,_lP,_lQ,_lO[3],_lO[4]],_lP,_lQ];}),_lR=B(_1s(_lL,new T(function(){return B(_4E(E(_lN)[2],_lL));}),_)),_lS=_lR,_lT=B(_8f(_lD,_)),_lU=_lT,_lV=B(_2p(_lE,new T(function(){return B(_4E(E(_lN)[3],_lE));}),_)),_lW=_lV;return new F(function(){return _kg(new T(function(){return E(E(_lN)[1]);}),_ki,_);});}}else{return E(_4B);}};return _lJ>_lI?B(_lK(_lI,[0,_lI])):B(_lK(_lJ,[0,_lJ]));}}else{return E(_4B);}}else{return E(_jV);}}}}};if(!E(_kx)){return E(_kB);}else{if(!E(_ky)){return E(_kB);}else{var _lX=E(_kh),_lY=_lX[2],_lZ=B(_jY(B(_8O(_jT,new T(function(){var _m0=B(_k5(_4P,_22,_kw));return _m0[0]==0?E(_6w):E(_m0[1]);})))));if(!_lZ[0]){return E(_jX);}else{if(!E(_lZ[2])[0]){var _m1=E(_lZ[1]),_m2=_m1[1];if(_m2>=0){var _m3=E(B(_4E(_lY,_m2))[2]);if(!_m3[0]){return E(_8H);}else{var _m4=B(_8C(_m3[1],_m3[2])),_m5=B(_b(E(_ku)[1]-40|0,90)),_m6=B(_1X(_lY,0))-1|0,_m7=function(_m8,_m9,_){if(_m8>=0){var _ma=B(_4E(_lY,_m8));if(!B(_7I(_m4[1],_m4[2],_ma[1],_ma[2]))){return new F(function(){return _2p(_m2,new T(function(){return _m2>=0?B(_4E(_lY,_m2)):E(_4B);}),_);});}else{var _mb=new T(function(){var _mc=B(_52(_lX[1],_lY,_lX[3],_lX[4],_m1,_m9)),_md=_mc[2];return [0,[0,_mc[1],_md,_mc[3],_mc[4]],_md];}),_me=new T(function(){return E(E(_mb)[2]);}),_mf=B(_2p(_m8,new T(function(){return B(_4E(_me,_m8));}),_)),_mg=_mf,_mh=B(_8f(_m1,_)),_mi=_mh,_mj=B(_2p(_m2,new T(function(){return B(_4E(_me,_m2));}),_)),_mk=_mj;return new F(function(){return _kg(new T(function(){return E(E(_mb)[1]);}),[1,new T(function(){return B(_l(_22,new T(function(){return B(_w(0,_m2,_8));})));})],_);});}}else{return E(_4B);}};return _m6>_m5?function(_aq){return new F(function(){return _m7(_m5,[0,_m5],_aq);});}:function(_aq){return new F(function(){return _m7(_m6,[0,_m6],_aq);});};}}else{return E(_4B);}}else{return E(_jV);}}}}}),_ml=setMouseoverCallback_ffi(function(_mm,_mn,_mo,_mp){var _mq=E(_kh),_mr=new T(function(){return fromJSStr(E(_mn)[1]);}),_ms=function(_){var _mt=new T(function(){return B(_8w(_4P,_22,_mr));}),_mu=new T(function(){return B(_8p(_2P,_mr));}),_mv=new T(function(){return B(_8p(_a,_mr));}),_mw=function(_){var _mx=E(_mr),_my=deleteByClass_ffi(toJSStr(_mx)),_mz=B(_kg(_mq,[1,_mx],_)),_mA=_mz;if(!E(_mt)){if(!E(_mu)){if(!E(_mv)){var _mB=consoleLog_ffi(toJSStr(B(unAppCStr("In onMouseover - Unhandled id/class: ",new T(function(){return B(_l(fromJSStr(E(_mm)[1]),[1,_8M,new T(function(){return fromJSStr(E(_mn)[1]);})]));})))));return _0;}else{return new F(function(){return _1h(_mq[3],_);});}}else{return new F(function(){return _2W(_mq[4],_);});}}else{var _mC=B(_jY(B(_8O(_jT,new T(function(){var _mD=B(_k5(_4P,_22,_mx));return _mD[0]==0?E(_6w):E(_mD[1]);})))));if(!_mC[0]){return E(_jX);}else{if(!E(_mC[2])[0]){var _mE=E(_mC[1]),_mF=_mE[1];if(_mF>=0){var _mG=B(_4E(_mq[2],_mF));return new F(function(){return _23(_mE,_mG[1],_mG[2],_);});}else{return E(_4B);}}else{return E(_jV);}}}};return !E(_mt)?!E(_mu)?!E(_mv)?_0:B(_mw(_)):B(_mw(_)):B(_mw(_));},_mH=E(_ki);return _mH[0]==0?E(_ms):!B(_8p(_mr,_mH[1]))?E(_ms):E(_1P);});return _0;},_mI=[1,_8,_8],_mJ=function(_mK){return _mK>1?[1,_8,new T(function(){return B(_mJ(_mK-1|0));})]:E(_mI);},_mL=new T(function(){return B(_mJ(4));}),_mM=new T(function(){return B(unCStr("ArithException"));}),_mN=new T(function(){return B(unCStr("GHC.Exception"));}),_mO=new T(function(){return B(unCStr("base"));}),_mP=new T(function(){var _mQ=hs_wordToWord64(4194982440),_mR=_mQ,_mS=hs_wordToWord64(3110813675),_mT=_mS;return [0,_mR,_mT,[0,_mR,_mT,_mO,_mN,_mM],_8];}),_mU=function(_mV){return E(_mP);},_mW=function(_mX){var _mY=E(_mX);return new F(function(){return _6x(B(_6t(_mY[1])),_mU,_mY[2]);});},_mZ=new T(function(){return B(unCStr("arithmetic underflow"));}),_n0=new T(function(){return B(unCStr("arithmetic overflow"));}),_n1=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_n2=new T(function(){return B(unCStr("denormal"));}),_n3=new T(function(){return B(unCStr("divide by zero"));}),_n4=new T(function(){return B(unCStr("loss of precision"));}),_n5=function(_n6){switch(E(_n6)){case 0:return E(_n0);case 1:return E(_mZ);case 2:return E(_n4);case 3:return E(_n3);case 4:return E(_n2);default:return E(_n1);}},_n7=function(_n8){return new F(function(){return _l(_mZ,_n8);});},_n9=function(_n8){return new F(function(){return _l(_n0,_n8);});},_na=function(_n8){return new F(function(){return _l(_n1,_n8);});},_nb=function(_n8){return new F(function(){return _l(_n2,_n8);});},_nc=function(_n8){return new F(function(){return _l(_n3,_n8);});},_nd=function(_n8){return new F(function(){return _l(_n4,_n8);});},_ne=function(_nf){switch(E(_nf)){case 0:return E(_n9);case 1:return E(_n7);case 2:return E(_nd);case 3:return E(_nc);case 4:return E(_nb);default:return E(_na);}},_ng=function(_nh,_ni){return new F(function(){return _6X(_ne,_nh,_ni);});},_nj=function(_nk,_nl){switch(E(_nl)){case 0:return E(_n9);case 1:return E(_n7);case 2:return E(_nd);case 3:return E(_nc);case 4:return E(_nb);default:return E(_na);}},_nm=[0,_nj,_n5,_ng],_nn=new T(function(){return [0,_mU,_nm,_no,_mW];}),_no=function(_n8){return [0,_nn,_n8];},_np=3,_nq=new T(function(){return B(_7h(_np,_no));}),_nr=function(_ns){var _nt=jsTrunc(_ns),_nu=_nt;return [0,_nu];},_nv=new T(function(){return [0,"(function(s){return s[0];})"];}),_nw=new T(function(){return B(_3T(_nv));}),_nx=function(_ny,_){var _nz=B(A(_nw,[E(_ny),_])),_nA=_nz;return new T(function(){return B(_nr(_nA));});},_nB=function(_nC,_){return new F(function(){return _nx(_nC,_);});},_nD=function(_nE,_nF){var _nG=_nE%_nF;if(_nE<=0){if(_nE>=0){return E(_nG);}else{if(_nF<=0){return E(_nG);}else{var _nH=E(_nG);return _nH==0?0:_nH+_nF|0;}}}else{if(_nF>=0){if(_nE>=0){return E(_nG);}else{if(_nF<=0){return E(_nG);}else{var _nI=E(_nG);return _nI==0?0:_nI+_nF|0;}}}else{var _nJ=E(_nG);return _nJ==0?0:_nJ+_nF|0;}}},_nK=new T(function(){return [0,"(function(s){return md51(s.join(\',\'));})"];}),_nL=new T(function(){return B(_3T(_nK));}),_nM=function(_nN,_){return new F(function(){return A(_nL,[E(_nN),_]);});},_nO=function(_nC,_){return new F(function(){return _nM(_nC,_);});},_nP=function(_nQ){return new F(function(){return _3P(function(_){var _=0;return new F(function(){return _nO(_nQ,_);});});});},_nR=function(_nS,_nT,_nU){while(1){var _nV=(function(_nW,_nX,_nY){if(_nW>_nX){var _nZ=_nX,_o0=_nW,_o1=_nY;_nS=_nZ;_nT=_o0;_nU=_o1;return null;}else{return [0,new T(function(){var _o2=(_nX-_nW|0)+1|0;switch(_o2){case -1:var _o3=[0,_nW];break;case 0:var _o3=E(_nq);break;default:var _o3=[0,B(_nD(B(_3P(function(_){var _=0;return new F(function(){return _nB(_nY,_);});}))[1],_o2))+_nW|0];}var _o4=_o3;return _o4;}),new T(function(){return B(_nP(_nY));})];}})(_nS,_nT,_nU);if(_nV!=null){return _nV;}}},_o5=function(_o6,_o7){var _o8=E(_o6);if(!_o8){return [0,_8,_o7];}else{var _o9=E(_o7);if(!_o9[0]){return [0,_8,_8];}else{var _oa=new T(function(){var _ob=B(_o5(_o8-1|0,_o9[2]));return [0,_ob[1],_ob[2]];});return [0,[1,_o9[1],new T(function(){return E(E(_oa)[1]);})],new T(function(){return E(E(_oa)[2]);})];}}},_oc=function(_od,_oe){var _of=E(_oe);if(!_of[0]){return [0];}else{var _og=new T(function(){var _oh=B(_nR(0,B(_1X(_of,0))-1|0,_od));return [0,_oh[1],_oh[2]];}),_oi=new T(function(){var _oj=E(E(_og)[1])[1];if(_oj>=0){var _ok=B(_o5(_oj,_of)),_ol=[0,_ok[1],_ok[2]];}else{var _ol=[0,_8,_of];}var _om=_ol,_on=_om;return _on;}),_oo=new T(function(){return E(E(_oi)[2]);});return [1,new T(function(){var _op=E(_oo);return _op[0]==0?E(_3g):E(_op[1]);}),new T(function(){return B(_oc(new T(function(){return E(E(_og)[2]);}),B(_l(E(_oi)[1],new T(function(){var _oq=E(_oo);return _oq[0]==0?E(_3y):E(_oq[2]);})))));})];}},_or=function(_){var _os=B(_3W(_)),_ot=_os,_ou=new T(function(){var _ov=B(_3B(_4z,new T(function(){return B(_oc(_ot,_4u));})));return [0,_ov[1],_ov[2]];}),_ow=new T(function(){return E(E(_ou)[2]);}),_ox=new T(function(){return E(E(_ou)[1]);}),_oy=B(_30(_mL,_ox,_8,_ow,_)),_oz=_oy;return new F(function(){return _kg([0,_mL,_ox,_8,_ow],_3N,_);});},_oA=[0,_or],_oB=function(_){var _oC=loadCards_ffi(E(_oA)[1]);return _0;},_oD=function(_){return new F(function(){return _oB(_);});};
var hasteMain = function() {B(A(_oD, [0]));};window.onload = hasteMain;