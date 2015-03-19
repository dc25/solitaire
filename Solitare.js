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

var _0=0,_1=function(_2,_3){while(1){var _4=E(_2);if(!_4[0]){return E(_3);}else{_2=_4[2];var _5=_3+1|0;_3=_5;continue;}}},_6=[0],_7=function(_8,_9){var _a=E(_8);return _a[0]==0?E(_9):[1,_a[1],new T(function(){return B(_7(_a[2],_9));})];},_b=new T(function(){return B(unCStr(": empty list"));}),_c=new T(function(){return B(unCStr("Prelude."));}),_d=function(_e){return new F(function(){return err(B(_7(_c,new T(function(){return B(_7(_e,_b));}))));});},_f=new T(function(){return B(unCStr("head"));}),_g=new T(function(){return B(_d(_f));}),_h=function(_i,_j){while(1){var _k=E(_i);if(!_k){return E(_j);}else{var _l=E(_j);if(!_l[0]){return [0];}else{_i=_k-1|0;_j=_l[2];continue;}}}},_m=function(_n,_o,_p){var _q=E(_n);if(!_q[0]){return [0];}else{var _r=E(_o);if(!_r[0]){return [0];}else{var _s=E(_p);return _s[0]==0?[0]:[1,[0,[1,_r[1],new T(function(){return E(E(_q[1])[1]);})],_s[1]],new T(function(){return B(_m(_q[2],_r[2],_s[2]));})];}}},_t=function(_u,_v){var _w=E(_v);return _w[0]==0?[0]:[1,new T(function(){return B(A(_u,[_w[1]]));}),new T(function(){return B(_t(_u,_w[2]));})];},_x=new T(function(){return B(unCStr("tail"));}),_y=new T(function(){return B(_d(_x));}),_z=function(_A){return E(E(_A)[2]);},_B=function(_C,_D){var _E=E(_C);if(!_E[0]){return [0,_6,_D];}else{var _F=_E[1],_G=_E[2],_H=new T(function(){var _I=E(_D);if(!_I[0]){var _J=E(_y);}else{var _K=B(_B(B(_m(_G,_I[2],new T(function(){return B(_t(_z,_G));}))),new T(function(){var _L=B(_1(_E,0));return _L>=0?B(_h(_L,_I)):E(_I);}))),_J=[0,_K[1],_K[2]];}return _J;});return [0,[1,[0,new T(function(){return E(E(_F)[1]);}),[1,new T(function(){var _M=E(_D);return _M[0]==0?E(_g):E(_M[1]);}),new T(function(){return E(E(_F)[2]);})]],new T(function(){return E(E(_H)[1]);})],new T(function(){return E(E(_H)[2]);})];}},_N=[0],_O=new T(function(){return [0,"(function(){return md51(jsRand().toString());})"];}),_P=function(_Q){var _R=B(A(_Q,[_])),_S=_R;return E(_S);},_T=function(_U){return new F(function(){return _P(function(_){var _=0;return new F(function(){return eval(E(_U)[1]);});});});},_V=function(_){return new F(function(){return A(_T,[_O,_]);});},_W=function(_){return new F(function(){return _V(_);});},_X=[1,_6,_6],_Y=function(_Z){return _Z>1?[1,_6,new T(function(){return B(_Y(_Z-1|0));})]:E(_X);},_10=new T(function(){return B(_Y(4));}),_11=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_12=new T(function(){return B(err(_11));}),_13=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_14=new T(function(){return B(err(_13));}),_15=function(_16,_17){while(1){var _18=E(_16);if(!_18[0]){return E(_14);}else{var _19=E(_17);if(!_19){return E(_18[1]);}else{_16=_18[2];_17=_19-1|0;continue;}}}},_1a=function(_1b,_1c,_1d,_1e,_){var _1f=placeCard_ffi(toJSStr(E(_1b)),toJSStr(E(_1c)),200+(imul(100,_1d)|0)|0,200+(imul(30,_1e)|0)|0);return _0;},_1g=function(_1h,_1i){var _1j=jsShowI(_1h),_1k=_1j;return new F(function(){return _7(fromJSStr(_1k),_1i);});},_1l=[0,41],_1m=[0,40],_1n=function(_1o,_1p,_1q){return _1p>=0?B(_1g(_1p,_1q)):_1o<=6?B(_1g(_1p,_1q)):[1,_1m,new T(function(){var _1r=jsShowI(_1p),_1s=_1r;return B(_7(fromJSStr(_1s),[1,_1l,_1q]));})];},_1t=function(_1u,_1v){while(1){var _1w=E(_1u);if(!_1w[0]){return E(_1v);}else{_1u=_1w[2];var _1x=[1,_1w[1],_1v];_1v=_1x;continue;}}},_1y=function(_1z){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_1n(9,_1z,_6));}))));});},_1A=new T(function(){return B(unCStr("heart"));}),_1B=new T(function(){return B(unCStr("diamond"));}),_1C=[0,95],_1D=new T(function(){return B(unCStr("spade"));}),_1E=new T(function(){return B(unCStr("club"));}),_1F=new T(function(){return B(unCStr("10"));}),_1G=new T(function(){return B(unCStr("jack"));}),_1H=new T(function(){return B(unCStr("queen"));}),_1I=new T(function(){return B(unCStr("king"));}),_1J=function(_1K,_1L){var _1M=E(_1K);switch(_1M){case 9:return new F(function(){return _7(_1F,[1,_1C,new T(function(){switch(E(_1L)){case 0:var _1N=E(_1A);break;case 1:var _1N=E(_1B);break;case 2:var _1N=E(_1D);break;default:var _1N=E(_1E);}return _1N;})]);});break;case 10:return new F(function(){return _7(_1G,[1,_1C,new T(function(){switch(E(_1L)){case 0:var _1O=E(_1A);break;case 1:var _1O=E(_1B);break;case 2:var _1O=E(_1D);break;default:var _1O=E(_1E);}return _1O;})]);});break;case 11:return new F(function(){return _7(_1H,[1,_1C,new T(function(){switch(E(_1L)){case 0:var _1P=E(_1A);break;case 1:var _1P=E(_1B);break;case 2:var _1P=E(_1D);break;default:var _1P=E(_1E);}return _1P;})]);});break;case 12:return new F(function(){return _7(_1I,[1,_1C,new T(function(){switch(E(_1L)){case 0:var _1Q=E(_1A);break;case 1:var _1Q=E(_1B);break;case 2:var _1Q=E(_1D);break;default:var _1Q=E(_1E);}return _1Q;})]);});break;default:return [1,new T(function(){var _1R=dataToTag(E(_1M))+49|0;if(_1R>>>0>1114111){var _1S=B(_1y(_1R));}else{var _1S=[0,_1R];}var _1T=_1S,_1U=_1T,_1V=_1U;return _1V;}),[1,_1C,new T(function(){switch(E(_1L)){case 0:var _1W=E(_1A);break;case 1:var _1W=E(_1B);break;case 2:var _1W=E(_1D);break;default:var _1W=E(_1E);}return _1W;})]];}},_1X=function(_1Y){var _1Z=E(_1Y);return new F(function(){return _1J(_1Z[1],_1Z[2]);});},_20=function(_21,_22,_23,_){var _24=B(_1(_22,0));if(_24<=2147483647){var _25=new T(function(){return B(unAppCStr("visibleColumn",new T(function(){return B(_1n(0,E(_21)[1],_6));})));}),_26=B(_1t(_23,_6));if(!_26[0]){return _0;}else{var _27=E(_21)[1],_28=B(_1a(B(_1X(_26[1])),_25,_27,_24,_)),_29=_28,_2a=E(_24);if(_2a==2147483647){return _0;}else{return new F(function(){return (function(_2b,_2c,_){while(1){var _2d=E(_2c);if(!_2d[0]){return _0;}else{var _2e=B(_1a(B(_1X(_2d[1])),_25,_27,_2b,_)),_2f=_2e,_2g=E(_2b);if(_2g==2147483647){return _0;}else{_2b=_2g+1|0;_2c=_2d[2];continue;}}}})(_2a+1|0,_26[2],_);});}}}else{return _0;}},_2h=function(_2i,_2j,_2k,_2l){var _2m=E(_2k);if(!_2m[0]){return E(_2j);}else{var _2n=E(_2l);if(!_2n[0]){return E(_2j);}else{return new F(function(){return A(_2i,[_2m[1],_2n[1],new T(function(){return B(_2h(_2i,_2j,_2m[2],_2n[2]));})]);});}}},_2o=new T(function(){return B(unCStr("back"));}),_2p=function(_2q,_2r){if(_2q<=_2r){var _2s=function(_2t){return [1,[0,_2t],new T(function(){if(_2t!=_2r){var _2u=B(_2s(_2t+1|0));}else{var _2u=[0];}return _2u;})];};return new F(function(){return _2s(_2q);});}else{return [0];}},_2v=new T(function(){return B(_2p(0,2147483647));}),_2w=function(_){return _0;},_2x=function(_2y,_2z,_2A,_){var _2B=B(A(_2h,[function(_2C,_2D,_2E,_){var _2F=B(_1a(_2o,new T(function(){return B(unAppCStr("hiddenColumn",new T(function(){return B(_1n(0,E(_2y)[1],_6));})));}),E(_2y)[1],E(_2C)[1],_)),_2G=_2F;return new F(function(){return A(_2E,[_]);});},_2w,_2v,_2z,_])),_2H=_2B;return new F(function(){return _20(_2y,_2z,_2A,_);});},_2I=[0,_6,_6],_2J=function(_2K,_2L,_2M,_2N,_2O,_2P){return [0,_2K,new T(function(){var _2Q=E(_2P)[1],_2R=new T(function(){var _2S=E(_2O)[1];return _2S>=0?B(_15(_2L,_2S)):E(_12);}),_2T=new T(function(){var _2U=E(_2O)[1],_2V=[1,new T(function(){var _2W=E(E(_2R)[1]);return _2W[0]==0?E(_2I):[0,_2W[2],[1,_2W[1],_6]];}),new T(function(){var _2X=_2U+1|0;return _2X>=0?B(_h(_2X,_2L)):E(_2L);})];if(_2U>0){var _2Y=function(_2Z,_30){var _31=E(_2Z);if(!_31[0]){return E(_2V);}else{var _32=_31[1];return _30>1?[1,_32,new T(function(){return B(_2Y(_31[2],_30-1|0));})]:[1,_32,_2V];}},_33=B(_2Y(_2L,_2U));}else{var _33=E(_2V);}var _34=_33;return _34;}),_35=new T(function(){return _2Q>=0?B(_15(_2T,_2Q)):E(_12);}),_36=[1,[0,new T(function(){return E(E(_35)[1]);}),new T(function(){return B(_7(E(_2R)[2],new T(function(){return E(E(_35)[2]);})));})],new T(function(){var _37=_2Q+1|0;return _37>=0?B(_h(_37,_2T)):E(_2T);})];if(_2Q>0){var _38=function(_39,_3a){var _3b=E(_39);if(!_3b[0]){return E(_36);}else{var _3c=_3b[1];return _3a>1?[1,_3c,new T(function(){return B(_38(_3b[2],_3a-1|0));})]:[1,_3c,_36];}},_3d=B(_38(_2T,_2Q));}else{var _3d=E(_36);}var _3e=_3d;return _3e;}),_2M,_2N];},_3f=new T(function(){return B(unCStr("Control.Exception.Base"));}),_3g=new T(function(){return B(unCStr("base"));}),_3h=new T(function(){return B(unCStr("PatternMatchFail"));}),_3i=new T(function(){var _3j=hs_wordToWord64(18445595),_3k=_3j,_3l=hs_wordToWord64(52003073),_3m=_3l;return [0,_3k,_3m,[0,_3k,_3m,_3g,_3f,_3h],_6];}),_3n=function(_3o){return E(_3i);},_3p=function(_3q){return E(E(_3q)[1]);},_3r=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_3s=new T(function(){return B(err(_3r));}),_3t=function(_3u,_3v,_3w){var _3x=new T(function(){var _3y=B(A(_3u,[_3w])),_3z=B(A(_3v,[new T(function(){var _3A=E(_3x);return _3A[0]==0?E(_3s):E(_3A[1]);})])),_3B=hs_eqWord64(_3y[1],_3z[1]),_3C=_3B;if(!E(_3C)){var _3D=[0];}else{var _3E=hs_eqWord64(_3y[2],_3z[2]),_3F=_3E,_3D=E(_3F)==0?[0]:[1,_3w];}var _3G=_3D,_3H=_3G;return _3H;});return E(_3x);},_3I=function(_3J){var _3K=E(_3J);return new F(function(){return _3t(B(_3p(_3K[1])),_3n,_3K[2]);});},_3L=function(_3M){return E(E(_3M)[1]);},_3N=function(_3O,_3P){return new F(function(){return _7(E(_3O)[1],_3P);});},_3Q=[0,44],_3R=[0,93],_3S=[0,91],_3T=function(_3U,_3V,_3W){var _3X=E(_3V);return _3X[0]==0?B(unAppCStr("[]",_3W)):[1,_3S,new T(function(){return B(A(_3U,[_3X[1],new T(function(){var _3Y=function(_3Z){var _40=E(_3Z);return _40[0]==0?E([1,_3R,_3W]):[1,_3Q,new T(function(){return B(A(_3U,[_40[1],new T(function(){return B(_3Y(_40[2]));})]));})];};return B(_3Y(_3X[2]));})]));})];},_41=function(_42,_43){return new F(function(){return _3T(_3N,_42,_43);});},_44=function(_45,_46,_47){return new F(function(){return _7(E(_46)[1],_47);});},_48=[0,_44,_3L,_41],_49=new T(function(){return [0,_3n,_48,_4a,_3I];}),_4a=function(_4b){return [0,_49,_4b];},_4c=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_4d=function(_4e,_4f){return new F(function(){return die(new T(function(){return B(A(_4f,[_4e]));}));});},_4g=function(_4h,_4i){var _4j=E(_4i);if(!_4j[0]){return [0,_6,_6];}else{var _4k=_4j[1];if(!B(A(_4h,[_4k]))){return [0,_6,_4j];}else{var _4l=new T(function(){var _4m=B(_4g(_4h,_4j[2]));return [0,_4m[1],_4m[2]];});return [0,[1,_4k,new T(function(){return E(E(_4l)[1]);})],new T(function(){return E(E(_4l)[2]);})];}}},_4n=[0,32],_4o=[0,10],_4p=[1,_4o,_6],_4q=function(_4r){return E(E(_4r)[1])==124?false:true;},_4s=function(_4t,_4u){var _4v=B(_4g(_4q,B(unCStr(_4t)))),_4w=_4v[1],_4x=function(_4y,_4z){return new F(function(){return _7(_4y,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_7(_4u,new T(function(){return B(_7(_4z,_4p));})));})));}));});},_4A=E(_4v[2]);if(!_4A[0]){return new F(function(){return _4x(_4w,_6);});}else{return E(E(_4A[1])[1])==124?B(_4x(_4w,[1,_4n,_4A[2]])):B(_4x(_4w,_6));}},_4B=function(_4C){return new F(function(){return _4d([0,new T(function(){return B(_4s(_4C,_4c));})],_4a);});},_4D=new T(function(){return B(_4B("Game.hs:(47,1)-(49,86)|function goesOnColumn"));}),_4E=function(_4F,_4G,_4H,_4I){var _4J=new T(function(){var _4K=E(_4I);if(!_4K[0]){var _4L=E(_4D);}else{var _4M=_4K[1];switch(E(_4G)){case 2:var _4N=E(_4M);switch(E(_4N[2])){case 2:var _4O=false;break;case 3:var _4O=false;break;default:var _4O=(dataToTag(E(E(_4F)))+1|0)==dataToTag(E(E(_4N[1])));}var _4P=_4O,_4Q=_4P;break;case 3:var _4R=E(_4M);switch(E(_4R[2])){case 2:var _4S=false;break;case 3:var _4S=false;break;default:var _4S=(dataToTag(E(E(_4F)))+1|0)==dataToTag(E(E(_4R[1])));}var _4T=_4S,_4Q=_4T;break;default:var _4U=E(_4M),_4V=_4U[1];switch(E(_4U[2])){case 2:var _4W=(dataToTag(E(E(_4F)))+1|0)==dataToTag(E(E(_4V)));break;case 3:var _4W=(dataToTag(E(E(_4F)))+1|0)==dataToTag(E(E(_4V)));break;default:var _4W=false;}var _4X=_4W,_4Q=_4X;}var _4L=_4Q;}return _4L;});return E(_4H)[0]==0?E(_4I)[0]==0?dataToTag(E(_4F))==12?true:false:E(_4J):E(_4J);},_4Y=[0,41],_4Z=[1,_4Y,_6],_50=new T(function(){return B(_1n(0,12,_4Z));}),_51=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_50));}),_52=function(_53){return new F(function(){return err(B(unAppCStr("toEnum{Rank}: tag (",new T(function(){return B(_1n(0,_53,_51));}))));});},_54=function(_55){return _55<0?B(_52(_55)):_55>12?B(_52(_55)):_55;},_56=function(_57){return new F(function(){return _54(E(_57)[1]);});},_58=[1,_4Y,_6],_59=new T(function(){return B(_1n(0,3,_58));}),_5a=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_59));}),_5b=function(_5c){return new F(function(){return err(B(unAppCStr("toEnum{Suit}: tag (",new T(function(){return B(_1n(0,_5c,_5a));}))));});},_5d=function(_5e){return _5e<0?B(_5b(_5e)):_5e>3?B(_5b(_5e)):_5e;},_5f=function(_5g){return new F(function(){return _5d(E(_5g)[1]);});},_5h=function(_5i,_5j,_5k,_5l){if(dataToTag(E(E(_5i)))!=dataToTag(E(E(_5k)))){return true;}else{switch(E(_5j)){case 0:switch(E(_5l)){case 0:return false;case 1:return true;case 2:return true;default:return true;}break;case 1:return E(_5l)==1?false:true;case 2:return E(_5l)==2?false:true;default:return E(_5l)==3?false:true;}}},_5m=function(_5n,_5o){var _5p=E(_5n),_5q=E(_5o);return new F(function(){return _5h(_5p[1],_5p[2],_5q[1],_5q[2]);});},_5r=function(_5s,_5t){switch(E(_5s)){case 0:switch(E(_5t)){case 0:return true;case 1:return false;case 2:return false;default:return false;}break;case 1:return E(_5t)==1?true:false;case 2:return E(_5t)==2?true:false;default:return E(_5t)==3?true:false;}},_5u=function(_5v,_5w,_5x,_5y){return dataToTag(E(E(_5v)))!=dataToTag(E(E(_5x)))?false:B(_5r(_5w,_5y));},_5z=function(_5A,_5B){var _5C=E(_5A),_5D=E(_5B);return new F(function(){return _5u(_5C[1],_5C[2],_5D[1],_5D[2]);});},_5E=[0,_5z,_5m],_5F=function(_5G){return E(E(_5G)[1]);},_5H=function(_5I,_5J,_5K){while(1){var _5L=E(_5K);if(!_5L[0]){return false;}else{if(!B(A(_5F,[_5I,_5J,_5L[1]]))){_5K=_5L[2];continue;}else{return true;}}}},_5M=function(_5N,_5O){var _5P=function(_5Q,_5R){while(1){var _5S=(function(_5T,_5U){var _5V=E(_5U);if(!_5V[0]){return [0];}else{var _5W=_5V[2];if(!B(A(_5N,[_5V[1]]))){var _5X=_5T+1|0;_5R=_5W;_5Q=_5X;return null;}else{return [1,[0,_5T],new T(function(){return B(_5P(_5T+1|0,_5W));})];}}})(_5Q,_5R);if(_5S!=null){return _5S;}}};return new F(function(){return _5P(0,_5O);});},_5Y=function(_5Z,_60){var _61=B(_5M(function(_62){return new F(function(){return _5H(_5E,_5Z,_62);});},B(_t(_z,_60))));return _61[0]==0?[0]:[1,_61[1]];},_63=function(_64,_65){while(1){var _66=E(_64);if(!_66[0]){return E(_65)[0]==0?true:false;}else{var _67=E(_65);if(!_67[0]){return false;}else{if(E(_66[1])[1]!=E(_67[1])[1]){return false;}else{_64=_66[2];_65=_67[2];continue;}}}}},_68=function(_69){return new F(function(){return fromJSStr(E(_69)[1]);});},_6a=function(_6b){return E(E(_6b)[1])==95?false:true;},_6c=function(_6d){var _6e=E(_6d);switch(_6e){case 9:return E(_1F);case 10:return E(_1G);case 11:return E(_1H);case 12:return E(_1I);default:return [1,new T(function(){var _6f=dataToTag(E(_6e))+49|0;if(_6f>>>0>1114111){var _6g=B(_1y(_6f));}else{var _6g=[0,_6f];}var _6h=_6g,_6i=_6h,_6j=_6i;return _6j;}),_6];}},_6k=function(_6l,_6m){var _6n=dataToTag(E(E(_6m))),_6o=dataToTag(E(E(_6l)));if(_6o<=_6n){var _6p=function(_6q){return [1,new T(function(){return _6q<0?B(_52(_6q)):_6q>12?B(_52(_6q)):_6q;}),new T(function(){if(_6q!=_6n){var _6r=B(_6p(_6q+1|0));}else{var _6r=[0];}return _6r;})];};return new F(function(){return _6p(_6o);});}else{return [0];}},_6s=0,_6t=12,_6u=new T(function(){return B(_6k(_6s,_6t));}),_6v=new T(function(){return B(_t(_6c,_6u));}),_6w=function(_6x){switch(E(_6x)){case 0:return E(_1A);case 1:return E(_1B);case 2:return E(_1D);default:return E(_1E);}},_6y=function(_6z){return [1,new T(function(){return _6z<0?B(_5b(_6z)):_6z>3?B(_5b(_6z)):_6z;}),new T(function(){var _6A=E(_6z);if(_6A==3){var _6B=[0];}else{var _6B=B(_6y(_6A+1|0));}return _6B;})];},_6C=new T(function(){return B(_6y(0));}),_6D=new T(function(){return B(_t(_6w,_6C));}),_6E=function(_6F,_6G){var _6H=new T(function(){var _6I=B(_4g(_6a,B(_68(_6F))));return [0,_6I[1],_6I[2]];}),_6J=B(_5M(function(_6K){return new F(function(){return _63(new T(function(){return E(E(_6H)[1]);}),_6K);});},_6v));if(!_6J[0]){return [0];}else{var _6L=B(_5M(function(_6K){return new F(function(){return _63(new T(function(){var _6M=E(E(_6H)[2]);return _6M[0]==0?E(_y):E(_6M[2]);}),_6K);});},_6D));if(!_6L[0]){return [0];}else{return new F(function(){return _5Y([0,new T(function(){return B(_56(_6J[1]));}),new T(function(){return B(_5f(_6L[1]));})],E(_6G)[2]);});}}},_6N=function(_6O,_6P){if(_6O<=0){if(_6O>=0){return new F(function(){return quot(_6O,_6P);});}else{if(_6P<=0){return new F(function(){return quot(_6O,_6P);});}else{return quot(_6O+1|0,_6P)-1|0;}}}else{if(_6P>=0){if(_6O>=0){return new F(function(){return quot(_6O,_6P);});}else{if(_6P<=0){return new F(function(){return quot(_6O,_6P);});}else{return quot(_6O+1|0,_6P)-1|0;}}}else{return quot(_6O-1|0,_6P)-1|0;}}},_6Q=function(_6R,_6S){while(1){var _6T=E(_6S);if(!_6T[0]){return E(_6R);}else{_6R=_6T[1];_6S=_6T[2];continue;}}},_6U=new T(function(){return B(unCStr("last"));}),_6V=new T(function(){return B(_d(_6U));}),_6W=function(_6X,_6Y,_6Z,_){var _70=E(_6Y),_71=B(_1(_70[1],0));if(_71<=2147483647){var _72=B(_1t(_70[2],_6));if(!_72[0]){return new F(function(){return A(_6Z,[_]);});}else{var _73=E(_72[1]),_74=toJSStr(B(unAppCStr("visibleColumn",new T(function(){return B(_1n(0,E(_6X)[1],_6));})))),_75=200+(imul(100,E(_6X)[1])|0)|0,_76=alignCard_ffi(toJSStr(B(_1J(_73[1],_73[2]))),_74,_75,200+(imul(30,_71)|0)|0),_77=E(_71);if(_77==2147483647){return new F(function(){return A(_6Z,[_]);});}else{var _78=B((function(_79,_7a,_){while(1){var _7b=E(_7a);if(!_7b[0]){return _0;}else{var _7c=E(_7b[1]),_7d=alignCard_ffi(toJSStr(B(_1J(_7c[1],_7c[2]))),_74,_75,200+(imul(30,_79)|0)|0),_7e=E(_79);if(_7e==2147483647){return _0;}else{_79=_7e+1|0;_7a=_7b[2];continue;}}}})(_77+1|0,_72[2],_)),_7f=_78;return new F(function(){return A(_6Z,[_]);});}}}else{return new F(function(){return A(_6Z,[_]);});}},_7g=[0,6],_7h=function(_7i,_7j,_){var _7k=new T(function(){return B(_2h(_6W,_2w,_2v,E(_7i)[2]));}),_7l=setDragEndCallback_ffi(function(_7m,_7n,_7o){var _7p=E(_7i),_7q=_7p[2];if(E(_7o)[1]<=200){return E(_7k);}else{var _7r=E(_7n)[1];if(_7r<=200){return E(_7k);}else{var _7s=B(_6E(_7m,_7p));if(!_7s[0]){return E(_7k);}else{var _7t=E(_7s[1]),_7u=_7t[1];if(_7u>=0){var _7v=E(B(_15(_7q,_7u))[2]);if(!_7v[0]){return E(_6V);}else{var _7w=B(_6Q(_7v[1],_7v[2])),_7x=B(_6N(_7r-200|0,100)),_7y=function(_7z,_7A,_){if(_7z>=0){var _7B=B(_15(_7q,_7z));if(!B(_4E(_7w[1],_7w[2],_7B[1],_7B[2]))){return new F(function(){return A(_7k,[_]);});}else{var _7C=B(_2J(_7p[1],_7q,_7p[3],_7p[4],_7t,_7A)),_7D=_7C[2],_7E=B(A(_2h,[_6W,_2w,_2v,_7D,_])),_7F=_7E,_7G=deleteBySelectionString_ffi(toJSStr(B(unAppCStr(".hiddenColumn",new T(function(){return B(_1n(0,_7u,_6));}))))),_7H=deleteBySelectionString_ffi(toJSStr(B(unAppCStr(".visibleColumn",new T(function(){return B(_1n(0,_7u,_6));}))))),_7I=B(_15(_7D,_7u)),_7J=B(_2x(_7t,_7I[1],_7I[2],_)),_7K=_7J;return new F(function(){return _7h([0,_7C[1],_7D,_7C[3],_7C[4]],[1,new T(function(){return B(unAppCStr(".visibleColumn",new T(function(){return B(_1n(0,_7u,_6));})));})],_);});}}else{return E(_12);}};return 6>_7x?function(_7L){return new F(function(){return _7y(_7x,[0,_7x],_7L);});}:function(_7L){return new F(function(){return _7y(6,_7g,_7L);});};}}else{return E(_12);}}}}}),_7M=setMouseoverCallback_ffi(function(_7N,_7O,_7P){var _7Q=E(_7i),_7R=B(_6E(_7N,_7Q));if(!_7R[0]){return E(_2w);}else{var _7S=_7R[1],_7T=new T(function(){return B(unAppCStr(".visibleColumn",new T(function(){return B(_1n(0,E(_7S)[1],_6));})));}),_7U=function(_){var _7V=E(_7T),_7W=deleteBySelectionString_ffi(toJSStr(_7V)),_7X=E(_7S),_7Y=_7X[1];if(_7Y>=0){var _7Z=B(_15(_7Q[2],_7Y)),_80=B(_20(_7X,_7Z[1],_7Z[2],_)),_81=_80;return new F(function(){return _7h(_7Q,[1,_7V],_);});}else{return E(_12);}},_82=E(_7j);return _82[0]==0?E(_7U):!B(_63(_7T,_82[1]))?E(_7U):E(_2w);}});return _0;},_83=function(_84,_85,_){while(1){var _86=E(_85);if(!_86[0]){return _0;}else{var _87=E(_86[1]),_88=B(_2x([0,_84],_87[1],_87[2],_)),_89=_88,_8a=E(_84);if(_8a==2147483647){return _0;}else{_84=_8a+1|0;_85=_86[2];continue;}}}},_8b=function(_8c){var _8d=E(_8c);if(!_8d[0]){return [0];}else{var _8e=function(_8f){var _8g=E(_8f);return _8g[0]==0?E(new T(function(){return B(_8b(_8d[2]));})):[1,[0,_8d[1],_8g[1]],new T(function(){return B(_8e(_8g[2]));})];};return new F(function(){return _8e(_6C);});}},_8h=new T(function(){return B(_8b(_6u));}),_8i=[0,_6,_6],_8j=[1,_8i,_6],_8k=function(_8l){return _8l>1?[1,_8i,new T(function(){return B(_8k(_8l-1|0));})]:E(_8j);},_8m=new T(function(){return B(_8k(7));}),_8n=new T(function(){return B(unCStr("ArithException"));}),_8o=new T(function(){return B(unCStr("GHC.Exception"));}),_8p=new T(function(){return B(unCStr("base"));}),_8q=new T(function(){var _8r=hs_wordToWord64(4194982440),_8s=_8r,_8t=hs_wordToWord64(3110813675),_8u=_8t;return [0,_8s,_8u,[0,_8s,_8u,_8p,_8o,_8n],_6];}),_8v=function(_8w){return E(_8q);},_8x=function(_8y){var _8z=E(_8y);return new F(function(){return _3t(B(_3p(_8z[1])),_8v,_8z[2]);});},_8A=new T(function(){return B(unCStr("arithmetic underflow"));}),_8B=new T(function(){return B(unCStr("arithmetic overflow"));}),_8C=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_8D=new T(function(){return B(unCStr("denormal"));}),_8E=new T(function(){return B(unCStr("divide by zero"));}),_8F=new T(function(){return B(unCStr("loss of precision"));}),_8G=function(_8H){switch(E(_8H)){case 0:return E(_8B);case 1:return E(_8A);case 2:return E(_8F);case 3:return E(_8E);case 4:return E(_8D);default:return E(_8C);}},_8I=function(_8J){return new F(function(){return _7(_8A,_8J);});},_8K=function(_8J){return new F(function(){return _7(_8B,_8J);});},_8L=function(_8J){return new F(function(){return _7(_8C,_8J);});},_8M=function(_8J){return new F(function(){return _7(_8D,_8J);});},_8N=function(_8J){return new F(function(){return _7(_8E,_8J);});},_8O=function(_8J){return new F(function(){return _7(_8F,_8J);});},_8P=function(_8Q){switch(E(_8Q)){case 0:return E(_8K);case 1:return E(_8I);case 2:return E(_8O);case 3:return E(_8N);case 4:return E(_8M);default:return E(_8L);}},_8R=function(_8S,_8T){return new F(function(){return _3T(_8P,_8S,_8T);});},_8U=function(_8V,_8W){switch(E(_8W)){case 0:return E(_8K);case 1:return E(_8I);case 2:return E(_8O);case 3:return E(_8N);case 4:return E(_8M);default:return E(_8L);}},_8X=[0,_8U,_8G,_8R],_8Y=new T(function(){return [0,_8v,_8X,_8Z,_8x];}),_8Z=function(_8J){return [0,_8Y,_8J];},_90=3,_91=new T(function(){return B(_4d(_90,_8Z));}),_92=function(_93){var _94=jsTrunc(_93),_95=_94;return [0,_95];},_96=new T(function(){return [0,"(function(s){return s[0];})"];}),_97=new T(function(){return B(_T(_96));}),_98=function(_99,_){var _9a=B(A(_97,[E(_99),_])),_9b=_9a;return new T(function(){return B(_92(_9b));});},_9c=function(_9d,_){return new F(function(){return _98(_9d,_);});},_9e=function(_9f,_9g){var _9h=_9f%_9g;if(_9f<=0){if(_9f>=0){return E(_9h);}else{if(_9g<=0){return E(_9h);}else{var _9i=E(_9h);return _9i==0?0:_9i+_9g|0;}}}else{if(_9g>=0){if(_9f>=0){return E(_9h);}else{if(_9g<=0){return E(_9h);}else{var _9j=E(_9h);return _9j==0?0:_9j+_9g|0;}}}else{var _9k=E(_9h);return _9k==0?0:_9k+_9g|0;}}},_9l=new T(function(){return [0,"(function(s){return md51(s.join(\',\'));})"];}),_9m=new T(function(){return B(_T(_9l));}),_9n=function(_9o,_){return new F(function(){return A(_9m,[E(_9o),_]);});},_9p=function(_9d,_){return new F(function(){return _9n(_9d,_);});},_9q=function(_9r){return new F(function(){return _P(function(_){var _=0;return new F(function(){return _9p(_9r,_);});});});},_9s=function(_9t,_9u,_9v){while(1){var _9w=(function(_9x,_9y,_9z){if(_9x>_9y){var _9A=_9y,_9B=_9x,_9C=_9z;_9t=_9A;_9u=_9B;_9v=_9C;return null;}else{return [0,new T(function(){var _9D=(_9y-_9x|0)+1|0;switch(_9D){case -1:var _9E=[0,_9x];break;case 0:var _9E=E(_91);break;default:var _9E=[0,B(_9e(B(_P(function(_){var _=0;return new F(function(){return _9c(_9z,_);});}))[1],_9D))+_9x|0];}var _9F=_9E;return _9F;}),new T(function(){return B(_9q(_9z));})];}})(_9t,_9u,_9v);if(_9w!=null){return _9w;}}},_9G=function(_9H,_9I){var _9J=E(_9H);if(!_9J){return [0,_6,_9I];}else{var _9K=E(_9I);if(!_9K[0]){return [0,_6,_6];}else{var _9L=new T(function(){var _9M=B(_9G(_9J-1|0,_9K[2]));return [0,_9M[1],_9M[2]];});return [0,[1,_9K[1],new T(function(){return E(E(_9L)[1]);})],new T(function(){return E(E(_9L)[2]);})];}}},_9N=function(_9O,_9P){var _9Q=E(_9P);if(!_9Q[0]){return [0];}else{var _9R=new T(function(){var _9S=B(_9s(0,B(_1(_9Q,0))-1|0,_9O));return [0,_9S[1],_9S[2]];}),_9T=new T(function(){var _9U=E(E(_9R)[1])[1];if(_9U>=0){var _9V=B(_9G(_9U,_9Q)),_9W=[0,_9V[1],_9V[2]];}else{var _9W=[0,_6,_9Q];}var _9X=_9W,_9Y=_9X;return _9Y;}),_9Z=new T(function(){return E(E(_9T)[2]);});return [1,new T(function(){var _a0=E(_9Z);return _a0[0]==0?E(_g):E(_a0[1]);}),new T(function(){return B(_9N(new T(function(){return E(E(_9R)[2]);}),B(_7(E(_9T)[1],new T(function(){var _a1=E(_9Z);return _a1[0]==0?E(_y):E(_a1[2]);})))));})];}},_a2=function(_){var _a3=B(_W(_)),_a4=_a3,_a5=B(_B(_8m,new T(function(){return B(_9N(_a4,_8h));}))),_a6=_a5[1],_a7=B(_83(0,_a6,_)),_a8=_a7;return new F(function(){return _7h([0,_10,_a6,_6,_a5[2]],_N,_);});},_a9=[0,_a2],_aa=function(_){var _ab=loadCards_ffi(E(_a9)[1]);return _0;},_ac=function(_){return new F(function(){return _aa(_);});};
var hasteMain = function() {B(A(_ac, [0]));};window.onload = hasteMain;