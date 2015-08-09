"use strict";
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

/* Hint to optimizer that an imported symbol is strict. */
function __strict(x) {return x}

// A tailcall.
function F(f) {
    this.f = f;
}

// A partially applied function. Invariant: members are never thunks.
function PAP(f, args) {
    this.f = f;
    this.args = args;
    this.arity = f.length - args.length;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Generic apply.
   Applies a function *or* a partial application object to a list of arguments.
   See https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls
   for more information.
*/
function A(f, args) {
    while(true) {
        f = E(f);
        if(f instanceof F) {
            f = E(B(f));
        }
        if(f instanceof PAP) {
            // f is a partial application
            if(args.length == f.arity) {
                // Saturated application
                return f.f.apply(null, f.args.concat(args));
            } else if(args.length < f.arity) {
                // Application is still unsaturated
                return new PAP(f.f, f.args.concat(args));
            } else {
                // Application is oversaturated; 
                var f2 = f.f.apply(null, f.args.concat(args.slice(0, f.arity)));
                args = args.slice(f.arity);
                f = B(f2);
            }
        } else if(f instanceof Function) {
            if(args.length == f.length) {
                return f.apply(null, args);
            } else if(args.length < f.length) {
                return new PAP(f, args);
            } else {
                var f2 = f.apply(null, args.slice(0, f.length));
                args = args.slice(f.length);
                f = B(f2);
            }
        } else {
            return f;
        }
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
    throw E(err);
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
        acc = B(A(f, [str.charCodeAt(i), acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,str.charCodeAt(i),new T(function() {
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
        s += String.fromCharCode(E(str[1]));
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
        return 0;
    } else if(a == b) {
        return 1;
    }
    return 2;
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

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
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
        return [1,e];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, es[i], els];
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
        els = [1, nl[i], els];
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
            return [1,elem];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,elem.childNodes[i]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,elem.childNodes[i]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, elem.childNodes[i], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(children[1]));
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
        arr.push(E(strs[1]));
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

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
                xs = [1, [0, ks[i], toHS(obj[ks[i]])], xs];
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
                B(A(cb,[[1,xhr.responseText],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

/* gettimeofday(2) */
function gettimeofday(tv, _tz) {
    var t = new Date().getTime();
    writeOffAddr("i32", 4, tv, 0, (t/1000)|0);
    writeOffAddr("i32", 4, tv, 1, ((t%1000)*1000)|0);
    return 0;
}

/* Utility functions for working with JSStrings. */

var _jss_singleton = String.fromCharCode;

function _jss_cons(c,s) {return String.fromCharCode(c)+s;}
function _jss_snoc(s,c) {return s+String.fromCharCode(c);}
function _jss_append(a,b) {return a+b;}
function _jss_len(s) {return s.length;}
function _jss_index(s,i) {return s.charCodeAt(i);}
function _jss_drop(s,i) {return s.substr(i);}
function _jss_substr(s,a,b) {return s.substr(a,b);}
function _jss_take(n,s) {return s.substr(0,n);}
// TODO: incorrect for some unusual characters.
function _jss_rev(s) {return s.split("").reverse().join("");}

function _jss_map(f,s) {
    f = E(f);
    var s2 = '';
    for(var i in s) {
        s2 += String.fromCharCode(E(f(s.charCodeAt(i))));
    }
    return s2;
}

function _jss_foldl(f,x,s) {
    f = E(f);
    for(var i in s) {
        x = A(f,[x,s.charCodeAt(i)]);
    }
    return x;
}

function _jss_re_match(s,re) {return s.search(re)>=0;}
function _jss_re_compile(re,fs) {return new RegExp(re,fs);}
function _jss_re_replace(s,re,rep) {return s.replace(re,rep);}

function _jss_re_find(re,s) {
    var a = s.match(re);
    return a ? mklst(a) : [0];
}

function mklst(arr) {
    var l = [0], len = arr.length-1;
    for(var i = 0; i <= len; ++i) {
        l = [1,arr[len-i],l];
    }
    return l;
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
var __stable_table;

function makeStableName(x) {
    if(x instanceof Object) {
        if(!x.stableName) {
            x.stableName = __next_stable_name;
            __next_stable_name += 1;
        }
        return {type: 'obj', name: x.stableName};
    } else {
        return {type: 'prim', name: Number(x)};
    }
}

function eqStableName(x, y) {
    return (x.type == y.type && x.name == y.name) ? 1 : 0;
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

/* For foreign import ccall "wrapper" */
function createAdjustor(args, f, a, b) {
    return function(){
        var x = f.apply(null, arguments);
        while(x instanceof F) {x = x.f();}
        return x;
    };
}

var __apply = function(f,as) {
    var arr = [];
    for(; as[0] === 1; as = as[2]) {
        arr.push(as[1]);
    }
    arr.reverse();
    return f.apply(null, arr);
}
var __app0 = function(f) {return f();}
var __app1 = function(f,a) {return f(a);}
var __app2 = function(f,a,b) {return f(a,b);}
var __app3 = function(f,a,b,c) {return f(a,b,c);}
var __app4 = function(f,a,b,c,d) {return f(a,b,c,d);}
var __app5 = function(f,a,b,c,d,e) {return f(a,b,c,d,e);}
var __jsNull = function() {return null;}
var __jsTrue = function() {return true;}
var __jsFalse = function() {return false;}
var __eq = function(a,b) {return a===b;}
var __createJSFunc = function(arity, f){
    if(f instanceof Function && arity === f.length) {
        return (function() {
            var x = f.apply(null,arguments);
            if(x instanceof T) {
                if(x.f !== __blackhole) {
                    var ff = x.f;
                    x.f = __blackhole;
                    return x.x = ff();
                }
                return x.x;
            } else {
                while(x instanceof F) {
                    x = x.f();
                }
                return E(x);
            }
        });
    } else {
        return (function(){
            var as = Array.prototype.slice.call(arguments);
            as.push(0);
            return E(B(A(f,as)));
        });
    }
}


function __arr2lst(elem,arr) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem],new T(function(){return __arr2lst(elem+1,arr);})]
}

function __lst2arr(xs) {
    var arr = [];
    xs = E(xs);
    for(;xs[0] === 1; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

var __new = function() {return ({});}
var __set = function(o,k,v) {o[k]=v;}
var __get = function(o,k) {return o[k];}
var __has = function(o,k) {return o[k]!==undefined;}

var _0=function(_1,_2,_3,_4){_3=E(_3);if(!_3[0]){return E(_2);}else{var _5=_3[2];_4=E(_4);if(!_4[0]){return E(_2);}else{var _6=_4[2],_7=new T(function(){return B(_0(_1,_2,_5,_6));});return new F(function(){return A(_1,[_3[1],_4[1],_7]);});}}},_8=0,_9=new T(function(){return (function (id, name, classname, x, y) { placeCard(id, name, classname, x, y); });}),_a=function(_b,_c,_d,_e,_){_b=E(_b);_c=E(_c);_d=E(_d);_9=E(_9);var _f=_9(toJSStr(_b),toJSStr(_c),toJSStr(_d),310+(imul(90,_e)|0)|0,20);return _8;},_g=function(_h,_i){_h=E(_h);if(!_h[0]){return E(_i);}else{var _j=_h[2],_k=new T(function(){return B(_g(_j,_i));});return [1,_h[1],_k];}},_l=function(_m,_n){var _o=jsShowI(_m);return new F(function(){return _g(fromJSStr(_o),_n);});},_p=41,_q=40,_r=function(_s,_t,_u){if(_t>=0){return new F(function(){return _l(_t,_u);});}else{if(_s<=6){return new F(function(){return _l(_t,_u);});}else{var _v=new T(function(){var _w=jsShowI(_t);return B(_g(fromJSStr(_w),[1,_p,_u]));});return [1,_q,_v];}}},_x=[0],_y=function(_z){var _A=new T(function(){return B(_r(9,_z,_x));});return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",_A)));});},_B=new T(function(){return B(unCStr("king"));}),_C=new T(function(){return B(unCStr("queen"));}),_D=new T(function(){return B(unCStr("jack"));}),_E=new T(function(){return B(unCStr("10"));}),_F=new T(function(){return B(unCStr("club"));}),_G=new T(function(){return B(unCStr("spade"));}),_H=new T(function(){return B(unCStr("diamond"));}),_I=new T(function(){return B(unCStr("heart"));}),_J=95,_K=function(_L,_M){_L=E(_L);switch(_L){case 9:var _N=new T(function(){_M=E(_M);switch(_M){case 0:return E(_I);break;case 1:return E(_H);break;case 2:return E(_G);break;default:return E(_F);}});return new F(function(){return _g(_E,[1,_J,_N]);});break;case 10:var _O=new T(function(){_M=E(_M);switch(_M){case 0:return E(_I);break;case 1:return E(_H);break;case 2:return E(_G);break;default:return E(_F);}});return new F(function(){return _g(_D,[1,_J,_O]);});break;case 11:var _P=new T(function(){_M=E(_M);switch(_M){case 0:return E(_I);break;case 1:return E(_H);break;case 2:return E(_G);break;default:return E(_F);}});return new F(function(){return _g(_C,[1,_J,_P]);});break;case 12:var _Q=new T(function(){_M=E(_M);switch(_M){case 0:return E(_I);break;case 1:return E(_H);break;case 2:return E(_G);break;default:return E(_F);}});return new F(function(){return _g(_B,[1,_J,_Q]);});break;default:var _R=new T(function(){_M=E(_M);switch(_M){case 0:return E(_I);break;case 1:return E(_H);break;case 2:return E(_G);break;default:return E(_F);}}),_S=new T(function(){_L=E(_L);var _T=dataToTag(_L),_U=_T+49|0;if(_U>>>0>1114111){return B(_y(_U));}else{return _U;}});return [1,_S,[1,_J,_R]];}},_V=new T(function(){return B(unCStr("emptyFoundationClass"));}),_W=new T(function(){return B(unCStr("base_only"));}),_X=function(_Y,_Z){while(1){_Y=E(_Y);if(!_Y[0]){return E(_Z);}else{var _10=_Y[2],_11=[1,_Y[1],_Z];_Y=_10;_Z=_11;continue;}}},_12=function(_13,_14,_){var _15=new T(function(){return B(_r(0,_13,_x));},1),_16=B(_a(_W,B(_g(_V,_15)),_V,_13,_)),_17=new T(function(){var _18=new T(function(){return B(_r(0,_13,_x));});return B(unAppCStr("foundation",_18));}),_19=B(_X(_14,_x)),_=_;while(1){_19=E(_19);if(!_19[0]){return _8;}else{var _1a=_19[1];_1a=E(_1a);var _1b=_1a[1],_1c=_1a[2],_1d=B(_a(B(_K(_1b,_1c)),B(_K(_1b,_1c)),_17,_13,_)),_1e=_19[2];_19=_1e;continue;}}},_1f=function(_1g,_1h,_1i,_){_1g=E(_1g);var _1j=B(_12(_1g,_1h,_));return new F(function(){return A(_1i,[_]);});},_1k=function(_1l,_1m){if(_1l<=_1m){var _1n=function(_1o){var _1p=new T(function(){if(_1o!=_1m){return B(_1n(_1o+1|0));}else{return [0];}});return [1,_1o,_1p];};return new F(function(){return _1n(_1l);});}else{return [0];}},_1q=new T(function(){return B(_1k(0,2147483647));}),_1r=function(_){return _8;},_1s=function(_1t,_1u,_1v,_1w,_1x,_){_1t=E(_1t);_1u=E(_1u);_1v=E(_1v);_9=E(_9);var _1y=_9(toJSStr(_1t),toJSStr(_1u),toJSStr(_1v),40+(imul(90,_1w)|0)|0,160+(imul(20,_1x)|0)|0);return _8;},_1z=function(_1A,_1B){while(1){_1A=E(_1A);if(!_1A[0]){return E(_1B);}else{var _1C=_1A[2],_1D=_1B+1|0;_1A=_1C;_1B=_1D;continue;}}},_1E=new T(function(){return B(unCStr("visibleColumn"));}),_1F=function(_1G,_1H,_1I,_){var _1J=B(_1z(_1H,0));if(_1J<=2147483647){var _1K=new T(function(){var _1L=new T(function(){_1G=E(_1G);return B(_r(0,_1G,_x));},1);return B(_g(_1E,_1L));}),_1M=B(_X(_1I,_x));if(!_1M[0]){return _8;}else{var _1N=_1M[1];_1G=E(_1G);var _1O=_1G;_1N=E(_1N);var _1P=_1N[1],_1Q=_1N[2],_1R=B(_1s(B(_K(_1P,_1Q)),B(_K(_1P,_1Q)),_1K,_1O,_1J,_));_1J=E(_1J);if(_1J==2147483647){return _8;}else{var _1S=_1J+1|0,_1T=_1M[2],_=_;while(1){_1T=E(_1T);if(!_1T[0]){return _8;}else{var _1U=_1T[1];_1U=E(_1U);var _1V=_1U[1],_1W=_1U[2],_1X=B(_1s(B(_K(_1V,_1W)),B(_K(_1V,_1W)),_1K,_1O,_1S,_));_1S=E(_1S);if(_1S==2147483647){return _8;}else{var _1Y=_1S+1|0,_1Z=_1T[2];_1S=_1Y;_1T=_1Z;continue;}}}}}}else{return _8;}},_20=new T(function(){return B(unCStr("emptyColumn"));}),_21=new T(function(){return B(unCStr("hiddenColumn"));}),_22=new T(function(){return B(unCStr("back"));}),_23=function(_24,_25,_){var _26=new T(function(){return B(_r(0,_24,_x));},1),_27=B(_1s(_W,B(_g(_20,_26)),_20,_24,0,_));_25=E(_25);var _28=_25[1],_29=new T(function(){var _2a=new T(function(){return B(_r(0,_24,_x));},1);return B(_g(_21,_2a));}),_2b=function(_2c,_2d,_2e,_){_2c=E(_2c);_2d=E(_2d);var _2f=B(_1s(_22,B(_K(_2d[1],_2d[2])),_29,_24,_2c,_));return new F(function(){return A(_2e,[_]);});},_2g=B(A(_0,[_2b,_1r,_1q,_28,_]));return new F(function(){return _1F(_24,_28,_25[2],_);});},_2h=function(_2i,_2j,_2k,_){_2i=E(_2i);var _2l=B(_23(_2i,_2j,_));return new F(function(){return A(_2k,[_]);});},_2m=new T(function(){return B(unCStr("emptyReserves"));}),_2n=function(_2o,_2p){if(_2o<=0){if(_2o>=0){return new F(function(){return quot(_2o,_2p);});}else{if(_2p<=0){return new F(function(){return quot(_2o,_2p);});}else{return quot(_2o+1|0,_2p)-1|0;}}}else{if(_2p>=0){if(_2o>=0){return new F(function(){return quot(_2o,_2p);});}else{if(_2p<=0){return new F(function(){return quot(_2o,_2p);});}else{return quot(_2o+1|0,_2p)-1|0;}}}else{return quot(_2o-1|0,_2p)-1|0;}}},_2q=new T(function(){return 40+B(_2n(90,2))|0;}),_2r=function(_2s,_2t,_2u,_){_2s=E(_2s);_2t=E(_2t);_2u=E(_2u);_2q=E(_2q);_9=E(_9);var _2v=_9(toJSStr(_2s),toJSStr(_2t),toJSStr(_2u),_2q,20);return _8;},_2w=new T(function(){return B(unCStr("hiddenReserves"));}),_2x=function(_2y,_){while(1){_2y=E(_2y);if(!_2y[0]){return _8;}else{var _2z=_2y[1];_2z=E(_2z);var _2A=B(_2r(_22,B(_K(_2z[1],_2z[2])),_2w,_)),_2B=_2y[2];_2y=_2B;continue;}}},_2C=new T(function(){return B(unCStr("base_only_reserves"));}),_2D=function(_2E,_){var _2F=B(_2r(_W,_2C,_2m,_));return new F(function(){return _2x(_2E,_);});},_2G=new T(function(){return B(unCStr("emptyDeck"));}),_2H=new T(function(){return B(unCStr("solitareDeck"));}),_2I=new T(function(){_2q=E(_2q);return _2q+90|0;}),_2J=function(_2K,_2L,_2M,_){_2K=E(_2K);_2L=E(_2L);_2M=E(_2M);_2I=E(_2I);_9=E(_9);var _2N=_9(toJSStr(_2K),toJSStr(_2L),toJSStr(_2M),_2I,20);return _8;},_2O=function(_2P,_){while(1){_2P=E(_2P);if(!_2P[0]){return _8;}else{var _2Q=_2P[1];_2Q=E(_2Q);var _2R=_2Q[1],_2S=_2Q[2],_2T=B(_2J(B(_K(_2R,_2S)),B(_K(_2R,_2S)),_2H,_)),_2U=_2P[2];_2P=_2U;continue;}}},_2V=new T(function(){return B(unCStr("base_only_deck"));}),_2W=function(_2X,_){var _2Y=B(_2J(_W,_2V,_2G,_));return new F(function(){return _2O(B(_X(_2X,_x)),_);});},_2Z=function(_30,_31,_32,_33,_){var _34=B(A(_0,[_2h,_1r,_1q,_31,_])),_35=B(A(_0,[_1f,_1r,_1q,_30,_])),_36=B(_2W(_32,_));return new F(function(){return _2D(_33,_);});},_37=new T(function(){return B(unCStr(": empty list"));}),_38=new T(function(){return B(unCStr("Prelude."));}),_39=function(_3a){var _3b=new T(function(){return B(_g(_3a,_37));},1);return new F(function(){return err(B(_g(_38,_3b)));});},_3c=new T(function(){return B(unCStr("head"));}),_3d=new T(function(){return B(_39(_3c));}),_3e=function(_3f,_3g){while(1){_3f=E(_3f);if(!_3f){return E(_3g);}else{_3g=E(_3g);if(!_3g[0]){return [0];}else{var _3h=_3f-1|0,_3i=_3g[2];_3f=_3h;_3g=_3i;continue;}}}},_3j=function(_3k,_3l,_3m){_3k=E(_3k);if(!_3k[0]){return [0];}else{var _3n=_3k[1],_3o=_3k[2];_3l=E(_3l);if(!_3l[0]){return [0];}else{var _3p=_3l[2];_3m=E(_3m);if(!_3m[0]){return [0];}else{var _3q=_3m[2],_3r=new T(function(){return B(_3j(_3o,_3p,_3q));}),_3s=new T(function(){_3n=E(_3n);return E(_3n[1]);});return [1,[0,[1,_3l[1],_3s],_3m[1]],_3r];}}}},_3t=function(_3u,_3v){_3v=E(_3v);if(!_3v[0]){return [0];}else{var _3w=_3v[1],_3x=_3v[2],_3y=new T(function(){return B(_3t(_3u,_3x));}),_3z=new T(function(){return B(A(_3u,[_3w]));});return [1,_3z,_3y];}},_3A=new T(function(){return B(unCStr("tail"));}),_3B=new T(function(){return B(_39(_3A));}),_3C=function(_3D){_3D=E(_3D);return E(_3D[2]);},_3E=function(_3F,_3G){_3F=E(_3F);if(!_3F[0]){return [0,_x,_3G];}else{var _3H=_3F[1],_3I=_3F[2],_3J=new T(function(){_3G=E(_3G);if(!_3G[0]){return E(_3B);}else{var _3K=new T(function(){var _3L=B(_1z(_3F,0));if(_3L>=0){return B(_3e(_3L,_3G));}else{return E(_3G);}}),_3M=new T(function(){return B(_3t(_3C,_3I));},1),_3N=B(_3E(B(_3j(_3I,_3G[2],_3M)),_3K));return [0,_3N[1],_3N[2]];}}),_3O=new T(function(){_3J=E(_3J);return E(_3J[2]);}),_3P=new T(function(){_3J=E(_3J);return E(_3J[1]);}),_3Q=new T(function(){_3H=E(_3H);return E(_3H[2]);}),_3R=new T(function(){_3G=E(_3G);if(!_3G[0]){return E(_3d);}else{return E(_3G[1]);}}),_3S=new T(function(){_3H=E(_3H);return E(_3H[1]);});return [0,[1,[0,_3S,[1,_3R,_3Q]],_3P],_3O];}},_3T=[0],_3U=(function(){return window['md51'](jsRand().toString());}),_3V=function(_){_3U=E(_3U);return new F(function(){return _3U();});},_3W=function(_){return new F(function(){return _3V(_);});},_3X=new T(function(){return __jsNull();}),_3Y=new T(function(){return E(_3X);}),_3Z=new T(function(){return E(_3Y);}),_40=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_41=new T(function(){return B(err(_40));}),_42=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_43=new T(function(){return B(err(_42));}),_44=function(_45,_46){while(1){_45=E(_45);if(!_45[0]){return E(_43);}else{_46=E(_46);if(!_46){return E(_45[1]);}else{var _47=_45[2],_48=_46-1|0;_45=_47;_46=_48;continue;}}}},_49=function(_4a,_4b){_4a=E(_4a);_4b=E(_4b);return _4a!=_4b;},_4c=function(_4d,_4e){_4d=E(_4d);_4e=E(_4e);return _4d==_4e;},_4f=[0,_4c,_49],_4g=function(_4h,_4i,_4j){var _4k=new T(function(){var _4l=_4i+1|0;if(_4l>=0){return B(_3e(_4l,_4h));}else{return E(_4h);}}),_4m=new T(function(){if(_4i>=0){return B(_44(_4h,_4i));}else{return E(_41);}}),_4n=[1,[1,_4j,_4m],_4k];if(_4i>0){var _4o=function(_4p,_4q){_4p=E(_4p);if(!_4p[0]){return E(_4n);}else{var _4r=_4p[1],_4s=_4p[2];if(_4q>1){var _4t=new T(function(){return B(_4o(_4s,_4q-1|0));});return [1,_4r,_4t];}else{return [1,_4r,_4n];}}};return new F(function(){return _4o(_4h,_4i);});}else{return E(_4n);}},_4u=[0,_x,_x],_4v=function(_4w,_4x,_4y,_4z,_4A,_4B){var _4C=new T(function(){_4B=E(_4B);var _4D=_4B,_4E=new T(function(){_4A=E(_4A);if(_4A>=0){return B(_44(_4x,_4A));}else{return E(_41);}}),_4F=new T(function(){_4A=E(_4A);var _4G=_4A,_4H=new T(function(){var _4I=_4G+1|0;if(_4I>=0){return B(_3e(_4I,_4x));}else{return E(_4x);}}),_4J=new T(function(){_4E=E(_4E);var _4K=_4E[1];_4K=E(_4K);if(!_4K[0]){return E(_4u);}else{return [0,_4K[2],[1,_4K[1],_x]];}}),_4L=[1,_4J,_4H];if(_4G>0){var _4M=function(_4N,_4O){_4N=E(_4N);if(!_4N[0]){return E(_4L);}else{var _4P=_4N[1],_4Q=_4N[2];if(_4O>1){var _4R=new T(function(){return B(_4M(_4Q,_4O-1|0));});return [1,_4P,_4R];}else{return [1,_4P,_4L];}}};return B(_4M(_4x,_4G));}else{return E(_4L);}}),_4S=new T(function(){var _4T=_4D+1|0;if(_4T>=0){return B(_3e(_4T,_4F));}else{return E(_4F);}}),_4U=new T(function(){if(_4D>=0){return B(_44(_4F,_4D));}else{return E(_41);}}),_4V=new T(function(){_4E=E(_4E);var _4W=new T(function(){_4U=E(_4U);return E(_4U[2]);},1);return B(_g(_4E[2],_4W));}),_4X=new T(function(){_4U=E(_4U);return E(_4U[1]);}),_4Y=[1,[0,_4X,_4V],_4S];if(_4D>0){var _4Z=function(_50,_51){_50=E(_50);if(!_50[0]){return E(_4Y);}else{var _52=_50[1],_53=_50[2];if(_51>1){var _54=new T(function(){return B(_4Z(_53,_51-1|0));});return [1,_52,_54];}else{return [1,_52,_4Y];}}};return B(_4Z(_4F,_4D));}else{return E(_4Y);}});return [0,_4w,_4C,_4y,_4z];},_55=function(_56,_57,_58,_59,_5a,_5b){var _5c=new T(function(){_5a=E(_5a);if(_5a>=0){return B(_44(_57,_5a));}else{return E(_41);}}),_5d=new T(function(){_5c=E(_5c);return E(_5c[2]);}),_5e=new T(function(){_5a=E(_5a);var _5f=_5a,_5g=new T(function(){var _5h=_5f+1|0;if(_5h>=0){return B(_3e(_5h,_57));}else{return E(_57);}}),_5i=new T(function(){_5c=E(_5c);var _5j=_5c[1];_5j=E(_5j);if(!_5j[0]){var _5k=new T(function(){_5d=E(_5d);if(!_5d[0]){return E(_3B);}else{return E(_5d[2]);}});return [0,_x,_5k];}else{_5d=E(_5d);if(!_5d[0]){return E(_3B);}else{var _5l=_5d[2];_5l=E(_5l);if(!_5l[0]){return [0,_5j[2],[1,_5j[1],_x]];}else{return [0,_5j,_5l];}}}}),_5m=[1,_5i,_5g];if(_5f>0){var _5n=function(_5o,_5p){_5o=E(_5o);if(!_5o[0]){return E(_5m);}else{var _5q=_5o[1],_5r=_5o[2];if(_5p>1){var _5s=new T(function(){return B(_5n(_5r,_5p-1|0));});return [1,_5q,_5s];}else{return [1,_5q,_5m];}}};return B(_5n(_57,_5f));}else{return E(_5m);}}),_5t=new T(function(){_5b=E(_5b);var _5u=new T(function(){_5d=E(_5d);if(!_5d[0]){return E(_3d);}else{return E(_5d[1]);}});return B(_4g(_56,_5b,_5u));});return [0,_5t,_5e,_58,_59];},_5v=function(_5w,_5x,_5y,_5z,_5A){var _5B=new T(function(){_5y=E(_5y);if(!_5y[0]){return E(_3B);}else{return E(_5y[2]);}}),_5C=new T(function(){_5A=E(_5A);var _5D=_5A,_5E=new T(function(){var _5F=_5D+1|0;if(_5F>=0){return B(_3e(_5F,_5x));}else{return E(_5x);}}),_5G=new T(function(){if(_5D>=0){return B(_44(_5x,_5D));}else{return E(_41);}}),_5H=new T(function(){_5G=E(_5G);return E(_5G[2]);}),_5I=new T(function(){_5y=E(_5y);if(!_5y[0]){return E(_3d);}else{return E(_5y[1]);}}),_5J=new T(function(){_5G=E(_5G);return E(_5G[1]);}),_5K=[1,[0,_5J,[1,_5I,_5H]],_5E];if(_5D>0){var _5L=function(_5M,_5N){_5M=E(_5M);if(!_5M[0]){return E(_5K);}else{var _5O=_5M[1],_5P=_5M[2];if(_5N>1){var _5Q=new T(function(){return B(_5L(_5P,_5N-1|0));});return [1,_5O,_5Q];}else{return [1,_5O,_5K];}}};return B(_5L(_5x,_5D));}else{return E(_5K);}});return [0,_5w,_5C,_5B,_5z];},_5R=new T(function(){return B(unCStr("Control.Exception.Base"));}),_5S=new T(function(){return B(unCStr("base"));}),_5T=new T(function(){return B(unCStr("PatternMatchFail"));}),_5U=new T(function(){var _5V=hs_wordToWord64(18445595),_5W=hs_wordToWord64(52003073);return [0,_5V,_5W,[0,_5V,_5W,_5S,_5R,_5T],_x];}),_5X=function(_5Y){return E(_5U);},_5Z=function(_60){_60=E(_60);return E(_60[1]);},_61=function(_62,_63,_64){var _65=B(A(_62,[_])),_66=B(A(_63,[_])),_67=hs_eqWord64(_65[1],_66[1]);_67=E(_67);if(!_67){return [0];}else{var _68=hs_eqWord64(_65[2],_66[2]);_68=E(_68);return (_68==0)?[0]:[1,_64];}},_69=function(_6a){_6a=E(_6a);return new F(function(){return _61(B(_5Z(_6a[1])),_5X,_6a[2]);});},_6b=function(_6c){_6c=E(_6c);return E(_6c[1]);},_6d=function(_6e,_6f){_6e=E(_6e);return new F(function(){return _g(_6e[1],_6f);});},_6g=44,_6h=93,_6i=91,_6j=function(_6k,_6l,_6m){_6l=E(_6l);if(!_6l[0]){return new F(function(){return unAppCStr("[]",_6m);});}else{var _6n=_6l[1],_6o=_6l[2],_6p=new T(function(){var _6q=new T(function(){var _6r=[1,_6h,_6m],_6s=function(_6t){_6t=E(_6t);if(!_6t[0]){return E(_6r);}else{var _6u=_6t[1],_6v=_6t[2],_6w=new T(function(){var _6x=new T(function(){return B(_6s(_6v));});return B(A(_6k,[_6u,_6x]));});return [1,_6g,_6w];}};return B(_6s(_6o));});return B(A(_6k,[_6n,_6q]));});return [1,_6i,_6p];}},_6y=function(_6z,_6A){return new F(function(){return _6j(_6d,_6z,_6A);});},_6B=function(_6C,_6D,_6E){_6D=E(_6D);return new F(function(){return _g(_6D[1],_6E);});},_6F=[0,_6B,_6b,_6y],_6G=new T(function(){return [0,_5X,_6F,_6H,_69];}),_6H=function(_6I){return [0,_6G,_6I];},_6J=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_6K=function(_6L,_6M){var _6N=new T(function(){return B(A(_6M,[_6L]));});return new F(function(){return die(_6N);});},_6O=function(_6P,_6Q){_6Q=E(_6Q);if(!_6Q[0]){return [0,_x,_x];}else{var _6R=_6Q[1],_6S=_6Q[2];if(!B(A(_6P,[_6R]))){return [0,_x,_6Q];}else{var _6T=new T(function(){var _6U=B(_6O(_6P,_6S));return [0,_6U[1],_6U[2]];}),_6V=new T(function(){_6T=E(_6T);return E(_6T[2]);}),_6W=new T(function(){_6T=E(_6T);return E(_6T[1]);});return [0,[1,_6R,_6W],_6V];}}},_6X=32,_6Y=10,_6Z=[1,_6Y,_x],_70=function(_71){_71=E(_71);return (_71==124)?false:true;},_72=function(_73,_74){var _75=B(_6O(_70,B(unCStr(_73)))),_76=_75[1],_77=_75[2],_78=function(_79,_7a){var _7b=new T(function(){var _7c=new T(function(){var _7d=new T(function(){return B(_g(_7a,_6Z));},1);return B(_g(_74,_7d));});return B(unAppCStr(": ",_7c));},1);return new F(function(){return _g(_79,_7b);});};_77=E(_77);if(!_77[0]){return new F(function(){return _78(_76,_x);});}else{var _7e=_77[1];_7e=E(_7e);if(_7e==124){return new F(function(){return _78(_76,[1,_6X,_77[2]]);});}else{return new F(function(){return _78(_76,_x);});}}},_7f=function(_7g){var _7h=new T(function(){return B(_72(_7g,_6J));});return new F(function(){return _6K([0,_7h],_6H);});},_7i=new T(function(){return B(_7f("Game.hs:(47,1)-(49,86)|function goesOnColumn"));}),_7j=function(_7k,_7l,_7m,_7n){var _7o=function(_7p){_7n=E(_7n);if(!_7n[0]){return E(_7i);}else{var _7q=_7n[1];_7l=E(_7l);switch(_7l){case 2:_7q=E(_7q);var _7r=_7q[1],_7s=_7q[2];_7s=E(_7s);switch(_7s){case 2:return false;case 3:return false;default:_7k=E(_7k);_7r=E(_7r);var _7t=dataToTag(_7r);_7k=E(_7k);var _7u=dataToTag(_7k);return (_7u+1|0)==_7t;}break;case 3:_7q=E(_7q);var _7v=_7q[1],_7w=_7q[2];_7w=E(_7w);switch(_7w){case 2:return false;case 3:return false;default:_7k=E(_7k);_7v=E(_7v);var _7x=dataToTag(_7v);_7k=E(_7k);var _7u=dataToTag(_7k);return (_7u+1|0)==_7x;}break;default:_7q=E(_7q);var _7y=_7q[1],_7z=_7q[2];_7z=E(_7z);switch(_7z){case 2:_7k=E(_7k);_7y=E(_7y);var _7A=dataToTag(_7y);_7k=E(_7k);var _7u=dataToTag(_7k);return (_7u+1|0)==_7A;case 3:_7k=E(_7k);_7y=E(_7y);var _7A=dataToTag(_7y);_7k=E(_7k);var _7u=dataToTag(_7k);return (_7u+1|0)==_7A;default:return false;}}}};_7m=E(_7m);if(!_7m[0]){_7n=E(_7n);if(!_7n[0]){_7k=E(_7k);var _7u=dataToTag(_7k);return (_7u==12)?true:false;}else{return new F(function(){return _7o(_);});}}else{return new F(function(){return _7o(_);});}},_7B=function(_7C,_7D,_7E){_7E=E(_7E);if(!_7E[0]){_7C=E(_7C);var _7F=dataToTag(_7C);return (_7F==0)?true:false;}else{var _7G=_7E[1];_7D=E(_7D);switch(_7D){case 0:_7G=E(_7G);var _7H=_7G[1],_7I=_7G[2];_7I=E(_7I);if(!_7I){_7C=E(_7C);_7H=E(_7H);var _7J=dataToTag(_7H);_7C=E(_7C);var _7F=dataToTag(_7C);return _7F==(_7J+1|0);}else{return false;}break;case 1:_7G=E(_7G);var _7K=_7G[1],_7L=_7G[2];_7L=E(_7L);if(_7L==1){_7C=E(_7C);_7K=E(_7K);var _7M=dataToTag(_7K);_7C=E(_7C);var _7F=dataToTag(_7C);return _7F==(_7M+1|0);}else{return false;}break;case 2:_7G=E(_7G);var _7N=_7G[1],_7O=_7G[2];_7O=E(_7O);if(_7O==2){_7C=E(_7C);_7N=E(_7N);var _7P=dataToTag(_7N);_7C=E(_7C);var _7F=dataToTag(_7C);return _7F==(_7P+1|0);}else{return false;}break;default:_7G=E(_7G);var _7Q=_7G[1],_7R=_7G[2];_7R=E(_7R);if(_7R==3){_7C=E(_7C);_7Q=E(_7Q);var _7S=dataToTag(_7Q);_7C=E(_7C);var _7F=dataToTag(_7C);return _7F==(_7S+1|0);}else{return false;}}}},_7T=new T(function(){return (function (classname) { deleteByClass(classname); });}),_7U=function(_7V,_){var _7W=new T(function(){_7V=E(_7V);return B(_r(0,_7V,_x));},1);_7T=E(_7T);var _7X=_7T(toJSStr(B(_g(_21,_7W)))),_7Y=new T(function(){_7V=E(_7V);return B(_r(0,_7V,_x));},1),_7Z=_7T(toJSStr(B(_g(_1E,_7Y))));return _8;},_80=function(_){_2G=E(_2G);_7T=E(_7T);var _81=_7T(toJSStr(_2G));_2H=E(_2H);var _82=_7T(toJSStr(_2H));return _8;},_83=function(_){_2m=E(_2m);_7T=E(_7T);var _84=_7T(toJSStr(_2m));_2w=E(_2w);var _85=_7T(toJSStr(_2w));return _8;},_86=function(_87,_){_87=E(_87);return new F(function(){return _2Z(_87[1],_87[2],_87[3],_87[4],_);});},_88=function(_89,_8a){while(1){_89=E(_89);if(!_89[0]){_8a=E(_8a);return (_8a[0]==0)?true:false;}else{var _8b=_89[1];_8a=E(_8a);if(!_8a[0]){return false;}else{var _8c=_8a[1];_8b=E(_8b);_8c=E(_8c);if(_8b!=_8c){return false;}else{var _8d=_89[2],_8e=_8a[2];_89=_8d;_8a=_8e;continue;}}}}},_8f=new T(function(){return (function (msg) { console.log(msg); });}),_8g=new T(function(){return (function (cb) { mouseoverCallback = cb; });}),_8h=(function(cb) { dragEndCallback = cb; }),_8i=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_8j=new T(function(){return B(err(_8i));}),_8k=function(_8l){_8l=E(_8l);return E(_8l[1]);},_8m=function(_8n,_8o,_8p){while(1){_8o=E(_8o);if(!_8o[0]){return true;}else{_8p=E(_8p);if(!_8p[0]){return false;}else{if(!B(A(_8k,[_8n,_8o[1],_8p[1]]))){return false;}else{var _8q=_8o[2],_8r=_8p[2];_8o=_8q;_8p=_8r;continue;}}}}},_8s=function(_8t,_8u){while(1){_8u=E(_8u);if(!_8u[0]){return E(_8t);}else{var _8v=_8u[1],_8w=_8u[2];_8t=_8v;_8u=_8w;continue;}}},_8x=new T(function(){return B(unCStr("last"));}),_8y=new T(function(){return B(_39(_8x));}),_8z=[1,_2H],_8A=[1,_2w],_8B=6,_8C=new T(function(){_2I=E(_2I);return _2I+90|0;}),_8D=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_8E=new T(function(){return B(err(_8D));}),_8F=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_8G=new T(function(){return B(err(_8F));}),_8H=new T(function(){return B(_7f("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_8I=function(_8J,_8K){while(1){var _8L=(function(_8M,_8N){_8M=E(_8M);switch(_8M[0]){case 0:_8N=E(_8N);if(!_8N[0]){return [0];}else{var _8O=B(A(_8M[1],[_8N[1]])),_8P=_8N[2];_8J=_8O;_8K=_8P;return null;}break;case 1:var _8O=B(A(_8M[1],[_8N])),_8P=_8N;_8J=_8O;_8K=_8P;return null;case 2:return [0];case 3:var _8Q=_8M[2],_8R=new T(function(){return B(_8I(_8Q,_8N));});return [1,[0,_8M[1],_8N],_8R];default:return E(_8M[1]);}})(_8J,_8K);if(_8L!=null){return _8L;}}},_8S=function(_8T,_8U){var _8V=function(_8W){_8U=E(_8U);if(_8U[0]==3){var _8X=_8U[2],_8Y=new T(function(){return B(_8S(_8T,_8X));});return [3,_8U[1],_8Y];}else{_8T=E(_8T);if(_8T[0]==2){return E(_8U);}else{_8U=E(_8U);if(_8U[0]==2){return E(_8T);}else{var _8Z=function(_90){_8U=E(_8U);if(_8U[0]==4){var _91=_8U[1];return [1,function(_92){return [4,new T(function(){return B(_g(B(_8I(_8T,_92)),_91));})];}];}else{_8T=E(_8T);if(_8T[0]==1){var _93=_8T[1];_8U=E(_8U);if(!_8U[0]){return [1,function(_94){return new F(function(){return _8S(B(A(_93,[_94])),_8U);});}];}else{var _95=_8U[1];return [1,function(_96){var _97=new T(function(){return B(A(_95,[_96]));});return new F(function(){return _8S(B(A(_93,[_96])),_97);});}];}}else{_8U=E(_8U);if(!_8U[0]){return E(_8H);}else{var _98=_8U[1];return [1,function(_99){var _9a=new T(function(){return B(A(_98,[_99]));});return new F(function(){return _8S(_8T,_9a);});}];}}}};_8T=E(_8T);switch(_8T[0]){case 1:var _9b=_8T[1];_8U=E(_8U);if(_8U[0]==4){var _9c=_8U[1];return [1,function(_9d){return [4,new T(function(){return B(_g(B(_8I(B(A(_9b,[_9d])),_9d)),_9c));})];}];}else{return new F(function(){return _8Z(_);});}break;case 4:var _9e=_8T[1];_8U=E(_8U);switch(_8U[0]){case 0:return [1,function(_9f){return [4,new T(function(){var _9g=new T(function(){return B(_8I(_8U,_9f));},1);return B(_g(_9e,_9g));})];}];case 1:var _9h=_8U[1];return [1,function(_9i){return [4,new T(function(){var _9j=new T(function(){return B(_8I(B(A(_9h,[_9i])),_9i));},1);return B(_g(_9e,_9j));})];}];default:var _9k=_8U[1];return [4,new T(function(){return B(_g(_9e,_9k));})];}break;default:return new F(function(){return _8Z(_);});}}}}};_8T=E(_8T);switch(_8T[0]){case 0:var _9l=_8T[1];_8U=E(_8U);if(!_8U[0]){var _9m=_8U[1];return [0,function(_9n){var _9o=new T(function(){return B(A(_9m,[_9n]));});return new F(function(){return _8S(B(A(_9l,[_9n])),_9o);});}];}else{return new F(function(){return _8V(_);});}break;case 3:var _9p=_8T[2],_9q=new T(function(){return B(_8S(_9p,_8U));});return [3,_8T[1],_9q];default:return new F(function(){return _8V(_);});}},_9r=41,_9s=[1,_9r,_x],_9t=40,_9u=[1,_9t,_x],_9v=function(_9w,_9x){while(1){_9w=E(_9w);if(!_9w[0]){_9x=E(_9x);return (_9x[0]==0)?true:false;}else{var _9y=_9w[1];_9x=E(_9x);if(!_9x[0]){return false;}else{var _9z=_9x[1];_9y=E(_9y);_9z=E(_9z);if(_9y!=_9z){return false;}else{var _9A=_9w[2],_9B=_9x[2];_9w=_9A;_9x=_9B;continue;}}}}},_9C=function(_9D,_9E){return (!B(_9v(_9D,_9E)))?true:false;},_9F=[0,_9v,_9C],_9G=function(_9H,_9I){_9H=E(_9H);switch(_9H[0]){case 0:var _9J=_9H[1];return [0,function(_9K){return new F(function(){return _9G(B(A(_9J,[_9K])),_9I);});}];case 1:var _9L=_9H[1];return [1,function(_9M){return new F(function(){return _9G(B(A(_9L,[_9M])),_9I);});}];case 2:return [2];case 3:var _9N=_9H[2],_9O=new T(function(){return B(_9G(_9N,_9I));});return new F(function(){return _8S(B(A(_9I,[_9H[1]])),_9O);});break;default:var _9P=function(_9Q){_9Q=E(_9Q);if(!_9Q[0]){return [0];}else{var _9R=_9Q[1],_9S=_9Q[2];_9R=E(_9R);var _9T=new T(function(){return B(_9P(_9S));},1);return new F(function(){return _g(B(_8I(B(A(_9I,[_9R[1]])),_9R[2])),_9T);});}},_9U=B(_9P(_9H[1]));return (_9U[0]==0)?[2]:[4,_9U];}},_9V=[2],_9W=function(_9X){return [3,_9X,_9V];},_9Y=function(_9Z,_a0){_9Z=E(_9Z);if(!_9Z){return new F(function(){return A(_a0,[_8]);});}else{var _a1=new T(function(){return B(_9Y(_9Z-1|0,_a0));});return [0,function(_a2){return E(_a1);}];}},_a3=function(_a4,_a5,_a6){var _a7=new T(function(){return B(A(_a4,[_9W]));}),_a8=function(_a9,_aa,_ab){while(1){var _ac=(function(_ad,_ae,_af){_ad=E(_ad);switch(_ad[0]){case 0:_ae=E(_ae);if(!_ae[0]){return E(_a5);}else{var _ag=B(A(_ad[1],[_ae[1]])),_ah=_ae[2],_ai=_af+1|0;_a9=_ag;_aa=_ah;_ab=_ai;return null;}break;case 1:var _ag=B(A(_ad[1],[_ae])),_ah=_ae,_ai=_af;_a9=_ag;_aa=_ah;_ab=_ai;return null;case 2:return E(_a5);case 3:var _aj=function(_ak){var _al=new T(function(){return B(_9G(_ad,_ak));}),_am=function(_an){return E(_al);};return new F(function(){return _9Y(_af,_am);});};return E(_aj);default:return function(_ao){return new F(function(){return _9G(_ad,_ao);});};}})(_a9,_aa,_ab);if(_ac!=null){return _ac;}}};return function(_ap){return new F(function(){return A(_a8,[_a7,_ap,0,_a6]);});};},_aq=function(_ar){return new F(function(){return A(_ar,[_x]);});},_as=function(_at,_au){var _av=function(_aw){_aw=E(_aw);if(!_aw[0]){return E(_aq);}else{var _ax=_aw[1],_ay=_aw[2];if(!B(A(_at,[_ax]))){return E(_aq);}else{var _az=new T(function(){return B(_av(_ay));}),_aA=function(_aB){var _aC=new T(function(){var _aD=function(_aE){return new F(function(){return A(_aB,[[1,_ax,_aE]]);});};return B(A(_az,[_aD]));});return [0,function(_aF){return E(_aC);}];};return E(_aA);}}};return function(_aG){return new F(function(){return A(_av,[_aG,_au]);});};},_aH=[6],_aI=function(_aJ){return E(_aJ);},_aK=new T(function(){return B(unCStr("valDig: Bad base"));}),_aL=new T(function(){return B(err(_aK));}),_aM=function(_aN,_aO){var _aP=function(_aQ,_aR){_aQ=E(_aQ);if(!_aQ[0]){var _aS=new T(function(){return B(A(_aR,[_x]));}),_aT=function(_aU){return new F(function(){return A(_aU,[_aS]);});};return E(_aT);}else{var _aV=_aQ[1],_aW=_aQ[2];_aN=E(_aN);_aV=E(_aV);var _aX=function(_aY){var _aZ=new T(function(){var _b0=function(_b1){return new F(function(){return A(_aR,[[1,_aY,_b1]]);});};return B(_aP(_aW,_b0));}),_b2=function(_b3){var _b4=new T(function(){return B(A(_aZ,[_b3]));});return [0,function(_b5){return E(_b4);}];};return E(_b2);};_aN=E(_aN);switch(_aN){case 8:if(48>_aV){var _b6=new T(function(){return B(A(_aR,[_x]));}),_b7=function(_b8){return new F(function(){return A(_b8,[_b6]);});};return E(_b7);}else{if(_aV>55){var _b9=new T(function(){return B(A(_aR,[_x]));}),_ba=function(_bb){return new F(function(){return A(_bb,[_b9]);});};return E(_ba);}else{return new F(function(){return _aX(_aV-48|0);});}}break;case 10:if(48>_aV){var _bc=new T(function(){return B(A(_aR,[_x]));}),_bd=function(_be){return new F(function(){return A(_be,[_bc]);});};return E(_bd);}else{if(_aV>57){var _bf=new T(function(){return B(A(_aR,[_x]));}),_bg=function(_bh){return new F(function(){return A(_bh,[_bf]);});};return E(_bg);}else{return new F(function(){return _aX(_aV-48|0);});}}break;case 16:if(48>_aV){if(97>_aV){if(65>_aV){var _bi=new T(function(){return B(A(_aR,[_x]));}),_bj=function(_bk){return new F(function(){return A(_bk,[_bi]);});};return E(_bj);}else{if(_aV>70){var _bl=new T(function(){return B(A(_aR,[_x]));}),_bm=function(_bn){return new F(function(){return A(_bn,[_bl]);});};return E(_bm);}else{return new F(function(){return _aX((_aV-65|0)+10|0);});}}}else{if(_aV>102){if(65>_aV){var _bo=new T(function(){return B(A(_aR,[_x]));}),_bp=function(_bq){return new F(function(){return A(_bq,[_bo]);});};return E(_bp);}else{if(_aV>70){var _br=new T(function(){return B(A(_aR,[_x]));}),_bs=function(_bt){return new F(function(){return A(_bt,[_br]);});};return E(_bs);}else{return new F(function(){return _aX((_aV-65|0)+10|0);});}}}else{return new F(function(){return _aX((_aV-97|0)+10|0);});}}}else{if(_aV>57){if(97>_aV){if(65>_aV){var _bu=new T(function(){return B(A(_aR,[_x]));}),_bv=function(_bw){return new F(function(){return A(_bw,[_bu]);});};return E(_bv);}else{if(_aV>70){var _bx=new T(function(){return B(A(_aR,[_x]));}),_by=function(_bz){return new F(function(){return A(_bz,[_bx]);});};return E(_by);}else{return new F(function(){return _aX((_aV-65|0)+10|0);});}}}else{if(_aV>102){if(65>_aV){var _bA=new T(function(){return B(A(_aR,[_x]));}),_bB=function(_bC){return new F(function(){return A(_bC,[_bA]);});};return E(_bB);}else{if(_aV>70){var _bD=new T(function(){return B(A(_aR,[_x]));}),_bE=function(_bF){return new F(function(){return A(_bF,[_bD]);});};return E(_bE);}else{return new F(function(){return _aX((_aV-65|0)+10|0);});}}}else{return new F(function(){return _aX((_aV-97|0)+10|0);});}}}else{return new F(function(){return _aX(_aV-48|0);});}}break;default:return E(_aL);}}},_bG=function(_bH){_bH=E(_bH);if(!_bH[0]){return [2];}else{return new F(function(){return A(_aO,[_bH]);});}};return function(_bI){return new F(function(){return A(_aP,[_bI,_aI,_bG]);});};},_bJ=10,_bK=[0,1],_bL=[0,2147483647],_bM=function(_bN,_bO){while(1){_bN=E(_bN);if(!_bN[0]){var _bP=_bN[1];_bO=E(_bO);if(!_bO[0]){var _bQ=_bO[1],_bR=addC(_bP,_bQ),_bS=_bR[2];_bS=E(_bS);if(!_bS){return [0,_bR[1]];}else{_bN=[1,I_fromInt(_bP)];_bO=[1,I_fromInt(_bQ)];continue;}}else{_bN=[1,I_fromInt(_bP)];continue;}}else{_bO=E(_bO);if(!_bO[0]){var _bT=[1,I_fromInt(_bO[1])];_bO=_bT;continue;}else{return [1,I_add(_bN[1],_bO[1])];}}}},_bU=new T(function(){return B(_bM(_bL,_bK));}),_bV=function(_bW){_bW=E(_bW);if(!_bW[0]){var _bX=_bW[1];_bX=E(_bX);return (_bX==(-2147483648))?E(_bU):[0, -_bX];}else{return [1,I_negate(_bW[1])];}},_bY=[0,10],_bZ=[0,0],_c0=function(_c1){return [0,_c1];},_c2=function(_c3,_c4){while(1){_c3=E(_c3);if(!_c3[0]){var _c5=_c3[1];_c4=E(_c4);if(!_c4[0]){var _c6=_c4[1];if(!(imul(_c5,_c6)|0)){return [0,imul(_c5,_c6)|0];}else{_c3=[1,I_fromInt(_c5)];_c4=[1,I_fromInt(_c6)];continue;}}else{_c3=[1,I_fromInt(_c5)];continue;}}else{_c4=E(_c4);if(!_c4[0]){var _c7=[1,I_fromInt(_c4[1])];_c4=_c7;continue;}else{return [1,I_mul(_c3[1],_c4[1])];}}}},_c8=function(_c9,_ca,_cb){while(1){_cb=E(_cb);if(!_cb[0]){return E(_ca);}else{var _cc=_cb[1];_cc=E(_cc);var _cd=B(_bM(B(_c2(_ca,_c9)),B(_c0(_cc)))),_ce=_cb[2];_ca=_cd;_cb=_ce;continue;}}},_cf=function(_cg){var _ch=new T(function(){var _ci=new T(function(){var _cj=function(_ck){var _cl=new T(function(){return B(_c8(_bY,_bZ,_ck));});return new F(function(){return A(_cg,[[1,_cl]]);});};return [1,B(_aM(_bJ,_cj))];}),_cm=function(_cn){_cn=E(_cn);if(_cn==43){var _co=function(_cp){var _cq=new T(function(){return B(_c8(_bY,_bZ,_cp));});return new F(function(){return A(_cg,[[1,_cq]]);});};return [1,B(_aM(_bJ,_co))];}else{return [2];}},_cr=function(_cs){_cs=E(_cs);if(_cs==45){var _ct=function(_cu){var _cv=new T(function(){return B(_bV(B(_c8(_bY,_bZ,_cu))));});return new F(function(){return A(_cg,[[1,_cv]]);});};return [1,B(_aM(_bJ,_ct))];}else{return [2];}};return B(_8S(B(_8S([0,_cr],[0,_cm])),_ci));}),_cw=function(_cx){_cx=E(_cx);return (_cx==69)?E(_ch):[2];},_cy=function(_cz){_cz=E(_cz);return (_cz==101)?E(_ch):[2];};return new F(function(){return _8S([0,_cy],[0,_cw]);});},_cA=function(_cB){return new F(function(){return A(_cB,[_3T]);});},_cC=function(_cD){return new F(function(){return A(_cD,[_3T]);});},_cE=function(_cF){var _cG=function(_cH){return new F(function(){return A(_cF,[[1,_cH]]);});};return function(_cI){_cI=E(_cI);return (_cI==46)?[1,B(_aM(_bJ,_cG))]:[2];};},_cJ=function(_cK){return [0,B(_cE(_cK))];},_cL=function(_cM){var _cN=function(_cO){var _cP=function(_cQ){var _cR=function(_cS){return new F(function(){return A(_cM,[[5,[1,_cO,_cQ,_cS]]]);});};return [1,B(_a3(_cf,_cC,_cR))];};return [1,B(_a3(_cJ,_cA,_cP))];};return new F(function(){return _aM(_bJ,_cN);});},_cT=function(_cU){return [1,B(_cL(_cU))];},_cV=function(_cW,_cX,_cY){while(1){_cY=E(_cY);if(!_cY[0]){return false;}else{if(!B(A(_8k,[_cW,_cX,_cY[1]]))){var _cZ=_cY[2];_cY=_cZ;continue;}else{return true;}}}},_d0=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_d1=function(_d2){return new F(function(){return _cV(_4f,_d2,_d0);});},_d3=8,_d4=16,_d5=function(_d6){var _d7=function(_d8){return new F(function(){return A(_d6,[[5,[0,_d3,_d8]]]);});},_d9=function(_da){return new F(function(){return A(_d6,[[5,[0,_d4,_da]]]);});},_db=function(_dc){_dc=E(_dc);switch(_dc){case 79:return [1,B(_aM(_d3,_d7))];case 88:return [1,B(_aM(_d4,_d9))];case 111:return [1,B(_aM(_d3,_d7))];case 120:return [1,B(_aM(_d4,_d9))];default:return [2];}},_dd=[0,_db];return function(_de){_de=E(_de);return (_de==48)?E(_dd):[2];};},_df=function(_dg){return [0,B(_d5(_dg))];},_dh=false,_di=true,_dj=function(_dk){var _dl=new T(function(){return B(A(_dk,[_d3]));}),_dm=new T(function(){return B(A(_dk,[_d4]));});return function(_dn){_dn=E(_dn);switch(_dn){case 79:return E(_dl);case 88:return E(_dm);case 111:return E(_dl);case 120:return E(_dm);default:return [2];}};},_do=function(_dp){return [0,B(_dj(_dp))];},_dq=92,_dr=function(_ds){return new F(function(){return A(_ds,[_bJ]);});},_dt=function(_du){_du=E(_du);if(!_du[0]){return E(_du[1]);}else{return new F(function(){return I_toInt(_du[1]);});}},_dv=function(_dw,_dx){_dw=E(_dw);if(!_dw[0]){var _dy=_dw[1];_dx=E(_dx);return (_dx[0]==0)?_dy<=_dx[1]:I_compareInt(_dx[1],_dy)>=0;}else{var _dz=_dw[1];_dx=E(_dx);return (_dx[0]==0)?I_compareInt(_dz,_dx[1])<=0:I_compare(_dz,_dx[1])<=0;}},_dA=function(_dB){return [2];},_dC=function(_dD){_dD=E(_dD);if(!_dD[0]){return E(_dA);}else{var _dE=_dD[1],_dF=_dD[2];_dF=E(_dF);if(!_dF[0]){return E(_dE);}else{var _dG=new T(function(){return B(_dC(_dF));}),_dH=function(_dI){var _dJ=new T(function(){return B(A(_dG,[_dI]));});return new F(function(){return _8S(B(A(_dE,[_dI])),_dJ);});};return E(_dH);}}},_dK=function(_dL){return [2];},_dM=function(_dN,_dO){var _dP=function(_dQ){return new F(function(){return A(_dQ,[_dN]);});},_dR=function(_dS,_dT){_dS=E(_dS);if(!_dS[0]){return E(_dP);}else{var _dU=_dS[1],_dV=_dS[2];_dT=E(_dT);if(!_dT[0]){return E(_dK);}else{var _dW=_dT[1],_dX=_dT[2];_dU=E(_dU);_dW=E(_dW);if(_dU!=_dW){return E(_dK);}else{var _dY=new T(function(){return B(_dR(_dV,_dX));}),_dZ=function(_e0){var _e1=new T(function(){return B(A(_dY,[_e0]));});return [0,function(_e2){return E(_e1);}];};return E(_dZ);}}}};return function(_e3){return new F(function(){return A(_dR,[_dN,_e3,_dO]);});};},_e4=new T(function(){return B(unCStr("SOH"));}),_e5=1,_e6=function(_e7){var _e8=new T(function(){return B(A(_e7,[_e5]));}),_e9=function(_ea){return E(_e8);};return [1,B(_dM(_e4,_e9))];},_eb=new T(function(){return B(unCStr("SO"));}),_ec=14,_ed=function(_ee){var _ef=new T(function(){return B(A(_ee,[_ec]));}),_eg=function(_eh){return E(_ef);};return [1,B(_dM(_eb,_eg))];},_ei=function(_ej){return [1,B(_a3(_e6,_ed,_ej))];},_ek=new T(function(){return B(unCStr("NUL"));}),_el=0,_em=function(_en){var _eo=new T(function(){return B(A(_en,[_el]));}),_ep=function(_eq){return E(_eo);};return [1,B(_dM(_ek,_ep))];},_er=new T(function(){return B(unCStr("STX"));}),_es=2,_et=function(_eu){var _ev=new T(function(){return B(A(_eu,[_es]));}),_ew=function(_ex){return E(_ev);};return [1,B(_dM(_er,_ew))];},_ey=new T(function(){return B(unCStr("ETX"));}),_ez=3,_eA=function(_eB){var _eC=new T(function(){return B(A(_eB,[_ez]));}),_eD=function(_eE){return E(_eC);};return [1,B(_dM(_ey,_eD))];},_eF=new T(function(){return B(unCStr("EOT"));}),_eG=4,_eH=function(_eI){var _eJ=new T(function(){return B(A(_eI,[_eG]));}),_eK=function(_eL){return E(_eJ);};return [1,B(_dM(_eF,_eK))];},_eM=new T(function(){return B(unCStr("ENQ"));}),_eN=5,_eO=function(_eP){var _eQ=new T(function(){return B(A(_eP,[_eN]));}),_eR=function(_eS){return E(_eQ);};return [1,B(_dM(_eM,_eR))];},_eT=new T(function(){return B(unCStr("ACK"));}),_eU=6,_eV=function(_eW){var _eX=new T(function(){return B(A(_eW,[_eU]));}),_eY=function(_eZ){return E(_eX);};return [1,B(_dM(_eT,_eY))];},_f0=new T(function(){return B(unCStr("BEL"));}),_f1=7,_f2=function(_f3){var _f4=new T(function(){return B(A(_f3,[_f1]));}),_f5=function(_f6){return E(_f4);};return [1,B(_dM(_f0,_f5))];},_f7=new T(function(){return B(unCStr("BS"));}),_f8=8,_f9=function(_fa){var _fb=new T(function(){return B(A(_fa,[_f8]));}),_fc=function(_fd){return E(_fb);};return [1,B(_dM(_f7,_fc))];},_fe=new T(function(){return B(unCStr("HT"));}),_ff=9,_fg=function(_fh){var _fi=new T(function(){return B(A(_fh,[_ff]));}),_fj=function(_fk){return E(_fi);};return [1,B(_dM(_fe,_fj))];},_fl=new T(function(){return B(unCStr("LF"));}),_fm=10,_fn=function(_fo){var _fp=new T(function(){return B(A(_fo,[_fm]));}),_fq=function(_fr){return E(_fp);};return [1,B(_dM(_fl,_fq))];},_fs=new T(function(){return B(unCStr("VT"));}),_ft=11,_fu=function(_fv){var _fw=new T(function(){return B(A(_fv,[_ft]));}),_fx=function(_fy){return E(_fw);};return [1,B(_dM(_fs,_fx))];},_fz=new T(function(){return B(unCStr("FF"));}),_fA=12,_fB=function(_fC){var _fD=new T(function(){return B(A(_fC,[_fA]));}),_fE=function(_fF){return E(_fD);};return [1,B(_dM(_fz,_fE))];},_fG=new T(function(){return B(unCStr("CR"));}),_fH=13,_fI=function(_fJ){var _fK=new T(function(){return B(A(_fJ,[_fH]));}),_fL=function(_fM){return E(_fK);};return [1,B(_dM(_fG,_fL))];},_fN=new T(function(){return B(unCStr("SI"));}),_fO=15,_fP=function(_fQ){var _fR=new T(function(){return B(A(_fQ,[_fO]));}),_fS=function(_fT){return E(_fR);};return [1,B(_dM(_fN,_fS))];},_fU=new T(function(){return B(unCStr("DLE"));}),_fV=16,_fW=function(_fX){var _fY=new T(function(){return B(A(_fX,[_fV]));}),_fZ=function(_g0){return E(_fY);};return [1,B(_dM(_fU,_fZ))];},_g1=new T(function(){return B(unCStr("DC1"));}),_g2=17,_g3=function(_g4){var _g5=new T(function(){return B(A(_g4,[_g2]));}),_g6=function(_g7){return E(_g5);};return [1,B(_dM(_g1,_g6))];},_g8=new T(function(){return B(unCStr("DC2"));}),_g9=18,_ga=function(_gb){var _gc=new T(function(){return B(A(_gb,[_g9]));}),_gd=function(_ge){return E(_gc);};return [1,B(_dM(_g8,_gd))];},_gf=new T(function(){return B(unCStr("DC3"));}),_gg=19,_gh=function(_gi){var _gj=new T(function(){return B(A(_gi,[_gg]));}),_gk=function(_gl){return E(_gj);};return [1,B(_dM(_gf,_gk))];},_gm=new T(function(){return B(unCStr("DC4"));}),_gn=20,_go=function(_gp){var _gq=new T(function(){return B(A(_gp,[_gn]));}),_gr=function(_gs){return E(_gq);};return [1,B(_dM(_gm,_gr))];},_gt=new T(function(){return B(unCStr("NAK"));}),_gu=21,_gv=function(_gw){var _gx=new T(function(){return B(A(_gw,[_gu]));}),_gy=function(_gz){return E(_gx);};return [1,B(_dM(_gt,_gy))];},_gA=new T(function(){return B(unCStr("SYN"));}),_gB=22,_gC=function(_gD){var _gE=new T(function(){return B(A(_gD,[_gB]));}),_gF=function(_gG){return E(_gE);};return [1,B(_dM(_gA,_gF))];},_gH=new T(function(){return B(unCStr("ETB"));}),_gI=23,_gJ=function(_gK){var _gL=new T(function(){return B(A(_gK,[_gI]));}),_gM=function(_gN){return E(_gL);};return [1,B(_dM(_gH,_gM))];},_gO=new T(function(){return B(unCStr("CAN"));}),_gP=24,_gQ=function(_gR){var _gS=new T(function(){return B(A(_gR,[_gP]));}),_gT=function(_gU){return E(_gS);};return [1,B(_dM(_gO,_gT))];},_gV=new T(function(){return B(unCStr("EM"));}),_gW=25,_gX=function(_gY){var _gZ=new T(function(){return B(A(_gY,[_gW]));}),_h0=function(_h1){return E(_gZ);};return [1,B(_dM(_gV,_h0))];},_h2=new T(function(){return B(unCStr("SUB"));}),_h3=26,_h4=function(_h5){var _h6=new T(function(){return B(A(_h5,[_h3]));}),_h7=function(_h8){return E(_h6);};return [1,B(_dM(_h2,_h7))];},_h9=new T(function(){return B(unCStr("ESC"));}),_ha=27,_hb=function(_hc){var _hd=new T(function(){return B(A(_hc,[_ha]));}),_he=function(_hf){return E(_hd);};return [1,B(_dM(_h9,_he))];},_hg=new T(function(){return B(unCStr("FS"));}),_hh=28,_hi=function(_hj){var _hk=new T(function(){return B(A(_hj,[_hh]));}),_hl=function(_hm){return E(_hk);};return [1,B(_dM(_hg,_hl))];},_hn=new T(function(){return B(unCStr("GS"));}),_ho=29,_hp=function(_hq){var _hr=new T(function(){return B(A(_hq,[_ho]));}),_hs=function(_ht){return E(_hr);};return [1,B(_dM(_hn,_hs))];},_hu=new T(function(){return B(unCStr("RS"));}),_hv=30,_hw=function(_hx){var _hy=new T(function(){return B(A(_hx,[_hv]));}),_hz=function(_hA){return E(_hy);};return [1,B(_dM(_hu,_hz))];},_hB=new T(function(){return B(unCStr("US"));}),_hC=31,_hD=function(_hE){var _hF=new T(function(){return B(A(_hE,[_hC]));}),_hG=function(_hH){return E(_hF);};return [1,B(_dM(_hB,_hG))];},_hI=new T(function(){return B(unCStr("SP"));}),_hJ=32,_hK=function(_hL){var _hM=new T(function(){return B(A(_hL,[_hJ]));}),_hN=function(_hO){return E(_hM);};return [1,B(_dM(_hI,_hN))];},_hP=new T(function(){return B(unCStr("DEL"));}),_hQ=127,_hR=function(_hS){var _hT=new T(function(){return B(A(_hS,[_hQ]));}),_hU=function(_hV){return E(_hT);};return [1,B(_dM(_hP,_hU))];},_hW=[1,_hR,_x],_hX=[1,_hK,_hW],_hY=[1,_hD,_hX],_hZ=[1,_hw,_hY],_i0=[1,_hp,_hZ],_i1=[1,_hi,_i0],_i2=[1,_hb,_i1],_i3=[1,_h4,_i2],_i4=[1,_gX,_i3],_i5=[1,_gQ,_i4],_i6=[1,_gJ,_i5],_i7=[1,_gC,_i6],_i8=[1,_gv,_i7],_i9=[1,_go,_i8],_ia=[1,_gh,_i9],_ib=[1,_ga,_ia],_ic=[1,_g3,_ib],_id=[1,_fW,_ic],_ie=[1,_fP,_id],_if=[1,_fI,_ie],_ig=[1,_fB,_if],_ih=[1,_fu,_ig],_ii=[1,_fn,_ih],_ij=[1,_fg,_ii],_ik=[1,_f9,_ij],_il=[1,_f2,_ik],_im=[1,_eV,_il],_in=[1,_eO,_im],_io=[1,_eH,_in],_ip=[1,_eA,_io],_iq=[1,_et,_ip],_ir=[1,_em,_iq],_is=[1,_ei,_ir],_it=new T(function(){return B(_dC(_is));}),_iu=[0,1114111],_iv=34,_iw=39,_ix=function(_iy){var _iz=new T(function(){return B(A(_iy,[_f1]));}),_iA=new T(function(){return B(A(_iy,[_f8]));}),_iB=new T(function(){return B(A(_iy,[_ff]));}),_iC=new T(function(){return B(A(_iy,[_fm]));}),_iD=new T(function(){return B(A(_iy,[_ft]));}),_iE=new T(function(){return B(A(_iy,[_fA]));}),_iF=new T(function(){return B(A(_iy,[_fH]));}),_iG=new T(function(){return B(A(_iy,[_dq]));}),_iH=new T(function(){return B(A(_iy,[_iw]));}),_iI=new T(function(){return B(A(_iy,[_iv]));}),_iJ=new T(function(){var _iK=function(_iL){var _iM=new T(function(){_iL=E(_iL);return B(_c0(_iL));}),_iN=function(_iO){var _iP=B(_c8(_iM,_bZ,_iO));if(!B(_dv(_iP,_iu))){return [2];}else{var _iQ=new T(function(){var _iR=B(_dt(_iP));if(_iR>>>0>1114111){return B(_y(_iR));}else{return _iR;}});return new F(function(){return A(_iy,[_iQ]);});}};return [1,B(_aM(_iL,_iN))];},_iS=new T(function(){var _iT=new T(function(){return B(A(_iy,[_hC]));}),_iU=new T(function(){return B(A(_iy,[_hv]));}),_iV=new T(function(){return B(A(_iy,[_ho]));}),_iW=new T(function(){return B(A(_iy,[_hh]));}),_iX=new T(function(){return B(A(_iy,[_ha]));}),_iY=new T(function(){return B(A(_iy,[_h3]));}),_iZ=new T(function(){return B(A(_iy,[_gW]));}),_j0=new T(function(){return B(A(_iy,[_gP]));}),_j1=new T(function(){return B(A(_iy,[_gI]));}),_j2=new T(function(){return B(A(_iy,[_gB]));}),_j3=new T(function(){return B(A(_iy,[_gu]));}),_j4=new T(function(){return B(A(_iy,[_gn]));}),_j5=new T(function(){return B(A(_iy,[_gg]));}),_j6=new T(function(){return B(A(_iy,[_g9]));}),_j7=new T(function(){return B(A(_iy,[_g2]));}),_j8=new T(function(){return B(A(_iy,[_fV]));}),_j9=new T(function(){return B(A(_iy,[_fO]));}),_ja=new T(function(){return B(A(_iy,[_ec]));}),_jb=new T(function(){return B(A(_iy,[_eU]));}),_jc=new T(function(){return B(A(_iy,[_eN]));}),_jd=new T(function(){return B(A(_iy,[_eG]));}),_je=new T(function(){return B(A(_iy,[_ez]));}),_jf=new T(function(){return B(A(_iy,[_es]));}),_jg=new T(function(){return B(A(_iy,[_e5]));}),_jh=new T(function(){return B(A(_iy,[_el]));}),_ji=function(_jj){_jj=E(_jj);switch(_jj){case 64:return E(_jh);case 65:return E(_jg);case 66:return E(_jf);case 67:return E(_je);case 68:return E(_jd);case 69:return E(_jc);case 70:return E(_jb);case 71:return E(_iz);case 72:return E(_iA);case 73:return E(_iB);case 74:return E(_iC);case 75:return E(_iD);case 76:return E(_iE);case 77:return E(_iF);case 78:return E(_ja);case 79:return E(_j9);case 80:return E(_j8);case 81:return E(_j7);case 82:return E(_j6);case 83:return E(_j5);case 84:return E(_j4);case 85:return E(_j3);case 86:return E(_j2);case 87:return E(_j1);case 88:return E(_j0);case 89:return E(_iZ);case 90:return E(_iY);case 91:return E(_iX);case 92:return E(_iW);case 93:return E(_iV);case 94:return E(_iU);case 95:return E(_iT);default:return [2];}},_jk=[0,_ji],_jl=new T(function(){return B(A(_it,[_iy]));}),_jm=function(_jn){_jn=E(_jn);return (_jn==94)?E(_jk):[2];};return B(_8S([0,_jm],_jl));});return B(_8S([1,B(_a3(_do,_dr,_iK))],_iS));}),_jo=function(_jp){_jp=E(_jp);switch(_jp){case 34:return E(_iI);case 39:return E(_iH);case 92:return E(_iG);case 97:return E(_iz);case 98:return E(_iA);case 102:return E(_iE);case 110:return E(_iC);case 114:return E(_iF);case 116:return E(_iB);case 118:return E(_iD);default:return [2];}};return new F(function(){return _8S([0,_jo],_iJ);});},_jq=function(_jr){return new F(function(){return A(_jr,[_8]);});},_js=function(_jt){_jt=E(_jt);if(!_jt[0]){return E(_jq);}else{var _ju=_jt[1],_jv=_jt[2];_ju=E(_ju);switch(_ju){case 9:var _jw=new T(function(){return B(_js(_jv));}),_jx=function(_jy){var _jz=new T(function(){return B(A(_jw,[_jy]));});return [0,function(_jA){return E(_jz);}];};return E(_jx);case 10:var _jB=new T(function(){return B(_js(_jv));}),_jC=function(_jD){var _jE=new T(function(){return B(A(_jB,[_jD]));});return [0,function(_jF){return E(_jE);}];};return E(_jC);case 11:var _jG=new T(function(){return B(_js(_jv));}),_jH=function(_jI){var _jJ=new T(function(){return B(A(_jG,[_jI]));});return [0,function(_jK){return E(_jJ);}];};return E(_jH);case 12:var _jL=new T(function(){return B(_js(_jv));}),_jM=function(_jN){var _jO=new T(function(){return B(A(_jL,[_jN]));});return [0,function(_jP){return E(_jO);}];};return E(_jM);case 13:var _jQ=new T(function(){return B(_js(_jv));}),_jR=function(_jS){var _jT=new T(function(){return B(A(_jQ,[_jS]));});return [0,function(_jU){return E(_jT);}];};return E(_jR);case 32:var _jV=new T(function(){return B(_js(_jv));}),_jW=function(_jX){var _jY=new T(function(){return B(A(_jV,[_jX]));});return [0,function(_jZ){return E(_jY);}];};return E(_jW);case 160:var _k0=new T(function(){return B(_js(_jv));}),_k1=function(_k2){var _k3=new T(function(){return B(A(_k0,[_k2]));});return [0,function(_k4){return E(_k3);}];};return E(_k1);default:var _k5=u_iswspace(_ju);_k5=E(_k5);if(!_k5){return E(_jq);}else{var _k6=new T(function(){return B(_js(_jv));}),_k7=function(_k8){var _k9=new T(function(){return B(A(_k6,[_k8]));});return [0,function(_ka){return E(_k9);}];};return E(_k7);}}}},_kb=function(_kc){var _kd=new T(function(){return B(_kb(_kc));}),_ke=function(_kf){_kf=E(_kf);return (_kf==92)?E(_kd):[2];},_kg=[0,_ke],_kh=function(_ki){return E(_kg);},_kj=function(_kk){return new F(function(){return A(_js,[_kk,_kh]);});},_kl=[1,_kj],_km=new T(function(){var _kn=function(_ko){return new F(function(){return A(_kc,[[0,_ko,_di]]);});};return B(_ix(_kn));}),_kp=function(_kq){_kq=E(_kq);switch(_kq){case 9:return E(_kl);case 10:return E(_kl);case 11:return E(_kl);case 12:return E(_kl);case 13:return E(_kl);case 32:return E(_kl);case 38:return E(_kd);case 160:return E(_kl);default:var _kr=u_iswspace(_kq);_kr=E(_kr);return (_kr==0)?[2]:E(_kl);}},_ks=[0,_kp],_kt=function(_ku){_ku=E(_ku);if(_ku==92){return E(_km);}else{return new F(function(){return A(_kc,[[0,_ku,_dh]]);});}},_kv=function(_kw){_kw=E(_kw);return (_kw==92)?E(_ks):[2];};return new F(function(){return _8S([0,_kv],[0,_kt]);});},_kx=function(_ky,_kz){var _kA=new T(function(){var _kB=new T(function(){return B(A(_ky,[_x]));});return B(A(_kz,[[1,_kB]]));}),_kC=function(_kD){_kD=E(_kD);var _kE=_kD[1],_kF=_kD[2];_kE=E(_kE);if(_kE==34){_kF=E(_kF);if(!_kF){return E(_kA);}else{var _kG=function(_kH){return new F(function(){return A(_ky,[[1,_kE,_kH]]);});};return new F(function(){return _kx(_kG,_kz);});}}else{var _kI=function(_kJ){return new F(function(){return A(_ky,[[1,_kE,_kJ]]);});};return new F(function(){return _kx(_kI,_kz);});}};return new F(function(){return _kb(_kC);});},_kK=new T(function(){return B(unCStr("_\'"));}),_kL=function(_kM){var _kN=u_iswalnum(_kM);_kN=E(_kN);if(!_kN){return new F(function(){return _cV(_4f,_kM,_kK);});}else{return true;}},_kO=function(_kP){_kP=E(_kP);return new F(function(){return _kL(_kP);});},_kQ=new T(function(){return B(unCStr(",;()[]{}`"));}),_kR=new T(function(){return B(unCStr(".."));}),_kS=new T(function(){return B(unCStr("::"));}),_kT=new T(function(){return B(unCStr("->"));}),_kU=64,_kV=[1,_kU,_x],_kW=126,_kX=[1,_kW,_x],_kY=new T(function(){return B(unCStr("=>"));}),_kZ=[1,_kY,_x],_l0=[1,_kX,_kZ],_l1=[1,_kV,_l0],_l2=[1,_kT,_l1],_l3=new T(function(){return B(unCStr("<-"));}),_l4=[1,_l3,_l2],_l5=124,_l6=[1,_l5,_x],_l7=[1,_l6,_l4],_l8=[1,_dq,_x],_l9=[1,_l8,_l7],_la=61,_lb=[1,_la,_x],_lc=[1,_lb,_l9],_ld=[1,_kS,_lc],_le=[1,_kR,_ld],_lf=function(_lg){var _lh=new T(function(){return B(A(_lg,[_aH]));}),_li=new T(function(){var _lj=new T(function(){var _lk=function(_ll){var _lm=new T(function(){return B(A(_lg,[[0,_ll]]));});return [0,function(_ln){_ln=E(_ln);return (_ln==39)?E(_lm):[2];}];};return B(_ix(_lk));}),_lo=function(_lp){_lp=E(_lp);switch(_lp){case 39:return [2];case 92:return E(_lj);default:var _lq=new T(function(){return B(A(_lg,[[0,_lp]]));});return [0,function(_lr){_lr=E(_lr);return (_lr==39)?E(_lq):[2];}];}},_ls=[0,_lo],_lt=new T(function(){var _lu=new T(function(){return B(_kx(_aI,_lg));}),_lv=new T(function(){var _lw=new T(function(){var _lx=new T(function(){var _ly=new T(function(){return [1,B(_a3(_df,_cT,_lg))];}),_lz=function(_lA){_lA=E(_lA);var _lB=u_iswalpha(_lA);_lB=E(_lB);if(!_lB){_lA=E(_lA);if(_lA==95){var _lC=function(_lD){return new F(function(){return A(_lg,[[3,[1,_lA,_lD]]]);});};return [1,B(_as(_kO,_lC))];}else{return [2];}}else{var _lE=function(_lF){return new F(function(){return A(_lg,[[3,[1,_lA,_lF]]]);});};return [1,B(_as(_kO,_lE))];}};return B(_8S([0,_lz],_ly));}),_lG=function(_lH){if(!B(_cV(_4f,_lH,_d0))){return [2];}else{var _lI=function(_lJ){var _lK=[1,_lH,_lJ];if(!B(_cV(_9F,_lK,_le))){return new F(function(){return A(_lg,[[4,_lK]]);});}else{return new F(function(){return A(_lg,[[2,_lK]]);});}};return [1,B(_as(_d1,_lI))];}};return B(_8S([0,_lG],_lx));}),_lL=function(_lM){if(!B(_cV(_4f,_lM,_kQ))){return [2];}else{return new F(function(){return A(_lg,[[2,[1,_lM,_x]]]);});}};return B(_8S([0,_lL],_lw));}),_lN=function(_lO){_lO=E(_lO);return (_lO==34)?E(_lu):[2];};return B(_8S([0,_lN],_lv));}),_lP=function(_lQ){_lQ=E(_lQ);return (_lQ==39)?E(_ls):[2];};return B(_8S([0,_lP],_lt));}),_lR=function(_lS){_lS=E(_lS);return (_lS[0]==0)?E(_lh):[2];};return new F(function(){return _8S([1,_lR],_li);});},_lT=0,_lU=function(_lV,_lW){var _lX=new T(function(){var _lY=new T(function(){var _lZ=function(_m0){var _m1=new T(function(){var _m2=new T(function(){return B(A(_lW,[_m0]));}),_m3=function(_m4){_m4=E(_m4);return (_m4[0]==2)?(!B(_88(_m4[1],_9s)))?[2]:E(_m2):[2];};return B(_lf(_m3));}),_m5=function(_m6){return E(_m1);};return [1,function(_m7){return new F(function(){return A(_js,[_m7,_m5]);});}];};return B(A(_lV,[_lT,_lZ]));}),_m8=function(_m9){_m9=E(_m9);return (_m9[0]==2)?(!B(_88(_m9[1],_9u)))?[2]:E(_lY):[2];};return B(_lf(_m8));}),_ma=function(_mb){return E(_lX);};return function(_mc){return new F(function(){return A(_js,[_mc,_ma]);});};},_md=function(_me,_mf,_mg){var _mh=function(_mi,_mj){var _mk=function(_ml){var _mm=new T(function(){_ml=E(_ml);return  -_ml;});return new F(function(){return A(_mj,[_mm]);});},_mn=function(_mo){return new F(function(){return A(_me,[_mo,_mi,_mk]);});},_mp=new T(function(){return B(_lf(_mn));}),_mq=function(_mr){return E(_mp);},_ms=function(_mt){return new F(function(){return A(_js,[_mt,_mq]);});},_mu=[1,_ms],_mv=function(_mw){_mw=E(_mw);if(_mw[0]==4){var _mx=_mw[1];_mx=E(_mx);if(!_mx[0]){return new F(function(){return A(_me,[_mw,_mi,_mj]);});}else{var _my=_mx[1],_mz=_mx[2];_my=E(_my);if(_my==45){_mz=E(_mz);if(!_mz[0]){return E(_mu);}else{return new F(function(){return A(_me,[_mw,_mi,_mj]);});}}else{return new F(function(){return A(_me,[_mw,_mi,_mj]);});}}}else{return new F(function(){return A(_me,[_mw,_mi,_mj]);});}},_mA=new T(function(){return B(_lf(_mv));}),_mB=function(_mC){return E(_mA);},_mD=new T(function(){return [1,B(_lU(_mh,_mj))];}),_mE=function(_mF){return new F(function(){return A(_js,[_mF,_mB]);});};return new F(function(){return _8S([1,_mE],_mD);});};return new F(function(){return _mh(_mf,_mg);});},_mG=function(_mH,_mI){return [2];},_mJ=function(_mK){_mK=E(_mK);if(!_mK[0]){var _mL=_mK[1],_mM=_mK[2];return [1,new T(function(){var _mN=new T(function(){_mL=E(_mL);return B(_c0(_mL));});return B(_c8(_mN,_bZ,_mM));})];}else{var _mO=_mK[1],_mP=_mK[2],_mQ=_mK[3];_mP=E(_mP);if(!_mP[0]){_mQ=E(_mQ);if(!_mQ[0]){return [1,new T(function(){return B(_c8(_bY,_bZ,_mO));})];}else{return [0];}}else{return [0];}}},_mR=function(_mS){_mS=E(_mS);if(_mS[0]==5){var _mT=B(_mJ(_mS[1]));if(!_mT[0]){return E(_mG);}else{var _mU=_mT[1],_mV=new T(function(){return B(_dt(_mU));}),_mW=function(_mX,_mY){return new F(function(){return A(_mY,[_mV]);});};return E(_mW);}}else{return E(_mG);}},_mZ=function(_n0){var _n1=[3,_n0,_9V],_n2=function(_n3){return E(_n1);};return [1,function(_n4){return new F(function(){return A(_js,[_n4,_n2]);});}];},_n5=new T(function(){return B(_md(_mR,_lT,_mZ));}),_n6=47,_n7=function(_n8){while(1){var _n9=(function(_na){_na=E(_na);if(!_na[0]){return [0];}else{var _nb=_na[1],_nc=_na[2];_nb=E(_nb);var _nd=_nb[2];_nd=E(_nd);if(!_nd[0]){var _ne=new T(function(){return B(_n7(_nc));});return [1,_nb[1],_ne];}else{_n8=_nc;return null;}}})(_n8);if(_n9!=null){return _n9;}}},_nf=function(_ng,_nh,_ni){while(1){_nh=E(_nh);if(!_nh[0]){return [1,_ni];}else{_ni=E(_ni);if(!_ni[0]){return [0];}else{if(!B(A(_8k,[_ng,_nh[1],_ni[1]]))){return [0];}else{var _nj=_nh[2],_nk=_ni[2];_nh=_nj;_ni=_nk;continue;}}}}},_nl=function(_nm,_nn){_nm=E(_nm);if(!_nm){return [0];}else{_nn=E(_nn);if(!_nn[0]){return [0];}else{var _no=_nn[2],_np=new T(function(){return B(_nl(_nm-1|0,_no));});return [1,_nn[1],_np];}}},_nq=function(_nr,_ns,_){var _nt=new T(function(){_nr=E(_nr);var _nu=_nr[3],_nv=_nr[4],_nw=new T(function(){return B(_3e(3,_nv));}),_nx=new T(function(){return B(_g(B(_X(B(_nl(3,_nv)),_x)),_nu));});return [0,[0,_nr[1],_nr[2],_nx,_nw],_nx,_nw];}),_ny=new T(function(){_nt=E(_nt);return E(_nt[3]);}),_nz=new T(function(){_nt=E(_nt);return E(_nt[2]);}),_nA=new T(function(){_nt=E(_nt);return E(_nt[1]);}),_nB=new T(function(){_nr=E(_nr);var _nC=_nr[3],_nD=new T(function(){return B(_X(_nC,_x));});return [0,[0,_nr[1],_nr[2],_x,_nD],_x,_nD];}),_nE=new T(function(){_nB=E(_nB);return E(_nB[3]);}),_nF=new T(function(){_nB=E(_nB);return E(_nB[1]);}),_nG=function(_nH,_nI){var _nJ=new T(function(){_nI=E(_nI);var _nK=String(_nI);return fromJSStr(_nK);}),_nL=new T(function(){var _nM=new T(function(){var _nN=B(_nf(_4f,_1E,_nJ));if(!_nN[0]){return E(_8j);}else{return E(_nN[1]);}});return B(_n7(B(_8I(_n5,_nM))));}),_nO=new T(function(){return B(_88(_2w,_nJ));}),_nP=new T(function(){return B(_88(_2H,_nJ));}),_nQ=new T(function(){return B(_8m(_4f,_1E,_nJ));}),_nR=function(_nS){var _nT=new T(function(){_nS=E(_nS);var _nU=Number(_nS);return jsTrunc(_nU);}),_nV=function(_nW,_){var _nX=new T(function(){_nW=E(_nW);var _nY=Number(_nW);return jsTrunc(_nY);}),_nZ=new T(function(){_nX=E(_nX);if(_nX<160){return false;}else{_nT=E(_nT);return _nT>=40;}}),_o0=function(_o1){var _o2=new T(function(){_nX=E(_nX);if(_nX>=160){return false;}else{_nT=E(_nT);return _nT>=310;}}),_o3=function(_o4){var _o5=function(_o6){var _o7=function(_o8){var _o9=function(_oa){_nP=E(_nP);if(!_nP){return new F(function(){return _86(_nr,_);});}else{_o2=E(_o2);if(!_o2){return new F(function(){return _86(_nr,_);});}else{_nr=E(_nr);var _ob=_nr[1],_oc=_nr[2],_od=_nr[3],_oe=_nr[4];_od=E(_od);if(!_od[0]){return E(_3d);}else{var _of=_od[1],_og=_od[2];_of=E(_of);var _oh=_of[1],_oi=_of[2];_nT=E(_nT);var _oj=B(_2n(_nT-310|0,90));if(3>_oj){if(_oj>=0){if(!B(_7B(_oh,_oi,B(_44(_ob,_oj))))){return new F(function(){return _2W(_od,_);});}else{var _ok=new T(function(){return B(_4g(_ob,_oj,_of));}),_ol=new T(function(){return B(_44(_ok,_oj));},1),_om=B(_12(_oj,_ol,_));return new F(function(){return _nq([0,_ok,_oc,_og,_oe],_ns,_);});}}else{return E(_41);}}else{if(!B(_7B(_oh,_oi,B(_44(_ob,3))))){return new F(function(){return _2W(_od,_);});}else{var _on=new T(function(){return B(_4g(_ob,3,_of));}),_oo=new T(function(){return B(_44(_on,3));},1),_op=B(_12(3,_oo,_));return new F(function(){return _nq([0,_on,_oc,_og,_oe],_ns,_);});}}}}}};_nP=E(_nP);if(!_nP){return new F(function(){return _o9(_);});}else{_nZ=E(_nZ);if(!_nZ){return new F(function(){return _o9(_);});}else{_nr=E(_nr);var _oq=_nr[1],_or=_nr[2],_os=_nr[3],_ot=_nr[4];_os=E(_os);if(!_os[0]){return E(_3d);}else{var _ou=_os[1];_ou=E(_ou);var _ov=_ou[1],_ow=_ou[2];_nT=E(_nT);var _ox=B(_2n(_nT-40|0,90)),_oy=function(_oz,_oA){if(_oz>=0){var _oB=B(_44(_or,_oz));if(!B(_7j(_ov,_ow,_oB[1],_oB[2]))){return new F(function(){return _2W(_os,_);});}else{var _oC=new T(function(){var _oD=B(_5v(_oq,_or,_os,_ot,_oA)),_oE=_oD[2];return [0,[0,_oD[1],_oE,_oD[3],_oD[4]],_oE];}),_oF=new T(function(){_oC=E(_oC);return B(_44(_oC[2],_oz));},1),_oG=B(_23(_oz,_oF,_)),_oH=new T(function(){_oC=E(_oC);return E(_oC[1]);});return new F(function(){return _nq(_oH,_ns,_);});}}else{return E(_41);}};if(6>_ox){return new F(function(){return _oy(_ox,_ox);});}else{return new F(function(){return _oy(6,_8B);});}}}}};_nP=E(_nP);if(!_nP){return new F(function(){return _o7(_);});}else{_nX=E(_nX);if(_nX>=160){return new F(function(){return _o7(_);});}else{_nT=E(_nT);_2q=E(_2q);if(_nT<_2q){return new F(function(){return _o7(_);});}else{_2I=E(_2I);if(_nT>=_2I){return new F(function(){return _o7(_);});}else{var _oI=B(_80(_)),_oJ=B(_83(_)),_oK=B(_2r(_W,_2C,_2m,_)),_oL=B(_2x(_nE,_));return new F(function(){return _nq(_nF,_8A,_);});}}}}};_nO=E(_nO);if(!_nO){return new F(function(){return _o5(_);});}else{_nX=E(_nX);if(_nX>=160){return new F(function(){return _o5(_);});}else{_nT=E(_nT);_2I=E(_2I);if(_nT<_2I){return new F(function(){return _o5(_);});}else{_8C=E(_8C);if(_nT>=_8C){return new F(function(){return _o5(_);});}else{var _oM=B(_83(_)),_oN=B(_2r(_W,_2C,_2m,_)),_oO=B(_2x(_ny,_)),_oP=B(_80(_)),_oQ=B(_2W(_nz,_));return new F(function(){return _nq(_nA,_8z,_);});}}}}};_nQ=E(_nQ);if(!_nQ){return new F(function(){return _o3(_);});}else{_o2=E(_o2);if(!_o2){return new F(function(){return _o3(_);});}else{_nr=E(_nr);var _oR=_nr[1],_oS=_nr[2],_oT=_nr[3],_oU=_nr[4];_nL=E(_nL);if(!_nL[0]){return E(_8G);}else{var _oV=_nL[1],_oW=_nL[2];_oW=E(_oW);if(!_oW[0]){_oV=E(_oV);var _oX=_oV;if(_oX>=0){var _oY=B(_44(_oS,_oX)),_oZ=_oY[2];_oZ=E(_oZ);if(!_oZ[0]){return E(_3d);}else{var _p0=_oZ[1];_p0=E(_p0);var _p1=_p0[1],_p2=_p0[2];_nT=E(_nT);var _p3=B(_2n(_nT-310|0,90)),_p4=B(_1z(_oR,0))-1|0,_p5=function(_p6,_p7){if(_p6>=0){if(!B(_7B(_p1,_p2,B(_44(_oR,_p6))))){return new F(function(){return _23(_oX,_oY,_);});}else{var _p8=new T(function(){var _p9=B(_55(_oR,_oS,_oT,_oU,_oV,_p7)),_pa=_p9[1],_pb=_p9[2];return [0,[0,_pa,_pb,_p9[3],_p9[4]],_pa,_pb];}),_pc=new T(function(){_p8=E(_p8);return B(_44(_p8[2],_p6));},1),_pd=B(_12(_p6,_pc,_)),_pe=B(_7U(_oV,_)),_pf=new T(function(){_p8=E(_p8);return B(_44(_p8[3],_oX));},1),_pg=B(_23(_oX,_pf,_)),_ph=new T(function(){_p8=E(_p8);return E(_p8[1]);});return new F(function(){return _nq(_ph,_ns,_);});}}else{return E(_41);}};if(_p4>_p3){return new F(function(){return _p5(_p3,_p3);});}else{return new F(function(){return _p5(_p4,_p4);});}}}else{return E(_41);}}else{return E(_8E);}}}}};_nQ=E(_nQ);if(!_nQ){var _pi=B(_o0(_));return _3Z;}else{_nZ=E(_nZ);if(!_nZ){var _pj=B(_o0(_));return _3Z;}else{_nr=E(_nr);var _pk=_nr[1],_pl=_nr[2],_pm=_nr[3],_pn=_nr[4];_nL=E(_nL);if(!_nL[0]){return E(_8G);}else{var _po=_nL[1],_pp=_nL[2];_pp=E(_pp);if(!_pp[0]){_po=E(_po);var _pq=_po;if(_pq>=0){var _pr=B(_44(_pl,_pq)),_ps=_pr[2];_ps=E(_ps);if(!_ps[0]){return E(_8y);}else{var _pt=B(_8s(_ps[1],_ps[2])),_pu=_pt[1],_pv=_pt[2];_nT=E(_nT);var _pw=B(_2n(_nT-40|0,90)),_px=B(_1z(_pl,0))-1|0,_py=function(_pz,_pA){if(_pz>=0){var _pB=B(_44(_pl,_pz));if(!B(_7j(_pu,_pv,_pB[1],_pB[2]))){return new F(function(){return _23(_pq,_pr,_);});}else{var _pC=new T(function(){var _pD=B(_4v(_pk,_pl,_pm,_pn,_po,_pA)),_pE=_pD[2];return [0,[0,_pD[1],_pE,_pD[3],_pD[4]],_pE];}),_pF=new T(function(){_pC=E(_pC);return E(_pC[2]);}),_pG=new T(function(){return B(_44(_pF,_pz));},1),_pH=B(_23(_pz,_pG,_)),_pI=B(_7U(_po,_)),_pJ=new T(function(){return B(_44(_pF,_pq));},1),_pK=B(_23(_pq,_pJ,_)),_pL=new T(function(){var _pM=new T(function(){return B(_r(0,_pq,_x));},1);return B(_g(_1E,_pM));}),_pN=new T(function(){_pC=E(_pC);return E(_pC[1]);});return new F(function(){return _nq(_pN,[1,_pL],_);});}}else{return E(_41);}};if(_px>_pw){var _pO=B(_py(_pw,_pw));return _3Z;}else{var _pP=B(_py(_px,_px));return _3Z;}}}else{return E(_41);}}else{return E(_8E);}}}}};return E(_nV);};return E(_nR);};_nG=E(_nG);var _pQ=__createJSFunc(5,_nG);_8h=E(_8h);var _pR=_8h(_pQ),_pS=function(_pT){var _pU=new T(function(){_pT=E(_pT);var _pV=String(_pT);return fromJSStr(_pV);}),_pW=function(_pX){var _pY=new T(function(){_pX=E(_pX);var _pZ=String(_pX);return fromJSStr(_pZ);}),_q0=[1,_pY],_q1=new T(function(){var _q2=new T(function(){var _q3=B(_nf(_4f,_1E,_pY));if(!_q3[0]){return E(_8j);}else{return E(_q3[1]);}});return B(_n7(B(_8I(_n5,_q2))));}),_q4=new T(function(){var _q5=new T(function(){return B(_g(_pU,[1,_n6,_pY]));});return B(unAppCStr("In onMouseover - Unhandled id/class: ",_q5));}),_q6=new T(function(){return B(_8m(_4f,_1E,_pY));}),_q7=new T(function(){return B(_88(_2w,_pY));}),_q8=new T(function(){return B(_88(_2H,_pY));}),_q9=function(_qa,_qb,_){_nr=E(_nr);var _qc=_nr[2],_qd=_nr[3],_qe=_nr[4],_qf=function(_){_pY=E(_pY);_7T=E(_7T);var _qg=_7T(toJSStr(_pY)),_qh=B(_nq(_nr,_q0,_));_q6=E(_q6);if(!_q6){_q7=E(_q7);if(!_q7){_q8=E(_q8);if(!_q8){_q4=E(_q4);_8f=E(_8f);var _qi=_8f(toJSStr(_q4));return _8;}else{return new F(function(){return _2W(_qd,_);});}}else{return new F(function(){return _2D(_qe,_);});}}else{_q1=E(_q1);if(!_q1[0]){return E(_8G);}else{var _qj=_q1[1],_qk=_q1[2];_qk=E(_qk);if(!_qk[0]){_qj=E(_qj);if(_qj>=0){var _ql=B(_44(_qc,_qj));return new F(function(){return _1F(_qj,_ql[1],_ql[2],_);});}else{return E(_41);}}else{return E(_8E);}}}};_ns=E(_ns);if(!_ns[0]){_q6=E(_q6);if(!_q6){_q7=E(_q7);if(!_q7){_q8=E(_q8);if(!_q8){return _3Z;}else{var _qm=B(_qf(_));return _3Z;}}else{var _qn=B(_qf(_));return _3Z;}}else{var _qo=B(_qf(_));return _3Z;}}else{if(!B(_88(_pY,_ns[1]))){_q6=E(_q6);if(!_q6){_q7=E(_q7);if(!_q7){_q8=E(_q8);if(!_q8){return _3Z;}else{var _qp=B(_qf(_));return _3Z;}}else{var _qq=B(_qf(_));return _3Z;}}else{var _qr=B(_qf(_));return _3Z;}}else{return _3Z;}}};return E(_q9);};return E(_pW);};_pS=E(_pS);var _qs=__createJSFunc(5,_pS);_8g=E(_8g);var _qt=_8g(_qs);return _8;},_qu=41,_qv=[1,_qu,_x],_qw=new T(function(){return B(_r(0,12,_qv));}),_qx=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_qw));}),_qy=function(_qz){var _qA=new T(function(){return B(_r(0,_qz,_qx));});return new F(function(){return err(B(unAppCStr("toEnum{Rank}: tag (",_qA)));});},_qB=function(_qC,_qD){_qC=E(_qC);_qD=E(_qD);var _qE=dataToTag(_qD),_qF=_qE;_qC=E(_qC);var _qG=dataToTag(_qC);if(_qG<=_qF){var _qH=function(_qI){var _qJ=new T(function(){if(_qI!=_qF){return B(_qH(_qI+1|0));}else{return [0];}}),_qK=new T(function(){if(_qI<0){return B(_qy(_qI));}else{if(_qI>12){return B(_qy(_qI));}else{return _qI;}}});return [1,_qK,_qJ];};return new F(function(){return _qH(_qG);});}else{return [0];}},_qL=0,_qM=12,_qN=new T(function(){return B(_qB(_qL,_qM));}),_qO=new T(function(){return B(_r(0,3,_qv));}),_qP=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_qO));}),_qQ=function(_qR){var _qS=new T(function(){return B(_r(0,_qR,_qP));});return new F(function(){return err(B(unAppCStr("toEnum{Suit}: tag (",_qS)));});},_qT=function(_qU){var _qV=new T(function(){_qU=E(_qU);if(_qU==3){return [0];}else{return B(_qT(_qU+1|0));}}),_qW=new T(function(){if(_qU<0){return B(_qQ(_qU));}else{if(_qU>3){return B(_qQ(_qU));}else{return _qU;}}});return [1,_qW,_qV];},_qX=new T(function(){return B(_qT(0));}),_qY=function(_qZ){_qZ=E(_qZ);if(!_qZ[0]){return [0];}else{var _r0=_qZ[1],_r1=_qZ[2],_r2=new T(function(){return B(_qY(_r1));}),_r3=function(_r4){_r4=E(_r4);if(!_r4[0]){return E(_r2);}else{var _r5=_r4[2],_r6=new T(function(){return B(_r3(_r5));});return [1,[0,_r0,_r4[1]],_r6];}};return new F(function(){return _r3(_qX);});}},_r7=new T(function(){return B(_qY(_qN));}),_r8=[0,_x,_x],_r9=[1,_r8,_x],_ra=function(_rb){if(_rb>1){var _rc=new T(function(){return B(_ra(_rb-1|0));});return [1,_r8,_rc];}else{return E(_r9);}},_rd=new T(function(){return B(_ra(7));}),_re=function(_rf){if(_rf>1){var _rg=new T(function(){var _rh=B(_re(_rf-1|0));return [1,_rh[1],_rh[2]];});return [0,_x,_rg];}else{return [0,_x,_x];}},_ri=new T(function(){var _rj=B(_re(4));return [1,_rj[1],_rj[2]];}),_rk=new T(function(){return (function (cb) { loadCards(cb); });}),_rl=new T(function(){return B(unCStr("ArithException"));}),_rm=new T(function(){return B(unCStr("GHC.Exception"));}),_rn=new T(function(){return B(unCStr("base"));}),_ro=new T(function(){var _rp=hs_wordToWord64(4194982440),_rq=hs_wordToWord64(3110813675);return [0,_rp,_rq,[0,_rp,_rq,_rn,_rm,_rl],_x];}),_rr=function(_rs){return E(_ro);},_rt=function(_ru){_ru=E(_ru);return new F(function(){return _61(B(_5Z(_ru[1])),_rr,_ru[2]);});},_rv=new T(function(){return B(unCStr("arithmetic underflow"));}),_rw=new T(function(){return B(unCStr("arithmetic overflow"));}),_rx=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_ry=new T(function(){return B(unCStr("denormal"));}),_rz=new T(function(){return B(unCStr("divide by zero"));}),_rA=new T(function(){return B(unCStr("loss of precision"));}),_rB=function(_rC){_rC=E(_rC);switch(_rC){case 0:return E(_rw);case 1:return E(_rv);case 2:return E(_rA);case 3:return E(_rz);case 4:return E(_ry);default:return E(_rx);}},_rD=function(_rE){return new F(function(){return _g(_rv,_rE);});},_rF=function(_rE){return new F(function(){return _g(_rw,_rE);});},_rG=function(_rE){return new F(function(){return _g(_rx,_rE);});},_rH=function(_rE){return new F(function(){return _g(_ry,_rE);});},_rI=function(_rE){return new F(function(){return _g(_rz,_rE);});},_rJ=function(_rE){return new F(function(){return _g(_rA,_rE);});},_rK=function(_rL){_rL=E(_rL);switch(_rL){case 0:return E(_rF);case 1:return E(_rD);case 2:return E(_rJ);case 3:return E(_rI);case 4:return E(_rH);default:return E(_rG);}},_rM=function(_rN,_rO){return new F(function(){return _6j(_rK,_rN,_rO);});},_rP=function(_rQ,_rR){_rR=E(_rR);switch(_rR){case 0:return E(_rF);case 1:return E(_rD);case 2:return E(_rJ);case 3:return E(_rI);case 4:return E(_rH);default:return E(_rG);}},_rS=[0,_rP,_rB,_rM],_rT=new T(function(){return [0,_rr,_rS,_rU,_rt];}),_rU=function(_rE){return [0,_rT,_rE];},_rV=3,_rW=new T(function(){return B(_rU(_rV));}),_rX=new T(function(){return die(_rW);}),_rY=function(_rZ){_rZ=E(_rZ);return E(_rZ[1]);},_s0=function(_s1,_s2,_s3,_s4,_s5){_s4=E(_s4);_s5=E(_s5);var _s6=B(A(_rY,[_s1,_s5]));return new F(function(){return A(_s2,[_s3,[1,_s6,_s4]]);});},_s7=function(_s8){return E(_s8);},_s9=function(_sa){return new F(function(){return __lst2arr(B(_3t(_s7,_sa)));});},_sb=[0,_s7,_s9],_sc=function(_sd,_){_sd=E(_sd);if(!_sd[0]){return _x;}else{var _se=_sd[1],_sf=B(_sc(_sd[2],_)),_sg=new T(function(){_se=E(_se);var _sh=Number(_se);return jsTrunc(_sh);});return [1,_sg,_sf];}},_si=function(_sj,_){var _sk=__arr2lst(0,_sj);return new F(function(){return _sc(_sk,_);});},_sl=function(_sm,_){_sm=E(_sm);return new F(function(){return _si(_sm,_);});},_sn=function(_so,_){return new T(function(){_so=E(_so);var _sp=Number(_so);return jsTrunc(_sp);});},_sq=[0,_sn,_sl],_sr=function(_ss){_ss=E(_ss);return E(_ss[1]);},_st=function(_su,_sv,_sw,_){_sv=E(_sv);_sw=E(_sw);var _sx=__apply(_sv,_sw);return new F(function(){return A(_sr,[_su,_sx,_]);});},_sy=function(_sz,_sA,_sB,_){return new F(function(){return _st(_sz,_sA,_sB,_);});},_sC=function(_sD,_sE,_){return new F(function(){return _sy(_sq,_sD,_sE,_);});},_sF=(function(s){return s[0];}),_sG=function(_sH){return new F(function(){return _s0(_sb,_sC,_sF,_x,_sH);});},_sI=function(_sJ,_sK){var _sL=_sJ%_sK;if(_sJ<=0){if(_sJ>=0){return E(_sL);}else{if(_sK<=0){return E(_sL);}else{_sL=E(_sL);return (_sL==0)?0:_sL+_sK|0;}}}else{if(_sK>=0){if(_sJ>=0){return E(_sL);}else{if(_sK<=0){return E(_sL);}else{_sL=E(_sL);return (_sL==0)?0:_sL+_sK|0;}}}else{_sL=E(_sL);return (_sL==0)?0:_sL+_sK|0;}}},_sM=function(_sN,_){_sN=E(_sN);if(!_sN[0]){return _x;}else{var _sO=B(_sM(_sN[2],_));return [1,_sN[1],_sO];}},_sP=function(_sQ,_){var _sR=__arr2lst(0,_sQ);return new F(function(){return _sM(_sR,_);});},_sS=function(_sT,_){_sT=E(_sT);return new F(function(){return _sP(_sT,_);});},_sU=function(_sV,_){return _sV;},_sW=[0,_sU,_sS],_sX=function(_sD,_sE,_){return new F(function(){return _sy(_sW,_sD,_sE,_);});},_sY=(function(s){return window['md51'](s.join(','));}),_sZ=function(_sH){return new F(function(){return _s0(_sb,_sX,_sY,_x,_sH);});},_t0=function(_t1){var _t2=B(A(_t1,[_]));return E(_t2);},_t3=function(_t4){var _t5=function(_){return new F(function(){return A(_sZ,[_t4,0]);});};return new F(function(){return _t0(_t5);});},_t6=function(_t7,_t8,_t9){while(1){var _ta=(function(_tb,_tc,_td){if(_tb>_tc){var _te=_tc,_tf=_tb,_tg=_td;_t7=_te;_t8=_tf;_t9=_tg;return null;}else{var _th=new T(function(){return B(_t3(_td));}),_ti=new T(function(){var _tj=(_tc-_tb|0)+1|0;switch(_tj){case -1:return _tb;break;case 0:return E(_rX);break;default:var _tk=function(_){return new F(function(){return A(_sG,[_td,0]);});};return B(_sI(B(_t0(_tk)),_tj))+_tb|0;}});return [0,_ti,_th];}})(_t7,_t8,_t9);if(_ta!=null){return _ta;}}},_tl=function(_tm,_tn){_tm=E(_tm);if(!_tm){return [0,_x,_tn];}else{_tn=E(_tn);if(!_tn[0]){return [0,_x,_x];}else{var _to=_tn[2],_tp=new T(function(){var _tq=B(_tl(_tm-1|0,_to));return [0,_tq[1],_tq[2]];}),_tr=new T(function(){_tp=E(_tp);return E(_tp[2]);}),_ts=new T(function(){_tp=E(_tp);return E(_tp[1]);});return [0,[1,_tn[1],_ts],_tr];}}},_tt=function(_tu,_tv){_tv=E(_tv);if(!_tv[0]){return [0];}else{var _tw=new T(function(){var _tx=B(_t6(0,B(_1z(_tv,0))-1|0,_tu));return [0,_tx[1],_tx[2]];}),_ty=new T(function(){_tw=E(_tw);var _tz=_tw[1];_tz=E(_tz);if(_tz>=0){var _tA=B(_tl(_tz,_tv));return [0,_tA[1],_tA[2]];}else{return [0,_x,_tv];}}),_tB=new T(function(){_ty=E(_ty);return E(_ty[2]);}),_tC=new T(function(){_ty=E(_ty);var _tD=new T(function(){_tB=E(_tB);if(!_tB[0]){return E(_3B);}else{return E(_tB[2]);}},1),_tE=new T(function(){_tw=E(_tw);return E(_tw[2]);});return B(_tt(_tE,B(_g(_ty[1],_tD))));}),_tF=new T(function(){_tB=E(_tB);if(!_tB[0]){return E(_3d);}else{return E(_tB[1]);}});return [1,_tF,_tC];}},_tG=function(_){var _tH=function(_){var _tI=B(_3W(_)),_tJ=_tI,_tK=new T(function(){var _tL=new T(function(){return B(_tt(_tJ,_r7));}),_tM=B(_3E(_rd,_tL));return [0,_tM[1],_tM[2]];}),_tN=new T(function(){_tK=E(_tK);return E(_tK[2]);}),_tO=new T(function(){_tK=E(_tK);return E(_tK[1]);}),_tP=B(_2Z(_ri,_tO,_x,_tN,_)),_tQ=B(_nq([0,_ri,_tO,_x,_tN],_3T,_));return _3Z;};_tH=E(_tH);var _tR=__createJSFunc(0,_tH);_rk=E(_rk);var _tS=_rk(_tR);return _8;},_tT=function(_){return new F(function(){return _tG(_);});};
var hasteMain = function() {B(A(_tT, [0]));};window.onload = hasteMain;