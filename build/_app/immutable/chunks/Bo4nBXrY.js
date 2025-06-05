/**
* @vue/shared v3.5.16
* (c) 2018-present Yuxi (Evan) You and Vue contributors
* @license MIT
**//*! #__NO_SIDE_EFFECTS__ */function Ht(t){const e=Object.create(null);for(const n of t.split(","))e[n]=1;return n=>n in e}const Kt=Object.assign,Xt=Object.prototype.hasOwnProperty,st=(t,e)=>Xt.call(t,e),z=Array.isArray,J=t=>$t(t)==="[object Map]",Bt=t=>typeof t=="string",X=t=>typeof t=="symbol",k=t=>t!==null&&typeof t=="object",Gt=Object.prototype.toString,$t=t=>Gt.call(t),Ft=t=>$t(t).slice(8,-1),lt=t=>Bt(t)&&t!=="NaN"&&t[0]!=="-"&&""+parseInt(t,10)===t,H=(t,e)=>!Object.is(t,e);/**
* @vue/reactivity v3.5.16
* (c) 2018-present Yuxi (Evan) You and Vue contributors
* @license MIT
**/let Wt,Mt=0,et;function ft(){Mt++}function ut(){if(--Mt>0)return;let t;for(;et;){let e=et;for(et=void 0;e;){const n=e.next;if(e.next=void 0,e.flags&=-9,e.flags&1)try{e.trigger()}catch(o){t||(t=o)}e=n}}if(t)throw t}let Z=!0;const Et=[];function Yt(){Et.push(Z),Z=!1}function Vt(){const t=Et.pop();Z=t===void 0?!0:t}class St{constructor(e){this.computed=e,this.version=0,this.activeLink=void 0,this.subs=void 0,this.map=void 0,this.key=void 0,this.sc=0}track(e){}trigger(e){this.version++,this.notify(e)}notify(e){ft();try{for(let n=this.subs;n;n=n.prevSub)n.sub.notify()&&n.sub.dep.notify()}finally{ut()}}}const it=new WeakMap,I=Symbol(""),ot=Symbol(""),K=Symbol("");function w(t,e,n){if(Z&&Wt){let o=it.get(t);o||it.set(t,o=new Map);let r=o.get(n);r||(o.set(n,r=new St),r.map=o,r.key=n),r.track()}}function E(t,e,n,o,r,l){const c=it.get(t);if(!c)return;const a=f=>{f&&f.trigger()};if(ft(),e==="clear")c.forEach(a);else{const f=z(t),i=f&&lt(n);if(f&&n==="length"){const s=Number(o);c.forEach((d,u)=>{(u==="length"||u===K||!X(u)&&u>=s)&&a(d)})}else switch((n!==void 0||c.has(void 0))&&a(c.get(n)),i&&a(c.get(K)),e){case"add":f?i&&a(c.get("length")):(a(c.get(I)),J(t)&&a(c.get(ot)));break;case"delete":f||(a(c.get(I)),J(t)&&a(c.get(ot)));break;case"set":J(t)&&a(c.get(I));break}}ut()}function C(t){const e=h(t);return e===t?e:(w(e,"iterate",K),S(t)?e:e.map(x))}function dt(t){return w(t=h(t),"iterate",K),t}const Jt={__proto__:null,[Symbol.iterator](){return rt(this,Symbol.iterator,x)},concat(...t){return C(this).concat(...t.map(e=>z(e)?C(e):e))},entries(){return rt(this,"entries",t=>(t[1]=x(t[1]),t))},every(t,e){return M(this,"every",t,e,void 0,arguments)},filter(t,e){return M(this,"filter",t,e,n=>n.map(x),arguments)},find(t,e){return M(this,"find",t,e,x,arguments)},findIndex(t,e){return M(this,"findIndex",t,e,void 0,arguments)},findLast(t,e){return M(this,"findLast",t,e,x,arguments)},findLastIndex(t,e){return M(this,"findLastIndex",t,e,void 0,arguments)},forEach(t,e){return M(this,"forEach",t,e,void 0,arguments)},includes(...t){return nt(this,"includes",t)},indexOf(...t){return nt(this,"indexOf",t)},join(t){return C(this).join(t)},lastIndexOf(...t){return nt(this,"lastIndexOf",t)},map(t,e){return M(this,"map",t,e,void 0,arguments)},pop(){return q(this,"pop")},push(...t){return q(this,"push",t)},reduce(t,...e){return bt(this,"reduce",t,e)},reduceRight(t,...e){return bt(this,"reduceRight",t,e)},shift(){return q(this,"shift")},some(t,e){return M(this,"some",t,e,void 0,arguments)},splice(...t){return q(this,"splice",t)},toReversed(){return C(this).toReversed()},toSorted(t){return C(this).toSorted(t)},toSpliced(...t){return C(this).toSpliced(...t)},unshift(...t){return q(this,"unshift",t)},values(){return rt(this,"values",x)}};function rt(t,e,n){const o=dt(t),r=o[e]();return o!==t&&!S(t)&&(r._next=r.next,r.next=()=>{const l=r._next();return l.value&&(l.value=n(l.value)),l}),r}const Zt=Array.prototype;function M(t,e,n,o,r,l){const c=dt(t),a=c!==t&&!S(t),f=c[e];if(f!==Zt[e]){const d=f.apply(t,l);return a?x(d):d}let i=n;c!==t&&(a?i=function(d,u){return n.call(this,x(d),u,t)}:n.length>2&&(i=function(d,u){return n.call(this,d,u,t)}));const s=f.call(c,i,o);return a&&r?r(s):s}function bt(t,e,n,o){const r=dt(t);let l=n;return r!==t&&(S(t)?n.length>3&&(l=function(c,a,f){return n.call(this,c,a,f,t)}):l=function(c,a,f){return n.call(this,c,x(a),f,t)}),r[e](l,...o)}function nt(t,e,n){const o=h(t);w(o,"iterate",K);const r=o[e](...n);return(r===-1||r===!1)&&de(n[0])?(n[0]=h(n[0]),o[e](...n)):r}function q(t,e,n=[]){Yt(),ft();const o=h(t)[e].apply(t,n);return ut(),Vt(),o}const Qt=Ht("__proto__,__v_isRef,__isVue"),At=new Set(Object.getOwnPropertyNames(Symbol).filter(t=>t!=="arguments"&&t!=="caller").map(t=>Symbol[t]).filter(X));function kt(t){X(t)||(t=String(t));const e=h(this);return w(e,"has",t),e.hasOwnProperty(t)}class It{constructor(e=!1,n=!1){this._isReadonly=e,this._isShallow=n}get(e,n,o){if(n==="__v_skip")return e.__v_skip;const r=this._isReadonly,l=this._isShallow;if(n==="__v_isReactive")return!r;if(n==="__v_isReadonly")return r;if(n==="__v_isShallow")return l;if(n==="__v_raw")return o===(r?l?le:Ct:l?ce:Ot).get(e)||Object.getPrototypeOf(e)===Object.getPrototypeOf(o)?e:void 0;const c=z(e);if(!r){let f;if(c&&(f=Jt[n]))return f;if(n==="hasOwnProperty")return kt}const a=Reflect.get(e,n,P(e)?e:o);return(X(n)?At.has(n):Qt(n))||(r||w(e,"get",n),l)?a:P(a)?c&&lt(n)?a:a.value:k(a)?r?Pt(a):zt(a):a}}class te extends It{constructor(e=!1){super(!1,e)}set(e,n,o,r){let l=e[n];if(!this._isShallow){const f=T(l);if(!S(o)&&!T(o)&&(l=h(l),o=h(o)),!z(e)&&P(l)&&!P(o))return f?!1:(l.value=o,!0)}const c=z(e)&&lt(n)?Number(n)<e.length:st(e,n),a=Reflect.set(e,n,o,P(e)?e:r);return e===h(r)&&(c?H(o,l)&&E(e,"set",n,o):E(e,"add",n,o)),a}deleteProperty(e,n){const o=st(e,n);e[n];const r=Reflect.deleteProperty(e,n);return r&&o&&E(e,"delete",n,void 0),r}has(e,n){const o=Reflect.has(e,n);return(!X(n)||!At.has(n))&&w(e,"has",n),o}ownKeys(e){return w(e,"iterate",z(e)?"length":I),Reflect.ownKeys(e)}}class ee extends It{constructor(e=!1){super(!0,e)}set(e,n){return!0}deleteProperty(e,n){return!0}}const re=new te,ne=new ee,at=t=>t,W=t=>Reflect.getPrototypeOf(t);function se(t,e,n){return function(...o){const r=this.__v_raw,l=h(r),c=J(l),a=t==="entries"||t===Symbol.iterator&&c,f=t==="keys"&&c,i=r[t](...o),s=n?at:e?ct:x;return!e&&w(l,"iterate",f?ot:I),{next(){const{value:d,done:u}=i.next();return u?{value:d,done:u}:{value:a?[s(d[0]),s(d[1])]:s(d),done:u}},[Symbol.iterator](){return this}}}}function Y(t){return function(...e){return t==="delete"?!1:t==="clear"?void 0:this}}function ie(t,e){const n={get(r){const l=this.__v_raw,c=h(l),a=h(r);t||(H(r,a)&&w(c,"get",r),w(c,"get",a));const{has:f}=W(c),i=e?at:t?ct:x;if(f.call(c,r))return i(l.get(r));if(f.call(c,a))return i(l.get(a));l!==c&&l.get(r)},get size(){const r=this.__v_raw;return!t&&w(h(r),"iterate",I),Reflect.get(r,"size",r)},has(r){const l=this.__v_raw,c=h(l),a=h(r);return t||(H(r,a)&&w(c,"has",r),w(c,"has",a)),r===a?l.has(r):l.has(r)||l.has(a)},forEach(r,l){const c=this,a=c.__v_raw,f=h(a),i=e?at:t?ct:x;return!t&&w(f,"iterate",I),a.forEach((s,d)=>r.call(l,i(s),i(d),c))}};return Kt(n,t?{add:Y("add"),set:Y("set"),delete:Y("delete"),clear:Y("clear")}:{add(r){!e&&!S(r)&&!T(r)&&(r=h(r));const l=h(this);return W(l).has.call(l,r)||(l.add(r),E(l,"add",r,r)),this},set(r,l){!e&&!S(l)&&!T(l)&&(l=h(l));const c=h(this),{has:a,get:f}=W(c);let i=a.call(c,r);i||(r=h(r),i=a.call(c,r));const s=f.call(c,r);return c.set(r,l),i?H(l,s)&&E(c,"set",r,l):E(c,"add",r,l),this},delete(r){const l=h(this),{has:c,get:a}=W(l);let f=c.call(l,r);f||(r=h(r),f=c.call(l,r)),a&&a.call(l,r);const i=l.delete(r);return f&&E(l,"delete",r,void 0),i},clear(){const r=h(this),l=r.size!==0,c=r.clear();return l&&E(r,"clear",void 0,void 0),c}}),["keys","values","entries",Symbol.iterator].forEach(r=>{n[r]=se(r,t,e)}),n}function Rt(t,e){const n=ie(t,e);return(o,r,l)=>r==="__v_isReactive"?!t:r==="__v_isReadonly"?t:r==="__v_raw"?o:Reflect.get(st(n,r)&&r in o?n:o,r,l)}const oe={get:Rt(!1,!1)},ae={get:Rt(!0,!1)},Ot=new WeakMap,ce=new WeakMap,Ct=new WeakMap,le=new WeakMap;function fe(t){switch(t){case"Object":case"Array":return 1;case"Map":case"Set":case"WeakMap":case"WeakSet":return 2;default:return 0}}function ue(t){return t.__v_skip||!Object.isExtensible(t)?0:fe(Ft(t))}function zt(t){return T(t)?t:Tt(t,!1,re,oe,Ot)}function Pt(t){return Tt(t,!0,ne,ae,Ct)}function Tt(t,e,n,o,r){if(!k(t)||t.__v_raw&&!(e&&t.__v_isReactive))return t;const l=ue(t);if(l===0)return t;const c=r.get(t);if(c)return c;const a=new Proxy(t,l===2?o:n);return r.set(t,a),a}function T(t){return!!(t&&t.__v_isReadonly)}function S(t){return!!(t&&t.__v_isShallow)}function de(t){return t?!!t.__v_raw:!1}function h(t){const e=t&&t.__v_raw;return e?h(e):t}const x=t=>k(t)?zt(t):t,ct=t=>k(t)?Pt(t):t;function P(t){return t?t.__v_isRef===!0:!1}function pe(t){return he(t,!1)}function he(t,e){return P(t)?t:new me(t,e)}class me{constructor(e,n){this.dep=new St,this.__v_isRef=!0,this.__v_isShallow=!1,this._rawValue=n?e:h(e),this._value=n?e:x(e),this.__v_isShallow=n}get value(){return this.dep.track(),this._value}set value(e){const n=this._rawValue,o=this.__v_isShallow||S(e)||T(e);e=o?e:h(e),H(e,n)&&(this._rawValue=e,this._value=o?e:x(e),this.dep.trigger())}}var ge=Object.create,Lt=Object.defineProperty,ve=Object.getOwnPropertyDescriptor,ye=Object.getOwnPropertyNames,xe=Object.getPrototypeOf,be=Object.prototype.hasOwnProperty,R=(t,e)=>()=>(e||t((e={exports:{}}).exports,e),e.exports),we=(t,e,n,o)=>{if(e&&typeof e=="object"||typeof e=="function")for(let r of ye(e))!be.call(t,r)&&r!==n&&Lt(t,r,{get:()=>e[r],enumerable:!(o=ve(e,r))||o.enumerable});return t},pt=(t,e,n)=>(n=t!=null?ge(xe(t)):{},we(Lt(n,"default",{value:t,enumerable:!0}),t)),jt=R(t=>{(function(e){typeof DO_NOT_EXPORT_CRC>"u"?typeof t=="object"?e(t):typeof define=="function"&&define.amd?define(function(){var n={};return e(n),n}):e({}):e({})})(function(e){e.version="0.3.0";function n(){for(var i=0,s=new Array(256),d=0;d!=256;++d)i=d,i=i&1?-306674912^i>>>1:i>>>1,i=i&1?-306674912^i>>>1:i>>>1,i=i&1?-306674912^i>>>1:i>>>1,i=i&1?-306674912^i>>>1:i>>>1,i=i&1?-306674912^i>>>1:i>>>1,i=i&1?-306674912^i>>>1:i>>>1,i=i&1?-306674912^i>>>1:i>>>1,i=i&1?-306674912^i>>>1:i>>>1,s[d]=i;return typeof Int32Array<"u"?new Int32Array(s):s}var o=n(),r=typeof Buffer<"u";function l(i){if(i.length>32768&&r)return a(new Buffer(i));for(var s=-1,d=i.length-1,u=0;u<d;)s=o[(s^i.charCodeAt(u++))&255]^s>>>8,s=o[(s^i.charCodeAt(u++))&255]^s>>>8;return u===d&&(s=s>>>8^o[(s^i.charCodeAt(u))&255]),s^-1}function c(i){if(i.length>1e4)return a(i);for(var s=-1,d=0,u=i.length-3;d<u;)s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255];for(;d<u+3;)s=s>>>8^o[(s^i[d++])&255];return s^-1}function a(i){for(var s=-1,d=0,u=i.length-7;d<u;)s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255],s=s>>>8^o[(s^i[d++])&255];for(;d<u+7;)s=s>>>8^o[(s^i[d++])&255];return s^-1}function f(i){for(var s=-1,d=0,u=i.length,p,m;d<u;)p=i.charCodeAt(d++),p<128?s=s>>>8^o[(s^p)&255]:p<2048?(s=s>>>8^o[(s^(192|p>>6&31))&255],s=s>>>8^o[(s^(128|p&63))&255]):p>=55296&&p<57344?(p=(p&1023)+64,m=i.charCodeAt(d++)&1023,s=s>>>8^o[(s^(240|p>>8&7))&255],s=s>>>8^o[(s^(128|p>>2&63))&255],s=s>>>8^o[(s^(128|m>>6&15|p&3))&255],s=s>>>8^o[(s^(128|m&63))&255]):(s=s>>>8^o[(s^(224|p>>12&15))&255],s=s>>>8^o[(s^(128|p>>6&63))&255],s=s>>>8^o[(s^(128|p&63))&255]);return s^-1}e.table=o,e.bstr=l,e.buf=c,e.str=f})}),_e=R((t,e)=>{var n=jt();e.exports=c;var o=new Uint8Array(4),r=new Int32Array(o.buffer),l=new Uint32Array(o.buffer);function c(a){if(a[0]!==137)throw new Error("Invalid .png file header");if(a[1]!==80)throw new Error("Invalid .png file header");if(a[2]!==78)throw new Error("Invalid .png file header");if(a[3]!==71)throw new Error("Invalid .png file header");if(a[4]!==13)throw new Error("Invalid .png file header: possibly caused by DOS-Unix line ending conversion?");if(a[5]!==10)throw new Error("Invalid .png file header: possibly caused by DOS-Unix line ending conversion?");if(a[6]!==26)throw new Error("Invalid .png file header");if(a[7]!==10)throw new Error("Invalid .png file header: possibly caused by DOS-Unix line ending conversion?");for(var f=!1,i=[],s=8;s<a.length;){o[3]=a[s++],o[2]=a[s++],o[1]=a[s++],o[0]=a[s++];var d=l[0]+4,u=new Uint8Array(d);u[0]=a[s++],u[1]=a[s++],u[2]=a[s++],u[3]=a[s++];var p=String.fromCharCode(u[0])+String.fromCharCode(u[1])+String.fromCharCode(u[2])+String.fromCharCode(u[3]);if(!i.length&&p!=="IHDR")throw new Error("IHDR header missing");if(p==="IEND"){f=!0,i.push({name:p,data:new Uint8Array(0)});break}for(var m=4;m<d;m++)u[m]=a[s++];o[3]=a[s++],o[2]=a[s++],o[1]=a[s++],o[0]=a[s++];var _=r[0],y=n.buf(u);if(y!==_)throw new Error("CRC values for "+p+" header do not match, PNG file is likely corrupted");var g=new Uint8Array(u.buffer.slice(4));i.push({name:p,data:g})}if(!f)throw new Error(".png file ended prematurely: no IEND header was found");return i}}),$e=R((t,e)=>{e.exports=function(n,o,r){var l=[],c=n.length;if(c===0)return l;var a=o<0?Math.max(0,o+c):o||0;for(r!==void 0&&(c=r<0?r+c:r);c-- >a;)l[c-a]=n[c];return l}}),Me=R((t,e)=>{var n=$e(),o=jt();e.exports=a;var r=new Uint8Array(4),l=new Int32Array(r.buffer),c=new Uint32Array(r.buffer);function a(f){var i=8,s=i,d;for(d=0;d<f.length;d++)i+=f[d].data.length,i+=12;var u=new Uint8Array(i);for(u[0]=137,u[1]=80,u[2]=78,u[3]=71,u[4]=13,u[5]=10,u[6]=26,u[7]=10,d=0;d<f.length;d++){var p=f[d],m=p.name,_=p.data,y=_.length,g=[m.charCodeAt(0),m.charCodeAt(1),m.charCodeAt(2),m.charCodeAt(3)];c[0]=y,u[s++]=r[3],u[s++]=r[2],u[s++]=r[1],u[s++]=r[0],u[s++]=g[0],u[s++]=g[1],u[s++]=g[2],u[s++]=g[3];for(var $=0;$<y;)u[s++]=_[$++];var v=g.concat(n(_)),b=o.buf(v);l[0]=b,u[s++]=r[3],u[s++]=r[2],u[s++]=r[1],u[s++]=r[0]}return u}}),Ee=R((t,e)=>{e.exports=n;function n(o,r){if(o=String(o),r=String(r),!/^[\x00-\xFF]+$/.test(o)||!/^[\x00-\xFF]+$/.test(r))throw new Error("Only Latin-1 characters are permitted in PNG tEXt chunks. You might want to consider base64 encoding and/or zEXt compression");if(o.length>=80)throw new Error('Keyword "'+o+'" is longer than the 79-character limit imposed by the PNG specification');for(var l=o.length+r.length+1,c=new Uint8Array(l),a=0,f,i=0;i<o.length;i++){if(!(f=o.charCodeAt(i)))throw new Error("0x00 character is not permitted in tEXt keywords");c[a++]=f}c[a++]=0;for(var s=0;s<r.length;s++){if(!(f=r.charCodeAt(s)))throw new Error("0x00 character is not permitted in tEXt content");c[a++]=f}return{name:"tEXt",data:c}}}),Se=R((t,e)=>{e.exports=n;function n(o){o.data&&o.name&&(o=o.data);for(var r=!0,l="",c="",a=0;a<o.length;a++){var f=o[a];if(r)f?c+=String.fromCharCode(f):r=!1;else if(f)l+=String.fromCharCode(f);else throw new Error("Invalid NULL character found. 0x00 character is not permitted in tEXt content")}return{keyword:c,text:l}}}),Ae=R(t=>{t.encode=Ee(),t.decode=Se()}),Ie=()=>{if(typeof window>"u"||typeof window.CanvasRenderingContext2D>"u"||!n())return;let t=CanvasRenderingContext2D.prototype,e=t.drawImage;if(!e){console.error("This script requires a basic implementation of drawImage");return}t.drawImage=function(c,a,f){if(arguments.length!==9)return e.apply(this,[...arguments]);let i=o(...arguments);if(!r(i))return e.apply(this,i)};function n(){let c=document.createElement("canvas").getContext("2d");c.fillRect(0,0,40,40),c.drawImage(c.canvas,-40,-40,80,80,50,50,20,20);let a=c.getImageData(50,50,30,30),f=new Uint32Array(a.data.buffer),i=(u,p)=>f[p*a.width+u],s=[[9,9],[20,9],[9,20],[20,20]],d=[[10,10],[19,10],[10,19],[19,19]];return s.some(([u,p])=>i(u,p)!==0)||d.some(([u,p])=>i(u,p)===0)}function o(c,a,f,i,s,d,u,p,m){let{width:_,height:y}=l(c);i<0&&(a+=i,i=Math.abs(i)),s<0&&(f+=s,s=Math.abs(s)),p<0&&(d+=p,p=Math.abs(p)),m<0&&(u+=m,m=Math.abs(m));let g=Math.max(a,0),$=Math.min(a+i,_),v=Math.max(f,0),b=Math.min(f+s,y),A=p/i,O=m/s;return[c,g,v,$-g,b-v,a<0?d-a*A:d,f<0?u-f*O:u,($-g)*A,(b-v)*O]}function r(c){return[3,4,7,8].some(a=>!c[a])}function l(c){let a=f=>{let i=globalThis[f];return i&&c instanceof i};if(a("HTMLImageElement"))return{width:c.naturalWidth,height:c.naturalHeight};if(a("HTMLVideoElement"))return{width:c.videoWidth,height:c.videoHeight};if(a("SVGImageElement"))throw new TypeError("SVGImageElement isn't yet supported as source image.","UnsupportedError");if(a("HTMLCanvasElement")||a("ImageBitmap"))return c}},Ut="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";function Re(t){return t.split("").reduce((e,n)=>e*64+Ut.indexOf(n),0)}function Oe(t){let e=Ut.split(""),n=[],o=0;for(let r of t)n[o++]=e.indexOf(r)/64;return n}function Ce(t){return t&&typeof t=="string"&&t.length===87&&t.split(".").length===2}function Dt(t){let e=t.split("."),n=e.map(a=>Oe(a)),o=n.map(a=>a[42]),r=n.map(a=>a.reduce((f,i)=>f+i)/a.length),l=e.map(a=>Re(a)%360),c=e.map((a,f)=>`hsl(${l[f]} ${o[f]*100}% ${r[f]*100}%)`);return{finals:o,decoded:n,angles:l,averages:r,colors:c}}function Q(t,e=3){return[...Array(Math.ceil(t.length/e))].map(()=>t.splice(0,e))}var ze=pt(_e()),Pe=pt(Me()),Te=pt(Ae()),ht=pe(null);function Le(t){try{let e=t.toDataURL("image/png").split(",")[1],n=atob(e),o=new Uint8Array(n.length);for(let r=0;r<n.length;r++)o[r]=n.charCodeAt(r);return o}catch(e){return ht.value="Failed to convert canvas to buffer: "+e.message,null}}function je(t,e){try{let n=`<metadata>
      <gun-data>${JSON.stringify(e)}</gun-data>
    </metadata>`;return t.replace("</svg>",`${n}</svg>`)}catch(n){return ht.value="Failed to embed data in SVG: "+n.message,null}}function Ue(t,e){try{let n=Le(t);if(!n)return null;let o=(0,ze.default)(n);return o.splice(-1,0,Te.default.encode("message",JSON.stringify(e))),(0,Pe.default)(o)}catch(n){return ht.value="Failed to embed data: "+n.message,null}}function De({pub:t,size:e,dark:n,draw:o,reflect:r,round:l,embed:c,p3:a}){let f=document.createElement("canvas");f.width=f.height=e;let i=f.getContext("2d"),{decoded:s,finals:d}=Dt(t);if(Ne({ctx:i,top:d[0],bottom:d[1],size:e,dark:n}),o=="squares"?(i.filter="blur(20px)",wt(s[0],i,e,a),i.filter="blur(0px)",i.globalCompositeOperation="color-burn",wt(s[1],i,e,a)):(_t(s[0],i,e,.42*e,a),i.globalCompositeOperation="multiply",_t(s[1],i,e,.125*e,a)),r&&(i.globalCompositeOperation="source-over",i.scale(-1,1),i.translate(-e/2,0),i.drawImage(f,e/2,0,e,e,0,0,e,e),i.setTransform(1,0,0,1,0,0)),l){let p=i.getImageData(0,0,e,e);i.clearRect(0,0,e,e),i.fillStyle=n?"#cccccc":"#ffffff",i.fillRect(0,0,e,e),i.putImageData(p,0,0),i.globalCompositeOperation="destination-in",i.beginPath(),i.arc(e/2,e/2,e/2,0,Math.PI*2),i.closePath(),i.fill()}let u=f.toDataURL("image/png");if(c){let p=Ue(f,{pub:t,content:c});if(p){let m=new Blob([p],{type:"image/png"});u=URL.createObjectURL(m)}}return u}function Ne({ctx:t,top:e=0,bottom:n=150,size:o=200,dark:r=!1}){let l=t.createLinearGradient(0,0,0,o),c=r?0:70;l.addColorStop(0,`hsl(0,0%,${c+e*30}%)`),l.addColorStop(1,`hsl(0,0%,${c+n*30}%)`),t.fillStyle=l,t.fillRect(0,0,o,o)}function wt(t,e,n,o){Q(t,14).forEach(r=>{if(r.length===14){let[l,c,a,f,i,s,d,u,p,m,_,y,g,$]=r,v=n/8+a*n*(7/8),b=e.createLinearGradient(l*n+v*u,0,l*n+v*g,n);b.addColorStop(0,o?`color(display-p3 ${f} ${i} ${s} / ${d})`:`rgba(${f*255}, ${i*255}, ${s*255}, ${d})`),b.addColorStop(1,o?`color(display-p3 ${p} ${m} ${_} / ${y})`:`rgba(${p*255}, ${m*255}, ${_*255}, ${y})`),e.fillStyle=b,e.translate(l*n,c*n),e.rotate($*Math.PI),e.fillRect(-v/2,-v/2,v,v),e.setTransform(1,0,0,1,0,0)}})}function _t(t,e,n,o,r){Q(t,7).forEach(l=>{if(l.length===7){let[c,a,f,i,s,d,u]=l;e.beginPath(),e.arc(n/2+c*n/2,a*n,f*o,0,2*Math.PI),e.fillStyle=r?`color(display-p3 ${i} ${s} ${d} / ${u})`:`rgba(${i*255}, ${s*255}, ${d*255}, ${u})`,e.closePath(),e.fill()}})}function qe({size:t=300,reflect:e=!0,follow:n=!0,finals:o=[.5,.5],averages:r=[.5,.5],breathVert:l=.03}={}){return`
    <script type="text/javascript"><![CDATA[
      // Compact state & precomputed constants
      const s = {
        e: [], // elements
        p: { x: null, y: null, tx: null, ty: null, a: false, lt: 0, lit: 0 }, // pointer state
        r: null, // rect
        a: false, // pulse active
        pt: 0, // pulse time
        pv: 0, // pulse value
        t: 0 // time
      },
      c = {
        sz: ${t},
        cx: ${t/2},
        cy: ${t/2},
        pz: ${t} * 1.2,
        bz: ${t} * 0.4 * ${o[1]},
        pr: ${t} * (0.5 + ${o[0]}/4),
        mv: (0.1 + 0.05*${r[0]}) * Math.max(0.8, Math.min(4.4, 200/${t})),
        bs: 0.1 * Math.max(0.8, Math.min(1.4, 200/${t})),
        bv: ${l} * Math.max(0.8, Math.min(1.4, 200/${t})),
        bd: 4500 + ${r[0]}*1000,
        pa: 0.6 * Math.max(0.8, Math.min(1.4, 200/${t})),
        pr: 200,
        pf: 700,
        rf: ${n},
        d: 900,
        td: 1500000,
        pp: 0.5+ 0.5*${r[1]}
      };

      // Init using IIFE
      (() => {
        const svg = document.currentScript?.closest('svg');
        if (!svg) return;
        
        // Update rect and get element collections
        const ur = () => s.r = svg.getBoundingClientRect();
        ur();
        
        // Cache elements for better performance
        s.e = [
          ...Array.from(svg.querySelectorAll('.interactive-circle')).map(el => {
            const x = +el.getAttribute('data-cx');
            const y = +el.getAttribute('data-cy');
            const r = +el.getAttribute('r');
            const o = +el.getAttribute('data-opacity');
            const m = Math.pow(r / (c.sz * 0.05), 0.7) * o;
            const mr = el.nextElementSibling && 
                      el.nextElementSibling.getAttribute('cx') === (c.sz - x).toString() ?
                      el.nextElementSibling : null;
            
            return {
              el, t: 'c',
              x, y, r,
              cx: x, cy: y,
              m, mr,
              o: Math.random() * Math.PI * 2
            };
          }),
          ...Array.from(svg.querySelectorAll('.interactive-square')).map(el => {
            const x = +el.getAttribute('data-cx');
            const y = +el.getAttribute('data-cy');
            const r = +el.getAttribute('data-r');
            const o = +el.getAttribute('data-opacity');
            const a = +el.getAttribute('data-angle') + (Math.random() * 20 - 10);
            
            return {
              el, t: 's',
              x, y, z: c.bz, r: a,
              cx: x, cy: y, cz: c.bz, cr: a, cs: 1,
              m: Math.pow((r / c.sz), 1.5) * o,
              o: Math.random() * Math.PI * 2
            };
          })
        ];
        
        // Initial transform for squares
        s.e.filter(el => el.t === 's').forEach(setTransform);
        
        // Helper functions for event handling
        const gc = e => ({ x: (e.touches?.[0] || e.changedTouches?.[0] || e).clientX, 
                           y: (e.touches?.[0] || e.changedTouches?.[0] || e).clientY });
        
        const inBounds = (x, y) => {
          const { left, top, width, height } = s.r || {};
          return x >= left && x <= left + width && y >= top && y <= top + height;
        };
        
        // Event handlers
        const up = e => {
          if (!s.r) ur();
          const { left, top, width, height } = s.r;
          const { x, y } = gc(e);
          
          if (!inBounds(x, y) && !e.type.startsWith('touch')) {
            if (s.p.a) pl(false);
            return;
          }
          
          // Calculate relative position
          const tx = ((x - left) / width) * c.sz;
          const ty = ((y - top) / height) * c.sz;
          
          // Init/update target position
          if (s.p.tx === null) {
            s.p.tx = tx;
            s.p.ty = ty;
          } else {
            s.p.tx = tx;
            s.p.ty = ty;
            
            // Handle reappearance after inactivity
            if (!s.p.a && performance.now() - s.p.lt > 100) {
              s.p.x = tx;
              s.p.y = ty;
            }
          }
          
          // Initialize actual position if first interaction
          if (s.p.x === null) {
            s.p.x = s.p.tx;
            s.p.y = s.p.ty;
          }
          
          s.p.a = true;
          s.p.lt = performance.now();
        };
        
        const pd = e => {
          up(e);
          s.a = true;
          s.pt = performance.now();
          if (e.type === 'touchstart') e.preventDefault();
        };
        
        const pu = e => {
          s.a = false;
          s.pt = performance.now();
          
          if (e.type === 'touchend' || e.type === 'touchcancel') {
            pl(true);
          }
        };
        
        const pl = (immediate = false) => {
          s.p.a = false;
          s.p.lit = performance.now();
          s.a = false;
          s.pt = s.p.lit;
          
          if (immediate) {
            s.p.tx = s.p.x = null;
            s.p.ty = s.p.y = null;
          }
        };
        
        // Event listeners
        window.addEventListener('resize', ur, { passive: true });
        svg.addEventListener('pointermove', up, { passive: true });
        svg.addEventListener('pointerenter', up, { passive: true });
        svg.addEventListener('pointerleave', pl, { passive: true });
        svg.addEventListener('pointerdown', pd, { passive: false });
        window.addEventListener('pointerup', pu, { passive: true });
        window.addEventListener('pointercancel', pu, { passive: true });
        
        // Start animation
        requestAnimationFrame(animate);
        
        // Setup auto-cleanup
        if (document.currentScript?.parentNode) {
          const cleanup = () => {
            window.removeEventListener('resize', ur);
            svg.removeEventListener('pointermove', up);
            svg.removeEventListener('pointerenter', up);
            svg.removeEventListener('pointerleave', pl);
            svg.removeEventListener('pointerdown', pd);
            window.removeEventListener('pointerup', pu);
            window.removeEventListener('pointercancel', pu);
            s.e = [];
          };
          
          new MutationObserver(mutations => {
            mutations.forEach(m => {
              if (m.removedNodes) {
                m.removedNodes.forEach(n => {
                  if (n === document.currentScript) {
                    cleanup();
                    observer.disconnect();
                  }
                });
              }
            });
          }).observe(document.currentScript.parentNode, { childList: true });
        }
      })();
      
      // Apply 3D transform to square elements
      function setTransform(el) {
        const ps = c.pz / (c.pz + el.cz);
        const px = c.cx + (el.cx - c.cx) * ps;
        const py = c.cy + (el.cy - c.cy) * ps;
        el.el.setAttribute('transform', 
          'translate('+ px+' '+ py+') rotate('+ el.cr+ ') scale(' + ps * el.cs + ')'
        );
      }
      
      // Calculate effects based on time and pulse
      function fx(el, t, pv) {
        const θ = (t % c.bd) / c.bd * Math.PI * 2 + el.o;
        const w = Math.sin(θ);
        const mf = 1 - Math.min(0.8, el.m);
        return {
          s: (1 + w * c.bs * mf) * (1 + pv * c.pa * Math.pow(mf, 2)),
          y: w * c.sz * c.bv * mf,
          p: pv * c.pa * Math.pow(mf, 2)
        };
      }
      
      // Animation loop
      function animate(time) {
        const dt = s.t ? time - s.t : 0;
        s.t = time;
        
        // Handle pointer inactivity
        if (!s.p.a && s.p.x !== null && time - s.p.lit > c.td) {
          s.p.x = s.p.y = s.p.tx = s.p.ty = null;
          s.e.forEach(el => { if (el.ractive) el.ractive = false; });
        }
        
        // Smooth pointer movement
        if (s.p.a && s.p.tx !== null && s.p.x !== null) {
          const dx = s.p.tx - s.p.x;
          const dy = s.p.ty - s.p.y;
          const dist = Math.sqrt(dx*dx + dy*dy);
          const factor = dist > c.sz * 0.3 ? 0.2 : c.pp;
          
          s.p.x += dx * factor;
          s.p.y += dy * factor;
        }
        
        // Update pulse
        s.pv = s.a ? 
          s.pv + (1 - s.pv) * Math.min(1, (time - s.pt) / c.pr) : 
          s.pv * Math.max(0, 1 - (time - s.pt) / c.pf);
        
        if (s.pv < 0.001) s.pv = 0;
        
        // Process elements
        const ps = Math.max(0.6, Math.min(1, c.sz / 150));
        const active = s.p.x !== null;
        
        s.e.forEach(el => {
          const sm = Math.min(dt / c.d, 1) * (el.t === 's' ? 0.3 : 0.1);
          const effect = fx(el, time, s.pv);
          
          if (el.t === 'c') { // Circle
            let tx = el.x;
            let ty = el.y + effect.y;
            
            if (active) {
              const dx = s.p.x - el.x;
              const dy = s.p.y - el.y;
              const d = Math.sqrt(dx * dx + dy * dy);
              const mv = c.sz * c.mv * (1 - Math.min(0.8, el.m));
              const m = Math.min(d, mv) * ps;
              
              if (d > 0.1) {
                tx += m * (dx / d);
                ty += m * (dy / d);
              }
            }
            
            // Smooth interpolation
            const cx_diff = tx - el.cx;
            const cy_diff = ty - el.cy;
            const c_dist = Math.sqrt(cx_diff*cx_diff + cy_diff*cy_diff);
            const c_factor = c_dist > 20 ? 0.15 : 0.1;
            
            el.cx += cx_diff * c_factor;
            el.cy += cy_diff * c_factor;
            
            // Apply changes
            el.el.setAttribute('cx', el.cx);
            el.el.setAttribute('cy', el.cy);
            el.el.setAttribute('r', el.r * effect.s);
            
            // Handle reflection
            if (el.mr && ${e}) {
              // Initialize reflection properties if needed
              const reflectX = c.sz - el.x;
              if (!el.rcx) {
                el.rcx = reflectX;
                el.rcy = el.y;
                el.rtx = reflectX;
                el.rty = el.y;
                el.ractive = false;
                el.rest = { x: reflectX, y: el.y };
              }
              
              // Update reflection activity state
              if (active && !el.ractive && s.p.lt - s.p.lit > 100) {
                el.ractive = true;
              } else if (!active && el.ractive && s.p.lit - s.p.lt > 50) {
                el.ractive = false;
              }
              
              // Set target position based on active state and mode
              if (!active || !${n}) {
                el.rtx = c.sz - el.cx;
                el.rty = el.cy;
              } else {
                const rdx = s.p.x - reflectX;
                const rdy = s.p.y - el.y;
                const rd = Math.sqrt(rdx * rdx + rdy * rdy);
                const rmv = c.sz * c.mv * (1 - Math.min(0.8, el.m));
                const rm = Math.min(rd, rmv) * ps;
                
                if (rd > 0.1) {
                  el.rtx = reflectX + rm * (rdx / rd);
                  el.rty = el.y + effect.y + rm * (rdy / rd);
                } else {
                  el.rtx = reflectX;
                  el.rty = el.y + effect.y;
                }
              }
              
              // Return to rest position when inactive
              if (!active && !s.p.x) {
                el.rtx = el.rest.x;
                el.rty = el.rest.y + effect.y;
              }
              
              // Smooth interpolation for reflection
              const rcx_diff = el.rtx - el.rcx;
              const rcy_diff = el.rty - el.rcy;
              const rc_dist = Math.sqrt(rcx_diff*rcx_diff + rcy_diff*rcy_diff);
              const rc_factor = rc_dist > 30 ? 0.05 : (rc_dist > 10 ? 0.07 : 0.09);
              
              el.rcx += rcx_diff * rc_factor;
              el.rcy += rcy_diff * rc_factor;
              
              // Apply reflection position
              el.mr.setAttribute('cx', el.rcx);
              el.mr.setAttribute('cy', el.rcy);
              el.mr.setAttribute('r', el.r * effect.s);
            }
          } else if (el.t === 's') { // Square
            let tx = el.x, ty = el.y + effect.y, tz = c.bz, tr = el.r, ts = 1;
            
            if (active) {
              const dx = s.p.x - el.x;
              const dy = s.p.y - el.y;
              const d = Math.sqrt(dx * dx + dy * dy);
              const mf = 1 - Math.min(0.8, el.m);
              const mv = c.sz * 0.1 * mf;
              const m = Math.min(d, mv) * ps;
              
              if (d > 0.1) {
                tx += m * (dx / d);
                ty += m * (dy / d);
              }
              
              // Apply depth & scale changes
              const n = Math.min(d / c.pr, 1);
              const zf = 1 - n * n;
              tz -= c.bz * zf * mf;
              ts += zf * 0.3 * mf;
              tr += Math.sin(d / 100) * 5 * mf;
            } else {
              // Idle rotation
              tr += Math.sin(time / 1500 + el.o) * 3 * (1 - Math.min(0.8, el.m));
            }
            
            // Apply pulse to Z depth
            tz -= effect.p * c.bz * 0.7 * (1 - Math.min(0.8, el.m));
            ts *= effect.s;
            
            // Smoothing based on distance
            const sx_diff = tx - el.cx;
            const sy_diff = ty - el.cy;
            const s_dist = Math.sqrt(sx_diff*sx_diff + sy_diff*sy_diff);
            const s_factor = Math.min(1, s_dist > 20 ? sm * 1.5 : sm);
            
            // Apply interpolation
            el.cx += sx_diff * s_factor;
            el.cy += sy_diff * s_factor;
            el.cz += (tz - el.cz) * sm;
            el.cr += (tr - el.cr) * sm;
            el.cs += (ts - el.cs) * sm;
            
            setTransform(el);
          }
        });
        
        requestAnimationFrame(animate);
      }
    ]]><\/script>
  `}function He({pub:t,size:e=200,dark:n=!1,draw:o="circles",reflect:r=!0,round:l=!0,embed:c=!0,svg:a}={}){let{decoded:f,finals:i,averages:s}=Dt(t),d=`
      <linearGradient id="bg" x1="0" y1="0" x2="0" y2="1">
        <stop offset="0%" stop-color="hsla(0,0%,${(n?0:70)+i[0]*30}%)"/>
        <stop offset="100%" stop-color="hsla(0,0%,${(n?0:70)+i[1]*30}%)"/>
      </linearGradient>
    `,u=(g,$=!1)=>Q(g,14).map(v=>{if(v.length!==14)return"";let[b,A,O,tt,B,G,L,j,U,D,F,mt,Nt,gt]=v,N=e/8+O*e*(7/8),vt=`gradient-${b}-${A}-${$?"2":"1"}`,yt=b*e,xt=A*e,qt=a==="interactive"?`class="interactive-square" data-cx="${yt}" data-cy="${xt}" data-r="${N}" data-angle="${gt*180}" data-opacity="${(L+mt)/2}"`:"";return`
          <defs>
            <linearGradient id="${vt}" x1="${j}" y1="0" x2="${Nt}" y2="1">
              <stop offset="0%" stop-color="color(display-p3 ${tt} ${B} ${G} / ${L})"/>
              <stop offset="100%" stop-color="color(display-p3 ${U} ${D} ${F} / ${mt})"/>
            </linearGradient>
          </defs>
          <g ${qt} transform="translate(${yt} ${xt}) rotate(${gt*180})">
            <rect 
              x="${-N/2}" y="${-N/2}" 
              width="${N}" height="${N}"
              fill="url(#${vt})"
              style="${$?"mix-blend-mode:color-burn;":"filter:blur(20px);"}"
            />
          </g>
        `}).join(""),p=(g,$,v=!1)=>Q(g,7).map(b=>{if(b.length!==7)return"";let[A,O,tt,B,G,L,j]=b,U=e/2+A*e/2,D=O*e,F=tt*$;return`
          <circle 
            ${a==="interactive"?`class="interactive-circle" data-cx="${U}" data-cy="${D}" data-opacity="${j}"`:""}
            cx="${U}" cy="${D}" r="${F}"
            fill="color(display-p3 ${B} ${G} ${L} / ${j})"

            style="${v?"mix-blend-mode:multiply;":""}"
          />
          ${r?`
          <circle 
            cx="${e-U}" cy="${D}" r="${F}"
            fill="color(display-p3 ${B} ${G} ${L} / ${j})"

            style="${v?"mix-blend-mode:multiply;":""}"
          />`:""}
        `}).join(""),m=l?`
      <defs>
        <clipPath id="circle-mask">
          <circle cx="${e/2}" cy="${e/2}" r="${e/2}" />
        </clipPath>
      </defs>
    `:"",_=a==="interactive"?qe({size:e,reflect:r,finals:i,averages:s}):"",y=`
      <svg 
        width="${e}" height="${e}" 
        viewBox="0 0 ${e} ${e}" 
        xmlns="http://www.w3.org/2000/svg"
        style="overflow: visible;"
        >
        <defs>${d}</defs>
        ${m}
        <g ${l?'clip-path="url(#circle-mask)"':""}>
          <rect x="${-e}" width="${3*e}" y="${-e}" height="${3*e}" fill="url(#bg)"/>
          ${o==="squares"?`${u(f[0],!1)}
             ${u(f[1],!0)}`:`${p(f[0],.42*e)}
             ${p(f[1],.125*e,!0)}`}
        </g>
        ${_}
      </svg>
    `;if(a==="interactive")return`data:image/svg+xml,${encodeURIComponent(y.trim())}`;if(c){let g={pub:t};c&&(g.content=c),y=je(y,g)}return`data:image/svg+xml;base64,${typeof btoa=="function"?btoa(y):Buffer.from(y).toString("base64")}`}var V={};function Ke({pub:t,size:e=200,dark:n=!1,draw:o="circles",reflect:r=!0,round:l=!0,embed:c=!0,svg:a=!0,p3:f=!0}={}){let i=typeof window<"u"&&typeof document<"u";if(!Ce(t))return"";if(a||!i)return He({pub:t,size:e,dark:n,draw:o,reflect:r,round:l,embed:c,svg:a});let s=JSON.stringify(arguments[0]);if(V!=null&&V[s])return V[s];let d=De({pub:t,size:e,dark:n,draw:o,reflect:r,round:l,embed:c,p3:f});return V[s]=d,d}Ie();export{Ke as G};
