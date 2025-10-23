
Security, Encryption, & Authorization - SEA:

SEA is split into two parts, the gun.user() chain and Gun.SEA utility. This page focuses on documentation for the utility library.

SEA is an easy API for the cryptographic primitives explained in the 1min animated explainer cartoon series, that wraps painful ones like the browser native WebCrypto API. We hope to have it swappable with WASM libsodium and/or local proxies to Electron/NodeJS or browser extensions.

Quickstart 
<script src="https://cdn.jsdelivr.net/npm/gun/gun.js"></script>
<script src="https://cdn.jsdelivr.net/npm/gun/sea.js"></script>
<script>
// var Gun = require('gun'); // in NodeJS 
// require('gun/sea');
var SEA = Gun.SEA;
;(async () => {
var pair = await SEA.pair();
var enc = await SEA.encrypt('hello self', pair);
var data = await SEA.sign(enc, pair);
console.log(data);
var msg = await SEA.verify(data, pair.pub);
var dec = await SEA.decrypt(msg, pair);
var proof = await SEA.work(dec, pair);
var check = await SEA.work('hello self', pair);
console.log(dec);
console.log(proof === check);
// now let's share private data with someone:
var alice = await SEA.pair();
var bob = await SEA.pair();
var enc = await SEA.encrypt('shared data', await SEA.secret(bob.epub, alice));
await SEA.decrypt(enc, await SEA.secret(alice.epub, bob));
// `.secret` is Elliptic-curve Diffie–Hellman
// Bob allows Alice to write to part of his graph, he creates a certificate for Alice
var certificate = await SEA.certify(alice.pub, ["^AliceOnly.*"], bob)
// Alice logs in 
const gun = Gun();
await gun.user().auth(alice);
// and uses the certificate
await gun.get('~'+bob.pub).get('AliceOnly').get('do-not-tell-anyone').put(enc, null, {opt: {cert: certificate}})
await gun.get('~'+bob.pub).get('AliceOnly').get('do-not-tell-anyone').once(console.log) // return 'enc'
})();
</script>
Return 
By default, SEA requires all shims to be compatible with callbacks so it can work in different javascript environments.

However, for the WebCrypto shim, it also supports an optional async await API because any browser that has WebCrypto is also going to have promise support.

As a result, and for sake of simplicity, documentation will use this for readability. Keep in mind that:

const data = await SEA.sign(a, b)
Is swappable for the official API:

SEA.sign(a, b, function(data){

});
Errors 
Unfortunately, the beauty of await is often ruined with a try catch pyramid of doom in practice. To get around this, errors get passed forward. However, passing errors forward with standard NodeJS callback style is awkward:

const [err, data] = await SEA.sign(a, b) // awkward
if(err){ ... }
So we have opted for a cleaner approach:

data = await SEA.util(a, b)
if(!data){ ... } // undefined === data
Note: Because SEA runs in production on dApps with millions of users, by default SEA will not throw errors (we have had developers complain it was crashing their networks). If you want SEA to throw while in development, turn SEA.throw = true on, but please do not use this in production.

If data exactly equal to undefined then something went wrong. No matter what.

Note: Careful not to confuse it with 0 or other falsy values

So then where is the error?

You might not like this, but it makes sense - the last error will be in an always easily accessible location, without worry about scope or context, which makes debugging happen faster:

console.log(SEA.err) // last known error
This is a good thing. And it works the same with the official callback style:

SEA.util(a, b, function(data){
  if(!data){ return console.log(SEA.err) } // undefined === data is safer
});
Work 
This gives you a Proof of Work (POW) / Hashing of Data

SEA.work(data, pair, callback, opt)
Parameters 
data 
The data to be hashed, work to be performed on.

pair (salt) [optional] 
You can pass pair of keys to use as salt. Salt will prevent others to pre-compute the work, so using your public key is not a good idea. If it is not specified, it will be random, which ruins your chance of ever being able to re-derive the work deterministically (most apps will want to do this, see examples below).

callback [optional] 
function to executed upon execution of proof

- r 
returns a string - hash of data if successful / else returns undefined

opt [optional] 
Object { name: 'SHA-256' || 'PBKDF2' (default); encode: 'base64' (default) || 'utf8' || 'hex'; iterations: (iterations to use on subtle.deriveBits); [optional] salt: (salt to use); [optional] hash: (hash to use); [optional] length: (length to use) / default setting from deriveBits; [optional] }

Return Value 
returns a promise with a string - hash of data if successful / else returns undefined

Example 
The user system uses PoW to allow for truly decentralized (and even offline-first) username/password based login without risking an attacker being able to brute-force crack your password. You can use this to create a forgot password hint system, or a variety of things:

function forgot(username, A1, A2){
  // A1 and A2 are answers to security questions they made earlier.
  var scrambled = await gun.user().get('hint').then();
  var proof = await Gun.SEA.work(A1, A2);
  var hint = await Gun.SEA.decrypt(scrambled, proof);
  return hint; // your password hint!
}
Note: Make sure the security questions cannot have answers that are easily guessed - most banks use ones that are terrible, have finite or short answers or ones that can be found online or in public records. Be careful!

Without the Proof of Work, an attacker could easily guess millions of answers a second and crack the hint at most in a few years. With the Proof of Work, every guess has to go through an exponential amount of work, limiting them to only a few guesses per second, making it possibly take 100+ years to crack.

The default cryptographic primitive that we chose for Proof of Work is PBKDF2, since our primary use case is for extending passwords.

Pair 
This generates a cryptographically secure public/private key pair - be careful not to leak the private keys!

Note: API subject to change we may change the parameters to accept data and work, in addition to generation.

You will need this for most of SEA's API, see those method's examples.

The default cryptographic primitives for the asymmetric keys are ECDSA for signing and ECDH for encryption.

SEA.pair(cb, opt)
Parameters 
Callback 
- r 
returns Object { pub: (public key); priv: (private key); epub: (public key for encryption); epriv: (private key for encryption); }

Return Value 
returns Object { pub: (public key); priv: (private key); epub: (public key for encryption); epriv: (private key for encryption); }

sign 
message = await SEA.sign(data, pair)
Adds a signature to a message, for data that you want to prevent attackers tampering with.

data is the content that you want to prove is authorized.
pair is from .pair.
Example 
The user system uses this to make it so that only you can write to your own data, and all other write attempts are rejected from being synchronized. For example:

var other = await SEA.pair();
var msg = await SEA.sign("I wrote this message! You did not.", pair);
console.log(await SEA.verify(msg, other.pub)); // false
The default cryptographic primitive signs a SHA256 fingerprint of the data.

certify 
THIS IS AN EARLY EXPERIMENTAL COMMUNITY SUPPORTED METHOD THAT MAY CHANGE API BEHAVIOR WITHOUT WARNING IN ANY FUTURE VERSION.

Moved here: SEA.certify

verify 
data = await SEA.verify(message, pair.pub)
Gets the data if and only if the message can be verified as coming from the person you expect.

message is what comes from .sign.
pair from .pair or its public key text (pair.pub).
Example 
The user system uses this to make it so that only you can write to your own data, and all other write attempts are rejected from being synchronized. For example:

var data = "I wrote this message! You did not.";
var msg = await SEA.sign(data, pair);
console.log(await SEA.verify(msg, pair.pub)); // true
Note: You must be careful to use a public key that you already trust, not one that just anybody could send to you (an attacker would try to send you a fake public key).

The default cryptographic primitive verifies a SHA256 fingerprint of the data.

encrypt 
message = await SEA.encrypt(data, pair) // pair.epriv will be used as a passphrase
Takes some data that you want to keep secret and encrypts it so nobody else can read it.

data is the content that you want to encrypt.
pair from .pair or a passphrase you want to use as a cipher to encrypt with.
Example 
var msg = await SEA.encrypt("Please do not tell this to anybody", pair);
console.log(await SEA.decrypt(msg, pair)); // true
Example - public key 
Assume that user1 wants to exchange a secret message with user2.

let user1 = await SEA.pair()
let user2 = await SEA.pair()

// user1 prepares a message
let passphrase = await SEA.secret(user2.epub, user1)
let msg = await SEA.encrypt("Please do not tell this to anybody", passphrase);

// user2 received the message and wants to decrypt it
let pass = await SEA.secret(user1.epub, user2)
console.log(await SEA.decrypt(msg, pass)); // true
decrypt 
data = await SEA.decrypt(message, pair)
Read the secret data, if and only if you are allowed to.

message is what comes from .encrypt.
pair from .pair or the passphrase to decypher the message.
Example 
var msg = await SEA.encrypt("Please do not tell this to anybody", 'secret passphrase');
console.log(await SEA.decrypt(msg, 'passphrase secret')); // false
Troubleshooting 
ReferenceError: CryptoKey is not defined 
In some scenarios (Node.js and browser), the following code result in an error:

const Gun = require('gun');
require('gun/sea');

let gun = Gun();
let user = gun.user();

user.create('alice', 'unsafepassword', ack => {
    // Causes ReferenceError: CryptoKey is not defined.
})
When this happens and you inspect Gun, you will find that Gun.SEA is undefined. A temporary workaround for this is to set it manually:

const Gun = require('gun');
const SEA = require('gun/sea');
if (typeof Gun.SEA === 'undefined') {
    Gun.SEA = SEA;
}