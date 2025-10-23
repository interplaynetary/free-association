## SEA - Security, Encryption and Authorization

[SEA](https://github.com/amark/gun/wiki/SEA) is used to implement GunDB's [security model](https://github.com/amark/gun/wiki/Security) (see [Wiki](https://github.com/amark/gun/wiki/Security,-Authentication,-Authorization)). It has to be loaded separately

```
<script src="https://cdn.jsdelivr.net/npm/gun/gun.js"></script>
<script src="https://cdn.jsdelivr.net/npm/gun/sea.js"></script>
```

and may then be used to generate cryptographic key pairs, encrypt and sign data or create [certificates](https://github.com/amark/gun/wiki/SEA.certify). SEA works asynchronously, if you want to wait for results you should therefore wrap your code in an asynchronous IIFE

```
<script>
  const SEA = GUN.SEA

  ;(async () => {
    ...
  })()
</script>
```

By default, SEA methods do not throw any exceptions but simply return `undefined` if they encounter any failures. If you want SEA to throw, you have to explicitly configure

```
  SEA.throw = true
```

### Keypair Generation

`SEA.pair` generates two new pairs of cryptographic keys

```
  let KeyPair = await SEA.pair()
  console.log('KeyPair',KeyPair)
```

It returns an object with the following properties:

```
{
  epriv: 'DArC...CW5c',             // 43 char.s
  epub:  'smJU...0jeI.wSCl...HpG0', // 87 char.s
  priv:  '"grPs...phao"',           // 43 char.s
  pub:   '9Su...ho1A.GU--...l0Wo',  // 87 char.s
}
```

`epriv` and `priv` are the private parts of a key pair and should be kept safe and not shared, `epub` and `pub` are their public counterparts and may safely be published (in fact, they often _have_ to be published)

### Symmetric Encryption

`SEA.encrypt` can be used to encrypt a given text, and `SEA.decrypt` to decrypt it again. Both methods require the same cipher in form of a literal passphrase but also accept a key pair generated with `SEA.pair` from which they then take the `epriv` part

```
  let originalText  = 'Lorem ipsum dolor sit amet'
  let encryptedText = await SEA.encrypt(originalText, 't0ps3cr3t!')
  let decryptedText = await SEA.decrypt(encryptedText,'t0ps3cr3t!')

  console.log('originalText ',originalText)
  console.log('encryptedText',encryptedText)
  console.log('decryptedText',decryptedText)
  console.log(originalText === decryptedText)
```

And now with a key pair

```
  let KeyPair = await SEA.pair()

  let originalText  = 'Lorem ipsum dolor sit amet'
  let encryptedText = await SEA.encrypt(originalText, KeyPair)
  let decryptedText = await SEA.decrypt(encryptedText,KeyPair)

  console.log('originalText ',originalText)
  console.log('encryptedText',encryptedText)
  console.log('decryptedText',decryptedText)
  console.log(originalText === decryptedText)
```

Encrpyted text is a string of the form

`'SEA{"ct":"YK0o...p6Lp","iv":"6vYQWNafgnDxfN6SJtQa","s":"URgUSMPvEWj0"}'`

and can directly be written into a node property.

### Signing

In order to verify the origin of a given message, that text can be signed with `SEA.sign` (using the private part of a key pair) and verified with `SEA.verify` (using the public part of a key pair): The receiver of a signed message must therefore know the public key of its originator

```
  let KeyPair = await SEA.pair()

  let originalText = 'Lorem ipsum dolor sit amet'
  let signedText   = await SEA.sign(originalText,KeyPair)
  let acceptedText = await SEA.verify(signedText,KeyPair.pub)

  console.log('originalText',originalText)
  console.log('signedText  ',signedText)
  console.log('acceptedText',acceptedText)

  let brokenText   = signedText.replace(/Lorem/,'lorem')
  let rejectedText = await SEA.verify(brokenText,KeyPair.pub)

  console.log('brokenText  ',brokenText)
  console.log('rejectedText',rejectedText) // undefined
```

Signed text is a string of the form

`SEA{"m":"Lorem ipsum dolor sit amet","s":"LWqj...iw=="}`

and can directly be written into a node property.

If transmitted "untampered" `SEA.verify` will extract the original message from the signed text and return it - otherwise it will simply return `undefined`

### Proof-of-Work

In order to put some computational burden on a client and delay certain operations (e.g., to prevent flooding a database or hamper brute force attacks on passwords), a "proof-of-work" can be requested

```
  let Text = 'Lorem ipsum dolor sit amet' // could be a password
  let Salt = Math.random()

  let Proof = await SEA.work(Text,Salt)
  console.log('Proof',Proof)

  console.log('same text',
    Proof === await SEA.work('Lorem ipsum dolor sit amet',Salt)
    ? 'matches'
    : 'differs'
  )
  console.log('other text',
    Proof === await SEA.work('lorem ipsum dolor sit amet',Salt)
    ? 'matches'
    : 'differs'
  )
```

This computes a hash value for the given text using the given `Salt` value. `Salt` should be random (in order to prevent hackers from pre-computing hash values for multiple passwords) but does not have to be kept very secret.

A common use case for "proof-of-work" is the password-based authentication of users: instead of storing the passwords themselves (as they could become compromised if an attacker would get access to the password database) their proof-of-work hash should be stored (since attackers would now have to run compute-intensive brute force attacks on every single password - individually, if every password's `Salt` differs from any other). Upon login, the given password is hashed again (with the same `Salt` as before) and compared to the stored one: if equal, access is granted, otherwise rejected.

`SEA.work` has the signature `SEA.work(data, salt, callback, options)` and can be configured in multiple ways by providing an object with `options`:

```
  const options = {
    name:'PBKDF2', // or 'SHA-256'
    encode:'base64', // or 'utf8', 'hex'
    iterations:10000, // number of iterations
    salt:..., // salt value to use
    hash:..., // hash method to use
    length:... // ?
  }
```

Since the result of running `SEA.work` is a cryptographic hash (and looks like a really random number), it can also be used for other purposes where such a number should be deterministically derived from some input - but with some computational burden in order to limit the number of brute force attacks per second.

The [wiki](https://github.com/amark/gun/wiki/SEA) gives an example where a user's password is encrypted using a "proof-of-work" built from the answers of two security questions (which only the password owner should know). The encrypted password is then stored and decrypted upon request when the same security questions are correctly answered again.

### Asymmetric Encryption

While a "symmetric" encryption requires the same cipher to be known both on the encrypting and the decrypting side (and allows anybody with access to that cipher to encrypt and decrypt at will), "asymmetric" encryption does not: here, a message is encrypted with the sender's _private_ and the receiver's _public_ key and decrypted with the sender's _public_ and the receiver's _private_ key - effectively limiting communication to the given sender and receiver.

`SEA.secret` can be used for that purpose as it derives a (symmetric) encryption/decryption key from a given public and private key belonging to different participants

```
  let Alice = await SEA.pair()
  let Bob   = await SEA.pair()

/**** from Alice to Bob - only ****/

  let originalText  = 'Lorem ipsum dolor sit amet'
  let encryptedText = await SEA.encrypt(originalText, await SEA.secret(Bob.epub, Alice))
  let decryptedText = await SEA.decrypt(encryptedText,await SEA.secret(Alice.epub, Bob))

  console.log('originalText ',originalText)
  console.log('encryptedText',encryptedText)
  console.log('decryptedText',decryptedText)
  console.log(originalText === decryptedText)
```

If a message shall be published once for multiple users with different key pairs, an approach can be used that has formerly be introduced by [Pretty Good Privacy](https://en.wikipedia.org/wiki/Pretty_Good_Privacy):

- create an arbitrary (practically random) encryption key and (symmetrically) encrypt your message with it
- now asymmetrically encrypt the encryption key itself for every intended receiver
- then publish both the encrypted message and all encrypted encryption keys

The receivers now either have to

- decrypt the encryption key made for them personally or
- simply try to decrypt all given encryption keys until they succeed

before they can actually decrypt the message itself. The [Wiki](https://github.com/amark/gun/wiki/Snippets) contains an example for this use case.

### Certificates

(t.b.w, see [Wiki](https://github.com/amark/gun/wiki/SEA.certify))

# Simple encryption - 1 to 1

This an example of how you can share encrypted data between two people.
Summary:

- user1 generates a secret based on his private key and user2's pub key
- user1 encrypts a plain text message using the secret
- user2 generates the equivalent of the same secret using user2 private key and user1 pub key
- user2 decrypts the encrypted message using the secret

```javascript
///////////////////////////////////
// On my side - logged in as myself
///////////////////////////////////
var myPair = gun.user()._.sea;
// retrieve bob's user
const bob = gun.user(bobPublicKey);
// generate encryption secret using bob's epub and my pair
// this means only bob will be able to regenerate this secret with my pub key and his pair
const secret = await SEA.secret(bob.epub, myPair);
// encrypt the data using the secret
const encryptedData = await SEA.encrypt('private message for bob', secret);

////////////////////////////////////
// on Bob's side - logged in as Bob
///////////////////////////////////
const myPair = gun.user()._.sea;
// generate the secret - this will output the same secret generated by myself
// but this time we generate with bobs pair and my epub
const secret = await SEA.secret(myPair.epub, bob);
// just decrypt the data using the secret
const decryptedData = await SEA.decrypt(encryptedData, secret);
```

# Multiple people encryption

This example shows how to encrypt data that can be decrypted by multiple people, each of them having their own secret.

Summary:

- user1 comes up with a plain text encryption key (it can be any text string)
- user1 encrypts some sensitive data using the plain text encryption key
- user1 wants to allow user2 to decrypt the data so he is generating a secret based on user1 private key and user2 pub key
- user1 uses this secret to encrypt the encryption key and shares this secret with user2
- user2 generates a secret with user2 private key and user1 pub key
- user2 uses the secret to decrypt the encryption key which was shared with him by user1
- user2 uses this decrypted encryption key to decrypt the sensitive data that he wanted

```javascript
async () => {
	/////////////////////////////////////////////////////////////////
	// Instead of logging in with actual users, we are
	// going to generate SEA pairs which is basically the same thing
	/////////////////////////////////////////////////////////////////

	// User 1 encrypts one message
	const user1 = await SEA.pair();

	const plainMessage = 'Hello, how are you?';
	const encryptionKey = 'this is my encryption key which is a normal string';
	const encryptedMessage = await SEA.encrypt(plainMessage, encryptionKey);

	// User 2, 3 and 4 will receive the message and decrypt it
	const user2 = await SEA.pair();
	const user3 = await SEA.pair();
	const user4 = await SEA.pair();

	// Each user gets an encrypted encryption key. If you print them, they all different
	const encryptedEncryptionKeyUser2 = await SEA.encrypt(
		encryptionKey,
		await SEA.secret(user2.epub, user1)
	);
	const encryptedEncryptionKeyUser3 = await SEA.encrypt(
		encryptionKey,
		await SEA.secret(user3.epub, user1)
	);
	const encryptedEncryptionKeyUser4 = await SEA.encrypt(
		encryptionKey,
		await SEA.secret(user4.epub, user1)
	);

	// Each user decrypts his own encrypted encryption key
	// These three decrypted encryptions keys that we get are all the same
	const decryptedEncryptionKeyUser2 = await SEA.decrypt(
		encryptedEncryptionKeyUser2,
		await SEA.secret(user1.epub, user2)
	);
	const decryptedEncryptionKeyUser3 = await SEA.decrypt(
		encryptedEncryptionKeyUser3,
		await SEA.secret(user1.epub, user3)
	);
	const decryptedEncryptionKeyUser4 = await SEA.decrypt(
		encryptedEncryptionKeyUser4,
		await SEA.secret(user1.epub, user4)
	);

	// Each user decrypts the encrypted message using the decrypted encryption key
	const decryptedMessageUser2 = await SEA.decrypt(encryptedMessage, decryptedEncryptionKeyUser2);
	const decryptedMessageUser3 = await SEA.decrypt(encryptedMessage, decryptedEncryptionKeyUser3);
	const decryptedMessageUser4 = await SEA.decrypt(encryptedMessage, decryptedEncryptionKeyUser4);
};
```

# Saving Arrays in Gun

Gun does not allow saving arrays directly. Because of this, we have to turn arrays into indexed objects. Technically, they become objects but practically we can still use it as collections of items.

In order to do achieve this, we need two functions. The function `getIndexedObjectFromArray` converts Arrays into something that Gun likes. The second one, `getArrayFromIndexedObject` turns them back into plain Arrays.

```javascript
const getIndexedObjectFromArray = (arr) => {
	return arr.reduce((acc, item) => {
		return {
			...acc,
			[item.id]: item
		};
	}, {});
};

const getArrayFromIndexedObject = (indexedObj) => {
	return Object.values(indexedObj);
};
```

If you have this array which you want to save in Gun:

```javascript
const animals = [
	{ id: '1', name: 'Dog' },
	{ id: '2', name: 'Cat' }
];

const indexedAnimals = getIndexedObjectFromArray(animals);
/*
{
    "1": {
        "id": "1",
        "name": "Dog"
    },
    "2": {
        "id": "2",
        "name": "Cat"
    }
};
*/

// save to gun
gun.get('animals').put(indexedAnimals);

// retrieve from gun individual animals
gun.get('animals').get('1').on(/* do something */);

// retrieve from gun the entire collection as a whole
gun.get('animals').load((data) => {
	delete data._;
	const normalAnimals = getArrayFromIndexedObject(data);
	/*
  [
    {
        "id": "1",
        "name": "Dog"
    },
    {
        "id": "2",
        "name": "Cat"
    }
  ]
  */
});
```
