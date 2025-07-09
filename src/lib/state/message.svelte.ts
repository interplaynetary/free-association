import { gun, user, userPub } from '$lib/state/gun.svelte';
import SEA from 'gun/sea';
import { get } from 'svelte/store';

// secrets would be given to us?
// We dont need writeable what we need is to be able to store the secrets in our private user-space only readable by the user (encrypted)
// use pubkey to decrypt our own user-space? (or does gun do it automatically)

var pair = user._.sea;

export function saveSecret(pubkey: string, secret: string) {
	// encrypt our own user-space
	user.get('secrets').get(pubkey).put(SEA.encrypt(secret, pair));
}

export function getSecret(pubkey: string) {
	// decrypt from our own user-space
	return user
		.get('secrets')
		.get(pubkey)
		.once((secret: string) => {
			return SEA.decrypt(secret, pair);
		});
}

interface Message {
	who: string;
	what: string;
	when: number;
	whopub?: string; // Match ChatMessage interface
	path?: string;
}

export function sendMessage(path: string, pubkey: string[], message: Message) {
	// pubKey -> messages
	user.get('messages').get(pubkey).set(message);
}

export function readMessages(fromPubKey: string, path: string, pubkeys: string[]) {
	// ourKey -> messages
	gun
		.get(`~${fromPubKey}`)
		.get('messages')
		.get(get(userPub))
		.map()
		.once((message: any) => {
			return SEA.decrypt(getSecret(fromPubKey), pair);
		});
}
