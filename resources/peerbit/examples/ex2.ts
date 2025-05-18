import { field, variant } from "@dao-xyz/borsh";
import { Program } from "@peerbit/program";
import { Documents, PutOperation, DeleteOperation } from "@peerbit/document";
import { v4 as uuid } from "uuid";
import { PublicSignKey, sha256Sync } from "@peerbit/crypto";
import { randomBytes } from "@peerbit/crypto";
import { concat } from "uint8arrays";
import { ReplicationOptions } from "@peerbit/shared-log";

@variant(0) // for versioning purposes, we can do @variant(1) when we create a new post type version
export class Post {
    @field({ type: "string" })
    id: string;

    @field({ type: PublicSignKey })
    from: PublicSignKey;

    @field({ type: "string" })
    message: string;

    constructor(properties: { from: PublicSignKey; message: string }) {
        this.id = uuid();
        this.from = properties.from;
        this.message = properties.message;
    }
}
type Args = { replicate?: ReplicationOptions };

@variant("room")
export class Room extends Program<Args> {
    @field({ type: "string" })
    name: string;

    @field({ type: Documents })
    messages: Documents<Post>;

    constructor(properties: { name: string; messages?: Documents<Post> }) {
        super();
        this.name = properties.name;
        this.messages =
            properties.messages ||
            new Documents({
                id: sha256Sync(
                    concat([
                        new TextEncoder().encode("room"),
                        new TextEncoder().encode(this.name),
                    ])
                ),
            });
    }

    get id() {
        return this.name;
    }

    // Setup lifecycle, will be invoked on 'open'
    async open(args?: Args): Promise<void> {
        await this.messages.open({
            type: Post,
            canPerform: async (props) => {
                if (props.type === "put") {
                    const post = props.value;
                    if (
                        !props.entry.signatures.find((x) =>
                            x.publicKey.equals(post!.from)
                        )
                    ) {
                        return false;
                    }
                    return true;
                } else if (props.type === "delete") {
                    const get = await this.messages.index.get(
                        props.operation.key
                    );
                    if (
                        !get ||
                        !props.entry.signatures.find((x) =>
                            x.publicKey.equals(get.from)
                        )
                    ) {
                        return false;
                    }
                    return true;
                }
                return false;
            },
            index: {
                canRead: async (identity) => {
                    return true; // Anyone can query
                },
            },
            replicate: args?.replicate,
        });
    }
}

class RoomIndexable {
    @field({ type: "string" })
    name: string;

    constructor(properties: { name: string }) {
        this.name = properties.name;
    }
}

@variant("lobby")
export class Lobby extends Program<Args> {
    @field({ type: Uint8Array })
    id: Uint8Array;

    @field({ type: Documents })
    rooms: Documents<Room, RoomIndexable>;

    constructor(properties: { id?: Uint8Array }) {
        super();
        this.id = properties.id || randomBytes(32);
        this.rooms = new Documents<Room, RoomIndexable>({ id: this.id });
    }

    // Setup lifecycle, will be invoked on 'open'
    async open(args?: Args): Promise<void> {
        await this.rooms.open({
            type: Room,

            canPerform: (entry) => {
                return Promise.resolve(true); // Anyone can create rooms
            },

            index: {
                idProperty: "name",
                type: RoomIndexable,
                canRead: (post, publicKey) => {
                    return Promise.resolve(true); // Anyone can search for rooms
                },
            },
            canOpen: (program) => {
                // Control whether someone can create a "room", which itself is a program with replication
                // Even if anyone could do "rooms.put(new Room())", that new entry has to be analyzed. And if it turns out that new entry represents a program
                // this means it should be handled in a special way (replication etc). This extra functionality needs requires peers to consider this additional security
                // boundary
                return Promise.resolve(false);
            },
            replicate: args?.replicate,
        });
    }
}
