import { field, variant } from "@dao-xyz/borsh";
import { Program } from "@peerbit/program";
import {
    Documents,
    PutOperation,
    DeleteOperation,
    IntegerCompare,
    SortDirection,
    SearchRequest,
    Sort,
    Compare,
    Query,
} from "@peerbit/document";
import { v4 as uuid } from "uuid";
import { PublicSignKey, sha256Sync } from "@peerbit/crypto";
import { concat } from "uint8arrays";
import { ReplicationOptions } from "@peerbit/shared-log";

const FROM = "from";
const MESSAGE = "message";
const TIMESTAMP = "timestamp";

@variant(0) // for versioning purposes, we can do @variant(1) when we create a new post type version
export class Post {
    @field({ type: "string" })
    id: string;

    @field({ type: PublicSignKey })
    [FROM]: PublicSignKey;

    @field({ type: "string" })
    [MESSAGE]: string;

    constructor(properties: { message: string; from: PublicSignKey }) {
        this.id = uuid();
        this.from = properties.from;
        this.message = properties.message;
    }
}

class IndexablePost {
    @field({ type: "string" })
    id: string;

    @field({ type: Uint8Array })
    [FROM]: Uint8Array;

    @field({ type: "string" })
    [MESSAGE]: string;

    @field({ type: "u64" })
    [TIMESTAMP]: bigint;

    constructor(post: Post, timestamp: bigint) {
        this.id = post.id;
        this[FROM] = post.from.bytes;
        this[MESSAGE] = post.message;
        this[TIMESTAMP] = timestamp;
    }
}

type Args = {
    replicate?: ReplicationOptions;
};

@variant("room")
export class Room extends Program<Args> {
    @field({ type: PublicSignKey })
    creator: PublicSignKey;

    @field({ type: Documents })
    messages: Documents<Post, IndexablePost>;

    constructor(properties: {
        creator: PublicSignKey;
        messages?: Documents<Post, IndexablePost>;
    }) {
        super();
        this.creator = properties.creator;
        this.messages =
            properties.messages ||
            new Documents<Post, IndexablePost>({
                id: sha256Sync(
                    concat([
                        new TextEncoder().encode("room"),
                        this.creator.bytes,
                    ])
                ),
            });
    }

    get id() {
        return this.creator.hashcode();
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
                type: IndexablePost,
                idProperty: "id",
                transform: (obj, context) => {
                    return new IndexablePost(obj, context.created);
                },
                canRead: async (document, publicKey) => {
                    return true; // Anyone can query
                },
            },
            replicate: args?.replicate,
        });
    }

    async getTimestamp(id: string) {
        const docs = await this.messages.index.getDetailed(id, {
            local: true,
        });
        return docs?.[0]?.results[0]?.context.created;
    }

    public async loadEarlier() {
        // get the earlist doc locally, query all docs earlier than this
        const firstIterator = await this.messages.index.iterate(
            new SearchRequest({
                query: [],
                sort: [
                    new Sort({
                        direction: SortDirection.ASC,
                        key: TIMESTAMP,
                    }),
                ],
            }),
            {
                local: true,
                remote: false,
            }
        );
        const earliestPost = (await firstIterator.next(1))[0];
        firstIterator.close();

        const query: Query[] = [];
        if (earliestPost) {
            const created = (
                await this.messages.index.getDetailed(earliestPost.id, {
                    local: true,
                })
            )?.[0].results[0]?.context.created;
            if (created != null) {
                query.push(
                    new IntegerCompare({
                        key: "timestamp",
                        compare: Compare.Less,
                        value: created,
                    })
                );
            }
        }
        const iterator = await this.messages.index.iterate(
            new SearchRequest({
                query,
                sort: [
                    new Sort({
                        direction: SortDirection.ASC,
                        key: TIMESTAMP,
                    }),
                ],
            }),
            {
                remote: {
                    replicate: true,
                },
                local: true,
            }
        );
        const next = await iterator.next(10);
        iterator.close();
        return next;
    }

    public async loadLater(than?: bigint) {
        // get the earlist doc locally, query all docs earlier than this

        const query: Query[] = [];

        if (than == null) {
            const lastIterator = await this.messages.index.iterate(
                new SearchRequest({
                    query: [],
                    sort: [
                        new Sort({
                            direction: SortDirection.DESC,
                            key: TIMESTAMP,
                        }),
                    ],
                }),
                {
                    local: true, // we only query locally too see what we don't have
                    remote: false,
                }
            );

            const latestPost = (await lastIterator.next(1))[0];
            lastIterator.close();

            if (latestPost) {
                const created = await this.getTimestamp(latestPost.id);
                if (created != null) {
                    query.push(
                        new IntegerCompare({
                            key: TIMESTAMP,
                            compare: Compare.Greater,
                            value: created,
                        })
                    );
                }
            }
        } else {
            query.push(
                new IntegerCompare({
                    key: TIMESTAMP,
                    compare: Compare.Greater,
                    value: than,
                })
            );
        }

        const iterator = await this.messages.index.iterate(
            new SearchRequest({
                query,
                sort: [
                    new Sort({
                        direction: SortDirection.ASC,
                        key: TIMESTAMP,
                    }),
                ],
            }),
            {
                remote: {
                    replicate: true,
                },
                local: true,
            }
        );
        const next = await iterator.next(10);
        iterator.close();
        return next;
    }
}
