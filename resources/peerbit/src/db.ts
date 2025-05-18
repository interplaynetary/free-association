import { Documents } from "@peerbit/document";
import { Program } from "@peerbit/program";
import { field, variant } from "@dao-xyz/borsh";
import { sha256Sync } from "@peerbit/crypto";
import { v4 as uuid } from "uuid";
import { Peerbit } from "peerbit";
import { webSockets } from "@libp2p/websockets";
import { circuitRelayTransport } from "@libp2p/circuit-relay-v2";
import * as filters from "@libp2p/websockets/filters";

export class SimpleDocument {
    @field({ type: "string" })
    id: string;

    @field({ type: "string" })
    content: string;

    constructor(properties: { content: string }) {
        this.id = uuid();
        this.content = properties.content;
    }
}

@variant("example-store")
export class ExampleStore extends Program {
    @field({ type: Documents })
    documents: Documents<SimpleDocument>;

    constructor() {
        super();
        this.documents = new Documents({
            id: sha256Sync(new TextEncoder().encode("example-store")),
        });
    }

    async open(args?: any): Promise<void> {
        await this.documents.open({
            type: SimpleDocument,
        });
    }
}

export const createClient = async (localNetwork = false) => {
    const client = await Peerbit.create({
        //  directory:   "./test", for persistance
        libp2p: {
            addresses: {
                // TMP disable because flaky behaviour with libp2p 1.8.1
                // re-enable when https://github.com/dao-xyz/peerbit/issues/302 closed
                listen: [
                    /* "/webrtc" */
                ],
            },
            connectionGater: localNetwork
                ? {
                      denyDialMultiaddr: () => {
                          // by default libp2p refuse to dial local addresses from the browser since they
                          // are usually sent by remote peers broadcasting undialable multiaddrs but
                          // here we are explicitly connecting to a local node so do not deny dialing
                          // any discovered address
                          return false;
                      },
                  }
                : undefined,
            transports: [
                webSockets({
                    filter: filters.all,
                }),
                circuitRelayTransport({
                    discoverRelays: 1,
                }),
                // TMP disable because flaky behaviour with libp2p 1.8.1
                // re-enable when https://github.com/dao-xyz/peerbit/issues/302 closed
                /*    webRTC(), */
            ],
        },
    });

    // for online apps
    if (localNetwork) {
        // will dial a local server
        // see https://peerbit.org/#/modules/deploy/server/?id=testing-locally
        // to get more info how to launch one
        await client.dial(
            "/ip4/127.0.0.1/tcp/8002/ws/p2p/" +
                (await (await fetch("http://localhost:8082/peer/id")).text())
        );
    } else {
        // will dial public relay servers
        await client.bootstrap();
    }

    return client;
};
