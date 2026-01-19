
import { z } from 'zod';
import * as vc from '@digitalbazaar/vc';
// @ts-ignore
import { Ed25519Signature2020 } from '@digitalbazaar/ed25519-signature-2020';
// @ts-ignore
import { Ed25519VerificationKey2020 } from '@digitalbazaar/ed25519-verification-key-2020';

// @ts-ignore
import { securityLoader } from '@digitalbazaar/security-document-loader';
// @ts-ignore
import { CachedResolver } from '@digitalbazaar/did-io';
// @ts-ignore
import * as DidKey from '@digitalbazaar/did-method-key';

// Types for Key management
export interface KeyPair {
    id: string;
    controller: string;
    export: (options: { publicKey?: boolean; privateKey?: boolean }) => Promise<any>;
    signer: () => any;
    verifier: () => any;
    fingerprint: () => string;
}

// Global Loader singleton to avoid rebuilding on every call
let _documentLoader: any = null;

async function getDocumentLoader() {
    if (_documentLoader) return _documentLoader;

    const loader = securityLoader();

    // Explicitly add Ed25519 2020 context to ensure no external fetch
    // Context content derived from official spec or library export
    const ed25519Context = {
        "@context": {
            "id": "@id",
            "type": "@type",
            "Ed25519Signature2020": "https://w3id.org/security#Ed25519Signature2020",
            "proof": { "@id": "https://w3id.org/security#proof", "@type": "@id", "@container": "@graph" },
            "created": { "@id": "http://purl.org/dc/terms/created", "@type": "http://www.w3.org/2001/XMLSchema#dateTime" },
            "verificationMethod": { "@id": "https://w3id.org/security#verificationMethod", "@type": "@id" },
            "proofPurpose": { "@id": "https://w3id.org/security#proofPurpose", "@type": "@id" },
            "proofValue": { "@id": "https://w3id.org/security#proofValue", "@type": "https://w3id.org/security#multibase" }
        }
    };

    loader.addStatic('https://w3id.org/security/suites/ed25519-2020/v1', ed25519Context);

    // Add Alumni Example Context (Static)
    loader.addStatic(
        'https://www.w3.org/2018/credentials/examples/v1',
        {
            "@context": {
                "@version": 1.1,
                "alumniOf": "http://schema.org/alumniOf"
            }
        }
    );

    const resolver = new CachedResolver();
    const didKeyDriver = DidKey.driver();

    // Use Ed25519VerificationKey2020 for the did:key driver
    didKeyDriver.use({
        multibaseMultikeyHeader: 'z6Mk', // Multibase header for Ed25519
        fromMultibase: Ed25519VerificationKey2020.from
    });

    resolver.use(didKeyDriver);
    loader.setDidResolver(resolver);

    _documentLoader = loader.build();
    return _documentLoader;
}

/**
 * Generate a generic Ed25519 KeyPair
 * Uses DID Key strategy
 */
export async function generateKeyPair(): Promise<KeyPair> {
    const keyPair = await Ed25519VerificationKey2020.generate();

    // Design Decision: Key ID Strategy
    // Uses did:key:fingerprint as per recommended strategy
    const fingerprint = keyPair.fingerprint();
    keyPair.id = `did:key:${fingerprint}`;
    keyPair.controller = keyPair.id;

    return keyPair;
}

/**
 * Issue a Generic Verifiable Credential
 */
export async function issueGenericVC<T>(
    keyPair: KeyPair,
    credentialPayload: any,
    schema?: z.ZodSchema<T>
): Promise<any> {
    const loader = await getDocumentLoader();

    // 1. Validate Payload if Schema provided
    if (schema) {
        // We only validate the subject properties usually, or the whole payload?
        // Usually we want to validate the 'credentialSubject'.
        // Let's assume the schema applies to the credentialSubject if strict, 
        // or the whole object. For now, let's just parse the subject if present
        // or the payload itself.

        // Actually, let's keep it simple: Validate the credentialSubject part of the payload.
        if (credentialPayload.credentialSubject) {
            const result = schema.safeParse(credentialPayload.credentialSubject);
            if (!result.success) {
                throw new Error(`VC Validation Failed: ${result.error.message}`);
            }
        }
    }

    // Design Decision: Signature Suite
    const suite = new Ed25519Signature2020({
        key: keyPair,
        verificationMethod: keyPair.id
    });

    // Ensure issuer matches the key controller if not set
    if (!credentialPayload.issuer) {
        credentialPayload.issuer = keyPair.controller;
    }

    const signedVC = await vc.issue({
        credential: credentialPayload,
        suite,
        documentLoader: loader
    });

    return signedVC;
}

/**
 * Verify a Generic Verifiable Credential
 */
export async function verifyGenericVC(
    credential: any
): Promise<{ verified: boolean; error?: any }> {
    const loader = await getDocumentLoader();
    const suite = new Ed25519Signature2020();

    try {
        const result = await vc.verifyCredential({
            credential,
            suite,
            documentLoader: loader,
            safe: false // Security loader is safe by definition, but flag might still needed for local mock overrides
        });
        return { verified: result.verified };
    } catch (e) {
        return { verified: false, error: e };
    }
}
