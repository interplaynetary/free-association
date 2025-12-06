/**
 * WebRTC Transport Adapter
 * 
 * Provides WebRTC DataChannel-based transport for Cap'n Web RPC.
 * Enables true peer-to-peer connections without relay servers.
 * 
 * Features:
 * - Direct P2P connections
 * - NAT traversal (with STUN/TURN)
 * - Low latency
 * - Binary data support
 */

import type { Transport, TransportMessage, TransportOptions } from './types';

export class WebRTCTransport implements Transport {
  private peerConnection: RTCPeerConnection | null = null;
  private dataChannel: RTCDataChannel | null = null;
  private messageHandler: ((message: TransportMessage) => void) | null = null;
  private options: TransportOptions;
  private iceServers: RTCIceServer[];

  constructor(
    iceServers: RTCIceServer[] = [],
    options: TransportOptions = {}
  ) {
    this.iceServers = iceServers.length > 0 ? iceServers : [
      { urls: 'stun:stun.l.google.com:19302' } // Public STUN server
    ];
    this.options = options;
  }

  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        // Create peer connection
        this.peerConnection = new RTCPeerConnection({
          iceServers: this.iceServers
        });

        // Create data channel
        this.dataChannel = this.peerConnection.createDataChannel('lambda-calculus', {
          ordered: true,
          maxRetransmits: 3
        });

        // Setup data channel handlers
        this.dataChannel.onopen = () => {
          console.log('[WebRTC] Data channel open');
          resolve();
        };

        this.dataChannel.onmessage = (event) => {
          try {
            const message = JSON.parse(event.data) as TransportMessage;
            this.messageHandler?.(message);
          } catch (error) {
            console.error('[WebRTC] Failed to parse message:', error);
          }
        };

        this.dataChannel.onerror = (error) => {
          console.error('[WebRTC] Data channel error:', error);
        };

        this.dataChannel.onclose = () => {
          console.log('[WebRTC] Data channel closed');
        };

        // Setup peer connection handlers
        this.peerConnection.onicecandidate = (event) => {
          if (event.candidate) {
            console.log('[WebRTC] ICE candidate:', event.candidate);
            // In real implementation, send to signaling server
          }
        };

        this.peerConnection.onconnectionstatechange = () => {
          console.log('[WebRTC] Connection state:', this.peerConnection?.connectionState);
        };

      } catch (error) {
        reject(error);
      }
    });
  }

  async disconnect(): Promise<void> {
    if (this.dataChannel) {
      this.dataChannel.close();
      this.dataChannel = null;
    }

    if (this.peerConnection) {
      this.peerConnection.close();
      this.peerConnection = null;
    }

    console.log('[WebRTC] Disconnected');
  }

  async send(message: TransportMessage): Promise<void> {
    if (!this.isConnected()) {
      throw new Error('Data channel not open');
    }

    try {
      this.dataChannel!.send(JSON.stringify(message));
    } catch (error) {
      console.error('[WebRTC] Failed to send message:', error);
      throw error;
    }
  }

  onMessage(handler: (message: TransportMessage) => void): void {
    this.messageHandler = handler;
  }

  isConnected(): boolean {
    return (
      this.dataChannel !== null &&
      this.dataChannel.readyState === 'open'
    );
  }

  /**
   * Create offer for initiating connection
   * Must be sent to remote peer via signaling
   */
  async createOffer(): Promise<RTCSessionDescriptionInit> {
    if (!this.peerConnection) {
      throw new Error('Peer connection not initialized');
    }

    const offer = await this.peerConnection.createOffer();
    await this.peerConnection.setLocalDescription(offer);
    return offer;
  }

  /**
   * Handle offer from remote peer
   * Creates answer to be sent back via signaling
   */
  async handleOffer(offer: RTCSessionDescriptionInit): Promise<RTCSessionDescriptionInit> {
    if (!this.peerConnection) {
      throw new Error('Peer connection not initialized');
    }

    await this.peerConnection.setRemoteDescription(offer);
    const answer = await this.peerConnection.createAnswer();
    await this.peerConnection.setLocalDescription(answer);
    return answer;
  }

  /**
   * Handle answer from remote peer
   */
  async handleAnswer(answer: RTCSessionDescriptionInit): Promise<void> {
    if (!this.peerConnection) {
      throw new Error('Peer connection not initialized');
    }

    await this.peerConnection.setRemoteDescription(answer);
  }

  /**
   * Add ICE candidate received from remote peer
   */
  async addIceCandidate(candidate: RTCIceCandidateInit): Promise<void> {
    if (!this.peerConnection) {
      throw new Error('Peer connection not initialized');
    }

    await this.peerConnection.addIceCandidate(candidate);
  }

  /**
   * Get local ICE candidates
   * These must be sent to remote peer via signaling
   */
  getLocalIceCandidates(): Promise<RTCIceCandidate[]> {
    return new Promise((resolve) => {
      const candidates: RTCIceCandidate[] = [];
      
      if (!this.peerConnection) {
        resolve(candidates);
        return;
      }

      this.peerConnection.onicecandidate = (event) => {
        if (event.candidate) {
          candidates.push(event.candidate);
        } else {
          // ICE gathering complete
          resolve(candidates);
        }
      };
    });
  }
}

/**
 * Create WebRTC transport with default STUN servers
 */
export function createWebRTCTransport(
  customIceServers?: RTCIceServer[],
  options?: TransportOptions
): WebRTCTransport {
  return new WebRTCTransport(customIceServers, options);
}

/**
 * Simple signaling helper (for demo purposes)
 * In production, use a proper signaling server
 */
export class SimpleSignaling {
  private offers = new Map<string, RTCSessionDescriptionInit>();
  private answers = new Map<string, RTCSessionDescriptionInit>();
  private candidates = new Map<string, RTCIceCandidateInit[]>();

  // Store offer from peer A
  storeOffer(peerId: string, offer: RTCSessionDescriptionInit): void {
    this.offers.set(peerId, offer);
  }

  // Get offer for peer B
  getOffer(peerId: string): RTCSessionDescriptionInit | undefined {
    return this.offers.get(peerId);
  }

  // Store answer from peer B
  storeAnswer(peerId: string, answer: RTCSessionDescriptionInit): void {
    this.answers.set(peerId, answer);
  }

  // Get answer for peer A
  getAnswer(peerId: string): RTCSessionDescriptionInit | undefined {
    return this.answers.get(peerId);
  }

  // Store ICE candidates
  storeCandidates(peerId: string, candidates: RTCIceCandidateInit[]): void {
    this.candidates.set(peerId, candidates);
  }

  // Get ICE candidates
  getCandidates(peerId: string): RTCIceCandidateInit[] {
    return this.candidates.get(peerId) || [];
  }
}

