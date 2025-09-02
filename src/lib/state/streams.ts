
/**
 * Enhanced Gun subscription wrapper using ReadableStream for proper lifecycle management
 */
export class GunSubscriptionStream<T> {
	private reader: ReadableStreamDefaultReader<T> | null = null;
	private stream: ReadableStream<T> | null = null;
	private isActive = false;
	private gunRef: any;
	private streamId: string;
	private hasReceivedData = false;

	constructor(
		private gunPath: () => any,
		private streamType: string,
		private onData: (data: T) => void,
		private onError?: (error: any) => void
	) {
		this.streamId = `${streamType}_${Math.random().toString(36).substr(2, 9)}`;
	}

	/**
	 * Start the subscription stream
	 */
	async start(): Promise<void> {
		if (this.isActive) {
			console.warn(`[STREAM] ${this.streamType} stream already active`);
			return;
		}

		try {
			this.isActive = true;
			console.log(`[STREAM] Starting ${this.streamType} stream ${this.streamId}`);

			this.stream = new ReadableStream<T>({
				start: (controller) => {
					try {
						// Get the Gun reference
						this.gunRef = this.gunPath();

						if (!this.gunRef) {
							console.warn(
								`[STREAM] No Gun reference for ${this.streamType} stream ${this.streamId}`
							);
							// Don't complete the stream - keep it open to retry later
							return;
						}

						// Set up the Gun subscription
						this.gunRef.on((data: T) => {
							if (this.isActive) {
								this.hasReceivedData = true;
								if (data !== null && data !== undefined) {
									controller.enqueue(data);
								}
							}
						});

						console.log(`[STREAM] ${this.streamType} stream ${this.streamId} started successfully`);
					} catch (error) {
						console.error(`[STREAM] Error starting ${this.streamType} stream:`, error);
						controller.error(error);
						this.onError?.(error);
					}
				},
				cancel: () => {
					console.log(`[STREAM] Cancelling ${this.streamType} stream ${this.streamId}`);
					this.cleanup();
				}
			});

			// Get the reader and start processing
			this.reader = this.stream.getReader();
			this.processStream();
		} catch (error) {
			console.error(`[STREAM] Failed to start ${this.streamType} stream:`, error);
			this.cleanup();
			this.onError?.(error);
		}
	}

	/**
	 * Process the stream data
	 */
	private async processStream(): Promise<void> {
		if (!this.reader) return;

		try {
			while (this.isActive) {
				const { value, done } = await this.reader.read();

				if (done) {
					console.log(`[STREAM] ${this.streamType} stream ${this.streamId} completed`);
					break;
				}

				if (value && this.isActive) {
					this.onData(value);
				}
			}
		} catch (error) {
			if (this.isActive) {
				console.error(`[STREAM] Error in ${this.streamType} stream:`, error);
				this.onError?.(error);
			}
		} finally {
			// Only cleanup if we're not active anymore (manual stop) or if we had an error
			// Don't cleanup just because no data was received
			if (!this.isActive) {
				this.cleanup();
			}
		}
	}

	/**
	 * Stop the subscription stream
	 */
	stop(): void {
		if (!this.isActive) return;

		console.log(`[STREAM] Stopping ${this.streamType} stream ${this.streamId}`);
		this.isActive = false;
		this.cleanup();
	}

	/**
	 * Clean up resources
	 */
	private cleanup(): void {
		this.isActive = false;

		try {
			// Cancel the reader
			if (this.reader) {
				this.reader.cancel();
				this.reader = null;
			}

			// Clean up Gun subscription
			if (this.gunRef && typeof this.gunRef.off === 'function') {
				this.gunRef.off();
				this.gunRef = null;
			}

			this.stream = null;
			console.log(`[STREAM] Cleaned up ${this.streamType} stream ${this.streamId}`);
		} catch (error) {
			console.error(`[STREAM] Error during cleanup of ${this.streamType} stream:`, error);
		}
	}

	/**
	 * Check if stream is active
	 */
	get active(): boolean {
		return this.isActive;
	}
}

/**
 * Enhanced subscription manager with ReadableStream support and memoization
 */
export class StreamSubscriptionManager {
	private activeStreams = new Map<string, GunSubscriptionStream<any>>();
	private subscriptionType: string;
	private lastContributorsList: string[] = [];
	private isUpdating = false;

	constructor(subscriptionType: string) {
		this.subscriptionType = subscriptionType;
	}

	/**
	 * Create a new subscription stream only if it doesn't already exist
	 */
	async createStream<T>(
		contributorId: string,
		gunPath: () => any,
		streamType: string,
		onData: (data: T) => void,
		onError?: (error: any) => void
	): Promise<void> {
		const streamKey = `${contributorId}_${streamType}`;

		// Check if stream already exists and is active
		const existingStream = this.activeStreams.get(streamKey);
		if (existingStream && existingStream.active) {
			/*console.log(
				`[STREAM-MANAGER] Stream ${streamKey} already exists and is active, skipping creation`
			);*/
			return;
		}

		// Only stop existing stream if it exists but is not active
		if (existingStream) {
			//console.log(`[STREAM-MANAGER] Stopping inactive stream for ${streamKey}`);
			existingStream.stop();
			this.activeStreams.delete(streamKey);
		}

		// Create new stream
		const stream = new GunSubscriptionStream(
			gunPath,
			`${this.subscriptionType}_${streamType}`,
			onData,
			onError
		);

		this.activeStreams.set(streamKey, stream);

		try {
			await stream.start();
			//console.log(`[STREAM-MANAGER] Created stream for ${streamKey}`);
		} catch (error) {
			//console.error(`[STREAM-MANAGER] Failed to create stream for ${streamKey}:`, error);
			this.activeStreams.delete(streamKey);
			throw error;
		}
	}

	/**
	 * Stop a specific stream
	 */
	stopStream(contributorId: string, streamType: string): void {
		const streamKey = `${contributorId}_${streamType}`;
		const stream = this.activeStreams.get(streamKey);

		if (stream) {
			//console.log(`[STREAM-MANAGER] Stopping stream for ${streamKey}`);
			stream.stop();
			this.activeStreams.delete(streamKey);
		}
	}

	/**
	 * Stop all streams for a contributor
	 */
	stopContributorStreams(contributorId: string): void {
		const keysToRemove: string[] = [];

		for (const [streamKey, stream] of this.activeStreams.entries()) {
			if (streamKey.startsWith(`${contributorId}_`)) {
				//console.log(`[STREAM-MANAGER] Stopping contributor stream: ${streamKey}`);
				stream.stop();
				keysToRemove.push(streamKey);
			}
		}

		keysToRemove.forEach((key) => this.activeStreams.delete(key));
	}

	/**
	 * Stop all streams
	 */
	stopAllStreams(): void {
		//console.log(`[STREAM-MANAGER] Stopping all ${this.subscriptionType} streams`);

		for (const [streamKey, stream] of this.activeStreams.entries()) {
			stream.stop();
		}

		this.activeStreams.clear();
		this.lastContributorsList = [];
	}

	/**
	 * Check if contributor lists are equal
	 */
	private arraysEqual(a: string[], b: string[]): boolean {
		if (a.length !== b.length) return false;
		const sortedA = [...a].sort();
		const sortedB = [...b].sort();
		return sortedA.every((val, i) => val === sortedB[i]);
	}

	/**
	 * Update subscriptions using delta-based approach with memoization
	 */
	async updateSubscriptions(
		newContributors: string[],
		createStreamFn: (contributorId: string) => Promise<void>
	): Promise<void> {
		// Prevent concurrent updates
		if (this.isUpdating) {
			//console.log(`[STREAM-MANAGER] ${this.subscriptionType} update already in progress, skipping`);
			return;
		}

		// Check if contributors list has actually changed
		if (this.arraysEqual(newContributors, this.lastContributorsList)) {
			/*console.log(
				`[STREAM-MANAGER] ${this.subscriptionType} contributors unchanged, skipping update`
			);*/
			return;
		}

		this.isUpdating = true;

		try {
			if (!newContributors.length) {
				/*console.log(
					`[STREAM-MANAGER] No ${this.subscriptionType} contributors, stopping all streams`
				);*/
				this.stopAllStreams();
				return;
			}

			/*console.log(
				`[STREAM-MANAGER] Updating ${this.subscriptionType} subscriptions for ${newContributors.length} contributors`
			);*/

			// Calculate current contributors from active streams
			const currentContributors = new Set<string>();
			for (const streamKey of this.activeStreams.keys()) {
				const contributorId = streamKey.split('_')[0];
				currentContributors.add(contributorId);
			}

			const newContributorSet = new Set(newContributors);
			const toAdd = newContributors.filter((id) => !currentContributors.has(id));
			const toRemove = Array.from(currentContributors).filter((id) => !newContributorSet.has(id));

			// Remove old streams for contributors no longer in the list
			for (const contributorId of toRemove) {
				//console.log(`[STREAM-MANAGER] Removing streams for contributor: ${contributorId}`);
				this.stopContributorStreams(contributorId);
			}

			// Add new streams for new contributors
			for (const contributorId of toAdd) {
				try {
					//console.log(`[STREAM-MANAGER] Adding streams for contributor: ${contributorId}`);
					await createStreamFn(contributorId);
				} catch (error) {
					/*console.error(
						`[STREAM-MANAGER] Failed to create streams for contributor ${contributorId}:`,
						error
					);*/
				}
			}

			// Update last contributors list
			this.lastContributorsList = [...newContributors];

			/*console.log(
				`[STREAM-MANAGER] ${this.subscriptionType} streams: +${toAdd.length} -${toRemove.length} (total: ${this.activeStreams.size})`
			);*/
		} finally {
			this.isUpdating = false;
		}
	}

	/**
	 * Get stream count for debugging
	 */
	get streamCount(): number {
		return this.activeStreams.size;
	}

	/**
	 * Get active stream keys for debugging
	 */
	get activeStreamKeys(): string[] {
		return Array.from(this.activeStreams.keys());
	}
}