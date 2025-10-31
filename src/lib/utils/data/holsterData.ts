/**
 * Writes data at a specific path inside a holster user's account.
 * 
 * @param user - The authenticated holster user instance
 * @param path - Array of strings representing the path to write to (e.g., ["settings", "theme", "color"])
 * @param value - The value to write (string, number, object, boolean, or null)
 * @param callback - Optional callback function that receives error (if any) or null on success
 * 
 * @example
 * // Write a simple value
 * writeAtPath(user, ["settings", "theme"], "dark", (err) => {
 *   if (err) console.error(err);
 * });
 * 
 * @example
 * // Write nested object
 * writeAtPath(user, ["profile", "preferences"], { notifications: true }, console.log);
 * 
 * @example
 * // Write to root level
 * writeAtPath(user, ["username"], "john_doe");
 */
export function writeAtPath(
  user: any,
  path: string[],
  value: string | number | object | boolean | null,
  callback?: (err: string | null) => void
): void {
  if (!user) {
    const error = "User instance is required";
    if (callback) callback(error);
    else console.error(error);
    return;
  }

  if (!path || path.length === 0) {
    const error = "Path must be a non-empty array of strings";
    if (callback) callback(error);
    else console.error(error);
    return;
  }

  // Start with user.get() for the first path segment
  let chain = user.get(path[0]);

  // Chain .next() for each subsequent path segment
  for (let i = 1; i < path.length; i++) {
    chain = chain.next(path[i]);
  }

  // Finally, put the value
  chain.put(value, callback);
}

/**
 * Reads data from a specific path inside a holster user's account.
 * 
 * @param user - The authenticated holster user instance (or unauthenticated for public keys)
 * @param path - Array of strings or [publicKey, ...path] representing the path to read from
 * @param callback - Callback function that receives the data or null if not found
 * 
 * @example
 * // Read from authenticated user's account
 * readAtPath(user, ["settings", "theme"], (data) => {
 *   console.log(data);
 * });
 * 
 * @example
 * // Read from another user's public account
 * readAtPath(user, ["public-key", "profile", "username"], console.log);
 */
export function readAtPath(
  user: any,
  path: string[],
  callback: (data: any) => void
): void {
  if (!user) {
    console.error("User instance is required");
    if (callback) callback(null);
    return;
  }

  if (!path || path.length === 0) {
    console.error("Path must be a non-empty array of strings");
    if (callback) callback(null);
    return;
  }

  // Start with user.get() for the first path segment
  let chain = user.get(path[0]);

  // Chain .next() for each subsequent path segment
  for (let i = 1; i < path.length; i++) {
    chain = chain.next(path[i]);
  }

  // Execute the callback with the retrieved data
  chain.on(callback, true);
}

/**
 * Listens for changes at a specific path inside a holster user's account.
 * 
 * @param user - The authenticated holster user instance
 * @param path - Array of strings representing the path to listen to
 * @param callback - Callback function that receives the updated data
 * @param getInitial - If true, immediately returns current data as well as listening for changes
 * @returns A function to unsubscribe from the listener
 * 
 * @example
 * // Listen for changes and get initial value
 * const unsubscribe = listenAtPath(user, ["settings", "theme"], (data) => {
 *   console.log("Theme changed:", data);
 * }, true);
 * 
 * // Later, stop listening
 * unsubscribe();
 */
export function listenAtPath(
  user: any,
  path: string[],
  callback: (data: any) => void,
  getInitial: boolean = false
): () => void {
  if (!user) {
    console.error("User instance is required");
    return () => {};
  }

  if (!path || path.length === 0) {
    console.error("Path must be a non-empty array of strings");
    return () => {};
  }

  // Start with user.get() for the first path segment
  let chain = user.get(path[0]);

  // Chain .next() for each subsequent path segment
  for (let i = 1; i < path.length; i++) {
    chain = chain.next(path[i]);
  }

  // Set up the listener
  chain.on(callback, getInitial);

  // Return unsubscribe function
  return () => {
    let unsubChain = user.get(path[0]);
    for (let i = 1; i < path.length; i++) {
      unsubChain = unsubChain.next(path[i]);
    }
    unsubChain.off(callback);
  };
}

