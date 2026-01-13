/**
 * Generate a deterministic color from an H3 index using hash-based HSL
 */
export function h3ToColor(h3Index: string): string {
    // Simple hash function
    let hash = 0;
    for (let i = 0; i < h3Index.length; i++) {
        hash = ((hash << 5) - hash) + h3Index.charCodeAt(i);
        hash = hash & hash; // Convert to 32bit integer
    }

    // Use hash to generate HSL values
    const hue = Math.abs(hash % 360);
    const saturation = 65 + (Math.abs(hash >> 8) % 20); // 65-85%
    const lightness = 50 + (Math.abs(hash >> 16) % 15); // 50-65%

    return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
}

/**
 * Generate a deterministic color with alpha channel
 */
export function h3ToColorWithAlpha(h3Index: string, alpha: number = 1): string {
    let hash = 0;
    for (let i = 0; i < h3Index.length; i++) {
        hash = ((hash << 5) - hash) + h3Index.charCodeAt(i);
        hash = hash & hash;
    }

    const hue = Math.abs(hash % 360);
    const saturation = 65 + (Math.abs(hash >> 8) % 20);
    const lightness = 50 + (Math.abs(hash >> 16) % 15);

    return `hsla(${hue}, ${saturation}%, ${lightness}%, ${alpha})`;
}
