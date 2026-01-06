/**
 * Standard Iterative Proportional Fitting (IPF) Implementation
 * 
 * A faithful TypeScript port of the generic N-dimensional IPF algorithm 
 * from the python `ipfn` library.
 * 
 * See: https://github.com/hudson-and-thames/ipfn
 */

// Basic N-Dimensional Array type (nested arrays)
export type NDArray = any[];

// Metadata for flat-array simulation of ND-array (if we needed performance)
// But for "faithful" mirroring of python structure, we'll try to support
// indexing and iteration similar to numpy, but in pure TS.

/**
 * Iterative Proportional Fitting Class
 */
export class GeneralIPF {
    original: any; // NDArray or similar
    aggregates: any[][]; // Marginals
    dimensions: number[][]; // Which dimensions correspond to which aggregates
    max_iteration: number;
    convergence_rate: number;
    verbose: number;
    rate_tolerance: number;

    constructor(
        original: any,
        aggregates: any[][],
        dimensions: number[][],
        max_iteration: number = 500,
        convergence_rate: number = 1e-5,
        verbose: number = 0,
        rate_tolerance: number = 1e-8
    ) {
        this.original = original;
        this.aggregates = aggregates;
        this.dimensions = dimensions;
        this.max_iteration = max_iteration;
        this.convergence_rate = convergence_rate;
        this.verbose = verbose;
        this.rate_tolerance = rate_tolerance;
    }

    /**
     * Python `index_axis_elem` equivalent.
     * Generates a slice index (in numpy terms).
     * 
     * Since TS arrays don't support `m[idx]`, we have to simulate slicing.
     * 
     * @param dims Total dimensions
     * @param axes Dimensions being aggregated
     * @param elems Indices for those dimensions
     */
    static index_axis_elem(dims: number, axes: number[], elems: number[]): (number | null)[] {
        let inc_axis = 0;
        const idx: (number | null)[] = [];
        for (let dim = 0; dim < dims; dim++) {
            if (inc_axis < axes.length) {
                if (dim === axes[inc_axis]) {
                    idx.push(elems[inc_axis]);
                    inc_axis++;
                } else {
                    idx.push(null); // Equivalent to np.s_[:]
                }
            } else {
                idx.push(null);
            }
        }
        return idx;
    }

    /**
     * Main iteration loop
     */
    iteration(): any {
        let m = this.clone(this.original);
        let conv = Number.POSITIVE_INFINITY;
        let old_conv = Number.NEGATIVE_INFINITY;
        let i = 0;

        // In TS we don't naturally distinguish DataFrame vs NDArray types the same way
        // We will assume `m` is an N-Dimensional array structure.

        while (
            i <= this.max_iteration &&
            conv > this.convergence_rate &&
            (i <= this.max_iteration && Math.abs(conv - old_conv) > this.rate_tolerance)
        ) {
            old_conv = conv;
            const result = this.ipfn_np(m, this.aggregates, this.dimensions);
            m = result.matrix;
            conv = result.max_conv;
            i++;
        }

        if (this.verbose > 0) {
            console.log(`IPF converged: ${conv <= this.convergence_rate}`);
        }

        return m;
    }


    /**
     * Core Algorithm (Numpy equivalent logic)
     */
    ipfn_np(m: any, aggregates: any[][], dimensions: number[][]): { matrix: any, max_conv: number } {
        const steps = aggregates.length;
        const dim = this.getShape(m).length;

        // We need to keep versions of the table (matrix)
        // Python: tables = [m] ... append zeros

        // For faithful port, we iterate through aggregates (steps)
        let matrix = this.clone(m); // Current working matrix

        for (let inc = 0; inc < steps; inc++) {
            const dims_to_agg = dimensions[inc];
            const target_marginal = aggregates[inc];

            // 1. Calculate current marginal sums along the preserved dimensions
            // Python: table_current.sum(...) BUT specialized for the dimensions

            // We iterate over every cell in the target marginal
            // For each element in the target marginal, we look up the corresponding slice in Matrix
            // Sum that slice, compare to target, and scale.

            // Getting all coordinate combinations for the target marginal
            const marginal_shape = this.getShape(target_marginal);
            const coords = this.cartesianProduct(marginal_shape.map(s => Array.from({ length: s }, (_, k) => k)));

            for (const item of coords) {
                // `item` is the coordinate in the marginal, e.g. [0] or [1, 2]

                // Construct the slice index for the full matrix
                // idx has specific indices for agg dims, and null for others
                const idx = GeneralIPF.index_axis_elem(dim, dims_to_agg, item);

                // Get current sum of the slice
                const { sum: mijk, indices: sliceIndices } = this.getSliceSumAndIndices(matrix, idx);

                // Get target value
                let xijk = target_marginal;
                for (const coord of item) {
                    xijk = xijk[coord];
                }
                // xijk is now the number (target value)

                // Update Rule
                if (mijk === 0) {
                    // Python: table_update[idx] = table_current_slice (no change if sum is 0)
                    // We do nothing.
                } else {
                    const factor = xijk / mijk;
                    // Apply factor to every element in the slice
                    this.applyFactorToSlice(matrix, sliceIndices, factor);
                }
            }
        }

        // Check convergence
        let max_conv = 0;
        for (let inc = 0; inc < steps; inc++) {
            const dims_to_agg = dimensions[inc];
            const target_marginal = aggregates[inc];

            const marginal_shape = this.getShape(target_marginal);
            const coords = this.cartesianProduct(marginal_shape.map(s => Array.from({ length: s }, (_, k) => k)));

            for (const item of coords) {
                const idx = GeneralIPF.index_axis_elem(dim, dims_to_agg, item);
                const { sum: mijk } = this.getSliceSumAndIndices(matrix, idx);

                let ori_ijk = target_marginal;
                for (const coord of item) {
                    ori_ijk = ori_ijk[coord];
                }

                if (ori_ijk !== 0) {
                    const diff = Math.abs(mijk / ori_ijk - 1);
                    if (diff > max_conv) max_conv = diff;
                }
            }
        }

        return { matrix, max_conv };
    }


    // --- Helpers for ND Arrays in JS ---

    clone(obj: any): any {
        return JSON.parse(JSON.stringify(obj));
    }

    getShape(arr: any): number[] {
        const shape = [];
        let curr = arr;
        while (Array.isArray(curr)) {
            shape.push(curr.length);
            curr = curr[0];
        }
        return shape;
    }

    // Equivalent to itertools.product
    cartesianProduct(arrays: any[][]): any[][] {
        return arrays.reduce((acc, curr) =>
            acc.flatMap(d => curr.map(e => [...d, e])),
            [[]] as any[][]);
    }

    // Get sum of a slice defined by `idx` (where null means 'all')
    // Also returns the flat paths to the elements so we can update them
    getSliceSumAndIndices(matrix: any, idx: (number | null)[]): { sum: number, indices: number[][] } {
        let sum = 0;
        const indices: number[][] = [];

        // Recursive traversal matching the idx pattern
        const traverse = (sub: any, depth: number, currentPath: number[]) => {
            if (depth === idx.length) {
                // Leaf
                sum += (sub as number);
                indices.push([...currentPath]);
                return;
            }

            const reqIndex = idx[depth];
            if (reqIndex !== null) {
                // Fixed index
                if (sub[reqIndex] !== undefined) {
                    traverse(sub[reqIndex], depth + 1, [...currentPath, reqIndex]);
                }
            } else {
                // Slice (all indices)
                for (let i = 0; i < sub.length; i++) {
                    traverse(sub[i], depth + 1, [...currentPath, i]);
                }
            }
        };

        traverse(matrix, 0, []);
        return { sum, indices };
    }

    applyFactorToSlice(matrix: any, indices: number[][], factor: number) {
        for (const path of indices) {
            let target = matrix;
            for (let i = 0; i < path.length - 1; i++) {
                target = target[path[i]];
            }
            target[path[path.length - 1]] *= factor;
        }
    }
}
