
import { describe, it, expect } from 'vitest';
import { GeneralIPF } from '../../ipf-standard';

describe('Standard IPF (Faithful Port)', () => {
    it('Solving the 2D example from Python ipfn documentation', () => {
        // From python ipfn docstring:
        // m = [[8., 4., 6., 7.], [3., 6., 5., 2.], [9., 11., 3., 1.]]
        // xip = [20., 18., 22.] (Row Sums)
        // xpj = [18., 16., 12., 14.] (Column Sums)

        const m = [
            [8, 4, 6, 7],
            [3, 6, 5, 2],
            [9, 11, 3, 1]
        ];

        const xip = [20, 18, 22]; // Aggregate along dim 1 (columns) -> Result has dim 0 (rows)
        const xpj = [18, 16, 12, 14]; // Aggregate along dim 0 (rows) -> Result has dim 1 (columns)

        // In the python example:
        // dimensions = [[0], [1]]
        // When we aggregate along dimension 1 (columns), we are left with dimension 0 (rows). 
        // Wait, the API says "dimensions preserved".
        // If we preserve dim 0, we sum over dim 1.
        // So xip corresponds to preserving dim 0. Correct.

        const aggregates = [xip, xpj];
        const dimensions = [[0], [1]];

        const ipf = new GeneralIPF(m, aggregates, dimensions);
        const result = ipf.iteration();

        // Check Row Sums
        const rowSums = result.map((row: number[]) => row.reduce((a, b) => a + b, 0));
        console.log('Row Sums:', rowSums);
        expect(rowSums[0]).toBeCloseTo(20, 1);
        expect(rowSums[1]).toBeCloseTo(18, 1);
        expect(rowSums[2]).toBeCloseTo(22, 1);

        // Check Column Sums
        const colSums = [0, 0, 0, 0];
        result.forEach((row: number[]) => {
            row.forEach((val, j) => colSums[j] += val);
        });
        console.log('Col Sums:', colSums);
        expect(colSums[0]).toBeCloseTo(18, 1);
        expect(colSums[1]).toBeCloseTo(16, 1);
        expect(colSums[2]).toBeCloseTo(12, 1);
        expect(colSums[3]).toBeCloseTo(14, 1);
    });

    it('Fails gracefully on zero-sum constraints (handling zero division)', () => {
        // If an aggregate is 0, the slice should become 0.
        const m = [[10, 10], [10, 10]]; // Sum 40
        const rowSums = [0, 0];
        const colSums = [0, 0];

        const ipf = new GeneralIPF(m, [rowSums, colSums], [[0], [1]]);
        const result = ipf.iteration();

        expect(result[0][0]).toBe(0);
        expect(result[1][1]).toBe(0);
    });
});
