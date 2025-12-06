/**
 * Tests for Lambda Calculus Combinators
 */

import { describe, it, expect } from 'vitest';
import {
  // Basic combinators
  I, K, S, B, C,
  // Composition
  pipe, compose, compose2, pipe2,
  // Currying
  curry2, curry3, uncurry2,
  // Point-free
  fmap, filterP, fold, foldr,
  // Maybe monad
  Just, Nothing, maybe, mapMaybe, bindMaybe,
  // Reader monad
  ask, mapReader, bindReader, runReader,
  // State monad
  get, put, mapState, bindState, runState,
  // Logic
  and, or, not, implies,
  // Pair
  pair, fst, snd,
  // Fixed point
  Y, memoY,
  // Lazy
  delay, force, memo,
  // Church encodings
  churchTrue, churchFalse, churchIf, churchZero, churchSucc, churchToNum,
  // Utilities
  tap, times, until,
} from '../combinators';

describe('Lambda Calculus Combinators', () => {
  describe('Basic Combinators', () => {
    it('I combinator (identity)', () => {
      expect(I(5)).toBe(5);
      expect(I('hello')).toBe('hello');
      expect(I({ a: 1 })).toEqual({ a: 1 });
    });

    it('K combinator (constant)', () => {
      expect(K(5)(10)).toBe(5);
      expect(K('first')('second')).toBe('first');
      const kFive = K(5);
      expect(kFive(1)).toBe(5);
      expect(kFive(2)).toBe(5);
    });

    it('S combinator (substitution)', () => {
      const add = (x: number) => (y: number) => x + y;
      const double = (x: number) => x * 2;
      const result = S(add)(double)(5);
      expect(result).toBe(15); // 5 + (5 * 2)
    });

    it('B combinator (composition)', () => {
      const double = (x: number) => x * 2;
      const addOne = (x: number) => x + 1;
      const composed = B(double)(addOne);
      expect(composed(5)).toBe(12); // (5 + 1) * 2
    });

    it('C combinator (flip)', () => {
      const subtract = (x: number) => (y: number) => x - y;
      const flipped = C(subtract);
      expect(subtract(10)(3)).toBe(7);
      expect(flipped(3)(10)).toBe(7);
    });
  });

  describe('Function Composition', () => {
    it('pipe - left to right', () => {
      const double = (x: number) => x * 2;
      const addOne = (x: number) => x + 1;
      const square = (x: number) => x * x;
      
      const result = pipe<number>(double, addOne, square)(5);
      expect(result).toBe(121); // ((5 * 2) + 1)^2 = 11^2 = 121
    });

    it('compose - right to left', () => {
      const double = (x: number) => x * 2;
      const addOne = (x: number) => x + 1;
      const square = (x: number) => x * x;
      
      const result = compose<number>(square, addOne, double)(5);
      expect(result).toBe(121); // ((5 * 2) + 1)^2 = 11^2 = 121
    });

    it('compose2', () => {
      const double = (x: number) => x * 2;
      const addOne = (x: number) => x + 1;
      const composed = compose2(double, addOne);
      expect(composed(5)).toBe(12);
    });

    it('pipe2', () => {
      const double = (x: number) => x * 2;
      const addOne = (x: number) => x + 1;
      const piped = pipe2(double, addOne);
      expect(piped(5)).toBe(11);
    });
  });

  describe('Currying', () => {
    it('curry2', () => {
      const add = (a: number, b: number) => a + b;
      const curriedAdd = curry2(add);
      expect(curriedAdd(5)(3)).toBe(8);
      
      const addFive = curriedAdd(5);
      expect(addFive(3)).toBe(8);
      expect(addFive(10)).toBe(15);
    });

    it('curry3', () => {
      const addThree = (a: number, b: number, c: number) => a + b + c;
      const curried = curry3(addThree);
      expect(curried(1)(2)(3)).toBe(6);
      
      const addOneAnd = curried(1);
      const addOneTwoAnd = addOneAnd(2);
      expect(addOneTwoAnd(3)).toBe(6);
    });

    it('uncurry2', () => {
      const curriedAdd = (a: number) => (b: number) => a + b;
      const uncurried = uncurry2(curriedAdd);
      expect(uncurried(5, 3)).toBe(8);
    });
  });

  describe('Point-Free Utilities', () => {
    it('fmap', () => {
      const double = (x: number) => x * 2;
      const result = fmap(double)([1, 2, 3, 4]);
      expect(result).toEqual([2, 4, 6, 8]);
    });

    it('filterP', () => {
      const isEven = (x: number) => x % 2 === 0;
      const result = filterP(isEven)([1, 2, 3, 4, 5, 6]);
      expect(result).toEqual([2, 4, 6]);
    });

    it('fold', () => {
      const add = (acc: number, x: number) => acc + x;
      const result = fold(add)(0)([1, 2, 3, 4, 5]);
      expect(result).toBe(15);
    });

    it('foldr', () => {
      const subtract = (x: number, acc: number) => acc - x;
      const result = foldr(subtract)(0)([1, 2, 3]);
      expect(result).toBe(-6); // 0 - 3 - 2 - 1
    });
  });

  describe('Maybe Monad', () => {
    it('Just and Nothing', () => {
      const j = Just(5);
      const n = Nothing<number>();
      
      expect(j.tag).toBe('just');
      expect(n.tag).toBe('nothing');
    });

    it('maybe', () => {
      const double = (x: number) => x * 2;
      const getOrZero = maybe(0)(double);
      
      expect(getOrZero(Just(5))).toBe(10);
      expect(getOrZero(Nothing())).toBe(0);
    });

    it('mapMaybe', () => {
      const double = (x: number) => x * 2;
      const mapped = mapMaybe(double);
      
      expect(mapped(Just(5))).toEqual(Just(10));
      expect(mapped(Nothing())).toEqual(Nothing());
    });

    it('bindMaybe', () => {
      const safeSqrt = (x: number) => x >= 0 ? Just(Math.sqrt(x)) : Nothing<number>();
      const bound = bindMaybe(safeSqrt);
      
      expect(bound(Just(16))).toEqual(Just(4));
      expect(bound(Just(-1))).toEqual(Nothing());
      expect(bound(Nothing())).toEqual(Nothing());
    });
  });

  describe('Reader Monad', () => {
    interface Config {
      multiplier: number;
      offset: number;
    }

    it('ask', () => {
      const reader = ask<Config>();
      const config = { multiplier: 2, offset: 10 };
      expect(reader(config)).toEqual(config);
    });

    it('mapReader', () => {
      type Reader<R, A> = (r: R) => A;
      const getMultiplier: Reader<Config, number> = (cfg: Config) => cfg.multiplier;
      const double = (x: number) => x * 2;
      const reader = mapReader(double)(getMultiplier);
      
      expect(reader({ multiplier: 5, offset: 0 })).toBe(10);
    });

    it('bindReader', () => {
      const getMultiplier = (cfg: Config) => cfg.multiplier;
      const multiplyByConfig = (x: number) => (cfg: Config) => x * cfg.multiplier;
      
      const reader = bindReader(multiplyByConfig)(getMultiplier);
      expect(reader({ multiplier: 3, offset: 0 })).toBe(9);
    });

    it('runReader', () => {
      const reader = (cfg: Config) => cfg.multiplier * 2;
      const result = runReader({ multiplier: 5, offset: 0 })(reader);
      expect(result).toBe(10);
    });
  });

  describe('State Monad', () => {
    it('get and put', () => {
      const getState = get<number>();
      expect(getState(42)).toEqual([42, 42]);
      
      const putState = put(100);
      expect(putState(42)).toEqual([undefined, 100]);
    });

    it('mapState', () => {
      type State<S, A> = (s: S) => [A, S];
      const getState: State<number, number> = get<number>();
      const double = (x: number) => x * 2;
      const doubled = mapState(double)(getState);
      
      expect(doubled(5)).toEqual([10, 5]);
    });

    it('bindState', () => {
      const increment = (x: number) => put<number>(x + 1);
      const computation = bindState(increment)(get<number>());
      
      expect(computation(5)).toEqual([undefined, 6]);
    });

    it('runState', () => {
      const computation = get<number>();
      const result = runState(42)(computation);
      expect(result).toEqual([42, 42]);
    });
  });

  describe('Logic Combinators', () => {
    const isEven = (x: number) => x % 2 === 0;
    const isPositive = (x: number) => x > 0;

    it('and', () => {
      const isEvenAndPositive = and(isEven)(isPositive);
      expect(isEvenAndPositive(4)).toBe(true);
      expect(isEvenAndPositive(3)).toBe(false);
      expect(isEvenAndPositive(-2)).toBe(false);
    });

    it('or', () => {
      const isEvenOrPositive = or(isEven)(isPositive);
      expect(isEvenOrPositive(4)).toBe(true);
      expect(isEvenOrPositive(3)).toBe(true);
      expect(isEvenOrPositive(-1)).toBe(false);
    });

    it('not', () => {
      const isOdd = not(isEven);
      expect(isOdd(3)).toBe(true);
      expect(isOdd(4)).toBe(false);
    });

    it('implies', () => {
      const evenImpliesPositive = implies(isEven)(isPositive);
      expect(evenImpliesPositive(4)).toBe(true);
      expect(evenImpliesPositive(3)).toBe(true); // vacuously true
      expect(evenImpliesPositive(-2)).toBe(false);
    });
  });

  describe('Pair Operations', () => {
    it('pair, fst, snd', () => {
      const p = pair(5)('hello');
      expect(fst(p)).toBe(5);
      expect(snd(p)).toBe('hello');
    });

    it('pair with different types', () => {
      const p = pair({ a: 1 })([1, 2, 3]);
      expect(fst(p)).toEqual({ a: 1 });
      expect(snd(p)).toEqual([1, 2, 3]);
    });
  });

  describe('Fixed Point Combinator', () => {
    it('Y combinator for factorial', () => {
      const factorial = Y<number, number>(
        (rec) => (n) => n <= 1 ? 1 : n * rec(n - 1)
      );
      
      expect(factorial(5)).toBe(120);
      expect(factorial(0)).toBe(1);
      expect(factorial(1)).toBe(1);
    });

    it('memoY for Fibonacci', () => {
      const fibonacci = memoY<number, number>(
        (rec) => (n) => n <= 1 ? n : rec(n - 1) + rec(n - 2)
      );
      
      expect(fibonacci(10)).toBe(55);
      expect(fibonacci(20)).toBe(6765);
    });
  });

  describe('Lazy Evaluation', () => {
    it('delay and force', () => {
      let executed = false;
      const thunk = delay(() => {
        executed = true;
        return 42;
      });
      
      expect(executed).toBe(false);
      expect(force(thunk)).toBe(42);
      expect(executed).toBe(true);
    });

    it('memo - call once', () => {
      let callCount = 0;
      const memoized = memo(() => {
        callCount++;
        return 42;
      });
      
      expect(force(memoized)).toBe(42);
      expect(callCount).toBe(1);
      
      expect(force(memoized)).toBe(42);
      expect(callCount).toBe(1); // Still 1!
    });
  });

  describe('Church Encodings', () => {
    it('Church booleans', () => {
      expect(churchIf(churchTrue)('yes')('no')).toBe('yes');
      expect(churchIf(churchFalse)('yes')('no')).toBe('no');
    });

    it('Church numerals', () => {
      expect(churchToNum(churchZero)).toBe(0);
      
      const one = churchSucc(churchZero);
      expect(churchToNum(one)).toBe(1);
      
      const two = churchSucc(one);
      expect(churchToNum(two)).toBe(2);
      
      const five = churchSucc(churchSucc(churchSucc(churchSucc(one))));
      expect(churchToNum(five)).toBe(5);
    });
  });

  describe('Utility Combinators', () => {
    it('tap', () => {
      let sideEffect = 0;
      const increment = (x: number) => { sideEffect = x; };
      const tapped = tap(increment);
      
      expect(tapped(42)).toBe(42);
      expect(sideEffect).toBe(42);
    });

    it('times', () => {
      const double = (x: number) => x * 2;
      const apply4Times = times(4)(double);
      expect(apply4Times(1)).toBe(16); // 2^4
    });

    it('until', () => {
      const isGreaterThan100 = (x: number) => x > 100;
      const double = (x: number) => x * 2;
      const doubleTill100 = until(isGreaterThan100)(double);
      
      expect(doubleTill100(1)).toBe(128); // 1 -> 2 -> 4 -> 8 -> 16 -> 32 -> 64 -> 128
      expect(doubleTill100(50)).toBe(200); // 50 -> 100 -> 200
    });
  });
});

