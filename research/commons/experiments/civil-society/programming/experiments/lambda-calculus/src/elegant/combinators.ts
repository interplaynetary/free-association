/**
 * Lambda Calculus Combinators and Functional Utilities
 * 
 * This module provides pure functional combinators following lambda calculus principles:
 * - Full currying
 * - Function composition
 * - Point-free style utilities
 * - Monadic patterns
 */

// ============================================================================
// Core Lambda Calculus Combinators
// ============================================================================

/**
 * Identity combinator: I = λx.x
 */
export const I = <T>(x: T): T => x;

/**
 * Constant combinator: K = λx.λy.x
 */
export const K = <T, U>(x: T) => (_y: U): T => x;

/**
 * Substitution combinator: S = λf.λg.λx.f(x)(g(x))
 */
export const S = <A, B, C>(f: (x: A) => (y: B) => C) => 
  (g: (x: A) => B) => 
  (x: A): C => 
    f(x)(g(x));

/**
 * Composition combinator: B = λf.λg.λx.f(g(x))
 */
export const B = <A, B, C>(f: (y: B) => C) => 
  (g: (x: A) => B) => 
  (x: A): C => 
    f(g(x));

/**
 * Flip combinator: C = λf.λx.λy.f(y)(x)
 */
export const C = <A, B, C>(f: (x: A) => (y: B) => C) => 
  (y: B) => 
  (x: A): C => 
    f(x)(y);

/**
 * Application combinator: $ = λf.λx.f(x)
 */
export const $ = <A, B>(f: (x: A) => B) => (x: A): B => f(x);

// ============================================================================
// Function Composition
// ============================================================================

/**
 * Left-to-right composition (pipe)
 */
export const pipe = <T>(...fns: Array<(x: any) => any>) => 
  (initial: T) => 
    fns.reduce((acc, fn) => fn(acc), initial);

/**
 * Right-to-left composition (compose)
 */
export const compose = <T>(...fns: Array<(x: any) => any>) => 
  (initial: T) => 
    fns.reduceRight((acc, fn) => fn(acc), initial);

/**
 * Compose two functions: (f ∘ g)(x) = f(g(x))
 */
export const compose2 = <A, B, C>(f: (y: B) => C, g: (x: A) => B) => 
  (x: A): C => 
    f(g(x));

/**
 * Pipe two functions: (f >> g)(x) = g(f(x))
 */
export const pipe2 = <A, B, C>(f: (x: A) => B, g: (y: B) => C) => 
  (x: A): C => 
    g(f(x));

// ============================================================================
// Currying Utilities
// ============================================================================

/**
 * Curry a 2-argument function
 */
export const curry2 = <A, B, R>(f: (a: A, b: B) => R) => 
  (a: A) => 
  (b: B): R => 
    f(a, b);

/**
 * Curry a 3-argument function
 */
export const curry3 = <A, B, C, R>(f: (a: A, b: B, c: C) => R) => 
  (a: A) => 
  (b: B) => 
  (c: C): R => 
    f(a, b, c);

/**
 * Curry a 4-argument function
 */
export const curry4 = <A, B, C, D, R>(f: (a: A, b: B, c: C, d: D) => R) => 
  (a: A) => 
  (b: B) => 
  (c: C) => 
  (d: D): R => 
    f(a, b, c, d);

/**
 * Uncurry a 2-level curried function
 */
export const uncurry2 = <A, B, R>(f: (a: A) => (b: B) => R) => 
  (a: A, b: B): R => 
    f(a)(b);

// ============================================================================
// Point-Free Utilities
// ============================================================================

/**
 * Map: fmap = λf.λxs.map(f)(xs)
 */
export const fmap = <A, B>(f: (x: A) => B) => 
  (xs: A[]): B[] => 
    xs.map(f);

/**
 * Filter: filter = λp.λxs.filter(p)(xs)
 */
export const filterP = <A>(predicate: (x: A) => boolean) => 
  (xs: A[]): A[] => 
    xs.filter(predicate);

/**
 * Reduce: fold = λf.λz.λxs.reduce(f, z)(xs)
 */
export const fold = <A, B>(f: (acc: B, x: A) => B) => 
  (initial: B) => 
  (xs: A[]): B => 
    xs.reduce(f, initial);

/**
 * Reduce right: foldr = λf.λz.λxs.reduceRight(f, z)(xs)
 */
export const foldr = <A, B>(f: (x: A, acc: B) => B) => 
  (initial: B) => 
  (xs: A[]): B => 
    xs.reduceRight((acc, x) => f(x, acc), initial);

/**
 * Flat map: flatMap = λf.λxs.flatMap(f)(xs)
 */
export const flatMap = <A, B>(f: (x: A) => B[]) => 
  (xs: A[]): B[] => 
    xs.flatMap(f);

// ============================================================================
// Monadic Patterns
// ============================================================================

/**
 * Maybe monad
 */
export type Maybe<T> = { tag: 'just'; value: T } | { tag: 'nothing' };

export const Just = <T>(value: T): Maybe<T> => ({ tag: 'just', value });
export const Nothing = <T>(): Maybe<T> => ({ tag: 'nothing' });

export const maybe = <A, B>(defaultValue: B) => 
  (f: (x: A) => B) => 
  (m: Maybe<A>): B => 
    m.tag === 'just' ? f(m.value) : defaultValue;

export const mapMaybe = <A, B>(f: (x: A) => B) => 
  (m: Maybe<A>): Maybe<B> => 
    m.tag === 'just' ? Just(f(m.value)) : Nothing();

export const bindMaybe = <A, B>(f: (x: A) => Maybe<B>) => 
  (m: Maybe<A>): Maybe<B> => 
    m.tag === 'just' ? f(m.value) : Nothing();

/**
 * Reader monad for threading context
 */
export type Reader<R, A> = (r: R) => A;

export const ask = <R>(): Reader<R, R> => (r: R) => r;

export const mapReader = <R, A, B>(f: (a: A) => B) => 
  (reader: Reader<R, A>): Reader<R, B> => 
  (r: R) => 
    f(reader(r));

export const bindReader = <R, A, B>(f: (a: A) => Reader<R, B>) => 
  (reader: Reader<R, A>): Reader<R, B> => 
  (r: R) => 
    f(reader(r))(r);

export const runReader = <R, A>(r: R) => 
  (reader: Reader<R, A>): A => 
    reader(r);

/**
 * State monad
 */
export type State<S, A> = (s: S) => [A, S];

export const get = <S>(): State<S, S> => (s: S) => [s, s];

export const put = <S>(s: S): State<S, void> => () => [undefined as void, s];

export const mapState = <S, A, B>(f: (a: A) => B) => 
  (state: State<S, A>): State<S, B> => 
  (s: S) => {
    const [a, s2] = state(s);
    return [f(a), s2];
  };

export const bindState = <S, A, B>(f: (a: A) => State<S, B>) => 
  (state: State<S, A>): State<S, B> => 
  (s: S) => {
    const [a, s2] = state(s);
    return f(a)(s2);
  };

export const runState = <S, A>(s: S) => 
  (state: State<S, A>): [A, S] => 
    state(s);

// ============================================================================
// Predicates and Logic
// ============================================================================

/**
 * Logical AND: and = λp.λq.λx.p(x) ∧ q(x)
 */
export const and = <T>(p: (x: T) => boolean) => 
  (q: (x: T) => boolean) => 
  (x: T): boolean => 
    p(x) && q(x);

/**
 * Logical OR: or = λp.λq.λx.p(x) ∨ q(x)
 */
export const or = <T>(p: (x: T) => boolean) => 
  (q: (x: T) => boolean) => 
  (x: T): boolean => 
    p(x) || q(x);

/**
 * Logical NOT: not = λp.λx.¬p(x)
 */
export const not = <T>(p: (x: T) => boolean) => 
  (x: T): boolean => 
    !p(x);

/**
 * Implication: implies = λp.λq.λx.¬p(x) ∨ q(x)
 */
export const implies = <T>(p: (x: T) => boolean) => 
  (q: (x: T) => boolean) => 
  (x: T): boolean => 
    !p(x) || q(x);

// ============================================================================
// Pair and Tuple Utilities
// ============================================================================

/**
 * Pair constructor: pair = λx.λy.λf.f(x)(y)
 */
export type Pair<A, B> = <R>(f: (a: A) => (b: B) => R) => R;

export const pair = <A, B>(a: A) => 
  (b: B): Pair<A, B> => 
  <R>(f: (x: A) => (y: B) => R): R => 
    f(a)(b);

/**
 * First projection: fst = λp.p(λx.λy.x)
 */
export const fst = <A, B>(p: Pair<A, B>): A => 
  p((x: A) => (_y: B) => x);

/**
 * Second projection: snd = λp.p(λx.λy.y)
 */
export const snd = <A, B>(p: Pair<A, B>): B => 
  p((_x: A) => (y: B) => y);

// ============================================================================
// Fixed Point Combinator
// ============================================================================

/**
 * Y combinator for recursion: Y = λf.(λx.f(x x))(λx.f(x x))
 * TypeScript approximation
 */
export const Y = <A, B>(f: (rec: (x: A) => B) => (x: A) => B): (x: A) => B => {
  const g = (h: any): ((x: A) => B) => f((x: A) => h(h)(x));
  return g(g);
};

/**
 * Memoized fixed point for performance
 */
export const memoY = <A, B>(f: (rec: (x: A) => B) => (x: A) => B): (x: A) => B => {
  const cache = new Map<A, B>();
  const rec = (x: A): B => {
    if (cache.has(x)) {
      return cache.get(x)!;
    }
    const result = f(rec)(x);
    cache.set(x, result);
    return result;
  };
  return rec;
};

// ============================================================================
// Lazy Evaluation
// ============================================================================

/**
 * Thunk for lazy evaluation
 */
export type Thunk<T> = () => T;

export const delay = <T>(computation: () => T): Thunk<T> => computation;

export const force = <T>(thunk: Thunk<T>): T => thunk();

/**
 * Memoized thunk (call-by-need)
 */
export const memo = <T>(computation: () => T): Thunk<T> => {
  let cached: { value: T } | undefined;
  return () => {
    if (!cached) {
      cached = { value: computation() };
    }
    return cached.value;
  };
};

// ============================================================================
// Church Encodings
// ============================================================================

/**
 * Church boolean: true = λt.λf.t
 */
export type ChurchBool = <T>(t: T) => (f: T) => T;

export const churchTrue: ChurchBool = <T>(t: T) => (_f: T) => t;
export const churchFalse: ChurchBool = <T>(_t: T) => (f: T) => f;

export const churchIf = <T>(cond: ChurchBool) => (thenVal: T) => (elseVal: T): T =>
  cond(thenVal)(elseVal);

/**
 * Church numerals: n = λf.λx.f^n(x)
 */
export type ChurchNum = <T>(f: (x: T) => T) => (x: T) => T;

export const churchZero: ChurchNum = <T>(_f: (x: T) => T) => (x: T) => x;

export const churchSucc = (n: ChurchNum): ChurchNum => 
  <T>(f: (x: T) => T) => 
  (x: T) => 
    f(n(f)(x));

export const churchToNum = (n: ChurchNum): number => 
  n((x: number) => x + 1)(0);

// ============================================================================
// Utility Combinators
// ============================================================================

/**
 * Tap: Perform side effect and return value
 */
export const tap = <T>(f: (x: T) => void) => 
  (x: T): T => {
    f(x);
    return x;
  };

/**
 * Trace: Log and return value
 */
export const trace = <T>(label: string) => 
  tap<T>((x: T) => console.log(label, x));

/**
 * Apply a function n times
 */
export const times = (n: number) => 
  <T>(f: (x: T) => T) => 
  (x: T): T => {
    let result = x;
    for (let i = 0; i < n; i++) {
      result = f(result);
    }
    return result;
  };

/**
 * Until: Apply function until predicate is true
 */
export const until = <T>(predicate: (x: T) => boolean) => 
  (f: (x: T) => T) => {
    const go = (x: T): T => predicate(x) ? x : go(f(x));
    return go;
  };

