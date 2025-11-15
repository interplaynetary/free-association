import {z} from "zod"
import * as utils from "./utils.js"

// ASCII character for group separator.
const group = String.fromCharCode(29)
// ASCII character for record separator.
const record = String.fromCharCode(30)

// Zod schemas for Radix tree structures
export const radixValueSchema = z.any()
export const radixTreeSchema: z.ZodType<any> = z.lazy(() =>
  z.record(
    z.string(),
    z.union([
      radixValueSchema,
      z.object({
        [group]: radixTreeSchema.optional(),
        [record]: radixValueSchema.optional(),
      }),
    ]),
  ),
)

export type RadixValue = z.infer<typeof radixValueSchema>
export type RadixTree = z.infer<typeof radixTreeSchema>

export interface RadixFunction {
  (keys?: string, value?: any, tree?: RadixTree): RadixTree | RadixValue | undefined
  [group]?: RadixTree
}

const Radix = (): RadixFunction => {
  const radix: RadixFunction = (keys?: string, value?: any, tree?: RadixTree) => {
    if (!tree) {
      if (!radix[group]) radix[group] = {}
      tree = radix[group]
    }
    if (!keys) return tree

    let i = 0
    let tmp: Record<string, any> = {}
    let key = keys[i]
    const max = keys.length - 1
    const noValue = typeof value === "undefined"
    // Find a matching value using the shortest string from keys.
    let found = tree[key]
    while (!found && i < max) {
      key += keys[++i]
      found = tree[key]
    }

    if (!found) {
      // If not found from the provided keys try matching with an existing key.
      const result = utils.obj.map(tree, (hasValue: any, hasKey: string) => {
        let j = 0
        let matchingKey = ""
        while (hasKey[j] === keys[j]) {
          matchingKey += hasKey[j++]
        }
        if (matchingKey) {
          if (noValue) {
            // matchingKey has to be as long as the original keys when reading.
            if (j <= max) return

            tmp[hasKey.slice(j)] = hasValue
            return hasValue
          }

          const replace: RadixTree = {
            [hasKey.slice(j)]: hasValue,
            [keys.slice(j)]: {[record]: value},
          }
          tree![matchingKey] = {[group]: replace}
          delete tree![hasKey]
          return true
        }
      })
      if (!result) {
        if (noValue) return

        if (!tree[key]) tree[key] = {}
        tree[key][record] = value
      } else if (noValue) {
        return tmp
      }
    } else if (i === max) {
      // If no value use the key provided to return a whole group or record.
      if (noValue) {
        // If an individual record isn't found then return the whole group.
        return typeof found[record] === "undefined" ? found[group] : found[record]
      }
      // Otherwise create a new record at the provided key for value.
      found[record] = value
    } else {
      // Found at a shorter key, try again.
      if (!found[group] && !noValue) found[group] = {}
      return radix(keys.slice(++i), value, found[group])
    }
  }
  return radix
}

Radix.map = function map(
  radix: RadixFunction | RadixTree,
  cb: (value: any, fullKey: string, key: string, pre: string[]) => any,
  opt?: boolean,
  pre?: string[],
): any {
  if (!pre) pre = []
  const tree = (radix as RadixFunction)[group] || radix
  const keys = Object.keys(tree).sort()
  let u: undefined

  for (let i = 0; i < keys.length; i++) {
    const key = keys[i]
    const found = tree[key]
    let tmp = found[record]
    if (typeof tmp !== "undefined") {
      tmp = cb(tmp, pre.join("") + key, key, pre)
      if (typeof tmp !== "undefined") return tmp
    } else if (opt) {
      cb(u, pre.join(""), key, pre)
    }
    if (found[group]) {
      pre.push(key)
      tmp = map(found[group], cb, opt, pre)
      if (typeof tmp !== "undefined") return tmp
      pre.pop()
    }
  }
}

export default Radix

