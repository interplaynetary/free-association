import {z} from "zod"

if (typeof btoa === "undefined") {
  const isNode = typeof document === "undefined"
  if (isNode) {
    (globalThis as any).btoa = (data: string) => {
      const Buffer = (globalThis as any).Buffer
      return Buffer.from(data, "binary").toString("base64")
    }
    (globalThis as any).atob = (data: string) => {
      const Buffer = (globalThis as any).Buffer
      return Buffer.from(data, "base64").toString("binary")
    }
  }
}

// Zod schema for encoding types
export const encodingSchema = z.enum(["utf8", "hex", "base64"])
export type EncodingType = z.infer<typeof encodingSchema>

// This is Array extended to have .toString(["utf8"|"hex"|"base64"])
class SeaArray extends Array<number> {
  static from<T>(arrayLike: ArrayLike<T> | Iterable<T>): SeaArray {
    const instance = Object.create(SeaArray.prototype)
    const arr = Array.from(arrayLike)
    Object.setPrototypeOf(arr, SeaArray.prototype)
    return Object.assign(instance, arr)
  }

  toString(enc?: EncodingType | string, start?: number, end?: number): string {
    if (!enc) enc = "utf8"
    if (!start) start = 0

    end = end ? Math.min(Math.max(end, start), this.length) : this.length

    if (enc === "hex") {
      const buf = new Uint8Array(this.slice(start, end))
      return Array.from(buf, byte => byte.toString(16).padStart(2, "0")).join("")
    }

    if (enc === "utf8") {
      return Array.from({length: end - start}, (_, i) => {
        const charCode = this[i + start]
        // Use unicode replacement character for invalid character codes.
        if (charCode < 0 || charCode > 0x10ffff) {
          return String.fromCharCode(0xfffd)
        }
        return String.fromCharCode(charCode)
      }).join("")
    }

    if (enc === "base64") {
      const utf8String = Array.from({length: end - start}, (_, i) => {
        const charCode = this[i + start]
        if (charCode < 0 || charCode > 255) {
          return String.fromCharCode(0xfffd)
        }
        return String.fromCharCode(charCode)
      }).join("")
      return btoa(utf8String)
    }

    return ""
  }
}

export default SeaArray

