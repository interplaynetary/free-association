import {z} from "zod"

// Zod schema for Dup store
export const dupStoreSchema = z.record(z.string(), z.number())
export type DupStore = z.infer<typeof dupStoreSchema>

export interface DupInterface {
  store: DupStore
  check: (id: string) => string | false
  track: (id: string) => string
  expiry: ReturnType<typeof setTimeout> | null
}

const Dup = (maxAge?: number): DupInterface => {
  // Allow maxAge to be passed in as tests wait on the setTimeout.
  if (!maxAge) maxAge = 9000
  const dup: DupInterface = {
    store: {},
    expiry: null,
    check: (id: string) => (dup.store[id] ? dup.track(id) : false),
    track: (id: string) => {
      // Keep the liveliness of the message up while it is being received.
      dup.store[id] = Date.now()
      if (!dup.expiry) {
        dup.expiry = setTimeout(() => {
          if (dup.expiry) return

          const now = Date.now()
          Object.keys(dup.store).forEach(id => {
            if (now - dup.store[id] > maxAge!) delete dup.store[id]
          })
          dup.expiry = null
        }, maxAge)
      }
      return id
    },
  }
  return dup
}

export default Dup

