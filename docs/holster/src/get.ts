import {z} from "zod"
import {match, type GraphNode, type GraphMetadata} from "./utils.js"

// Zod schemas for Get operations
export const lexGetSchema = z.object({
  "#": z.string(),
  ".": z.union([z.string(), z.record(z.any())]).optional(),
})

export type LexGet = z.infer<typeof lexGetSchema>

export type GraphType = Record<
  string,
  GraphNode & {
    _: GraphMetadata
  }
>

const Get = (lex: LexGet, graph: GraphType): GraphType | undefined => {
  if (!lex || typeof lex !== "object") {
    throw new TypeError("lex must be an object")
  }
  if (!graph || typeof graph !== "object") {
    throw new TypeError("graph must be an object")
  }

  const soul = lex["#"]
  if (!soul || typeof soul !== "string") {
    throw new TypeError("soul must be a string")
  }

  if (!graph[soul]) return

  const node: GraphNode & {_: GraphMetadata} = {_: {"#": soul, ">": {}}}

  if (typeof lex["."] === "string") {
    const key = lex["."]
    if (typeof graph[soul][key] === "undefined") return

    node[key] = graph[soul][key]
    node._[">"][key] = graph[soul]._[">"][key]
  } else {
    for (const key of Object.keys(graph[soul])) {
      if (match(lex["."], key)) {
        node[key] = graph[soul][key]
        node._[">"][key] = graph[soul]._[">"][key]
      }
    }
  }
  return {[soul]: node}
}

export default Get

