import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {checkCodesSchema} from "$lib/server/schemas/holster"
import {user} from "$lib/server/holster/core"
import {checkCodes} from "$lib/server/holster/invite-codes"

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = checkCodesSchema.safeParse(body)

  if (!result.success) {
    error(400, "codes required")
  }

  if (!user.is) {
    error(500, "Host error")
  }

  if (await checkCodes(result.data.codes)) {
    return text("") // ok
  }

  error(400, "duplicate code found")
}

