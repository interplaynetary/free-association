import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {requestInviteCodeSchema} from "$lib/server/schemas/holster"
import {requestInvite} from "$lib/server/holster/utils"

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = requestInviteCodeSchema.safeParse(body)

  if (!result.success) {
    error(400, "email required")
  }

  requestInvite(result.data.email)
  return text("Invite code requested")
}

