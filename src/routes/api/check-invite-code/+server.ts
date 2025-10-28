import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {checkInviteCodeSchema} from "$lib/server/schemas/rsstream"
import {user, inviteCodes} from "$lib/server/rsstream/holster"

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = checkInviteCodeSchema.safeParse(body)

  if (!result.success) {
    error(400, result.error.message)
  }

  const code = result.data.code

  if (inviteCodes.has(code)) {
    return text("") // ok
  }

  if (!user.is) {
    error(500, "Host error")
  }

  // This just provides relevant errors.
  return new Promise((resolve, reject) => {
    user.get("accounts").next(code, (used: any) => {
      if (used) {
        if (code === "admin") {
          error(400, "Please provide an invite code")
        }
        error(400, "Invite code already used")
      }
      error(404, "Invite code not found")
    })
  })
}

