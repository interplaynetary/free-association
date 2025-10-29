import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {sendInviteCodeSchema} from "$lib/server/schemas/holster"
import {sendInviteCode} from "$lib/server/holster/utils"
import {checkAuth} from "$lib/server/holster/auth"

export const POST: RequestHandler = async (event) => {
  const authError = checkAuth(event)
  if (authError) return authError

  const body = await event.request.json()
  const result = sendInviteCodeSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {code, email} = result.data

  sendInviteCode(code, email)
  return text("")
}

