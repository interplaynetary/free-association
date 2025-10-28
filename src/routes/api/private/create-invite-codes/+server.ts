import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {createInviteCodesSchema} from "$lib/server/schemas/holster"
import {user} from "$lib/server/holster/core"
import {createInviteCodes} from "$lib/server/holster/invite-codes"
import {checkAuth} from "$lib/server/holster/auth"

export const POST: RequestHandler = async (event) => {
  const authError = checkAuth(event)
  if (authError) return authError

  const body = await event.request.json()
  const result = createInviteCodesSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {code, count} = result.data

  if (!user.is) {
    error(500, "Host error")
  }

  const account = await new Promise(res => {
    user.get("accounts").next(code, res)
  })

  if (!account || !(account as any).epub) {
    error(404, "Account not found")
  }

  if ((account as any).validate) {
    error(400, "Email not validated")
  }

  if (await createInviteCodes(count, code, account as any)) {
    return text("")
  }

  error(
    500,
    "Error creating codes. Please check logs for errors and try again",
  )
}

