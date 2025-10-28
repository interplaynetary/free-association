import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {updateFeedLimitSchema} from "$lib/server/schemas/rsstream"
import {user} from "$lib/server/rsstream/holster"
import {checkAuth} from "$lib/server/rsstream/auth"

export const POST: RequestHandler = async (event) => {
  const authError = checkAuth(event)
  if (authError) return authError

  const body = await event.request.json()
  const result = updateFeedLimitSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {code, limit} = result.data

  if (!user.is) {
    error(500, "Host error")
  }

  const account = await new Promise(res => {
    user.get("accounts").next(code, res)
  })

  if (!account) {
    error(404, "Account not found")
  }

  if ((account as any).validate) {
    error(400, "Email not validated")
  }

  return new Promise((resolve, reject) => {
    user
      .get("accounts")
      .next(code)
      .put({feeds: limit}, (err: any) => {
        if (err) {
          console.log(err)
          error(500, "Error updating feed limit")
        }

        resolve(text(""))
      })
  })
}

