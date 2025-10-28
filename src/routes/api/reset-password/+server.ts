import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {resetPasswordSchema} from "$lib/server/schemas/holster"
import {user, holster} from "$lib/server/holster/core"
import {newCode, resetPassword as sendResetPasswordEmail} from "$lib/server/holster/utils"

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = resetPasswordSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {code, email} = result.data

  if (!user.is) {
    error(500, "Host error")
  }

  const account = await new Promise(res => {
    user.get("accounts").next(code, res)
  })

  if (!account) {
    error(404, "Account not found")
  }

  let increment = 0
  const match = (account as any).username.match(/\.(\d)$/)
  if (match) {
    increment = Number(match[1])
  }
  if (increment === 9) {
    error(400, "Too many password resets")
  }

  const accountEmail = await holster.SEA.decrypt((account as any).email, user.is)
  if (accountEmail !== email) {
    error(400, "Email does not match invite code")
  }
  if ((account as any).validate) {
    error(400, "Please validate your email first")
  }

  const reset = newCode()
  const remaining = 8 - increment
  const data = {
    reset: await holster.SEA.encrypt(reset, user.is),
    expiry: Date.now() + 86400000,
  }

  return new Promise((resolve, reject) => {
    user
      .get("accounts")
      .next(code)
      .put(data, (err: any) => {
        if (err) {
          console.log(err)
          error(500, "Host error")
        }

        sendResetPasswordEmail((account as any).name, remaining, email, code, reset)
        resolve(text("Reset password email sent"))
      })
  })
}

