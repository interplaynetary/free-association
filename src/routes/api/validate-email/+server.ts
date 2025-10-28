import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {validateEmailSchema} from "$lib/server/schemas/rsstream"
import {user, holster} from "$lib/server/rsstream/holster"

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = validateEmailSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {code, validate} = result.data

  if (!user.is) {
    error(500, "Host error")
  }

  const account = await new Promise(res => {
    user.get("accounts").next(code, res)
  })

  if (!account) {
    error(404, "Account not found")
  }

  if (!(account as any).validate) {
    return text("Email already validated")
  }

  const validateCode = await holster.SEA.decrypt((account as any).validate, user.is)
  if (validateCode !== validate) {
    error(400, "Validation code does not match")
  }

  return new Promise((resolve, reject) => {
    user
      .get("accounts")
      .next(code)
      .put({validate: null}, (err: any) => {
        if (err) {
          console.log(err)
          error(500, "Host error")
        }

        resolve(text("Email validated"))
      })
  })
}

