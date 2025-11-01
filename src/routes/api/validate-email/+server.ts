import {error} from "@sveltejs/kit"
import {validateEmailSchema} from "$lib/server/schemas/holster"
import {user} from "$lib/server/holster/core"
import {holsterNext, holsterNextPut, holsterDecrypt, ensureAuthenticated} from "$lib/server/holster/db"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  validateEmailSchema,
  async ({data}) => {
    const {code, validate} = data

    ensureAuthenticated()

    const account = await holsterNext("accounts", code)

    if (!account) {
      error(404, "Account not found")
    }

    if (!(account as any).validate) {
      return {message: "Email already validated"}
    }

    const validateCode = await holsterDecrypt((account as any).validate, user.is)
    if (validateCode !== validate) {
      error(400, "Validation code does not match")
    }

    await holsterNextPut("accounts", code, {validate: null})

    return {message: "Email validated"}
  }
)

