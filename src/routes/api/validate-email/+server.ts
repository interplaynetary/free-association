import { error } from "@sveltejs/kit"
import { validateEmailSchema } from "$lib/server/schemas/mesh"
import { user } from "$lib/server/mesh/core"
import { meshNext, meshNextPut, meshDecrypt, ensureAuthenticated } from "$lib/server/mesh/db"
import { createPOSTHandler } from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  validateEmailSchema,
  async ({ data }) => {
    const { code, validate } = data

    ensureAuthenticated()

    const account = await meshNext("accounts", code)

    if (!account) {
      error(404, "Account not found")
    }

    if (!(account as any).validate) {
      return { message: "Email already validated" }
    }

    const validateCode = await meshDecrypt((account as any).validate, user.is)
    if (validateCode !== validate) {
      error(400, "Validation code does not match")
    }

    await meshNextPut("accounts", code, { validate: null })

    return { message: "Email validated" }
  }
)

