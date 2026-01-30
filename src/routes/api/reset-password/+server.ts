import { error } from "@sveltejs/kit"
import { resetPasswordSchema } from "$lib/server/schemas/mesh"
import { user } from "$lib/server/mesh/core"
import { meshNext, meshNextPut, meshDecrypt, meshEncrypt, ensureAuthenticated } from "$lib/server/mesh/db"
import { newCode, resetPassword as sendResetPasswordEmail } from "$lib/server/mesh/utils"
import { createPOSTHandler } from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  resetPasswordSchema,
  async ({ data }) => {
    const { code, email } = data

    ensureAuthenticated()

    const account = await meshNext("accounts", code)

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

    const accountEmail = await meshDecrypt((account as any).email, user.is)
    if (accountEmail !== email) {
      error(400, "Email does not match invite code")
    }
    if ((account as any).validate) {
      error(400, "Please validate your email first")
    }

    const reset = newCode()
    const remaining = 8 - increment
    const resetData = {
      reset: await meshEncrypt(reset, user.is),
      expiry: Date.now() + 86400000,
    }

    await meshNextPut("accounts", code, resetData)

    sendResetPasswordEmail((account as any).name, remaining, email, code, reset)

    return { message: "Reset password email sent" }
  }
)

