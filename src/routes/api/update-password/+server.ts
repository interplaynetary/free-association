import { error } from "@sveltejs/kit"
import { updatePasswordSchema } from "$lib/server/schemas/mesh"
import { user, mesh } from "$lib/server/mesh/core"
import { meshNext, meshNextPut, meshDecrypt, ensureAuthenticated } from "$lib/server/mesh/db"
import { createPOSTHandler } from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  updatePasswordSchema,
  async ({ data: requestData }) => {
    const { code, reset, pub, epub, username: userName, name } = requestData

    ensureAuthenticated()

    const account = await meshNext("accounts", code)

    if (!account) {
      error(404, "Account not found")
    }

    if (!(account as any).reset) {
      error(404, "Reset code not found")
    }

    if (!(account as any).expiry || (account as any).expiry < Date.now()) {
      error(400, "Reset code has expired")
    }

    const resetCode = await meshDecrypt((account as any).reset, user.is)
    if (resetCode !== reset) {
      error(400, "Reset code does not match")
    }

    const accountData = {
      pub,
      epub,
      username: userName,
      name,
      prev: (account as any).pub,
    }

    // Update account
    await meshNextPut("accounts", code, accountData)

    // Update account map
    await meshNextPut("map", "account:" + pub, code)

    // Update shared invite codes for this account (async operation)
    user
      .get("shared")
      .next("invite_codes")
      .next(code, async (codes: any) => {
        if (codes) {
          const oldSecret = await mesh.SEA.secret(account, user.is)
          const newSecret = await mesh.SEA.secret(accountData, user.is)

          for (const [key, encrypted] of Object.entries(codes)) {
            if (!key || !encrypted) continue

            try {
              const dec = await mesh.SEA.decrypt(encrypted, oldSecret)
              const shared = await mesh.SEA.encrypt(dec, newSecret)
              await meshNextPut("shared", ["invite_codes", code, key].join('/'), shared)
            } catch (err) {
              console.log('Error re-encrypting shared code:', err)
            }
          }
        }
      })

    return { previousPub: (account as any).pub }
  }
)

