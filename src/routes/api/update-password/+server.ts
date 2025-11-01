import {error} from "@sveltejs/kit"
import {updatePasswordSchema} from "$lib/server/schemas/holster"
import {user, holster} from "$lib/server/holster/core"
import {holsterNext, holsterNextPut, holsterDecrypt, ensureAuthenticated} from "$lib/server/holster/db"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  updatePasswordSchema,
  async ({data: requestData}) => {
    const {code, reset, pub, epub, username: userName, name} = requestData

    ensureAuthenticated()

    const account = await holsterNext("accounts", code)

    if (!account) {
      error(404, "Account not found")
    }

    if (!(account as any).reset) {
      error(404, "Reset code not found")
    }

    if (!(account as any).expiry || (account as any).expiry < Date.now()) {
      error(400, "Reset code has expired")
    }

    const resetCode = await holsterDecrypt((account as any).reset, user.is)
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
    await holsterNextPut("accounts", code, accountData)

    // Update account map
    await holsterNextPut("map", "account:" + pub, code)

    // Update shared invite codes for this account (async operation)
    user
      .get("shared")
      .next("invite_codes")
      .next(code, async (codes: any) => {
        if (codes) {
          const oldSecret = await holster.SEA.secret(account, user.is)
          const newSecret = await holster.SEA.secret(accountData, user.is)
          
          for (const [key, encrypted] of Object.entries(codes)) {
            if (!key || !encrypted) continue

            try {
              const dec = await holster.SEA.decrypt(encrypted, oldSecret)
              const shared = await holster.SEA.encrypt(dec, newSecret)
              await holsterNextPut("shared", ["invite_codes", code, key].join('/'), shared)
            } catch (err) {
              console.log('Error re-encrypting shared code:', err)
            }
          }
        }
      })

    return {previousPub: (account as any).pub}
  }
)

