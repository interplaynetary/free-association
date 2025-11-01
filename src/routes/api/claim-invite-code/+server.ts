import {error} from "@sveltejs/kit"
import {claimInviteCodeSchema} from "$lib/server/schemas/holster"
import {user, holster, inviteCodes, host} from "$lib/server/holster/core"
import {holsterNext, holsterNextPut, holsterEncrypt, ensureAuthenticated, holsterDelete} from "$lib/server/holster/db"
import {newCode, validateEmail} from "$lib/server/holster/utils"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  claimInviteCodeSchema,
  async ({data: requestData}) => {
    const {code, pub, epub, username: userName, email} = requestData
    const invite = inviteCodes.get(code)

    if (!invite) {
      error(404, "Invite code not found")
    }

    ensureAuthenticated()

    // Prepare account data
    const validate = newCode()
    const encValidate = await holsterEncrypt(validate, user.is)
    const encEmail = await holsterEncrypt(email, user.is)
    
    const accountData = {
      pub,
      epub,
      username: userName,
      name: userName,
      email: encEmail,
      validate: encValidate,
      ref: invite.owner,
      host: host,
      feeds: 10,
      subscribed: 0,
    }

    // Create account
    await holsterNextPut("accounts", code, accountData)

    // Map code to user's public key for easier login
    await holsterNextPut("map", "account:" + pub, code)

    // Send validation email
    validateEmail(userName, email, code, validate)

    // Remove invite code from available codes
    await holsterDelete("available", ["invite_codes", invite.key].join('/'))

    // Remove from in-memory cache
    inviteCodes.delete(code)
    
    if (code === "admin") {
      return ""
    }

    // Clean up shared codes from invite owner (async operation)
    const ownerAccount = await holsterNext("accounts", invite.owner)
    
    if (!ownerAccount || !(ownerAccount as any).epub) {
      console.log(`Account not found for invite.owner: ${invite.owner}`)
      return ""
    }

    // Remove from shared codes asynchronously
    user
      .get("shared")
      .next("invite_codes")
      .next(invite.owner, async (codes: any) => {
        if (!codes) return

        try {
          const secret = await holster.SEA.secret(ownerAccount, user.is)
          
          for (const [key, encrypted] of Object.entries(codes)) {
            if (!key || !encrypted) continue

            const shared = await holster.SEA.decrypt(encrypted, secret)
            if (code === shared) {
              await holsterDelete("shared", ["invite_codes", invite.owner, key].join('/'))
              break
            }
          }
        } catch (err) {
          console.log('Error cleaning up shared invite code:', err)
        }
      })

    return ""
  },
  {emptyResponse: true}
)

