import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {claimInviteCodeSchema} from "$lib/server/schemas/holster"
import {user, holster, inviteCodes, host} from "$lib/server/holster/core"
import {newCode, validateEmail} from "$lib/server/holster/utils"

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = claimInviteCodeSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {code, pub, epub, username: userName, email} = result.data
  const invite = inviteCodes.get(code)

  if (!invite) {
    error(404, "Invite code not found")
  }

  if (!user.is) {
    error(500, "Host error")
  }

  const validate = newCode()
  const encValidate = await holster.SEA.encrypt(validate, user.is)
  const encEmail = await holster.SEA.encrypt(email, user.is)
  const data = {
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

  let err = await new Promise(res => {
    user.get("accounts").next(code).put(data, res)
  })
  if (err) {
    console.log(err)
    error(500, "Host error")
  }

  // Also map the code to the user's public key to make login easier.
  err = await new Promise(res => {
    user
      .get("map")
      .next("account:" + pub)
      .put(code, res)
  })
  if (err) {
    console.log(err)
    error(500, "Host error")
  }

  validateEmail(userName, email, code, validate)

  // Remove invite code as it's no longer available.
  err = await new Promise(res => {
    user.get("available").next("invite_codes").next(invite.key).put(null, res)
  })
  if (err) {
    console.log(err)
    error(500, "Host error")
  }

  inviteCodes.delete(code)
  if (code === "admin") {
    return text("")
  }

  // Also remove from shared codes of the invite owner.
  const account = await new Promise(res => {
    user.get("accounts").next(invite.owner, res)
  })
  if (!account || !(account as any).epub) {
    console.log(`Account not found for invite.owner: ${invite.owner}`)
    return text("")
  }

  user
    .get("shared")
    .next("invite_codes")
    .next(invite.owner, async (codes: any) => {
      if (!codes) return

      const secret = await holster.SEA.secret(account, user.is)
      let found = false
      for (const [key, encrypted] of Object.entries(codes)) {
        if (found) break

        if (!key || !encrypted) continue

        let shared = await holster.SEA.decrypt(encrypted, secret)
        if (code === shared) {
          found = true
          user
            .get("shared")
            .next("invite_codes")
            .next(invite.owner)
            .next(key)
            .put(null, (err: any) => {
              if (err) console.log(err)
            })
        }
      }
    })

  return text("")
}

