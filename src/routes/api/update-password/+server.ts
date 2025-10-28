import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {updatePasswordSchema} from "$lib/server/schemas/rsstream"
import {user, holster} from "$lib/server/rsstream/holster"

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = updatePasswordSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {code, reset, pub, epub, username: userName, name} = result.data

  if (!user.is) {
    error(500, "Host error")
  }

  const account = await new Promise(res => {
    user.get("accounts").next(code, res)
  })

  if (!account) {
    error(404, "Account not found")
  }

  if (!(account as any).reset) {
    error(404, "Reset code not found")
  }

  if (!(account as any).expiry || (account as any).expiry < Date.now()) {
    error(400, "Reset code has expired")
  }

  const resetCode = await holster.SEA.decrypt((account as any).reset, user.is)
  if (resetCode !== reset) {
    error(400, "Reset code does not match")
  }

  const data = {
    pub,
    epub,
    username: userName,
    name,
    prev: (account as any).pub,
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

        user
          .get("map")
          .next("account:" + pub)
          .put(code, (err: any) => {
            if (err) {
              console.log(err)
              error(500, "Host error")
            }

            // Also update shared invite codes for this account.
            user
              .get("shared")
              .next("invite_codes")
              .next(code, async (codes: any) => {
                if (codes) {
                  const oldSecret = await holster.SEA.secret(account, user.is)
                  const newSecret = await holster.SEA.secret(data, user.is)
                  for (const [key, encrypted] of Object.entries(codes)) {
                    if (!key || !encrypted) continue

                    const dec = await holster.SEA.decrypt(encrypted, oldSecret)
                    const shared = await holster.SEA.encrypt(dec, newSecret)
                    const err = await new Promise(res => {
                      user
                        .get("shared")
                        .next("invite_codes")
                        .next(code)
                        .next(key)
                        .put(shared, res)
                    })
                    if (err) console.log(err)
                  }
                }
                resolve(text((account as any).pub))
              })
          })
      })
  })
}

