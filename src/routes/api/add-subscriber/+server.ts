import {error, json, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {addSubscriberSchema} from "$lib/server/schemas/rsstream"
import {user, holster} from "$lib/server/rsstream/holster"

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = addSubscriberSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    return json({error: firstError.message}, {status: 400})
  }

  const {code, url: signedUrl} = result.data

  if (!user.is) {
    error(500, "Host error")
  }

  const account = await new Promise(res => {
    user.get("accounts").next(code, res)
  })

  if (!account) {
    return json({error: "Account not found"}, {status: 404})
  }

  const url = await holster.SEA.verify(signedUrl, account)
  if (!url) {
    return json({error: "Could not verify signed url"}, {status: 400})
  }

  if ((account as any).subscribed === (account as any).feeds) {
    return json(
      {error: `Account currently has a limit of ${(account as any).feeds} feeds`},
      {status: 400},
    )
  }

  const feed = await new Promise(res => {
    user.get("feeds").next(url, {".": "subscriber_count"}, res)
  })

  if (!feed) {
    console.log("Feed not found for add-subscriber", url)
    return text("")
  }

  return new Promise((resolve, reject) => {
    user
      .get("feeds")
      .next(url)
      .put({subscriber_count: (feed as any).subscriber_count + 1}, (err: any) => {
        if (err) {
          console.log(err)
          resolve(json({error: "Error adding to feed subscriber_count"}, {status: 500}))
          return
        }

        user
          .get("accounts")
          .next(code)
          .put({subscribed: (account as any).subscribed + 1}, (err: any) => {
            if (err) {
              console.log(err)
              resolve(json({error: "Error adding to account subscribed"}, {status: 500}))
              return
            }

            resolve(text(""))
          })
      })
  })
}

