import {error, json} from "@sveltejs/kit"
import {addSubscriberSchema} from "$lib/server/schemas/holster"
import {user} from "$lib/server/holster/core"
import {holsterNext, holsterNextPut, holsterVerify, ensureAuthenticated} from "$lib/server/holster/db"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  addSubscriberSchema,
  async ({data}) => {
    const {code, url: signedUrl} = data

    ensureAuthenticated()

    const account = await holsterNext("accounts", code)

    if (!account) {
      return json({error: "Account not found"}, {status: 404})
    }

    const url = await holsterVerify(signedUrl, account)
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
      return ""
    }

    try {
      await holsterNextPut("feeds", url, {subscriber_count: (feed as any).subscriber_count + 1})
      await holsterNextPut("accounts", code, {subscribed: (account as any).subscribed + 1})
      return ""
    } catch (err) {
      console.log(err)
      return json({error: "Error updating subscriber count"}, {status: 500})
    }
  },
  {emptyResponse: true}
)

