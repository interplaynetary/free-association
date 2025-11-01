import {error, json} from "@sveltejs/kit"
import {removeSubscriberSchema} from "$lib/server/schemas/holster"
import {user} from "$lib/server/holster/core"
import {config} from "$lib/server/config"
import {holsterNext, holsterNextPut, holsterVerify} from "$lib/server/holster/db"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"

const addFeedUrl = config.addFeedUrl
const addFeedID = config.addFeedId
const addFeedApiKey = config.addFeedApiKey

export const POST = createPOSTHandler(
  removeSubscriberSchema,
  async ({data}) => {
    const {code, url: signedUrl} = data

    if (!user.is) {
      error(500, "Host error")
    }

    const account = await holsterNext("accounts", code)

    if (!account) {
      return json({error: "Account not found"}, {status: 404})
    }

    const url = await holsterVerify(signedUrl, account)
    if (!url) {
      return json({error: "Could not verify signed url"}, {status: 400})
    }

    const feed = await new Promise(res => {
      user.get("feeds").next(url, {".": "subscriber_count"}, res)
    })

    if (!feed) {
      console.log("Feed not found for remove-subscriber", url)
      return ""
    }

    if ((feed as any).subscriber_count === 0) {
      console.log("remove-subscriber called but subscriber_count is 0", url)
      return ""
    }

    if ((feed as any).subscriber_count === 1) {
      if (!addFeedUrl || !addFeedID || !addFeedApiKey) {
        console.log("Could not remove feed, env not set")
      } else {
        try {
          const remove = await fetch(addFeedUrl, {
            method: "POST",
            headers: {
              "Content-Type": "application/x-www-form-urlencoded",
            },
            body: `id=${addFeedID}&key=${addFeedApiKey}&action=remove-feed&xmlUrl=${encodeURIComponent(url)}`,
          })
          if (!remove.ok) {
            console.log("Error from", addFeedUrl, url, remove.statusText)
          }
        } catch (err) {
          console.log(err)
        }
      }
    }

    try {
      await holsterNextPut("feeds", url, {subscriber_count: (feed as any).subscriber_count - 1})
      await holsterNextPut("accounts", code, {subscribed: (account as any).subscribed - 1})
      return ""
    } catch (err) {
      console.log(err)
      return json({error: "Error updating subscriber count"}, {status: 500})
    }
  },
  {emptyResponse: true}
)

