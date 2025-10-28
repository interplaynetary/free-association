import {error, json, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {removeSubscriberSchema} from "$lib/server/schemas/rsstream"
import {user, holster} from "$lib/server/rsstream/holster"
import {env} from "$env/dynamic/private"

const addFeedUrl = env.ADD_FEED_URL
const addFeedID = env.ADD_FEED_ID
const addFeedApiKey = env.ADD_FEED_API_KEY

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = removeSubscriberSchema.safeParse(body)

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

  const feed = await new Promise(res => {
    user.get("feeds").next(url, {".": "subscriber_count"}, res)
  })

  if (!feed) {
    console.log("Feed not found for remove-subscriber", url)
    return text("")
  }

  if ((feed as any).subscriber_count === 0) {
    console.log("remove-subscriber called but subscriber_count is 0", url)
    return text("")
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

  return new Promise((resolve, reject) => {
    user
      .get("feeds")
      .next(url)
      .put({subscriber_count: (feed as any).subscriber_count - 1}, (err: any) => {
        if (err) {
          console.log(err)
          resolve(json({error: "Error removing from subscriber_count"}, {status: 500}))
          return
        }

        user
          .get("accounts")
          .next(code)
          .put({subscribed: (account as any).subscribed - 1}, (err: any) => {
            if (err) {
              console.log(err)
              resolve(json({error: "Error removing from account subscribed"}, {status: 500}))
              return
            }

            resolve(text(""))
          })
      })
  })
}

