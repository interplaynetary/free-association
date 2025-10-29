import {error, json} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {addFeedSchema, addFeedResponseSchema} from "$lib/server/schemas/holster"
import {user, holster} from "$lib/server/holster/core"
import {env} from "$env/dynamic/private"

const addFeedUrl = env.ADD_FEED_URL
const addFeedID = env.ADD_FEED_ID
const addFeedApiKey = env.ADD_FEED_API_KEY

export const POST: RequestHandler = async ({request}) => {
  const body = await request.json()
  const result = addFeedSchema.safeParse(body)

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

  if (!addFeedUrl || !addFeedID || !addFeedApiKey) {
    return json({error: "Could not add feed, env not set"}, {status: 500})
  }

  try {
    const add = await fetch(addFeedUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
      },
      body: `id=${addFeedID}&key=${addFeedApiKey}&action=add-feed&xmlUrl=${encodeURIComponent(url)}`,
    })

    if (!add.ok) {
      console.log("Error from", addFeedUrl, url, add.statusText)
      return json({error: "Error adding feed"}, {status: 500})
    }

    const feed = await add.json()
    if (feed.error) {
      console.log("Error from", addFeedUrl, url)
      console.log(feed.error)
      return json({error: "Error adding feed"}, {status: 500})
    }

    if (!feed.add || !feed.add.url || !feed.add.title) {
      console.log("No feed data from", addFeedUrl, url)
      console.log(feed)
      return json({error: "Error adding feed"}, {status: 500})
    }

    const data = {
      title: feed.add.title,
      description: feed.add.description ?? "",
      html_url: feed.add.html_url ?? "",
      language: feed.add.language ?? "",
      image: feed.add.image ?? "",
      subscriber_count: 1,
    }

    return new Promise((resolve, reject) => {
      user
        .get("feeds")
        .next(feed.add.url)
        .put(data, (err: any) => {
          if (err) {
            console.log(err)
            resolve(json({error: "Error saving feed"}, {status: 500}))
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

              resolve(json(feed))
            })
        })
    })
  } catch (err) {
    console.log(err)
    return json({error: "Error adding feed"}, {status: 500})
  }
}

