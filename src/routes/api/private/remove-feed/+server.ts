import {error, text} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {removeFeedSchema} from "$lib/server/schemas/rsstream"
import {user} from "$lib/server/rsstream/holster"
import {checkAuth} from "$lib/server/rsstream/auth"

export const POST: RequestHandler = async (event) => {
  const authError = checkAuth(event)
  if (authError) return authError

  const body = await event.request.json()
  const result = removeFeedSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {url} = result.data

  if (!user.is) {
    error(500, "Host error")
  }

  // Don't modify subscriber_count so users can still call remove-subscriber.
  const data = {
    title: "",
    description: "",
    html_url: "",
    language: "",
    image: "",
  }

  return new Promise((resolve, reject) => {
    user
      .get("feeds")
      .next(url)
      .put(data, (err: any) => {
        if (err) {
          console.log(err)
          error(500, "Error removing feed")
        }

        resolve(text(""))
      })
  })
}

