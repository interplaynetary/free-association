import {removeFeedSchema} from "$lib/server/schemas/holster"
import {holsterNextPut, ensureAuthenticated} from "$lib/server/holster/db"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  removeFeedSchema,
  async ({data}) => {
    const {url} = data

    ensureAuthenticated()

    // Don't modify subscriber_count so users can still call remove-subscriber.
    const feedData = {
      title: "",
      description: "",
      html_url: "",
      language: "",
      image: "",
    }

    await holsterNextPut("feeds", url, feedData)
    return ""
  },
  {
    requireAuth: true,
    authOptions: {allowBasic: true, allowJwt: false, allowApiKey: false},
    emptyResponse: true
  }
)

