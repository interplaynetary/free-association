import {text, error} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {user} from "$lib/server/holster/core"

export const GET: RequestHandler = async () => {
  if (user.is) {
    return text(user.is.pub)
  }

  error(404, "Host public key not found")
}

