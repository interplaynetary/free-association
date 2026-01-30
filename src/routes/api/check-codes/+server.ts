import { error } from "@sveltejs/kit"
import { checkCodesSchema } from "$lib/server/schemas/mesh"
import { checkCodes } from "$lib/server/mesh/invite-codes"
import { createPOSTHandler } from "$lib/server/middleware/request-handler"
import { ensureAuthenticated } from "$lib/server/mesh/db"

export const POST = createPOSTHandler(
  checkCodesSchema,
  async ({ data }) => {
    ensureAuthenticated()

    if (await checkCodes(data.codes)) {
      return ""
    }

    error(400, "duplicate code found")
  },
  { emptyResponse: true }
)

