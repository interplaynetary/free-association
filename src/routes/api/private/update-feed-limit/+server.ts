import { error } from "@sveltejs/kit"
import { updateFeedLimitSchema } from "$lib/server/schemas/mesh"
import { getAccountByCodeOrFail, meshNextPut } from "$lib/server/mesh/db"
import { createPOSTHandler } from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  updateFeedLimitSchema,
  async ({ data }) => {
    const { code, limit } = data

    const account = await getAccountByCodeOrFail(code)

    if ((account as any).validate) {
      error(400, "Email not validated")
    }

    await meshNextPut("accounts", code, { feeds: limit })
    return ""
  },
  {
    requireAuth: true,
    authOptions: { allowBasic: true, allowJwt: false, allowApiKey: false },
    emptyResponse: true
  }
)

