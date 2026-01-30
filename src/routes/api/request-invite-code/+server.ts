import { requestInviteCodeSchema } from "$lib/server/schemas/mesh"
import { requestInvite } from "$lib/server/mesh/utils"
import { createPOSTHandler } from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  requestInviteCodeSchema,
  async ({ data }) => {
    requestInvite(data.email)
    return { message: "Invite code requested" }
  }
)

