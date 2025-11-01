import {requestInviteCodeSchema} from "$lib/server/schemas/holster"
import {requestInvite} from "$lib/server/holster/utils"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  requestInviteCodeSchema,
  async ({data}) => {
    requestInvite(data.email)
    return {message: "Invite code requested"}
  }
)

