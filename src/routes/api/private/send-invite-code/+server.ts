import {sendInviteCodeSchema} from "$lib/server/schemas/holster"
import {sendInviteCode} from "$lib/server/holster/utils"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"

export const POST = createPOSTHandler(
  sendInviteCodeSchema,
  async ({data}) => {
    const {code, email} = data
    sendInviteCode(code, email)
    return ""
  },
  {
    requireAuth: true,
    authOptions: {allowBasic: true, allowJwt: false, allowApiKey: false},
    emptyResponse: true
  }
)

