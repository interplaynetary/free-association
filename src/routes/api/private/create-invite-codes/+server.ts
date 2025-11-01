import {error} from "@sveltejs/kit"
import {createInviteCodesSchema} from "$lib/server/schemas/holster"
import {createInviteCodes} from "$lib/server/holster/invite-codes"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"
import {getAccountByCodeOrFail} from "$lib/server/holster/db"

export const POST = createPOSTHandler(
  createInviteCodesSchema,
  async ({data}) => {
    const {code, count} = data

    const account = await getAccountByCodeOrFail(code)

    if ((account as any).validate) {
      error(400, "Email not validated")
    }

    if (await createInviteCodes(count, code, account as any)) {
      return ""
    }

    error(
      500,
      "Error creating codes. Please check logs for errors and try again",
    )
  },
  {
    requireAuth: true,
    authOptions: {allowBasic: true, allowJwt: false, allowApiKey: false},
    emptyResponse: true
  }
)

