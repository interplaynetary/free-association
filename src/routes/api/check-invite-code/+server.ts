import {error} from "@sveltejs/kit"
import {checkInviteCodeSchema} from "$lib/server/schemas/holster"
import {user, inviteCodes} from "$lib/server/holster/core"
import {createPOSTHandler} from "$lib/server/middleware/request-handler"
import {ensureAuthenticated, holsterNext} from "$lib/server/holster/db"

export const POST = createPOSTHandler(
  checkInviteCodeSchema,
  async ({data}) => {
    const {code} = data

    if (inviteCodes.has(code)) {
      return "" // ok
    }

    ensureAuthenticated()

    // Check if code is already used
    const used = await holsterNext("accounts", code)
    
    if (used) {
      if (code === "admin") {
        error(400, "Please provide an invite code")
      }
      error(400, "Invite code already used")
    }
    
    error(404, "Invite code not found")
  },
  {emptyResponse: true}
)

