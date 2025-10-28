import {env} from "$env/dynamic/private"
import {user, holster, inviteCodes} from "./core"
import {newCode} from "./utils"
import type {AccountData} from "$lib/server/schemas/holster"

/**
 * Check if codes are unique across local accounts and existing invite codes
 */
export async function checkCodes(newCodes: string[]): Promise<boolean> {
  const codes = Object.keys(
    await new Promise(res => {
      user.get("accounts", res)
    }),
  )
  for (let i = 0; i < newCodes.length; i++) {
    if (codes.includes(newCodes[i])) return false
    if (inviteCodes.has(newCodes[i])) return false
  }
  return true
}

/**
 * Check codes against federated hosts
 */
export async function checkHosts(newCodes: string[]): Promise<boolean> {
  // Check for a comma separated list of federated hosts that should be checked
  // for duplicate codes. Note that the other servers don't need to store the
  // codes, they just each need to check that the list they create doesn't
  // contain duplicates when they also want to store new codes.
  const hosts = env.FEDERATED_HOSTS
  if (!hosts) return true

  const urls = hosts.split(",").map(url => url + "/check-codes")
  return (
    await Promise.all(
      urls.map(async url => {
        try {
          const res = await fetch(url, {
            method: "POST",
            headers: {
              "Content-Type": "application/json;charset=utf-8",
            },
            body: JSON.stringify({
              codes: newCodes,
            }),
          })
          if (!res.ok) {
            console.log(`checkHosts ${res.status} from ${res.url}`)
          }
          return res.ok
        } catch (error) {
          console.log(error)
          return false
        }
      }),
    )
  ).every(ok => ok)

  // Notes for further federated updates:
  // Other hosts can decide if they want to allow logins from federated user
  // accounts by listening to get("accounts").on() for each of the known
  // federated hosts and adding them to their own list of accounts. "host" is
  // provided in the account data to point users to their host server, but the
  // user could provide their email to another host to allow password resets
  // their too... it would just be stored on the other host account data the
  // same as it's stored here, without sharing between servers. That server
  // can replace the "host" field in their account data in that case, and can
  // store their own validation code.
}

/**
 * Create new invite codes
 */
export async function createInviteCodes(
  count: number,
  owner: string,
  account: AccountData,
): Promise<boolean> {
  let i = 0
  let newCodes: string[] = []
  while (i++ < count) {
    newCodes.push(newCode())
  }
  if (!(await checkCodes(newCodes)) || !(await checkHosts(newCodes))) {
    // If a duplicate code is found, return false and the request can be tried
    // again. More likely that a federated host is not reachable though, so
    // the list will need updating before making the request again.
    return false
  }

  const secret = await holster.SEA.secret(account, user.is)
  for (let i = 0; i < newCodes.length; i++) {
    const invite = {code: newCodes[i], owner: owner}
    const enc = await holster.SEA.encrypt(invite, user.is)
    let err = await new Promise(res => {
      user.get("available").next("invite_codes").put(enc, true, res)
    })
    if (err) {
      console.log(err)
      return false
    }

    console.log("New invite code available", invite)
    const shared = await holster.SEA.encrypt(newCodes[i], secret)
    err = await new Promise(res => {
      user.get("shared").next("invite_codes").next(owner).put(shared, true, res)
    })
    if (err) {
      console.log(err)
      return false
    }
  }
  return true
}

