import crypto from "crypto"
import nodemailer from "nodemailer"
import {env} from "$env/dynamic/private"
import {host} from "./core"

// ============================================================================
// Code Generation
// ============================================================================

export function newCode(): string {
  const chars = "bcdfghjkmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ123456789"

  let code = ""
  while (code.length < 8) {
    code += chars.charAt(Math.floor(Math.random() * chars.length))
  }
  return code
}

// ============================================================================
// Content Hashing
// ============================================================================

export function createContentHash(data: {
  title: string
  content: string
  author: string
  permalink: string
}): string {
  const hashInput = `${data.title}|${data.content}|${data.author}|${data.permalink}`
  return crypto.createHash("md5").update(hashInput).digest("hex")
}

// ============================================================================
// Data Mapping
// ============================================================================

export function mapEnclosure(e: any) {
  if (!e) return null

  let found = false
  let enclosure: {
    photo?: Record<string, string>
    audio?: Record<string, boolean>
    video?: Record<string, boolean>
  } = {}

  // Optimize by checking existence first before allocation
  if (e.photo?.length > 0) {
    enclosure.photo = {}
    for (const p of e.photo) {
      if (p?.link) {
        found = true
        enclosure.photo[p.link] = p.alt || ""
      }
    }
  }
  if (e.audio?.length > 0) {
    enclosure.audio = {}
    for (const a of e.audio) {
      if (a) {
        found = true
        enclosure.audio[a] = true
      }
    }
  }
  if (e.video?.length > 0) {
    enclosure.video = {}
    for (const v of e.video) {
      if (v) {
        found = true
        enclosure.video[v] = true
      }
    }
  }
  return found ? enclosure : null
}

export function mapCategory(c: string[]) {
  if (!c?.length) return null

  let found = false
  let category: Record<string, boolean> = {}
  for (const value of c) {
    if (value) {
      found = true
      category[value] = true
    }
  }
  return found ? category : null
}

// ============================================================================
// Email Functions
// ============================================================================

export function mail(email: string, subject: string, message: string, bcc?: string) {
  if (!env.MAIL_FROM) {
    console.log("email", email)
    console.log("subject", subject)
    console.log("message", message)
    return
  }

  let data: any = {
    from: env.MAIL_FROM,
    to: email,
    subject: subject,
    text: message,
  }
  if (bcc) {
    data.bcc = bcc
  }
  nodemailer.createTransport({sendmail: true}).sendMail(data, (err: any, info: any) => {
    if (err) {
      console.log("sendmail returned an error:")
      console.log(err)
      return
    }
    console.log("mail", info)
  })
}

export function requestInvite(email: string) {
  const message = `Thanks for requesting an invite code at ${host}

There is a waiting list to create new accounts, an invite code will be sent to your email address ${email} when it becomes available.`

  const bcc = env.MAIL_BCC
  // If MAIL_FROM is not set then the message is already logged.
  if (env.MAIL_FROM && !bcc) {
    console.log("Invite request", email)
  }
  mail(email, "Invite request", message, bcc)
}

export function sendInviteCode(code: string, email: string) {
  const message = `Hello, thanks for waiting!

You can now create an account at ${host}/register using your invite code ${code}
`
  mail(email, "Invite code", message)
}

export function validateEmail(name: string, email: string, code: string, validate: string) {
  const message = `Hello ${name}

Thanks for creating an account at ${host}

Please validate your email at ${host}/validate-email?code=${code}&validate=${validate}

If you ever need to reset your password you will then be able to use ${host}/reset-password with the code ${code}
`
  mail(email, "Validate your email", message)
}

export function resetPassword(
  name: string,
  remaining: number,
  email: string,
  code: string,
  reset: string,
) {
  const message = `Hello ${name}

You can now update your password at ${host}/update-password?username=${name}&code=${code}&reset=${reset}

This link will be valid to use for the next 24 hours.

${remaining <= 5 ? `Note that you can only reset your password ${remaining} more time${remaining != 1 ? "s" : ""}.` : ""}
`
  mail(email, "Update your password", message)
}

