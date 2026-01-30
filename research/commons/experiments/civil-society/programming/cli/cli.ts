#!/usr/bin/env bun
/**
 * Mesh + Git CLI
 * 
 * A command-line interface for managing Git repositories with Mesh storage.
 * 
 * Commands:
 *   create <username> <password>           - Create a new Mesh user
 *   auth <username> <password>             - Authenticate and load user
 *   change-pass <old> <new>                - Change password for current user
 *   logout                                 - Sign out current user
 *   
 *   git-init <repo>                        - Initialize a Git repository
 *   git-write <repo> <file> <content>      - Write a file to repository
 *   git-add <repo> <file>                  - Stage a file for commit
 *   git-commit <repo> <message>            - Commit staged changes
 *   git-status <repo>                      - Show repository status
 *   git-log <repo> [limit]                 - Show commit history
 *   git-push <repo>                        - Push repository to Mesh
 *   git-pull <repo> [pubkey]               - Pull repository from Mesh
 */

import { user } from "./mesh.ts"
import * as gitAdapter from "./git-adapter.ts"

// ═══════════════════════════════════════════════════════════════════
// UTILITIES
// ═══════════════════════════════════════════════════════════════════

function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms))
}

async function waitForAuth(timeout: number = 5000): Promise<void> {
  const start = Date.now()
  while (!user.is && Date.now() - start < timeout) {
    await sleep(100)
  }
  if (!user.is) {
    throw new Error("Authentication timeout")
  }
}

function showHelp(): void {
  console.log(`
Mesh + Git CLI

AUTHENTICATION COMMANDS:
  create <username> <password>           Create a new Mesh user
  auth <username> <password>             Authenticate and load user
  change-pass <old> <new>                Change password for current user
  logout                                 Sign out current user

GIT COMMANDS:
  git-init <repo>                        Initialize a Git repository
  git-write <repo> <file> <content>      Write a file to repository
  git-add <repo> <file>                  Stage a file for commit
  git-commit <repo> <message>            Commit staged changes
  git-status <repo>                      Show repository status
  git-log <repo> [limit]                 Show commit history
  git-push <repo>                        Push repository to Mesh
  git-pull <repo> [pubkey]               Pull repository from Mesh

EXAMPLES:
  # Create account and initialize repo
  bun cli/cli.ts create alice secret123
  bun cli/cli.ts auth alice secret123
  bun cli/cli.ts git-init myrepo
  
  # Make changes and push
  bun cli/cli.ts git-write myrepo README.md "# My Project"
  bun cli/cli.ts git-add myrepo README.md
  bun cli/cli.ts git-commit myrepo "Initial commit"
  bun cli/cli.ts git-push myrepo
  
  # Pull from another user
  bun cli/cli.ts git-pull myrepo <pubkey>
`)
}

// ═══════════════════════════════════════════════════════════════════
// COMMAND HANDLERS
// ═══════════════════════════════════════════════════════════════════

async function handleCreate(username: string, password: string): Promise<void> {
  return new Promise((resolve, reject) => {
    console.log(`Creating user: ${username}`)
    user.create(username, password, (ack: any) => {
      if (ack && ack.err) {
        console.error(`✗ Error: ${ack.err}`)
        reject(new Error(ack.err))
      } else {
        console.log(`✓ User created: ${username}`)
        if (user.is) {
          console.log(`  Public key: ${user.is.pub}`)
        }
        resolve()
      }
    })
  })
}

async function handleAuth(username: string, password: string): Promise<void> {
  return new Promise((resolve, reject) => {
    console.log(`Authenticating: ${username}`)
    user.auth(username, password, async (ack: any) => {
      if (ack && ack.err) {
        console.error(`✗ Error: ${ack.err}`)
        reject(new Error(ack.err))
      } else {
        // Wait for user.is to be populated
        await waitForAuth()
        console.log(`✓ Authenticated: ${username}`)
        if (user.is) {
          console.log(`  Public key: ${user.is.pub}`)
        }
        resolve()
      }
    })
  })
}

async function handleChangePassword(oldPassword: string, newPassword: string): Promise<void> {
  if (!user.is) {
    throw new Error("Not authenticated. Run 'auth' first.")
  }

  return new Promise((resolve, reject) => {
    console.log(`Changing password for: ${user.is.username}`)
    user.change(user.is.username, oldPassword, newPassword, (ack: any) => {
      if (ack && ack.err) {
        console.error(`✗ Error: ${ack.err}`)
        reject(new Error(ack.err))
      } else {
        console.log(`✓ Password changed successfully`)
        resolve()
      }
    })
  })
}

function handleLogout(): void {
  if (!user.is) {
    console.log("Not authenticated")
    return
  }

  const username = user.is.username
  user.leave()
  console.log(`✓ Logged out: ${username}`)
}

// ═══════════════════════════════════════════════════════════════════
// MAIN
// ═══════════════════════════════════════════════════════════════════

async function main(): Promise<void> {
  const [cmd, ...args] = process.argv.slice(2)

  if (!cmd || cmd === 'help' || cmd === '--help' || cmd === '-h') {
    showHelp()
    return
  }

  try {
    switch (cmd) {
      // Authentication commands
      case "create": {
        const [username, password] = args
        if (!username || !password) {
          throw new Error("Usage: create <username> <password>")
        }
        await handleCreate(username, password)
        break
      }

      case "auth": {
        const [username, password] = args
        if (!username || !password) {
          throw new Error("Usage: auth <username> <password>")
        }
        await handleAuth(username, password)
        break
      }

      case "change-pass": {
        const [oldPass, newPass] = args
        if (!oldPass || !newPass) {
          throw new Error("Usage: change-pass <old> <new>")
        }
        await handleChangePassword(oldPass, newPass)
        break
      }

      case "logout": {
        handleLogout()
        break
      }

      // Git commands
      case "git-init": {
        const [repo] = args
        if (!repo) {
          throw new Error("Usage: git-init <repo>")
        }
        await gitAdapter.initRepo(repo)
        break
      }

      case "git-write": {
        const [repo, filepath, ...contentParts] = args
        if (!repo || !filepath || contentParts.length === 0) {
          throw new Error("Usage: git-write <repo> <file> <content>")
        }
        const content = contentParts.join(" ")
        await gitAdapter.writeFile(repo, filepath, content)
        break
      }

      case "git-add": {
        const [repo, filepath] = args
        if (!repo || !filepath) {
          throw new Error("Usage: git-add <repo> <file>")
        }
        await gitAdapter.addFile(repo, filepath)
        break
      }

      case "git-commit": {
        const [repo, ...msgParts] = args
        if (!repo || msgParts.length === 0) {
          throw new Error("Usage: git-commit <repo> <message>")
        }
        const message = msgParts.join(" ")
        await gitAdapter.commit(repo, message)
        break
      }

      case "git-status": {
        const [repo] = args
        if (!repo) {
          throw new Error("Usage: git-status <repo>")
        }
        await gitAdapter.status(repo)
        break
      }

      case "git-log": {
        const [repo, limitStr] = args
        if (!repo) {
          throw new Error("Usage: git-log <repo> [limit]")
        }
        const limit = limitStr ? parseInt(limitStr, 10) : 10
        await gitAdapter.log(repo, limit)
        break
      }

      case "git-push": {
        const [repo] = args
        if (!repo) {
          throw new Error("Usage: git-push <repo>")
        }
        await gitAdapter.push(repo)
        break
      }

      case "git-pull": {
        const [repo, pubkey] = args
        if (!repo) {
          throw new Error("Usage: git-pull <repo> [pubkey]")
        }
        await gitAdapter.pull(repo, pubkey)
        break
      }

      default:
        console.error(`Unknown command: ${cmd}`)
        showHelp()
        process.exit(1)
    }

    // Give Mesh time to sync
    await sleep(500)
    process.exit(0)
  } catch (error) {
    console.error(`\n✗ Error: ${error instanceof Error ? error.message : String(error)}`)
    process.exit(1)
  }
}

main()

