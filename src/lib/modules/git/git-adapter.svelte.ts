/**
 * Browser-Compatible Git-Holster Storage Adapter
 * 
 * Client-side Git operations using isomorphic-git + LightningFS in the browser
 * Syncs with Holster for distributed storage
 */
import * as git from "isomorphic-git"
import LightningFS from "@isomorphic-git/lightning-fs"
import { holsterUser } from "$lib/network/holster"

// ═══════════════════════════════════════════════════════════════════
// FILESYSTEM SETUP
// ═══════════════════════════════════════════════════════════════════

const FS = new LightningFS("holster-git-browser", { wipe: false })
const pfs = FS.promises

// ═══════════════════════════════════════════════════════════════════
// STATE
// ═══════════════════════════════════════════════════════════════════

export interface GitState {
  initialized: boolean
  currentRepo: string | null
  lastError: string | null
}

let state = $state<GitState>({
  initialized: false,
  currentRepo: null,
  lastError: null
})

export function getGitState(): GitState {
  return state
}

// ═══════════════════════════════════════════════════════════════════
// PATH UTILITIES
// ═══════════════════════════════════════════════════════════════════

export function repoPath(pub: string, repo: string): string {
  return `/holster/${pub}/git/${repo}`
}

export function getCurrentRepoPath(repo: string): string {
  if (!holsterUser.is) {
    throw new Error("No authenticated user")
  }
  return repoPath(holsterUser.is.pub, repo)
}

// ═══════════════════════════════════════════════════════════════════
// GIT OPERATIONS
// ═══════════════════════════════════════════════════════════════════

export async function initRepo(repo: string): Promise<string> {
  try {
    if (!holsterUser.is) {
      throw new Error("No authenticated user")
    }
    
    const dir = getCurrentRepoPath(repo)
    await pfs.mkdir(dir, { recursive: true })
    await git.init({ fs: pfs, dir, defaultBranch: 'main' })
    
    state.currentRepo = repo
    state.lastError = null
    state.initialized = true
    
    console.log(`[GIT] ✓ Initialized repo: ${repo}`)
    return dir
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

export async function writeFile(repo: string, filepath: string, content: string): Promise<void> {
  try {
    const dir = getCurrentRepoPath(repo)
    const fullPath = `${dir}/${filepath}`
    const dirPath = fullPath.substring(0, fullPath.lastIndexOf('/'))
    
    await pfs.mkdir(dirPath, { recursive: true })
    await pfs.writeFile(fullPath, content, 'utf8')
    
    console.log(`[GIT] ✓ Wrote: ${filepath}`)
    state.lastError = null
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

export async function readFile(repo: string, filepath: string): Promise<string> {
  try {
    const dir = getCurrentRepoPath(repo)
    const fullPath = `${dir}/${filepath}`
    const content = await pfs.readFile(fullPath, 'utf8')
    
    state.lastError = null
    return content
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

export async function addFile(repo: string, filepath: string): Promise<void> {
  try {
    const dir = getCurrentRepoPath(repo)
    await git.add({ fs: pfs, dir, filepath })
    
    console.log(`[GIT] ✓ Added: ${filepath}`)
    state.lastError = null
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

export async function commit(repo: string, message: string): Promise<string> {
  try {
    if (!holsterUser.is) {
      throw new Error("No authenticated user")
    }
    
    const dir = getCurrentRepoPath(repo)
    
    const sha = await git.commit({
      fs: pfs,
      dir,
      message,
      author: {
        name: holsterUser.is.username || 'anonymous',
        email: `${holsterUser.is.pub}@holster`,
        timestamp: Math.floor(Date.now() / 1000)
      },
      committer: {
        name: holsterUser.is.username || 'anonymous',
        email: `${holsterUser.is.pub}@holster`,
        timestamp: Math.floor(Date.now() / 1000)
      }
    })
    
    console.log(`[GIT] ✓ Commit: ${sha}`)
    state.lastError = null
    
    return sha
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

export async function log(repo: string, limit: number = 10): Promise<any[]> {
  try {
    const dir = getCurrentRepoPath(repo)
    const commits = await git.log({ fs: pfs, dir, depth: limit, ref: 'HEAD' })
    
    state.lastError = null
    return commits
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

export async function status(repo: string): Promise<{
  staged: string[]
  modified: string[]
  untracked: string[]
}> {
  try {
    const dir = getCurrentRepoPath(repo)
    const statusMatrix = await git.statusMatrix({ fs: pfs, dir })
    
    const staged: string[] = []
    const modified: string[] = []
    const untracked: string[] = []
    
    for (const [filepath, head, workdir, stage] of statusMatrix) {
      if (head === 0 && workdir === 2 && stage === 2) {
        staged.push(filepath)
      } else if (head === 1 && workdir === 2 && stage === 2) {
        modified.push(filepath)
      } else if (head === 0 && workdir === 2 && stage === 0) {
        untracked.push(filepath)
      }
    }
    
    state.lastError = null
    return { staged, modified, untracked }
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

// ═══════════════════════════════════════════════════════════════════
// HOLSTER SYNC (PUSH/PULL)
// ═══════════════════════════════════════════════════════════════════

export async function push(repo: string, onProgress?: (current: number, total: number) => void): Promise<string> {
  try {
    if (!holsterUser.is) {
      throw new Error("No authenticated user")
    }
    
    const pub = holsterUser.is.pub
    const dir = getCurrentRepoPath(repo)
    
    console.log(`[GIT] Pushing ${repo} to Holster...`)
    
    // Get HEAD commit
    const headOid = await git.resolveRef({ fs: pfs, dir, ref: 'HEAD' })
    console.log(`[GIT]   HEAD: ${headOid}`)
    
    // Walk all reachable objects from HEAD
    const oids = await git.listObjects({ fs: pfs, dir, oids: [headOid] })
    console.log(`[GIT]   Objects: ${oids.length}`)
    
    // Store each object in Holster
    let stored = 0
    for (const oid of oids) {
      try {
        const { object, type } = await git.readObject({ fs: pfs, dir, oid })
        const data = Buffer.from(object).toString('base64')
        
        await new Promise<void>((resolve, reject) => {
          holsterUser.get(`git/${repo}/objects/${oid}`).put({
            type,
            data,
            time: Date.now()
          }, (ack: any) => {
            if (ack?.err) {
              reject(new Error(ack.err))
            } else {
              resolve()
            }
          })
        })
        
        stored++
        onProgress?.(stored, oids.length)
      } catch (error) {
        console.error(`[GIT]   ✗ Failed to store object ${oid}:`, error)
      }
    }
    
    // Update ref
    await new Promise<void>((resolve, reject) => {
      holsterUser.get(`git/${repo}/refs/heads/main`).put({
        sha: headOid,
        time: Date.now()
      }, (ack: any) => {
        if (ack?.err) {
          reject(new Error(ack.err))
        } else {
          resolve()
        }
      })
    })
    
    console.log(`[GIT] ✓ Pushed ${stored} objects to Holster`)
    state.lastError = null
    
    return headOid
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

export async function pull(repo: string, fromPub?: string, onProgress?: (current: number, total: number) => void): Promise<void> {
  try {
    if (!holsterUser.is) {
      throw new Error("No authenticated user")
    }
    
    const sourcePub = fromPub || holsterUser.is.pub
    const dir = getCurrentRepoPath(repo)
    
    console.log(`[GIT] Pulling ${repo} from Holster...`)
    console.log(`[GIT]   Source: ${sourcePub.slice(0, 20)}...`)
    
    // Get remote ref
    const refData = await new Promise<any>((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('Timeout fetching remote ref')), 10000)
      holsterUser.get(sourcePub).get(`git/${repo}/refs/heads/main`).once((data: any) => {
        clearTimeout(timeout)
        if (!data || !data.sha) {
          reject(new Error('No remote ref found'))
        } else {
          resolve(data)
        }
      })
    })
    
    console.log(`[GIT]   Remote HEAD: ${refData.sha}`)
    
    // Fetch all objects recursively
    const fetchedOids = new Set<string>()
    const toFetch = [refData.sha]
    let fetched = 0
    
    while (toFetch.length > 0) {
      const oid = toFetch.pop()!
      if (fetchedOids.has(oid)) continue
      
      // Get object from Holster
      const objData = await new Promise<any>((resolve, reject) => {
        const timeout = setTimeout(() => reject(new Error(`Timeout fetching ${oid}`)), 10000)
        holsterUser.get(sourcePub).get(`git/${repo}/objects/${oid}`).once((data: any) => {
          clearTimeout(timeout)
          if (!data || !data.data) {
            reject(new Error(`Object ${oid} not found`))
          } else {
            resolve(data)
          }
        })
      })
      
      // Write object to local git
      const objectBuffer = Buffer.from(objData.data, 'base64')
      await git.writeObject({
        fs: pfs,
        dir,
        type: objData.type,
        object: objectBuffer,
        oid
      })
      
      fetchedOids.add(oid)
      fetched++
      onProgress?.(fetched, fetched + toFetch.length)
      
      // Parse object to find references
      if (objData.type === 'commit') {
        const commit = await git.readCommit({ fs: pfs, dir, oid })
        toFetch.push(commit.commit.tree)
        commit.commit.parent.forEach((parent: string) => toFetch.push(parent))
      } else if (objData.type === 'tree') {
        const { tree } = await git.readTree({ fs: pfs, dir, oid })
        tree.forEach((entry: any) => {
          if (entry.type === 'tree' || entry.type === 'blob') {
            toFetch.push(entry.oid)
          }
        })
      }
    }
    
    console.log(`[GIT] ✓ Pulled ${fetchedOids.size} objects from Holster`)
    
    // Update local ref
    await git.writeRef({
      fs: pfs,
      dir,
      ref: 'refs/heads/main',
      value: refData.sha,
      force: true
    })
    
    state.lastError = null
  } catch (error) {
    state.lastError = error instanceof Error ? error.message : String(error)
    throw error
  }
}

// ═══════════════════════════════════════════════════════════════════
// CLEANUP
// ═══════════════════════════════════════════════════════════════════

export function cleanup(): void {
  state.initialized = false
  state.currentRepo = null
  state.lastError = null
}

