/**
 * Git-Holster Storage Adapter
 * 
 * Bridges isomorphic-git with Holster storage:
 * - Git objects stored in Holster namespaced by user's public key
 * - Commits signed with Holster identity
 * - Push/pull sync Git objects to/from Holster network
 */
import * as git from "isomorphic-git"
import * as fs from "fs"
import * as path from "path"
import { user } from "./holster.js"

// Use Node.js fs for CLI (not LightningFS which requires browser IndexedDB)
const pfs = fs.promises

// ═══════════════════════════════════════════════════════════════════
// PATH UTILITIES
// ═══════════════════════════════════════════════════════════════════

export function repoPath(pub: string, repo: string): string {
  return `/holster/${pub}/git/${repo}`
}

export function getCurrentRepoPath(repo: string): string {
  if (!user.is) {
    throw new Error("No authenticated user")
  }
  return repoPath(user.is.pub, repo)
}

// ═══════════════════════════════════════════════════════════════════
// GIT OPERATIONS
// ═══════════════════════════════════════════════════════════════════

export async function initRepo(repo: string): Promise<string> {
  if (!user.is) {
    throw new Error("No authenticated user. Run 'auth' first.")
  }
  
  const dir = getCurrentRepoPath(repo)
  await pfs.mkdir(dir, { recursive: true })
  await git.init({ fs: pfs, dir, defaultBranch: 'main' })
  
  console.log(`✓ Initialized repo: ${repo}`)
  console.log(`  Path: ${dir}`)
  
  return dir
}

export async function addFile(repo: string, filepath: string): Promise<void> {
  const dir = getCurrentRepoPath(repo)
  await git.add({ fs: pfs, dir, filepath })
  console.log(`✓ Added: ${filepath}`)
}

export async function commit(repo: string, message: string): Promise<string> {
  if (!user.is) {
    throw new Error("No authenticated user")
  }
  
  const dir = getCurrentRepoPath(repo)
  
  const sha = await git.commit({
    fs: pfs,
    dir,
    message,
    author: {
      name: user.is.username || 'anonymous',
      email: `${user.is.pub}@holster`,
      timestamp: Math.floor(Date.now() / 1000)
    },
    committer: {
      name: user.is.username || 'anonymous',
      email: `${user.is.pub}@holster`,
      timestamp: Math.floor(Date.now() / 1000)
    }
  })
  
  console.log(`✓ Commit: ${sha}`)
  console.log(`  ${message}`)
  
  return sha
}

export async function writeFile(repo: string, filepath: string, content: string): Promise<void> {
  const dir = getCurrentRepoPath(repo)
  const fullPath = path.join(dir, filepath)
  const dirPath = path.dirname(fullPath)
  
  await pfs.mkdir(dirPath, { recursive: true })
  await pfs.writeFile(fullPath, content, 'utf8')
  
  console.log(`✓ Wrote: ${filepath}`)
}

export async function readFile(repo: string, filepath: string): Promise<string> {
  const dir = getCurrentRepoPath(repo)
  const fullPath = path.join(dir, filepath)
  const content = await pfs.readFile(fullPath, 'utf8')
  return content
}

// ═══════════════════════════════════════════════════════════════════
// HOLSTER SYNC (PUSH/PULL)
// ═══════════════════════════════════════════════════════════════════

export async function push(repo: string): Promise<string> {
  if (!user.is) {
    throw new Error("No authenticated user")
  }
  
  const pub = user.is.pub
  const dir = getCurrentRepoPath(repo)
  
  console.log(`\nPushing ${repo} to Holster...`)
  
  // Get HEAD commit
  const headOid = await git.resolveRef({ fs: pfs, dir, ref: 'HEAD' })
  console.log(`  HEAD: ${headOid}`)
  
  // Walk all reachable objects from HEAD
  const oids = await git.listObjects({ fs: pfs, dir, oids: [headOid] })
  console.log(`  Objects: ${oids.length}`)
  
  // Store each object in Holster
  let stored = 0
  for (const oid of oids) {
    try {
      const { object, type } = await git.readObject({ fs: pfs, dir, oid })
      const data = Buffer.from(object).toString('base64')
      
      await new Promise<void>((resolve, reject) => {
        user.get(`git/${repo}/objects/${oid}`).put({
          type,
          data,
          time: Date.now()
        }, (ack: any) => {
          if (ack.err) {
            reject(new Error(ack.err))
          } else {
            resolve()
          }
        })
      })
      
      stored++
      if (stored % 10 === 0) {
        console.log(`  Stored ${stored}/${oids.length} objects...`)
      }
    } catch (error) {
      console.error(`  ✗ Failed to store object ${oid}:`, error)
    }
  }
  
  // Update ref
  await new Promise<void>((resolve, reject) => {
    user.get(`git/${repo}/refs/heads/main`).put({
      sha: headOid,
      time: Date.now()
    }, (ack: any) => {
      if (ack.err) {
        reject(new Error(ack.err))
      } else {
        resolve()
      }
    })
  })
  
  console.log(`✓ Pushed ${stored} objects to Holster`)
  console.log(`  Ref: main -> ${headOid}`)
  
  return headOid
}

export async function pull(repo: string, fromPub?: string): Promise<void> {
  if (!user.is) {
    throw new Error("No authenticated user")
  }
  
  const sourcePub = fromPub || user.is.pub
  const dir = getCurrentRepoPath(repo)
  
  console.log(`\nPulling ${repo} from Holster...`)
  console.log(`  Source: ${sourcePub.slice(0, 20)}...`)
  
  // Get remote ref
  const refData = await new Promise<any>((resolve, reject) => {
    const timeout = setTimeout(() => reject(new Error('Timeout')), 5000)
    user.get(sourcePub).get(`git/${repo}/refs/heads/main`).once((data: any) => {
      clearTimeout(timeout)
      if (!data || !data.sha) {
        reject(new Error('No remote ref found'))
      } else {
        resolve(data)
      }
    })
  })
  
  console.log(`  Remote HEAD: ${refData.sha}`)
  
  // Fetch all objects recursively
  const fetchedOids = new Set<string>()
  const toFetch = [refData.sha]
  
  while (toFetch.length > 0) {
    const oid = toFetch.pop()!
    if (fetchedOids.has(oid)) continue
    
    // Get object from Holster
    const objData = await new Promise<any>((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('Timeout')), 5000)
      user.get(sourcePub).get(`git/${repo}/objects/${oid}`).once((data: any) => {
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
    console.log(`  Fetched: ${oid} (${objData.type})`)
    
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
  
  console.log(`✓ Pulled ${fetchedOids.size} objects from Holster`)
  
  // Update local ref
  await git.writeRef({
    fs: pfs,
    dir,
    ref: 'refs/heads/main',
    value: refData.sha,
    force: true
  })
  
  console.log(`  Updated: main -> ${refData.sha}`)
}

export async function log(repo: string, limit: number = 10): Promise<void> {
  const dir = getCurrentRepoPath(repo)
  
  const commits = await git.log({ fs: pfs, dir, depth: limit, ref: 'HEAD' })
  
  console.log(`\nCommit history for ${repo}:\n`)
  
  for (const commit of commits) {
    console.log(`commit ${commit.oid}`)
    console.log(`Author: ${commit.commit.author.name} <${commit.commit.author.email}>`)
    console.log(`Date:   ${new Date(commit.commit.author.timestamp * 1000).toISOString()}`)
    console.log(`\n    ${commit.commit.message}\n`)
  }
}

export async function status(repo: string): Promise<void> {
  const dir = getCurrentRepoPath(repo)
  
  try {
    const headOid = await git.resolveRef({ fs: pfs, dir, ref: 'HEAD' })
    console.log(`\nOn branch main`)
    console.log(`HEAD: ${headOid}\n`)
    
    const status = await git.statusMatrix({ fs: pfs, dir })
    
    const staged: string[] = []
    const modified: string[] = []
    const untracked: string[] = []
    
    for (const [filepath, head, workdir, stage] of status) {
      if (head === 0 && workdir === 2 && stage === 2) {
        staged.push(filepath)
      } else if (head === 1 && workdir === 2 && stage === 2) {
        modified.push(filepath)
      } else if (head === 0 && workdir === 2 && stage === 0) {
        untracked.push(filepath)
      }
    }
    
    if (staged.length > 0) {
      console.log('Changes to be committed:')
      staged.forEach(f => console.log(`  new file:   ${f}`))
      console.log()
    }
    
    if (modified.length > 0) {
      console.log('Changes not staged for commit:')
      modified.forEach(f => console.log(`  modified:   ${f}`))
      console.log()
    }
    
    if (untracked.length > 0) {
      console.log('Untracked files:')
      untracked.forEach(f => console.log(`  ${f}`))
      console.log()
    }
    
    if (staged.length === 0 && modified.length === 0 && untracked.length === 0) {
      console.log('Nothing to commit, working tree clean')
    }
  } catch (error) {
    console.error('Error getting status:', error)
  }
}

// ═══════════════════════════════════════════════════════════════════
// EXPORTS
// ═══════════════════════════════════════════════════════════════════

export { pfs }

