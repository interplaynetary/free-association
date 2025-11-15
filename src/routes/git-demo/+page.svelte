<script lang="ts">
  import * as git from '$lib/git'
  import { holsterUser } from '$lib/network/holster'
  
  let repo = $state('demo-app')
  let filename = $state('data.json')
  let content = $state('{"message": "Hello from Holster + Git!"}')
  let commitMessage = $state('Save data')
  let pullFromPubkey = $state('')
  let commits = $state<any[]>([])
  let status = $state<any>(null)
  let output = $state('')
  let loading = $state(false)
  
  async function handleInit() {
    try {
      loading = true
      output = 'Initializing repository...'
      await git.initRepo(repo)
      output = `✓ Repository '${repo}' initialized!`
    } catch (error) {
      output = `✗ Error: ${error instanceof Error ? error.message : String(error)}`
    } finally {
      loading = false
    }
  }
  
  async function handleWrite() {
    try {
      loading = true
      output = 'Writing file...'
      await git.writeFile(repo, filename, content)
      output = `✓ File '${filename}' written!`
    } catch (error) {
      output = `✗ Error: ${error instanceof Error ? error.message : String(error)}`
    } finally {
      loading = false
    }
  }
  
  async function handleAdd() {
    try {
      loading = true
      output = 'Staging file...'
      await git.addFile(repo, filename)
      output = `✓ File '${filename}' staged!`
      await updateStatus()
    } catch (error) {
      output = `✗ Error: ${error instanceof Error ? error.message : String(error)}`
    } finally {
      loading = false
    }
  }
  
  async function handleCommit() {
    try {
      loading = true
      output = 'Committing...'
      const sha = await git.commit(repo, commitMessage)
      output = `✓ Committed: ${sha.slice(0, 7)}`
      await updateStatus()
      await updateLog()
    } catch (error) {
      output = `✗ Error: ${error instanceof Error ? error.message : String(error)}`
    } finally {
      loading = false
    }
  }
  
  async function handlePush() {
    try {
      loading = true
      output = 'Pushing to Holster...'
      const sha = await git.push(repo, (current, total) => {
        output = `Pushing... ${current}/${total} objects`
      })
      output = `✓ Pushed to Holster! HEAD: ${sha.slice(0, 7)}`
    } catch (error) {
      output = `✗ Error: ${error instanceof Error ? error.message : String(error)}`
    } finally {
      loading = false
    }
  }
  
  async function handlePull() {
    try {
      loading = true
      output = 'Pulling from Holster...'
      await git.pull(repo, pullFromPubkey || undefined, (current, total) => {
        output = `Pulling... ${current}/${total} objects`
      })
      output = `✓ Pulled from Holster!`
      await updateLog()
    } catch (error) {
      output = `✗ Error: ${error instanceof Error ? error.message : String(error)}`
    } finally {
      loading = false
    }
  }
  
  async function updateStatus() {
    try {
      status = await git.status(repo)
    } catch (error) {
      console.error('Error getting status:', error)
    }
  }
  
  async function updateLog() {
    try {
      commits = await git.log(repo, 10)
    } catch (error) {
      console.error('Error getting log:', error)
      commits = []
    }
  }
</script>

<div class="container max-w-6xl mx-auto p-8">
  <h1 class="text-4xl font-bold mb-2">Holster + Git Demo</h1>
  <p class="text-gray-600 mb-8">Browser-based Git operations with distributed Holster storage</p>
  
  {#if !holsterUser.is}
    <div class="bg-yellow-50 border-l-4 border-yellow-400 p-4 mb-8">
      <p class="text-yellow-700">
        ⚠️ You need to be authenticated to use Git operations.
        Please log in first.
      </p>
    </div>
  {:else}
    <div class="bg-green-50 border-l-4 border-green-400 p-4 mb-8">
      <p class="text-green-700">
        ✓ Authenticated as <strong>{holsterUser.is.username}</strong>
      </p>
      <p class="text-xs text-green-600 mt-1 font-mono">
        {holsterUser.is.pub.slice(0, 40)}...
      </p>
    </div>
  {/if}
  
  <div class="grid grid-cols-1 lg:grid-cols-2 gap-8">
    <!-- Left Column: Controls -->
    <div class="space-y-6">
      <div class="bg-white shadow-md rounded-lg p-6">
        <h2 class="text-2xl font-semibold mb-4">Repository</h2>
        
        <div class="space-y-4">
          <div>
            <label class="block text-sm font-medium mb-2">Repo Name</label>
            <input 
              bind:value={repo}
              class="w-full px-3 py-2 border rounded-md"
              placeholder="my-repo"
            />
          </div>
          
          <button 
            onclick={handleInit}
            disabled={loading || !holsterUser.is}
            class="w-full bg-blue-600 text-white px-4 py-2 rounded-md hover:bg-blue-700 disabled:opacity-50"
          >
            Initialize Repository
          </button>
        </div>
      </div>
      
      <div class="bg-white shadow-md rounded-lg p-6">
        <h2 class="text-2xl font-semibold mb-4">Make Changes</h2>
        
        <div class="space-y-4">
          <div>
            <label class="block text-sm font-medium mb-2">File Name</label>
            <input 
              bind:value={filename}
              class="w-full px-3 py-2 border rounded-md"
              placeholder="data.json"
            />
          </div>
          
          <div>
            <label class="block text-sm font-medium mb-2">Content</label>
            <textarea 
              bind:value={content}
              rows="4"
              class="w-full px-3 py-2 border rounded-md font-mono text-sm"
              placeholder='{"key": "value"}'
            ></textarea>
          </div>
          
          <button 
            onclick={handleWrite}
            disabled={loading || !holsterUser.is}
            class="w-full bg-purple-600 text-white px-4 py-2 rounded-md hover:bg-purple-700 disabled:opacity-50"
          >
            Write File
          </button>
          
          <button 
            onclick={handleAdd}
            disabled={loading || !holsterUser.is}
            class="w-full bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 disabled:opacity-50"
          >
            Stage File (git add)
          </button>
        </div>
      </div>
      
      <div class="bg-white shadow-md rounded-lg p-6">
        <h2 class="text-2xl font-semibold mb-4">Commit & Sync</h2>
        
        <div class="space-y-4">
          <div>
            <label class="block text-sm font-medium mb-2">Commit Message</label>
            <input 
              bind:value={commitMessage}
              class="w-full px-3 py-2 border rounded-md"
              placeholder="Save changes"
            />
          </div>
          
          <button 
            onclick={handleCommit}
            disabled={loading || !holsterUser.is}
            class="w-full bg-green-600 text-white px-4 py-2 rounded-md hover:bg-green-700 disabled:opacity-50"
          >
            Commit
          </button>
          
          <button 
            onclick={handlePush}
            disabled={loading || !holsterUser.is}
            class="w-full bg-orange-600 text-white px-4 py-2 rounded-md hover:bg-orange-700 disabled:opacity-50"
          >
            Push to Holster
          </button>
          
          <div class="border-t pt-4">
            <label class="block text-sm font-medium mb-2">Pull from Public Key (optional)</label>
            <input 
              bind:value={pullFromPubkey}
              class="w-full px-3 py-2 border rounded-md font-mono text-xs"
              placeholder="SE2G7abc123..."
            />
            <button 
              onclick={handlePull}
              disabled={loading || !holsterUser.is}
              class="w-full mt-2 bg-teal-600 text-white px-4 py-2 rounded-md hover:bg-teal-700 disabled:opacity-50"
            >
              Pull from Holster
            </button>
          </div>
        </div>
      </div>
      
      <div class="bg-white shadow-md rounded-lg p-6">
        <h2 class="text-2xl font-semibold mb-4">Actions</h2>
        <div class="space-y-2">
          <button 
            onclick={updateStatus}
            disabled={loading || !holsterUser.is}
            class="w-full bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 disabled:opacity-50"
          >
            Refresh Status
          </button>
          <button 
            onclick={updateLog}
            disabled={loading || !holsterUser.is}
            class="w-full bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 disabled:opacity-50"
          >
            Refresh Log
          </button>
        </div>
      </div>
    </div>
    
    <!-- Right Column: Output -->
    <div class="space-y-6">
      <div class="bg-white shadow-md rounded-lg p-6">
        <h2 class="text-2xl font-semibold mb-4">Output</h2>
        <div class="bg-gray-900 text-green-400 p-4 rounded-md font-mono text-sm min-h-[100px]">
          {#if loading}
            <div class="animate-pulse">⏳ {output}</div>
          {:else if output}
            {output}
          {:else}
            <span class="text-gray-600">Ready.</span>
          {/if}
        </div>
      </div>
      
      {#if status}
        <div class="bg-white shadow-md rounded-lg p-6">
          <h2 class="text-2xl font-semibold mb-4">Repository Status</h2>
          
          {#if status.staged.length > 0}
            <div class="mb-4">
              <h3 class="font-semibold text-green-600 mb-2">Staged (ready to commit)</h3>
              <ul class="list-disc list-inside text-sm">
                {#each status.staged as file}
                  <li class="text-green-700">{file}</li>
                {/each}
              </ul>
            </div>
          {/if}
          
          {#if status.modified.length > 0}
            <div class="mb-4">
              <h3 class="font-semibold text-yellow-600 mb-2">Modified (not staged)</h3>
              <ul class="list-disc list-inside text-sm">
                {#each status.modified as file}
                  <li class="text-yellow-700">{file}</li>
                {/each}
              </ul>
            </div>
          {/if}
          
          {#if status.untracked.length > 0}
            <div class="mb-4">
              <h3 class="font-semibold text-gray-600 mb-2">Untracked</h3>
              <ul class="list-disc list-inside text-sm">
                {#each status.untracked as file}
                  <li class="text-gray-700">{file}</li>
                {/each}
              </ul>
            </div>
          {/if}
          
          {#if status.staged.length === 0 && status.modified.length === 0 && status.untracked.length === 0}
            <p class="text-gray-500 text-sm">Working tree clean</p>
          {/if}
        </div>
      {/if}
      
      {#if commits.length > 0}
        <div class="bg-white shadow-md rounded-lg p-6">
          <h2 class="text-2xl font-semibold mb-4">Commit History</h2>
          <div class="space-y-4">
            {#each commits as commit}
              <div class="border-l-4 border-blue-500 pl-4 py-2">
                <div class="font-mono text-sm text-gray-600">
                  {commit.oid.slice(0, 7)}
                </div>
                <div class="font-semibold mt-1">
                  {commit.commit.message}
                </div>
                <div class="text-sm text-gray-600 mt-1">
                  {commit.commit.author.name} &lt;{commit.commit.author.email}&gt;
                </div>
                <div class="text-xs text-gray-500 mt-1">
                  {new Date(commit.commit.author.timestamp * 1000).toLocaleString()}
                </div>
              </div>
            {/each}
          </div>
        </div>
      {/if}
    </div>
  </div>
</div>

<style>
  .container {
    font-family: system-ui, -apple-system, sans-serif;
  }
</style>

