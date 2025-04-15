If you're trying to use GUN with vite build tool, you can bump into a couple of issues.

First is the fact that vite pre-bundles the dependencies, so you can't import something from inside of them. That's exactly what we do when we import SEA from 'gun/sea' and all the gun/libs too. So you need to add a list of included optimized dependencies to vite.config.js for vite to know that you'll need their insides.

Second - there's the problem with vite including the 'text-encoding' node polyfill that is required by SEA.js in node environment and is not needed in modern browsers. It unnecessary blows up the file size of your js bundle by +700kB. We don't want that. So first step is to exclude the package from the build. This works, but you'll encounter an error of a wrong import. So here comes the solution noted by Evan You himself. We just create a custom plugin, that properly removes the unnecessary extension.

With the help of @jojobyte in Gun discord channel we figured out a rather universal function to do the job. So here's the final vite.config.js that makes Gun play nice with vite:

const moduleExclude = match => {
  const m = id => id.indexOf(match) > -1
  return {
    name: `exclude-${match}`,
    resolveId(id) {
      if (m(id)) return id
    },
    load(id) {
      if (m(id)) return `export default {}`
    },
  }
}

export default {
  optimizeDeps: {
    include: [
      'gun',
      'gun/gun',
      'gun/sea',
      'gun/sea.js',
      'gun/lib/then',
      'gun/lib/webrtc',
      'gun/lib/radix',
      'gun/lib/radisk',
      'gun/lib/store',
      'gun/lib/rindexed',
    ],
  },
  plugins: [
    moduleExclude('text-encoding'),
  ],
}
Then you'll need to import gun and all the libs in your app.

import Gun from 'gun/gun'
import SEA from 'gun/sea.js'
import 'gun/lib/radix'
import 'gun/lib/radisk'
import 'gun/lib/store'
import 'gun/lib/rindexed'
import 'gun/lib/webrtc'
import 'gun/nts'

export const gun = Gun({peers: ['https://your.gun.peer'], localStorage:false})
And you're good to go! You can clone a @gun-vue/demo template to have an easy start. It also includes some very useful vite plugins pre-configured. If you manage to improve the setup - please send a PR to the repo.