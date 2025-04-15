Svelte
Guide 
Build a Svelte chat app in a few minutes with popular YouTube developer evangelist @fireship_dev!


Community 
Here are some community contributed libraries for combining GUN and Svelte:

This Svelte example shows how gorgeous the 2 can be integrated.

<script>
// ./src/App.svelte
import { gun } from "./svelte-gun.js"
const cats = gun.get("pets").get("cats").map()

let catName = ""
</script>

{#each $cats as [key, cat]}
    <div key={key} id={key}>{cat.name}</div>
{/each}

<input bind:value={catName} on:submit={() => cats.set({ name: catName })} />
To see this approach in action, check the Svelte REPL.

Before we show you the magic, let's go over a more verbose integration & a tutorial so you can understand what is happening.

Or if you want to start with an example chat app, check out this repo!

Guide 
Svelte has an approach to reactivity that couples very well with GUN, since you are able to use gun directly as a store within Svelte components. As a result, you can easily bind GUN data to Svelte in a way that feels like vanilla JavaScript, and is similar to the vanilla examples used in most of GUNs documentation.

Check out the examples below or try your hand in at a simple todos example with the Svelte REPL.

A note on SvelteKit 
While the examples and snippets listed here work for a Svelte setup, you will find some errors when trying them out on SvelteKit, such as this one:

window is not defined
ReferenceError: window is not defined
This is because of the SSR provided by SvelteKit, where the CLI will try to initialize Gun server-side instead of client-side. To disable it, you will have to force Gun scripts to render client-side, which is super easy to do!

For that, you have two options, in no particular order:

Method A 
Instead of doing:

// App.svelte
<script>
    import Gun from "gun/gun"
    const gun = Gun()
    ...
</script>
You can do:

<script>
    import { onMount } from 'svelte'
    import Gun from "gun/gun"

    onMount(() => {
        const gun = Gun()
    })
    ...
</script>
This works because onMount() runs only when mounting the component client-side, effectively bypassing SSR.

Method B 
Another approach is to disable SSR as specified in Sveltekit's documentation: https://kit.svelte.dev/docs/hooks#handle.

// hooks.js
** @type {import('@sveltejs/kit').Handle} */
export async function handle({ event, resolve }) {
    const response = await resolve(event, {
      ssr: !event.url.pathname.startsWith('/'),
    })
   
    return response;
}
Using SEA 
if you try to use SEA with the default rollup config you will get an error about buffer not being defined. modify your rollup.config.js so that you ensure preferBuiltins: false is set

export default {
     plugins: [
          resolve({
        browser: true,
            dedupe: importee => importee === 'svelte' || importee.startsWith('svelte/'),
        preferBuiltins: false
      }),
     ]
}
Create your own GUN bindings 
Sometimes you would rather use a store to do additional processing of Gun data, for example to render a date with a specific format.

We have two recommended approaches to bind data to GUN with custom stores. One for use directly in Svelte components and one for writing stores that can be reused across components.

Using GUN in a single component 
The recommended approach to bind GUN data directly to a Svelte component is this:

<script>
import { gun } from "./gunSetup.js" // Only initialize GUN once

// Initialize a variable to store data from GUN
let store = {}

// Set up a GUN listener to update the value
gun.get("cats").map().on(function(data, key) {
    if (data) {
        store[key] = data
    } else {
        // gun.map() can return null (deleted) values for keys
        // if so, this else clause will update your local variable
        delete store[key]
        store = store
    }
})

// Reshape data to convenient variables for the view
$: cats = Object.entries(store)
</script>

{#each cats as [key, cat]}
<div>{cat.name}</div>
{/each}
Svelte will watch for updates to the store variable,### ### ## # ### ## # and since we use the label $ with the cats variable it will update the UI when both variables changes.

This approach only works in compiled .svelte files, since Svelte wraps the variables with the invalidate() function which will tell the components to update once the variables change.

Adding a Svelte store for GUN data 
Luckily, we can add our own subscriber system in regular .js files by writing our own custom store. This approach is easy to work with, since you can easily add your own custom methods to write GUN data or even adding more gun.get() calls for nested data.

The store below is a simplified version of the official Svelte writable store. You can also check it out in the interactive demo

// stores.js
import Gun from "gun/gun"
const gun = Gun()

function customStore(ref, methods = {}) {
  const store = {}
  const subscribers = []

  // Add a listener to GUN data
  ref.on(function(data, key) {
      /* If the ref._get matches the data key it means we are getting
       * data from a call to gun.get(), and so we don't need the store
       * to be an object with nested data. Otherwise we are getting data
       * from a call to map() and should nest the data in an object
       */
      if (ref._.get === key) {
        store = data
      } else if (!data) {
        /* This clause will not work as intended on null values / false / 0
         * if you use such data consider subscribing to a parent node instead
         * eg. gun.get("temperature") instead of gun.get("temperature").get("value").
         * Or you can pass a validate() function (TODO: add example)
         */
        delete store[key]  
      } else {
         store[key] = data
      }
      // Tell each subscriber that data has been updated
      for (let i = 0; i < subscribers.length; i += 1) {
        subscribers[i](store)
      }
  })
  

  function subscribe(subscriber) {
    subscribers.push(subscriber)
    
    // Publish initial value
    subscriber(store)

    // return cleanup function to be called on component dismount
    return () => {
      const index = subscribers.indexOf(subscriber)
      if (index !== -1) {
        subscribers.splice(index, 1)
      }
      if (!subscribers.length) {
        ref.off()
      }
    }
  }

  return { ...methods, subscribe }
}

const ref = gun.get("messages")
export const messages = customStore(ref.map(), {
  add: text => ref.set({ text, sender: "moi", icon: "😺" }),
  delete: key => ref.get(key).put(null)
})
This pattern makes for very clean markup in the component itself

<script>
import { messages } from "./stores.js"
</script>

<input placeholder="write message" on:change={e => messages.add(e.target.value) && (e.target.value = "")} />

{#each Object.entries($messages) as [key, {sender, text, icon}] (key)}
<div style="padding: .4rem">
  {icon} {sender}: {text}
  <a href="#" on:click|preventDefault={() => messages.delete(key)}>delete</a>
</div>
{/each}
Tutorial 
In this tutorial we will build a very basic todo application.

Before we begin you should be familiar with Svelte and its APIs, as we will only focus on the data binding to GUN.

Step 1 - Set up GUN 
Let's create a new file called initGun.js. It's better to do this in a separate file since you only want to initialize gun once. We'll import the gun object into the Svelte components from this file.

// initGun.js
import Gun from "gun/gun"
export const gun = Gun()
// App.svelte
<script>
import { gun } from "./initGun.js"
</script>
Step 2 - Create an input to add Todos 
// App.svelte
<script>
// ...

let input = ""
const create = () => gun.get("todos").get(input).put({ title: input, done: false })
</script>

<input placeholder="Add todo" bind:value={input} />
<button on:click={() => create() && (input = "")}>Add</button>
Step 3 - List Todos from GUN 
// App.svelte
<script>
// ...

let store = {}
gun.get("todos").map().on(function(data, key) {
    if (data) {
        store[key] = data
    } else {
        // gun.map() can return null (deleted) values for keys
        // if so, this else clause will update your local variable
        delete store[key]
        store = store
    }
})

// This syntax should familiar to you if you completed the official Svelte tutorial
// We'll convert the store to a sorted list that can be iterated in the view
$: todos = Object.entries(store).sort((a, b) => a[1].done)
</script>

{#each todos as [key, todo]}
<div id={key}>
    {todo.title} {todo.done ? "😺" : "😾"}
</div>
{/each}
Step 4 - Add update and delete actions 
<script>
// ...
const update = (key, value) => gun.get("todos").get(key).get("done").put(value)
const remove = key => gun.get("todos").get(key).put(null)
</script>

{#each todos as [key, todo]}
<div id={key}>
    <input type="checkbox" checked={todo.done} on:change={() => update(key, !todo.done)} />
    {todo.title} <a href="/" on:click|preventDefault={() => remove(key)}>remove</a> {todo.done ? "😺" : "😾"}
</div>
{/each}
And now we're finished! Check it out in the Svelte REPL

Using Svelte's writable/readable stores 
You can also merge GUN with Svelte's own stores, but they are harder to work with compared to using the custom store in the (recommended) example at the top of this page.

Simple example with writable 
// stores.js
import { gun } from "./initGun.js"
import { writable } from "svelte/store"

function createStore(ref, initialValue) {
    const { set, subscribe } = writable(initialValue)
    ref.on(set)
    return {
        subscribe
    }
}

const todosRef = gun.get("todos")
export const todos = createStore(todosRef, {})
Merged with svelte's writable store 
// stores.js
import { gun } from "./initGun.js"
import { writable } from "svelte/store"

function createMapStore(ref) {
    const { update, subscribe } = writable({})
    ref.on(function(data, key) {
        if (data) {
            update(store => ({...store, [key]: data}))
        } else {
            update(store => {
                delete store[key]
                return store
            })
        }
    })        

    return {
        subscribe,
        update: (key, value) => ref.get(key).put(value)
    }
}

const todosRef = gun.get("todos").map()
export const todos = createMapStore(todosRef, {})
Magic 
Curious how the gorgeous quick start example worked? Well, it turns out Svelte looks for a .subscribe attribute, so we can trick Svelte into pulling straight from GUN by adding this special extension:

// ./src/svelte-gun.js
import Gun from "gun/gun"

Gun.chain.subscribe = function(publish) {
  var gun = this
  var at = gun._
  var isMap = !!at && !!at.back && !!at.back.each

  if (isMap) {
    var store = new Map()
    publish(Array.from(store))
    gun = gun.on((data, _key, as) => {
      var key = _key || ((data||{})._||{})['#'] || as.via.soul
      if (data === null) {
        store.delete(key)
      } else {
        store.set(key, data)
      }
      publish(Array.from(store))
    })
  } else {
    gun = gun.on(data => publish(data))
  }

  return gun.off
}
export const gun = Gun()
export default Gun
Now Svelte will automatically pull from it. Certainly makes writing GUN powered Svelte apps easy! But we hope you enjoyed both the verbose integration and the magical ones.

Anything to add? Add more to this article!