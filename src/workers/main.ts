import Worker from "web-worker";

const url = new URL("./worker.js", import.meta.url);
const worker = new Worker(url);

worker.addEventListener("message", (e) => {
  console.log(e.data); // "hiya!"
});

worker.postMessage("hello");

/*

- Spawning a Web Worker:
Can I use GUN inside a Web Worker #
Yes! You just need to fake the window object and then load all the scripts with importScipts(). Web Workers don't have access to localStorage, so you'll have to use indexedDB RAD adapter to be able to persist data in the browser IndexedDB. So you can have a separate thread to handle GUN requests. But mind that a worker will have to load and execute all those scripts at the start and that may create a delay before the first message being processed.


const window = {
  crypto: self.crypto,
  TextEncoder: self.TextEncoder,
  TextDecoder: self.TextDecoder,
  WebSocket: self.WebSocket,
};

importScripts("../js/gun.min.js");
importScripts("../js/sea.js");
importScripts("../js/radix.js");
importScripts("../js/radisk.js");
importScripts("../js/store.js");
importScripts("../js/rindexed.js");

const gun = new window.Gun({ localStorage: false });

onmessage = (e) => {
  gun
    .get(e.data)
    .map()
    .on((d, k) => {
      postMessage({ k, d });
    });
};
*/

/*
This article is about working with nested stores, or stores of stores, or reactive arrays of stores, or whatever you would want to call them. Basicly this:

const stores = writable([writable(0), writable(0)]);
Why would you want this? Weeell, that part is up to you.. But if you find yourself doing this, stop for a second and consider if you are over-complicating things or if you really need this. Maybe there is an easier way to set up things.

Super quickly about THE BEST FRAMEWORK ❤️
Svelte.js is a javascript framework, that I have fallen completely in love with, and I will allow myself to be super opinionated about it :). It is THE BEST framework, because it is super fast. It’s fast because it is clever about figuring out how reactive values depend on each other at compile time, rather than just updating everything all the time.

Svelte shifts as much work as possible out of the browser and into your build step. No more manual optimisations — just faster, more efficient apps. — svelte.dev

It’s kind off the “keep it simple, stupid” of javascript frameworks.

Super quickly about svelte stores
If you have never heard of svelte stores, chances are this article is not for you. Go use them, I’ll be here if you wanna come back some day! :)

Just to remind ourselves. A store is something that allow us to subscribe to data like this:

const number = writable(0);
number.subscribe(value => console.log(value)); // will log out 42.
number.set(42);
The “problem”
I have recently been working on a project that has a lot of reactive data (using a streaming database, that is why). Here I ended up with reactive arrays (or stores) that contained objects with stores. Example:

const animals = writable([new Animal(), new Animal()]);
/// ...
class Animal {
  name = writable("Fido");
  age = writable(14);
}
Say we wanted to create a derived store (a store based on other stores) that calculates the total age of all Animal classes in the animals store. We now have a problem because a derived store, needs to know how many stores it need to derive from at the time of creation.

const summed = derived([a, b], ([$a, $b]) => $a + $b);
In the example above, a and b are stores, that are used to compute a third store — summed. When the value of a and/or b changes, the summed store will also be updated.

The problem with our animals example, is that we are working with a store that is containing an array og variable length. The array may be changed at run time.

The problem we are trying to solve is this:

Recompute the value of the stores in the animals store, whenever the animals store changes, or any stores currently referenced by the animals store changes.

Disclaimer about the solution
Making such a nested store, will probably lead to a lot of calculations, because we are re-calculating the store whenever any of the data in it changes.

If you are working with longer arrays, or the calculations are a bit on the heavy side, consider running the calculations off the main thread in a web worker. This will keep your app from stalling.

The solution I provide here, uses a scheduler class. So named, because it schedules updates of the store.

The solution
Goal: Calculate the total age of all Animal classes in the animals store.

To do this we create a SumSchedulerclass. The class will contain a sum store, that will hold our value.

class SumScheduler {
  sum = writable(0);
}
Next we should think of a way to add and remove stores from the scheduler. When a store is added, the scheduler should listen to changes on the store, and schedule update calls whenever a store value changes.

// in SumScheduler
add = (number_store) => {
    const uuid = crypto.randomUUID();
    this.sum_map.set(uuid, number_store); // is a Map object on the class.

    number_store.subscribe(() => {
        this.schedule();
    })
}
Here I am using crypto.randomUUID to generate a random key for the map. This might be overkill, and you could do this in another way if you want. You could use a Set, an array, or generate a random key in another way. The important part is that we are saving the store in a way, so that we can iterate our added stores later on.

We also need to be able to remove an added store, if we no longer wish to update the sum based on it. Let’s have our add function return an Unsubscriber function.

add = (number_store) => {
  // ... other code
  const unsub = number_store.subscribe(() => {
      this.schedule();
  })

  return () => {
      unsub();
      this.sum_map.delete(uuid);
  }
}
Last thing is to update our sum store:

// in SumScheduler
update = () => {
  // get current values of the added stores.
  const static_stores = [...this.sum_map.values()].map(store => get(store));
  // calculate sum
  const sum = static_stores.reduce((acc, curr) => acc + curr, 0);
  // update sum store
  this.sum.set(sum);
}
Here we make use of get(store) . get is a svelte function that will retrieve the current value of a store. Other than that we just calculate the sum and store it in the sum store on our class.

Optimizations…?
You will notice I left out the scheduler. Technically you don’t need it, if you just want a working example. Also, im running the calculations in the class, instead of using a web worker to handle the calculations off the main thread. Let me know if you need an article that works through that, I’m happy to make one. :)

If not, here is the complete code

Complete example
export default class SumScheduler {
    sum = writable(0);
    sum_map = new Map();

    add = (number_store) => {
        const uuid = crypto.randomUUID();
        this.sum_map.set(uuid, number_store);

        const unsub = number_store.subscribe(() => {
            this.schedule();
        })

        return () => {
            unsub();
            this.sum_map.delete(uuid);
        }
    };

    schedule(){
        // possibly run schedule logic here
        this.update();
    }

    update(){
        const static_stores = [...this.sum_map.values()].map(store => get(store));

        const sum = static_stores.reduce((acc, curr) => acc + curr, 0);
        this.sum.set(sum);
    }


}
const a = writable(0);
const b = writable(0);

// using the class
const scheduler = new SumScheduler();
schedule.sum.subscribe((sum) => {
  console.log(sum) // will log out 0, 1, 3
})

// adding stores
const unsub_a = scheduler.add(a);
scheduler.add(b);

// update underlying stores
a.set(1);
b.set(2);

// remove
unsub_a();
a.set(1000); // will not be included in sum any longer


*/
