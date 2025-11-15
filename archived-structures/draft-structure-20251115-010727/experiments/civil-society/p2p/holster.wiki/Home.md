## Introduction
Holster is a service for synchronising data between devices using Node, Deno, Bun or the browser.

It provides access to an [API](https://github.com/mblaney/holster/wiki/API) which has functions to `get()` and `put()` data, and `on()` to listen for updates. It can write data to _"public space"_, which should only be used for testing or private applications, since it's available to all clients to update. It can also write data to _"user space"_ using the [User API](https://github.com/mblaney/holster/wiki/User-API), which will store data under user accounts and sign and verify all updates.

The User API relies heavily on the [SEA API](https://github.com/mblaney/holster/wiki/SEA-API) (Security, Encryption and Authorization), which is also very useful in other applications using Holster, so those functions are made available via `holster.SEA` so you don't need to include it yourself.

Holster also provides access to a lower level [Wire API](https://github.com/mblaney/holster/wiki/Wire-API), which allows access to the wire specification used by WebSockets to communicate. These functions are made available under `holster.wire`. See the [examples folder](https://github.com/mblaney/holster/tree/main/examples) for a comparison of how they can be used. There are also examples of how to use the User and SEA API's.

The Holster API itself uses the wire API, but it aims to simplify requests by chaining keys and allows storing nested data. The allowed data types  are the same as GunDB's: strings, numbers, objects, `true`, `false` and `null`. The graph data stored on disk consists of nodes referenced by id, which is what the wire API works with. This means the Holster API is responsible for resolving keys to their node ids, and creating ids for objects where they don't exist.

The wire API uses the store API, which is not exposed to the user. The store API also provides `get()` and `put()` methods which themselves call `radisk()`, the radix-on-disk function initially from [Porting Gun](https://github.com/gundb/port). I mention these layers because one of the goals of Holster is to make it easy to follow the code. Each layer can be run on it's own and has it's own tests. If there's a problem at any layer a test should be added that shows it was fixed.

## Why Holster?
Holster exists because the application I was working on that used GunDB was hitting some problems. The biggest issue seems to be that writing a lot of data to disk causes Radisk to stop returning acknowledgments and can lead to missing data or even unusable files. I spent a lot of time trying to fix this in the GunDB code and decided it wasn't possible without a lot of changes.

Fortunately, the concept of how GunDB works and a lot of the current code was also available in Porting Gun. This code is not only a lot more readable, but Mark helpfully walks through it in a video from Nordic.js. This means Holster is still an implementation of the Gun specification and aims to have the same features in the future. The main goal at the moment however is readable code and test coverage. Load testing disk writes will also be more important than adding any new features.

## Next steps

* Write more applications that user Holster. This will help make sure existing functionality is working as expected, and will show what needs to be added next.
* Introduce something like GunDB's panic tests to ensure Holster works under load testing.
* Once Holster is working as expected with a single server, look at multiple peers, `DAM` and WebRTC from GunDB.