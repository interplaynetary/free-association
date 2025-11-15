Holster is a real-time data synchronisation service that seamlessly connects
devices using Node.js, Deno, Bun or the browser. Built with modern ES modules,
it features end-to-end encryption, intelligent conflict resolution, and
cross-platform compatibility.

✨ Real-time sync across all connected devices\
🔐 Built-in encryption with user authentication\
⚡ Zero configuration with smart performance optimisation\
🌐 Universal compatibility - works everywhere JavaScript runs

Try it out at [holster.haza.website](https://holster.haza.website)!

A build version of Holster is also provided using
[esbuild](https://esbuild.github.io), to run in production.

Check out the [Github Wiki](https://github.com/mblaney/holster/wiki) for how
to get started using the API, and for more information.

### Quick Start

- Clone this repo and run `npm install`.
- Run the server with `node src/index.js`.
- Open `http://localhost:3000/examples/index.html` in the browser.
- You will then also have access to the Holster API via the `holster` object in
the console.

### Development

- When modifying src files run: `npx prettier src --write && npm run build`
- When modifying tests run: `npx prettier test --write`
- To run the tests use: `npm run test`
