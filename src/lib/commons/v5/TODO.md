- refine protocol so that its tree only? depracated utilites in there which are replaced by those in match
- create simple server that is a user / relay and generically puts data into its space
- remove unecessary wrappers around zod safe parse
- transition collective to v5
- commitments with hashing /itc on consuming inputs?

rss stream server

initializeHolster should be the same abstraction across our whole app
get rssstream to use /middleware /rate-limit maybe even /auth
fix type errors

get the holster relay to run on server start up? am not sure hooks.server.ts is the place for this

Make issues for:
- standardize translations for international support
- decider route

Make tests for holster folder ts

Reimplement changePassword in Header.svelte