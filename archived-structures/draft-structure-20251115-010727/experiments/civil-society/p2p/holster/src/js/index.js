import express from "express"
import path from "path"
import {fileURLToPath} from "url"
import Holster from "../holster.js"

const holster = Holster()

// Note that express is only used here to serve the examples folder. It is not
// required to run Holster itself.
const dirname = path.dirname(fileURLToPath(import.meta.url))

const app = express()

app.use(express.static(path.join(dirname, "..")))

app.get("/", (req, res) => {
  res.redirect("/examples/index.html")
})

app.listen(3000)
