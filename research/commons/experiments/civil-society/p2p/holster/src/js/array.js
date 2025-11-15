if (typeof btoa === "undefined") {
  globalThis.btoa = data => Buffer.from(data, "binary").toString("base64")
  globalThis.atob = data => Buffer.from(data, "base64").toString("binary")
}

// This is Array extended to have .toString(["utf8"|"hex"|"base64"])
function SeaArray() {}
Object.assign(SeaArray, {from: Array.from})
SeaArray.prototype = Object.create(Array.prototype)

SeaArray.prototype.toString = function (enc, start, end) {
  if (!enc) enc = "utf8"
  if (!start) start = 0

  end = end ? Math.min(Math.max(end, start), this.length) : this.length

  if (enc === "hex") {
    const buf = new Uint8Array(this.slice(start, end))
    return Array.from(buf, byte => byte.toString(16).padStart(2, "0")).join("")
  }

  if (enc === "utf8") {
    return Array.from({length: end - start}, (_, i) => {
      const charCode = this[i + start]
      // Use unicode replacement character for invalid character codes.
      if (charCode < 0 || charCode > 0x10ffff) {
        return String.fromCharCode(0xfffd)
      }
      return String.fromCharCode(charCode)
    }).join("")
  }

  if (enc === "base64") {
    const utf8String = Array.from({length: end - start}, (_, i) => {
      const charCode = this[i + start]
      if (charCode < 0 || charCode > 255) {
        return String.fromCharCode(0xfffd)
      }
      return String.fromCharCode(charCode)
    }).join("")
    return btoa(utf8String)
  }
}

export default SeaArray
