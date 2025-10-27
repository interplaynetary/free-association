import SeaArray from "./array.js"

// Security constants
const SECURITY_LIMITS = {
  MAX_BUFFER_SIZE: 10 * 1024 * 1024, // 10MB limit
  MAX_STRING_LENGTH: 1024 * 1024, // 1MB string limit
}

// Secure Buffer implementation for SEA.js
// Compatible with NodeJS safe-buffer but with enhanced security
function SafeBuffer(...props) {
  console.warn("new SafeBuffer() is deprecated, please use SafeBuffer.from()")
  return SafeBuffer.from(...props)
}

SafeBuffer.prototype = Object.create(Array.prototype)

// Input validation helpers
const validateStringInput = (input, encoding) => {
  if (typeof input !== "string") {
    throw new TypeError("Input must be a string for encoding: " + encoding)
  }

  if (input.length > SECURITY_LIMITS.MAX_STRING_LENGTH) {
    throw new RangeError(
      `String too large: ${input.length} > ${SECURITY_LIMITS.MAX_STRING_LENGTH}`,
    )
  }
}

const validateBufferSize = (size, operation = "buffer operation") => {
  if (size > SECURITY_LIMITS.MAX_BUFFER_SIZE) {
    throw new RangeError(
      `Buffer size too large for ${operation}: ${size} > ${SECURITY_LIMITS.MAX_BUFFER_SIZE}`,
    )
  }
}

// Secure hex parsing
const parseHexString = hexString => {
  validateStringInput(hexString, "hex")

  // Remove any whitespace and convert to lowercase
  const cleanHex = hexString.replace(/\s/g, "").toLowerCase()

  // Validate hex format: only 0-9, a-f characters
  if (!/^[0-9a-f]*$/.test(cleanHex)) {
    throw new TypeError("Invalid hex string: contains non-hex characters")
  }

  // Must be even length
  if (cleanHex.length % 2 !== 0) {
    throw new TypeError("Invalid hex string: must have even length")
  }

  if (cleanHex.length === 0) {
    return new Uint8Array(0)
  }

  validateBufferSize(cleanHex.length / 2, "hex parsing")

  const bytes = new Uint8Array(cleanHex.length / 2)
  for (let i = 0; i < cleanHex.length; i += 2) {
    const hexByte = cleanHex.substr(i, 2)
    bytes[i / 2] = parseInt(hexByte, 16)
  }
  return bytes
}

const encodeUtf8String = str => {
  validateStringInput(str, "utf8")

  try {
    // Use standard TextEncoder for proper UTF-8 encoding
    const encoder = new TextEncoder()
    const bytes = encoder.encode(str)

    validateBufferSize(bytes.length, "UTF-8 encoding")
    return bytes
  } catch (err) {
    throw new TypeError("Failed to encode UTF-8 string: " + err.message)
  }
}

const encodeBinaryString = str => {
  validateStringInput(str, "binary")

  validateBufferSize(str.length, "binary encoding")

  const bytes = new Uint8Array(str.length)
  for (let i = 0; i < str.length; i++) {
    const charCode = str.charCodeAt(i)

    // Binary strings should only contain bytes (0-255)
    if (charCode > 255) {
      throw new TypeError(
        `Invalid binary string: character at position ${i} exceeds byte range (${charCode} > 255)`,
      )
    }

    bytes[i] = charCode
  }
  return bytes
}

// Secure base64 decoding
const parseBase64String = base64String => {
  validateStringInput(base64String, "base64")

  // Remove any whitespace
  const cleanBase64 = base64String.replace(/\s/g, "")

  // Validate base64 format
  if (!/^[A-Za-z0-9+/]*={0,2}$/.test(cleanBase64)) {
    throw new TypeError("Invalid base64 string: contains invalid characters")
  }

  try {
    const decodedString = atob(cleanBase64)
    validateBufferSize(decodedString.length, "base64 decoding")

    const bytes = new Uint8Array(decodedString.length)
    for (let i = 0; i < decodedString.length; i++) {
      bytes[i] = decodedString.charCodeAt(i)
    }

    return bytes
  } catch (err) {
    throw new TypeError("Failed to decode base64 string: " + err.message)
  }
}

// Secure array-like input processing
const processArrayLikeInput = input => {
  let bytes

  if (input instanceof ArrayBuffer) {
    validateBufferSize(input.byteLength, "ArrayBuffer processing")
    bytes = new Uint8Array(input)
  } else if (ArrayBuffer.isView(input)) {
    validateBufferSize(
      input.byteLength || input.length,
      "TypedArray processing",
    )
    bytes = new Uint8Array(input.buffer, input.byteOffset, input.byteLength)
  } else if (Array.isArray(input)) {
    validateBufferSize(input.length, "Array processing")

    // Validate all array elements are valid byte values
    for (let i = 0; i < input.length; i++) {
      const val = input[i]
      if (
        typeof val !== "number" ||
        val < 0 ||
        val > 255 ||
        !Number.isInteger(val)
      ) {
        throw new TypeError(
          `Invalid byte value at index ${i}: ${val} (must be integer 0-255)`,
        )
      }
    }

    bytes = new Uint8Array(input)
  } else if (
    typeof input === "object" &&
    input !== null &&
    typeof input.length === "number"
  ) {
    // Handle array-like objects
    validateBufferSize(input.length, "array-like processing")

    const arr = Array.from(input)
    for (let i = 0; i < arr.length; i++) {
      const val = arr[i]
      if (
        typeof val !== "number" ||
        val < 0 ||
        val > 255 ||
        !Number.isInteger(val)
      ) {
        throw new TypeError(
          `Invalid byte value at index ${i}: ${val} (must be integer 0-255)`,
        )
      }
    }

    bytes = new Uint8Array(arr)
  } else {
    throw new TypeError("Unsupported input type: " + typeof input)
  }

  return bytes
}

Object.assign(SafeBuffer, {
  from() {
    const argCount = arguments.length

    if (argCount === 0) {
      throw new TypeError("SafeBuffer.from() requires at least one argument")
    }

    const input = arguments[0]

    if (input === null || input === undefined) {
      throw new TypeError("First argument must not be null or undefined")
    }

    let bytes

    if (typeof input === "string") {
      const encoding = (arguments[1] || "utf8").toLowerCase()

      switch (encoding) {
        case "hex":
          bytes = parseHexString(input)
          break

        case "utf8":
        case "utf-8":
          bytes = encodeUtf8String(input)
          break

        case "binary":
        case "latin1":
          bytes = encodeBinaryString(input)
          break

        case "base64":
          bytes = parseBase64String(input)
          break

        case "ascii":
          // ASCII is subset of UTF-8, but validate range
          validateStringInput(input, "ascii")
          for (let i = 0; i < input.length; i++) {
            if (input.charCodeAt(i) > 127) {
              throw new TypeError(
                `Invalid ASCII character at position ${i}: ${input.charCodeAt(i)} > 127`,
              )
            }
          }
          bytes = encodeUtf8String(input) // Safe since ASCII is UTF-8 subset
          break

        default:
          throw new TypeError("Unknown encoding: " + encoding)
      }
    } else {
      bytes = processArrayLikeInput(input)
    }

    if (!bytes || bytes.length === 0) {
      return SeaArray.from(new Uint8Array(0))
    }

    try {
      return SeaArray.from(bytes)
    } catch (err) {
      throw new Error("Failed to create SafeBuffer: " + err.message)
    }
  },

  alloc(length, fill = 0) {
    if (typeof length !== "number" || !Number.isInteger(length) || length < 0) {
      throw new TypeError("Length must be a non-negative integer")
    }

    validateBufferSize(length, "buffer allocation")

    // Validate fill value
    if (typeof fill === "number") {
      if (fill < 0 || fill > 255 || !Number.isInteger(fill)) {
        throw new TypeError("Fill value must be an integer between 0 and 255")
      }
    } else if (typeof fill === "string") {
      if (fill.length !== 1) {
        throw new TypeError("Fill string must be exactly one character")
      }
      fill = fill.charCodeAt(0)
      if (fill > 255) {
        throw new TypeError("Fill character code must be <= 255")
      }
    } else {
      throw new TypeError("Fill must be a number or single character string")
    }

    try {
      const buffer = new Uint8Array(length)
      if (fill !== 0) {
        buffer.fill(fill)
      }
      return SeaArray.from(buffer)
    } catch (err) {
      throw new Error("Failed to allocate buffer: " + err.message)
    }
  },

  concat(arr) {
    if (!Array.isArray(arr)) {
      throw new TypeError("First argument must be an array")
    }

    if (arr.length === 0) {
      return SeaArray.from(new Uint8Array(0))
    }

    // Validate all array elements and calculate total size
    let totalLength = 0
    const validatedBuffers = []

    for (let i = 0; i < arr.length; i++) {
      const item = arr[i]

      if (!item) {
        throw new TypeError(`Array element at index ${i} is null or undefined`)
      }

      let bytes
      try {
        bytes = processArrayLikeInput(item)
      } catch (err) {
        throw new TypeError(
          `Invalid array element at index ${i}: ${err.message}`,
        )
      }

      totalLength += bytes.length
      validateBufferSize(totalLength, "buffer concatenation")

      validatedBuffers.push(bytes)
    }

    try {
      const result = new Uint8Array(totalLength)
      let offset = 0

      for (const buffer of validatedBuffers) {
        result.set(buffer, offset)
        offset += buffer.length
      }

      return SeaArray.from(result)
    } catch (err) {
      throw new Error("Failed to concatenate buffers: " + err.message)
    }
  },

  // Helper method to check if object is SafeBuffer
  isBuffer(obj) {
    return (
      obj instanceof SeaArray ||
      (obj && typeof obj === "object" && obj.constructor === SafeBuffer)
    )
  },

  // Get byte length of string in specific encoding
  byteLength(string, encoding = "utf8") {
    if (typeof string !== "string") {
      throw new TypeError("First argument must be a string")
    }

    switch (encoding.toLowerCase()) {
      case "utf8":
      case "utf-8":
        return new TextEncoder().encode(string).length

      case "ascii":
      case "binary":
      case "latin1":
        return string.length

      case "base64":
        const clean = string.replace(/\s/g, "")
        return (
          Math.floor((clean.length * 3) / 4) - (clean.match(/=/g) || []).length
        )

      case "hex":
        return Math.floor(string.replace(/\s/g, "").length / 2)

      default:
        throw new TypeError("Unknown encoding: " + encoding)
    }
  },
})

SafeBuffer.prototype.from = SafeBuffer.from
SafeBuffer.prototype.toString = function (
  encoding = "utf8",
  start = 0,
  end = this.length,
) {
  // Input validation
  if (typeof encoding !== "string") {
    throw new TypeError("Encoding must be a string")
  }

  if (typeof start !== "number" || !Number.isInteger(start) || start < 0) {
    throw new TypeError("Start must be a non-negative integer")
  }

  if (typeof end !== "number" || !Number.isInteger(end) || end < start) {
    throw new TypeError("End must be an integer >= start")
  }

  // Delegate to SeaArray with validation
  try {
    return SeaArray.prototype.toString.call(this, encoding, start, end)
  } catch (err) {
    throw new Error("Failed to convert buffer to string: " + err.message)
  }
}

export default SafeBuffer
