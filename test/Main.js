import crypto from "crypto";

export const randomBytes = x => new Uint8Array(crypto.randomBytes(x));
