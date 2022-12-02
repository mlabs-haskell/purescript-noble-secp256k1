const lib = require('@noble/secp256k1');

exports.randomPrivateKey = () => lib.utils.randomPrivateKey();
exports._sha256 = bytes => () => lib.utils.sha256(bytes);
