const lib = require('@noble/secp256k1');

exports._sign = message => privateKey => () =>
    lib.schnorr.sign(message, privateKey);

exports._verify = signature => message => publicKey => () =>
    lib.schnorr.verify(signature, message, publicKey);

exports.getSchnorrPublicKey = privateKey => lib.schnorr.getPublicKey(privateKey);

exports._showBytes = bytes => '[ ' + bytes.join(', ') + ' ]';
