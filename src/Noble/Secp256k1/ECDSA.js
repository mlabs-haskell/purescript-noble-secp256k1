const lib = require("@noble/secp256k1");

exports.getECDSAPublicKey = privateKey => isCompressed =>
  lib.getPublicKey(privateKey, isCompressed);

exports._sign = msgHash => privateKey => der => () =>
    lib.sign(msgHash, privateKey, {der});

exports._signWithRecoveredBit =
    tuple => msgHash => privateKey => der => () =>
        lib.sign(msgHash, privateKey, { recovered: true, der }).then(res => tuple(res[0])(res[1]));

exports.verifyECDSA = signature => msgHash => publicKey =>
  lib.verify(signature, msgHash, publicKey);

exports.getECDSASharedSecret = privateKey => publicKey => isCompressed =>
  lib.getSharedSecret(privateKey, publicKey, isCompressed);

exports.recoverECDSAPublicKey =
  msgHash => signature => recovery => isCompressed =>
    lib.recoverPublicKey(msgHash, signature, recovery, isCompressed);

exports._isValidPrivateKey = privateKey =>
  lib.utils.isValidPrivateKey(privateKey);
