const lib = require('@noble/secp256k1');

exports.getPublicKey = privateKey => isCompressed =>
    lib.getPublicKey(privateKey, isCompressed);

exports._sign = msgHash => privateKey => () =>
    lib.sign(msgHash, privateKey);

exports._signWithRecoveredBit =
    tuple => msgHash => privateKey => () =>
        lib.sign(msgHash, privateKey, { recovered: true }).then(res => tuple(res[0])(res[1]));

exports.verify = signature => msgHash => publicKey =>
    lib.verify(signature, msgHash, publicKey);

exports.getSharedSecret = privateKey => publicKey => isCompressed =>
    lib.getSharedSecret(privateKey, publicKey, isCompressed);

exports.recoverPublicKey = msgHash => signature => recovery => isCompressed =>
    lib.recoverPublicKey(msgHash, signature, recovery, isCompressed);
