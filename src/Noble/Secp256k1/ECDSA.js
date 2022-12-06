const lib = require('@noble/secp256k1');

exports.getECDSAPublicKey = privateKey => isCompressed =>
    lib.getPublicKey(privateKey, isCompressed);

exports._sign = msgHash => privateKey => () =>
    lib.sign(msgHash, privateKey);

exports._signWithRecoveredBit =
    tuple => msgHash => privateKey => () =>
        lib.sign(msgHash, privateKey, { recovered: true }).then(res => tuple(res[0])(res[1]));

exports.verifyECDSA = signature => msgHash => publicKey =>
    lib.verify(signature, msgHash, publicKey);

exports.getECDSASharedSecret = privateKey => publicKey => isCompressed =>
    lib.getSharedSecret(privateKey, publicKey, isCompressed);

exports.recoverECDSAPublicKey = msgHash => signature => recovery => isCompressed =>
    lib.recoverPublicKey(msgHash, signature, recovery, isCompressed);

exports._showBytes = bytes => '[ ' + bytes.join(', ') + ' ]';

exports._isValidPrivateKey = privateKey => lib.utils.isValidPrivateKey(privateKey);

exports._byteLength = bytes => bytes.byteLength
