const lib = require("@noble/secp256k1");

exports.randomPrivateKey = () => lib.utils.randomPrivateKey();

exports._sha256 = bytes => () => lib.utils.sha256(bytes);

exports._hashToPrivateKey = nothing => just => bytes => {
    try {
        return just(lib.utils.hashToPrivateKey(bytes));
    } catch (_) {
        return nothing;
    }
};

exports.isValidPrivateKey = lib.utils.isValidPrivateKey;
