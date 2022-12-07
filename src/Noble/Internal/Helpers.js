exports.byteLength = bytes => bytes.byteLength;

exports._ordIntArray = f => xs => ys => {
  for (let i = 0; i < xs.length && i < ys.length; i++) {
    if (xs[i] !== ys[i]) {
      return xs[i] < ys[i] ? -1 : 1;
    }
  }
  return xs.length === ys.length ? 0 : xs.length > ys.length ? -1 : 1;
};

exports.showBytes = bytes => "[ " + bytes.join(", ") + " ]";
