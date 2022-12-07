exports.byteLength = bytes => bytes.byteLength;

exports._ordIntArray = f => xs => ys => {
  for (let i = 0; i < xs.length && i < ys.length; i++) {
    let o = f(xs[i])(ys[i]);
    if (o !== 0) {
      return o
    }
  }
  return xs.length === ys.length ? 0 : xs.length > ys.length ? -1 : 1;
};

exports.showBytes = bytes => "[" + bytes.join(", ") + "]";
