var path = require('path')
exports._sendFile = function (resp, path, cb) {
  return function () {
    resp.sendFile(path, function (err) {
      return cb(err)();
    });
  };
};

exports._pathResolve = function (p) {
    return path.resolve(p)
};