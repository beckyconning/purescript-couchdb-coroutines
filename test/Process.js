// module Test.Process

exports.exit = function exit(rv) {
  return function() {
    try { process.exit(rv); } catch (e) {
      try { phantom.exit(rv); } catch (e) {}
    };
  };
};
