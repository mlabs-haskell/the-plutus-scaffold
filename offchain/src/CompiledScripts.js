
// scripts = {"alwaysSucceed" : "some script" }
let scripts = {};

let scriptsDir = require.context('Scripts', true, /\.plutus$/);

function importAll(r) {
  r.keys().forEach((key) => (scripts[key] = r(key)));
}

console.log(scripts);

exports.scripts = scripts;


