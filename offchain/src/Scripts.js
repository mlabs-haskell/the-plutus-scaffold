let read_script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {

} else { 

  const fs = require("fs");
  const path = require("path");
  read_script = fp => {
    return fs.readFileSync(
      path.resolve(__dirname, 
  "../../compiled-scripts/".concat(fp)),
      "utf8"
    );
  };
  
}

let always_succeeds_envelope;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
always_succeeds_envelope = require("Scripts/always_succeeds.plutus");;
} else { 
always_succeeds_envelope = read_script("always_succeeds.plutus");;
}
exports.always_succeeds_envelope = always_succeeds_envelope;

let nft_hash_applied_envelope;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
nft_hash_applied_envelope = require("Scripts/nft_hash_applied.plutus");;
} else { 
nft_hash_applied_envelope = read_script("nft_hash_applied.plutus");;
}
exports.nft_hash_applied_envelope = nft_hash_applied_envelope;

let nft_no_hash_applied_envelope;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
nft_no_hash_applied_envelope = require("Scripts/nft_no_hash_applied.plutus");;
} else { 
nft_no_hash_applied_envelope = read_script("nft_no_hash_applied.plutus");;
}
exports.nft_no_hash_applied_envelope = nft_no_hash_applied_envelope;

let password_validator_envelope;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
password_validator_envelope = require("Scripts/password_validator.plutus");;
} else { 
password_validator_envelope = read_script("password_validator.plutus");;
}
exports.password_validator_envelope = password_validator_envelope;

let simple_policy_envelope;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
simple_policy_envelope = require("Scripts/simple_policy.plutus");;
} else { 
simple_policy_envelope = read_script("simple_policy.plutus");;
}
exports.simple_policy_envelope = simple_policy_envelope;
