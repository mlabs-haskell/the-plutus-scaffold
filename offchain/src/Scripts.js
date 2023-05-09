let read_script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {

} else { 

  const fs = require("fs");
  const path = require("path");
  read_script = fp => {
    return fs.readFileSync(
      path.resolve(__dirname, 
  "../../compiled-scripts".concat(fp)),
      "utf8"
    );
  };
  
}

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
always_succeeds_envelope = require("Scripts/fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e.plutus");;
} else { 
always_succeeds_envelope = read_script("fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e.plutus");;
}
exports.always_succeeds_envelope = always_succeeds_envelope;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
nft_hash_applied_envelope = require("Scripts/8c9d3c0a940175bbe29e8b25060c24c498a9f771a84f06084fc9a2fb.plutus");;
} else { 
nft_hash_applied_envelope = read_script("8c9d3c0a940175bbe29e8b25060c24c498a9f771a84f06084fc9a2fb.plutus");;
}
exports.nft_hash_applied_envelope = nft_hash_applied_envelope;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
nft_no_hash_applied_envelope = require("Scripts/fc005b39fd817fce3b65c524db57955d16845cc1c2f147494f13e691.plutus");;
} else { 
nft_no_hash_applied_envelope = read_script("fc005b39fd817fce3b65c524db57955d16845cc1c2f147494f13e691.plutus");;
}
exports.nft_no_hash_applied_envelope = nft_no_hash_applied_envelope;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
password_validator_envelope = require("Scripts/489b2c9bc2bc7faa058d9b999744514bac555c476d0c8751b1b03e33.plutus");;
} else { 
password_validator_envelope = read_script("489b2c9bc2bc7faa058d9b999744514bac555c476d0c8751b1b03e33.plutus");;
}
exports.password_validator_envelope = password_validator_envelope;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
simple_policy_envelope = require("Scripts/eb6abc150d4335524a2176ce93831f718dabf10d53864a32cbdabb96.plutus");;
} else { 
simple_policy_envelope = read_script("eb6abc150d4335524a2176ce93831f718dabf10d53864a32cbdabb96.plutus");;
}
exports.simple_policy_envelope = simple_policy_envelope;
