let read_script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {

} else { 

  const fs = require("fs");
  const path = require("path");
  read_script = fp => {
    return fs.readFileSync(
      path.resolve(__dirname, 
  "../../../compiled-scripts/".concat(fp)),
      "utf8"
    );
  };
  
}

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
always_succeeds = require("Scripts/fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e.plutus");;
} else { 
always_succeeds = read_script("fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e.plutus");;
}
exports.always_succeeds = always_succeeds;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
nft_hash_applied = require("Scripts/8c9d3c0a940175bbe29e8b25060c24c498a9f771a84f06084fc9a2fb.plutus");;
} else { 
nft_hash_applied = read_script("8c9d3c0a940175bbe29e8b25060c24c498a9f771a84f06084fc9a2fb.plutus");;
}
exports.nft_hash_applied = nft_hash_applied;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
nft_no_hash_applied = require("Scripts/fc005b39fd817fce3b65c524db57955d16845cc1c2f147494f13e691.plutus");;
} else { 
nft_no_hash_applied = read_script("fc005b39fd817fce3b65c524db57955d16845cc1c2f147494f13e691.plutus");;
}
exports.nft_no_hash_applied = nft_no_hash_applied;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
password_validator = require("Scripts/489b2c9bc2bc7faa058d9b999744514bac555c476d0c8751b1b03e33.plutus");;
} else { 
password_validator = read_script("489b2c9bc2bc7faa058d9b999744514bac555c476d0c8751b1b03e33.plutus");;
}
exports.password_validator = password_validator;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
simple_policy = require("Scripts/0a6f6ae0a5f1a7606510fd0837c5e47dacdb87cc768e5d3df8f64ce5.plutus");;
} else { 
simple_policy = read_script("0a6f6ae0a5f1a7606510fd0837c5e47dacdb87cc768e5d3df8f64ce5.plutus");;
}
exports.simple_policy = simple_policy;
