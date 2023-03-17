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
nft_hash_applied = require("Scripts/493177bb4111452fac2d8abe7f8f79e800eec4cba24f1d53ae3efa3c.plutus");;
} else { 
nft_hash_applied = read_script("493177bb4111452fac2d8abe7f8f79e800eec4cba24f1d53ae3efa3c.plutus");;
}
exports.nft_hash_applied = nft_hash_applied;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
nft_no_hash_applied = require("Scripts/d1f18edece2ee922df6a5ca14dc01741dc62e00863ac9994846c75fd.plutus");;
} else { 
nft_no_hash_applied = read_script("d1f18edece2ee922df6a5ca14dc01741dc62e00863ac9994846c75fd.plutus");;
}
exports.nft_no_hash_applied = nft_no_hash_applied;
