# Usage Notes 

The pre-commit hooks can be installed with `nix develop .#preCommit`. 

The default pre-commit configuration assumes that you have [tsfmt](https://www.npmjs.com/package/typescript-formatter) installed. If you do not want to install `tsfmt` or have no need for it, delete the `tsfmt` attribute from pre-commit-check.nix. 
