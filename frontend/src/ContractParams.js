// TAG: DEPLOYMENT_CTLRUNTIME
let ContractParamsModule;
if (typeof DEPLOYMENT_CTLRUNTIME != "undefined" && DEPLOYMENT_CTLRUNTIME) {
  ContractParamsModule = require("./DeploymentContractParams.js");
} else {
  ContractParamsModule = require("./LocalContractParams.js");
}

let testnetNamiConfig = ContractParamsModule.testnetNamiConfig;
let testnetGeroConfig = ContractParamsModule.testnetGeroConfig;
let testnetFlintConfig = ContractParamsModule.testnetFlintConfig;
let testnetEternlConfig = ContractParamsModule.testnetEternlConfig;
let testnetLodeConfig = ContractParamsModule.testnetLodeConfig;
let testnetNuFiConfig = ContractParamsModule.testnetNuFiConfig;

export {
  testnetNamiConfig,
  testnetGeroConfig,
  testnetFlintConfig,
  testnetEternlConfig,
  testnetLodeConfig,
  testnetNuFiConfig,
};
