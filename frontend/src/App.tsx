import React, {useState, useEffect} from 'react';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';
import  './App.css';
import 'react-tabs/style/react-tabs.css';
import { payToPassword
       , spendFromPassword
       , mintTokens
       , burnTokens
       , insertPWTXHash
       , lookupTXHashByPW
       , testnetNamiConfig
       , testnetGeroConfig
       , testnetFlintConfig
       , testnetEternlConfig
       , testnetLodeConfig
       , testnetNuFiConfig
       } from './Offchain.js';

function App() {
  /* We track whether a user has selected their wallet & which wallet they
   * have selected in the state of the top-level component
   *
   * NOTE: We pass the `set` functions to the WalletMenu component
   * as callbacks to enable Child -> Parent component communication, and
   * we pass the selected wallet value (really: a ContractParams value)
   * to the GUI proper */
  const [walletSelected,setWalletSelected] = React.useState(false);
  const [wallet,setWallet] = React.useState(testnetEternlConfig);
  /* If a user has not selected a wallet, we display the wallet
   * selection component. Once they have chosen a wallet, we
   * display the GUI proper.
   * */
  if (walletSelected) {
  const tabs = TopLevelTabs({wallet: wallet});
  return (
    <div className="App">
      <header className="App-header">
        <p>
          Demo React GUI
        </p>
      </header>
      {tabs}
    </div>
  );
  } else {
    return (
      <WalletMenu
        walletHandler={setWallet}
        walletSelected={setWalletSelected}
      />
    );
  }
}

/* `any` should really be the JS runtime representation of
 * CTL's `ContractParams` type. Unfotunately, there is not an
 * ergonomic way to generate TS type declarations for corresponding
 * PureScript/CTL types. While it may be viable to manually write
 * those declarations for extremely simple types, it is likely that
 * projects which require complex CTL types to be available in the frontend
 * will have to use this escape hatch. */
type Wallet = {wallet: any}

/*
   Tab Component
*/
function TopLevelTabs(props: Wallet) {
  return (
    <div >
      <div className="hintBox">Hint: Open developer console to see what's happening.</div>
      <Tabs
        selectedTabClassName = "selectedTab"
        >
        <TabList
        className="TopLevelTabList">
          <Tab>Script</Tab>
          <Tab>NFT</Tab>
        </TabList>
        <TabPanel><div className="frameContainer"><ScriptFrame wallet={props.wallet} /></div></TabPanel>
        <TabPanel><div className="frameContainer"><NFTFrame wallet={props.wallet} /></div></TabPanel>
      </Tabs>
    </div>
  );
}

/*
   Script GUI Component (Locking/Unlocking)
*/
const ScriptFrame = (props: Wallet) => {
  return (
    <div className="NFTFrame">
      <ScriptForm
        wallet={props.wallet}
      />
    </div>
  )
}

/* A TypeScript type definition that corresponds to the PWTxHash record we defined in the
 * offchain code. In principle we can construct TS type defs for any PS type we need, but
 * in practice it probably only makes sense to do so for simple types we have defined ourselves
 * in the offchain */
type PWTxHash = {password: String, txHash : Uint8Array}

const ScriptForm = (props: Wallet) => {
  const [input,setInput] = React.useState({ada: "", password: "", pwTxHashes: []});
  /* Because `paytoPassword` returns a value (wrapped in a promise), we need an async handler
   * function to deal w/ the promise and get the returned value so that it can be stored in the
   * component's state for further use.
   * */
  const handleLock = async () => {
    const promise: Promise<Uint8Array> = payToPassword (props.wallet, input.password, input.ada);
    let hash: Uint8Array  = await promise;
    setInput({ ada: input.ada
             , password: input.password
             , pwTxHashes: insertPWTXHash (input.password, hash, input.pwTxHashes)});
    alert('Locking \n ADA: ' + input.ada + '\n Password: ' + input.password);
  }
  /* Note that `spendFromPassword` does not return a value and therefore
   * does not need to be an async function*/
  const handleUnlock = () => {
      let mhash = lookupTXHashByPW (input.password, input.pwTxHashes);
      if (mhash.value0) { // <- This is apparently how you deconstruct PS `Maybe` values in JS (useful to know)
          let pwtxhash: PWTxHash = mhash.value0;
          spendFromPassword (props.wallet, pwtxhash.txHash, pwtxhash.password) () ;
      } else {
          alert("No TxHash for the provided password. Perhaps you forgot to lock funds?")
      }
  }

  const onChangeAda = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({  ada: e.target.value
              , password: input.password
              , pwTxHashes: input.pwTxHashes})
  }

  const onChangePassword = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({ ada: input.ada
             , password: e.target.value
             , pwTxHashes: input.pwTxHashes })
  }

  return (
    <form>
      <InputBox
         lbl={'Ada Value:'}
         val={input.ada}
         onChange={onChangeAda}
      />
      <InputBox
         lbl={'Password:'}
         val={input.password}
         onChange={onChangePassword}
      />
      <Button
        text={'Lock Funds'}
        onClick={handleLock}
      />
      <Button
        text={'Unlock Funds'}
        onClick={handleUnlock}
      />
    </form>
  );
}

/*
   NFT GUI Component (Minting/Burning)
*/
const NFTFrame = (props: Wallet) => {
  return (
    <div className="NFTFrame">
      <NFTForm
        wallet={props.wallet}
      />
    </div>
  )
}

const NFTForm = (props: Wallet) => {
  const [input,setInput] = React.useState({tokenName: '', quantity: ''});

  const handleMint = () => {mintTokens (props.wallet,input.tokenName, input.quantity) ()}

  const handleBurn = () => {burnTokens (props.wallet,input.tokenName, input.quantity) ()}

  const onChangeQuantity = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({tokenName: input.tokenName, quantity: e.target.value})
  }

  const onChangeTokenName = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({tokenName: e.target.value, quantity: input.quantity})
  }

  return (
    <form>
      <InputBox
        lbl={'Name:'}
        val={input.tokenName}
        onChange={onChangeTokenName}
      />
      <InputBox
        lbl={'Quantity:'}
        val={input.quantity}
        onChange={onChangeQuantity}
      />
      <Button
        text={'Mint'}
        onClick={handleMint}
      />
      <Button
        text={'Burn'}
        onClick={handleBurn}
      />
    </form>
  );

}

/*
   Form Child Components (Input Boxes & Buttons)
*/
type InputProps = {lbl : string, val : string, onChange : (e: React.ChangeEvent<HTMLInputElement>) => void}

const InputBox = (props : InputProps) => {
  return (
    <div className="inputBox">
      <label>{props.lbl}</label>
        <input
           value={props.val}
           onChange={props.onChange}
        />
    </div>
  )
}

type ButtonProps = {text: string, onClick: () => void}

const Button = (props : ButtonProps) => {
 return (
   <button
     type="button"
     className="button"
     onClick={props.onClick}>
      {props.text}
   </button>
 )
}

/* Again, the type of `walletHandler` is really
 * `(e: Contract.ContractParams) => Void`, but we need the `any`
 * escape hatch to avoid having to manually construct a set of very complicated
 * TS types that correspond to PS types. These handlers function as callbacks that
 * allow Child -> Parent communication so that we can propagate the selected wallet
 * into the GUI proper
 * */
type WalletProps = { walletHandler: (e: any) => void
                   , walletSelected: (b : boolean ) => void }

/* A simple dropdown component that allows(/forces) users to select a wallet.
 * For the purposes of this example we only use testnet wallet configurations, however
 * this could be adapted to use mainnet configurations as well.
 * */
const WalletMenu = (props: WalletProps) => {

  const handleNami = () => {
    props.walletHandler(testnetNamiConfig);
    props.walletSelected(true);
  };

  const handleEternl = () => {
    props.walletHandler(testnetEternlConfig);
    props.walletSelected(true);
  };

  const handleGero = () => {
    props.walletHandler(testnetGeroConfig);
    props.walletSelected(true);
  };

  const handleFlint = () => {
    props.walletHandler(testnetFlintConfig);
    props.walletSelected(true);
  };

  const handleLode = () => {
    props.walletHandler(testnetLodeConfig);
    props.walletSelected(true);
  };

  const handleNuFi = () => {
    props.walletHandler(testnetNuFiConfig);
    props.walletSelected(true);
  };

  return (
    <div className="dropdown">
        <ul className="menu">
          <div className="menuText"><text>Choose a wallet </text></div>
          <li className="menu-item">
            <button onClick={handleNami}>Nami</button>
          </li>
          <li className="menu-item">
            <button onClick={handleEternl}>Eternl</button>
          </li>
          <li className="menu-item">
            <button onClick={handleGero}>Gero</button>
          </li>
          <li className="menu-item">
            <button onClick={handleFlint}>Flint</button>
          </li>
          <li className="menu-item">
            <button onClick={handleLode}>Lode</button>
          </li>
          <li className="menu-item">
            <button onClick={handleNuFi}>NuFi</button>
          </li>
        </ul>
    </div>
  );
}

export default App;
