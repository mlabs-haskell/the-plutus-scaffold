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
import { Hook, Unhook, Console, Decode} from 'console-feed';
import { Message as MessageComponent } from "console-feed/lib/definitions/Component";
import { Message as MessageConsole } from "console-feed/lib/definitions/Console";

function App() {
  const [walletSelected,setWalletSelected] = React.useState(false);
  const [wallet,setWallet] = React.useState(testnetEternlConfig);

  if (walletSelected) {
  const tabs = TopLevelTabs({wallet: wallet});
  return (
    <div className="App">
      <header className="App-header">
        <p>
          Example React GUI
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

/* It's too difficult to give this a meaning */
type Wallet = {wallet: any}

/*
   Tab Component
*/
function TopLevelTabs(props: Wallet) {
  return (
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
      <div className="LogBox">
          <LogsContainer />
      </div>
    </div>
  )
}

type PWTxHash = {password: String, txHash : Uint8Array}

const ScriptForm = (props: Wallet) => {
  const [input,setInput] = React.useState({ada: "", password: "", pwTxHashes: []});

  // TODO: Figure out how to pattern match on a PS `Maybe` value in JS
  const handleLock = async () => {
    const promise: Promise<Uint8Array> = payToPassword (props.wallet, input.password, input.ada);
    let hash: Uint8Array  = await promise;
    setInput({ ada: input.ada
             , password: input.password
             , pwTxHashes: insertPWTXHash (input.password, hash, input.pwTxHashes)});
    alert('Locking \n ADA: ' + input.ada + '\n Password: ' + input.password);
  }

  // TODO: insert a real unlock function once I've figured out the stack overflow on importing error
  const handleUnlock = () => {
      let mhash = lookupTXHashByPW (input.password, input.pwTxHashes);
      if (mhash.value0) {
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
      <div className="LogBox">
          <LogsContainer />
      </div>
    </div>
  )
}

const NFTForm = (props: Wallet) => {
  const [input,setInput] = React.useState({tokenName: '', quantity: ''});

  // Placeholder
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

// from https://github.com/samdenty/console-feed/issues/57
const LogsContainer = () => {
  const [logs, setLogs] = useState<MessageConsole[]>([]);

  // run once!
  useEffect(() => {
    Hook(
      window.console,
      (log) => setLogs((currLogs) => [...currLogs, log]),
      false
    );
    return () => {Unhook(window.console as any)};
  }, []);

  return <Console
    logs={logs as MessageComponent[]} variant="dark" />;
};

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

type WalletProps = { walletHandler: (e: any) => void
                   , walletSelected: (b : boolean ) => void }

const WalletMenu = (props: WalletProps) => {
  const [open,setOpen] = React.useState(true);

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
