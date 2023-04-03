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
       } from './Offchain.js';
import { Hook, Unhook, Console, Decode} from 'console-feed';
import { Message as MessageComponent } from "console-feed/lib/definitions/Component";
import { Message as MessageConsole } from "console-feed/lib/definitions/Console";

function App() {
  const tabs = TopLevelTabs();
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
}

/*
   Tab Component
*/
function TopLevelTabs() {
  return (
    <Tabs
      selectedTabClassName = "selectedTab"
      >
      <TabList
       className="TopLevelTabList">
        <Tab>Script</Tab>
        <Tab>NFT</Tab>
      </TabList>
      <TabPanel><div className="frameContainer"><ScriptFrame /></div></TabPanel>
      <TabPanel><div className="frameContainer"><NFTFrame /></div></TabPanel>
    </Tabs>
  );
}

/*
   Script GUI Component (Locking/Unlocking)
*/
const ScriptFrame = () => {
  return (
    <div className="NFTFrame">
      <ScriptForm />
      <div className="LogBox">
          <LogsContainer />
      </div>
    </div>
  )
}

type PWTxHash = {password: String, txHash : Uint8Array}

const ScriptForm = () => {
  const [input,setInput] = React.useState({ada: "", password: "", pwTxHashes: []});

  // TODO: Figure out how to pattern match on a PS `Maybe` value in JS
  const handleLock = async () => {
    const promise: Promise<Uint8Array> = payToPassword (input.password, input.ada);
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
          spendFromPassword (pwtxhash.txHash, pwtxhash.password) () ;
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
const NFTFrame = () => {
  return (
    <div className="NFTFrame">
      <NFTForm />
      <div className="LogBox">
          <LogsContainer />
      </div>
    </div>
  )
}

const NFTForm = () => {
  const [input,setInput] = React.useState({tokenName: '', quantity: ''});

  // Placeholder
  const handleMint = () => {mintTokens (input.tokenName, input.quantity) ()}

  const handleBurn = () => {burnTokens (input.tokenName, input.quantity) ()}

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

export default App;
