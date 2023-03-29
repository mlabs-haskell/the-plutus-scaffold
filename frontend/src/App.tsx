import React from 'react';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';
import  './App.css';
import 'react-tabs/style/react-tabs.css';
import { // square
        payToPassword
      , spendFromPassword
      , mintTokens
      , burnTokens
      , insertPWTXHash
      , lookupTXHashByPW
      , deletePWTXHash
       } from './Offchain.js';

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
    <div className="ScriptFrame">
      <ScriptForm />
    </div>
  )
}

type PWTxHash = {password: String, txHash : Uint8Array}

const ScriptForm = () => {
    const initHashes : Array<PWTxHash> = [];
    const [input,setInput] = React.useState({ada: "", password: ""});
    const [txHashes,setTxHashes] = React.useState({hashes: initHashes});

    async function handleLock() {
        payToPassword(input.ada)(input.password).then(
          (txhash: Uint8Array) => {
                    const newHashes: Array<PWTxHash> = insertPWTXHash(input.password)(txhash)(txHashes.hashes);
        setTxHashes({hashes: newHashes})
          }
        )

        // (payToPassword(inp.ada)(inp.password))
        //    .then((resp : Uint8Array) => {
        //const newHashes = insertPWTXHash(inp.password)(resp)(txHashes.hashes);
        //setTxHashes({hashes: newHashes});

          };


  // TODO: insert a real unlock function once I've figured out the stack overflow on importing error
  const handleUnlock = () => {alert('Locking \n ADA: ' + input.ada + '\n Password: ' + input.password)}

  const onChangeAda = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({  ada: e.target.value
              , password: input.password})
  }

  const onChangePassword = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({ ada: input.ada
             , password: e.target.value })
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
    </div>
  )
}

const NFTForm = () => {
  const [input,setInput] = React.useState({tokenName: '', quantity: ''});

  // Placeholder
  const handleMint = () => {/* mintTokens (input.tokenName) (input.quantity) () */}

  const handleBurn = () => {/* burnTokens (input.tokenName) (input.quantity) () */}

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

type ButtonProps = {text: string, onClick: (e: React.MouseEvent<HTMLButtonElement, MouseEvent>) => void}

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
