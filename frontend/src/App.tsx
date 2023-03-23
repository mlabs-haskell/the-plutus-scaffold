import React from 'react';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';
import './App.css';
import 'react-tabs/style/react-tabs.css';
import { square, always_succeeds } from './Offchain.js';

function App() {
  const tabs = TopLevelTabs();
  return (
    <div className="App">
      <header className="App-header">
        <p>
          Example React GUI (2^2={square(2)} and a script={always_succeeds})
        </p>
      </header>
      {tabs}
    </div>
  );
}

function TopLevelTabs() {
  return (
    <Tabs>
      <div className="TopLevelTabList">
      <TabList>
        <Tab>Script</Tab>
        <Tab>NFT</Tab>
      </TabList>
      </div>
      <TabPanel><div className="frameContainer"><ScriptFrame /></div></TabPanel>
      <TabPanel><div className="frameContainer"><NFTFrame /></div></TabPanel>
    </Tabs>
  );
}

const ScriptForm = () => {
  const [input,setInput] = React.useState({ada: "", password: ""});

  // Placeholder
  const handleLock = () => {alert('Locking \n ADA: ' + input.ada + '\n Password: ' + input.password)}

  const handleUnlock = () => {alert('Locking \n ADA: ' + input.ada + '\n Password: ' + input.password)}

  const onChangeAda = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({ada: e.target.value, password: input.password})
  }

  const onChangePassword = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({ada: input.ada, password: e.target.value})
  }

  return (
    <form>
      <label>
        ADA Value:
        <input
           value={input.ada}
           onChange={onChangeAda}
        />
      </label>
      <label>
      Password:
        <input
           value={input.password}
           onChange={onChangePassword}
        />
      </label>
      <button
         type="button"
         onClick={handleLock}>
      Lock Funds
      </button>
      <button
         type="button"
         onClick={handleUnlock}>
      Unlock Funds
      </button>
    </form>
  );
}

const NFTForm = () => {
  const [input,setInput] = React.useState({tokens: ''});

  // Placeholder
  const handleMint = () => {alert('Minting \n Tokens: ' + input.tokens)}

  const handleBurn = () => {alert('Burning \n Tokens: ' + input.tokens)}

  const onChangeTokens= (e: React.ChangeEvent<HTMLInputElement>) => {
    setInput({tokens: e.target.value})
  }

  return (
    <form>
      <label>
        Tokens:
        <input
           value={input.tokens}
           onChange={onChangeTokens}
        />
      </label>
      <button
         type="button"
         onClick={handleMint}>
      Mint
      </button>
      <button
         type="button"
         onClick={handleBurn}>
      Burn
      </button>
    </form>
  );

}

const ScriptFrame = () => {
  return (
    <div className="ScriptFrame">
      <ScriptForm />
    </div>
  )
}

const NFTFrame = () => {
  return (
    <div className="NFTFrame">
      <NFTForm />
    </div>
  )
}

export default App;
