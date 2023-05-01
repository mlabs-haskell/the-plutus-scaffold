import React from 'react';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';
import './App.css';
import 'react-tabs/style/react-tabs.css';
import {
    payToPassword
    , spendFromPassword
    , mintTokens
    , burnTokens
    , passwordFromAsciiJS
    , stringToTokenNameJS
    , stringToPosBigIntJS
    , testnetNamiConfig
    , testnetGeroConfig
    , testnetFlintConfig
    , testnetEternlConfig
    , testnetLodeConfig
    , testnetNuFiConfig
} from './Offchain.js';

// mere documentation
type ContractParams = any;
declare module "./Offchain.js" {
    export function payToPassword(a: ContractParams, b: Uint8Array, c: BigInt): Promise<Uint8Array>;
    export function spendFromPassword(a: ContractParams, b: Uint8Array, c: Uint8Array): void;
    export function mintTokens(a: ContractParams, b: Uint8Array, c: BigInt): void;
    export function burnTokens(a: ContractParams, b: Uint8Array, c: BigInt): void;
    export var testnetNamiConfig: ContractParams;
    export var testnetGeroConfig: ContractParams;
    export var testnetFlintConfig: ContractParams;
    export var testnetEternlConfig: ContractParams;
    export var testnetLodeConfig: ContractParams;
    export var testnetNuFiConfig: ContractParams;
    export function passwordFromAsciiJS(a: string): Uint8Array | null;
    export function stringToPosBigIntJS(a: string): BigInt | null;
    export function stringToTokenNameJS(a: string): Uint8Array | null;
};

function App() {
    /* We track whether a user has selected their wallet & which wallet they
     * have selected in the state of the top-level component
     * 
     * `wallet` is null when not selected yet and otherwise is ctl's ContractParams defined to be used with a specified wallet.
     *
     * NOTE: We pass the `set` functions to the WalletMenu component
     * as callbacks to enable Child -> Parent component communication, and
     * we pass the selected wallet value (really: a ContractParams value)
     * to the proper GUI */
    // const [walletSelected, setWalletSelected] = React.useState(false);
    const [maybeWallet, setMaybeWallet] = React.useState(null);
    /* If a user has not selected a wallet, we display the wallet
     * selection component. Once they have chosen a wallet, we
     * display the proper GUI.
     * */
    if (maybeWallet) {
        return (
            <div className="App">
                <header className="App-header">
                    <p>
                        Demo React GUI
                    </p>
                </header>
                <TopLevelTabs wallet={maybeWallet}/>
            </div>
        );
    } else {
        return (
            <WalletMenu
                walletSetter={setMaybeWallet}
            />
        );
    }
}

/* ContractParams = any, which should really be the JS runtime 
 * representation of CTL's `ContractParams` type. 
 * But we lack representation of this type here.
 * Pass in config values imported from the offchain api.   */
type Wallet = { wallet: ContractParams }

/*
   Tab Component, contains two parts of the GUI: the Script interface and the NFT interface
*/
function TopLevelTabs(props: Wallet) {
    return (
        <div >
            <div className="hintBox">Hint: Open developer console to see whats happening.</div>
            <Tabs
                selectedTabClassName="selectedTab"
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

/*
This form is used to interact with the password validator script.

At any point in time either the "Lock" or the "Unlock" actions are available (and the second button is disabled).
Initially user can "Lock" funds and only after the locking transaction was confirmed can he "Unlock".
When he "Unlocks" the cycle repeats.
This is tracked by state `maybeTxHash`: 
    - when `null` it means that the user can "Lock"
    - otherwise it means that user has just "Locked" and the state variable contains a hash of the previous locking transaction
*/
const ScriptForm = (props: Wallet) => {
    const [input, setInput] = React.useState({ ada: "", password: "" });
    const [maybeTxHash, setMaybeTxHash] = React.useState<null | Uint8Array>(null);
    
    const errDecodePassword = "Error: Couldn't parse password string. Perhaps you used a non-ascii character?";
    const errDecodeAda = "Error: Couldn't parse ADA value string into an integer";

    /*  User submits a password and an Ada amount, we lock specified amount of Ada at the password validator for the password.
    */
    const handleLock = async () => {
        const okPassword = passwordFromAsciiJS(input.password);
        const okAda = stringToPosBigIntJS(input.ada);

        if (!okPassword) {
            alert(errDecodePassword)
        } else {
        if (!okAda) {
            alert(errDecodeAda)
        } else {
            const txConfirmation: Promise<Uint8Array> = payToPassword(props.wallet, okPassword, okAda);
            let txhash: Uint8Array = await txConfirmation;
            setMaybeTxHash(txhash);
            alert('Locking \n ADA: ' + input.ada + '\n Password: ' + input.password + '\n TxHash:' + txhash.toString());
        }}

    }

    /*  User submits just a password, we unlock funds locked at the last locking transaction.
        We remembered the hash of that transaction in maybeTxHash state variable.
    */
    const handleUnlock = () => {
        const okPassword = passwordFromAsciiJS(input.password);

        if (!okPassword) {
            alert(errDecodePassword);
        }
        else {
            // here maybeTxHash is not null, because of the invariant
            spendFromPassword(props.wallet, maybeTxHash!, okPassword);
            setMaybeTxHash(null);
        }
    }

    const onChangeAda = (e: React.ChangeEvent<HTMLInputElement>) => {
        setInput({
            ada: e.target.value
            , password: input.password
        })
    }

    const onChangePassword = (e: React.ChangeEvent<HTMLInputElement>) => {
        setInput({
            ada: input.ada
            , password: e.target.value
        })
    }

    const lockButtonDisabled: boolean = (maybeTxHash != null);
    const unlockButtonDisabled: boolean = !lockButtonDisabled;

    return (
        <form>
            <InputBox
                label={'Ada Value:'}
                value={input.ada}
                onChange={onChangeAda}
                isDisabled={lockButtonDisabled}
            />
            <InputBox
                label={'Password:'}
                value={input.password}
                onChange={onChangePassword}
                isDisabled={false}
            />
            <Button
                text={'Lock Funds'}
                onClick={handleLock}
                isDisabled={lockButtonDisabled}
            />
            <Button
                text={'Unlock Funds'}
                onClick={handleUnlock}
                isDisabled={unlockButtonDisabled}
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

/*
This form is used to interact with the minting policy by minting or burning named tokens.
*/
const NFTForm = (props: Wallet) => {
    const [input, setInput] = React.useState({ tokenName: '', quantity: '' });

    const tokName = stringToTokenNameJS(input.tokenName);
    // Parse to POSITIVE integer
    const mintVal = stringToPosBigIntJS(input.quantity);
    const errDecodeAmount = (amount : string) =>  "Error: Could not convert " + amount + " to a BigInt Value!";
    const errDecodeTokenName = (tokenName : string) => "Error: Could not convert " + tokenName + " to a Token Name";
    
    const handleMintBurnAux = (contract: (a: ContractParams, b: Uint8Array, c: BigInt) => void) => () => {
        if (!tokName) {
            alert(errDecodeTokenName(input.tokenName))
        } else {
            if (!mintVal) {
                alert(errDecodeAmount(input.quantity))
            } else {
                contract(props.wallet, tokName, mintVal)
            }
        }
    };

    const handleMint = handleMintBurnAux(mintTokens);
    const handleBurn = handleMintBurnAux(burnTokens);

    const onChangeQuantity = (e: React.ChangeEvent<HTMLInputElement>) => {
        setInput({ tokenName: input.tokenName, quantity: e.target.value })
    }

    const onChangeTokenName = (e: React.ChangeEvent<HTMLInputElement>) => {
        setInput({ tokenName: e.target.value, quantity: input.quantity })
    }

    return (
        <form>
            <InputBox
                label={'Name:'}
                value={input.tokenName}
                onChange={onChangeTokenName}
                isDisabled={false}
            />
            <InputBox
                label={'Quantity:'}
                value={input.quantity}
                onChange={onChangeQuantity}
                isDisabled={false}
            />
            <Button
                text={'Mint'}
                onClick={handleMint}
                isDisabled={false}
            />
            <Button
                text={'Burn'}
                onClick={handleBurn}
                isDisabled={false}
            />
        </form>
    );

}

/*
   Form Child Components (Input Boxes & Buttons)
*/
type InputProps = {
    label: string,
    value: string,
    onChange: (e: React.ChangeEvent<HTMLInputElement>) => void,
    isDisabled: boolean
}

/*
Input box for inputs like "password", "ada amount", "token name"
*/
const InputBox = (props: InputProps) =>
    <div className="inputBox">
        <label>{props.label}</label>
        <input
            value={props.value}
            onChange={props.onChange}
            disabled={props.isDisabled}
        />
    </div>

type ButtonProps = { text: string, onClick: () => void, isDisabled: boolean }

const Button = (props: ButtonProps) => {
    if (props.isDisabled) {
        return (
            <button
                type="button"
                className="button-disabled"
                disabled={true}>
                {props.text}
            </button>
        )
    } else {
        return (
            <button
                type="button"
                className="button"
                onClick={props.onClick}>
                {props.text}
            </button>
        )
    }
}

/* The type of `walletSetter` is really
 * `(e: ContractParams) => Void`. 
 * It's a setter of wallet passed down from the parent GUI component.
 * That is: walletSetter(testnetNamiConfig) sets the state variable tracking the selected wallet
 *  to the chosen wallet (or really the ContractParams that specify the wallet, not "wallet" itself)
 * */
type WalletProps = {
    walletSetter: (e: any) => void
}

/* A simple dropdown component that allows(/forces) users to select a wallet.
 * For the purposes of this example we only use testnet wallet configurations, however
 * this could be adapted to use mainnet configurations as well.
 * */
const WalletMenu = (props: WalletProps) => {

    // type MenuItemProps = ;
    function MenuItem(propsMenu: {text : string , wallet : ContractParams}) {
        return (
            <li className="menu-item">
                <button onClick={() =>
                    props.walletSetter(propsMenu.wallet)}
                >
                {propsMenu.text}
                </button>
            </li>
        );
    }

    return (
        <div className="dropdown">
            <ul className="menu">
                <div className="menuText">Choose a wallet</div>
                <MenuItem text="Nami" wallet={testnetNamiConfig}/>
                <MenuItem text="Eternl" wallet={testnetEternlConfig}/>
                <MenuItem text="Gero" wallet={testnetGeroConfig}/>
                <MenuItem text="Flint" wallet={testnetFlintConfig}/>
                <MenuItem text="Lode" wallet={testnetLodeConfig}/>
                <MenuItem text="NuFi" wallet={testnetNuFiConfig}/>
            </ul>
        </div>
    );
}

export default App;
