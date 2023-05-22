# Getting Started

## Prerequisites

First ensure that your development environment matches production environment, by updating onchain scripts in ../compiled-scripts and including an up-to-date offchain bundle with `make bundle-offchain-api`.

The frontend project is fully managed by node+npm. You can use nix provided devshell for frontend or install node seperately in version `14.20.1`.

Run

```sh
npm install
```

when you enter the directory for the first time (and when you update node dependencies).

## Available Scripts

In the project directory, you can run:

### `npm start`

Runs the app in the development mode.\
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits.\
You will also see any lint errors in the console.

### `npm run build-bundle`

Builds the app for production to the `build` folder.\
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.\
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.

## Learn More

To learn React, check out the [React documentation](https://reactjs.org/).
