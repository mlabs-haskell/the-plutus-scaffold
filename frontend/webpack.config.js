
const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");

module.exports = {

  entry: "./src/index.tsx",
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "build"),
  },

  devServer: {
    static: {
      directory: path.join(__dirname, "build"),
    },
    port: 4008,
    // Dont know if we need this - let's uncomment if some error message contains word "kupo"
    // proxy: {
    //   "/kupo": {
    //     // `KUPO_HOST` env variable must be set to the base URL of the Kupo
    //     // service, otherwise all requests to Kupo will fail.
    //     target: process.env.KUPO_HOST || "http://localhost:1442",
    //     changeOrigin: true,
    //     pathRewrite: { "^/kupo": "" },
    //   },
    // },
  },

  devtool: 'inline-source-map',

  module: {
    rules: [
      {
        test: /\.(png|jpg|gif)$/i,
        type: "asset",
      },
      {
        test: /\.plutus$/i,
        type: "asset/source",
      },
      {
        test: /\.css$/,
        use: ["css-loader"],
      },
      {
        test: /\.(ts|tsx)$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },

  resolve: {
    extensions: [".js", ".jsx", ".tsx", ".ts"],
    fallback: {
      buffer: require.resolve("buffer/"),
      http: false,
      url: false,
      stream: false,
      crypto: false,
      https: false,
      net: false,
      tls: false,
      zlib: false,
      os: false,
      path: false,
      fs: false,
      readline: false,
      child_process: false,
    },
  },

  plugins: [
    new NodePolyfillPlugin(),
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
    new HtmlWebpackPlugin({
      title: "mlabs-plutus-scaffold",
      template: path.join(__dirname, "public", "index.html"),
      // inject: false, // See stackoverflow.com/a/38292765/3067181
    }),
  ]
};