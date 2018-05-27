const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');
const path = require('path');

module.exports = {
  entry: './src/index.tsx',
  output: {
    path: __dirname + "/build",
    filename: 'bundle.js',
  },

  resolve: {
    extensions: [".ts", ".tsx", ".js", ".json"],
    modules: [
      path.resolve('./node_modules'),
      path.resolve('./src')
    ]
  },

  module: {
    rules: [
      { test: /\.tsx?$/, loader: 'awesome-typescript-loader' },
      { enforce: "pre", test: /\.js$/, loader: 'source-map-loader' },
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({ template: './src/index.html' }),
  ],

};