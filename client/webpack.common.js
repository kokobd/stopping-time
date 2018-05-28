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
      {
        test: /\.css$/,
        use: [
          "style-loader",
          "css-loader"
        ]
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [{
          loader: 'file-loader',
          options: {
            name: '[name].[ext]',
            outputPath: 'fonts/'
          }
        }]
      }
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({ template: './src/index.html' }),
  ],

};