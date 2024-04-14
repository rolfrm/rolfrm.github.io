const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');
module.exports = {
	 mode: 'development',
	 entry: '../../src/foxlisp-web.js', // Entry point of your application

  output: {
    filename: 'bundle.js', // Output bundle file
    path: path.resolve("dist"), // Output directory
    publicPath: "/",
  },
  module: {
    rules: [
      {
        test: /\.\.\/\.\.\/lisp\/\.(lisp)$/i,
        type: "asset/resource"
      }
    ]
  },
  plugins: [
    new CopyWebpackPlugin({patterns: [
      { from: '../../lisp', to: 'lisp' }
    ]})
  ],
	 devServer: {
	 static: {
        directory: __dirname,
    },
    compress: true,
    port: 9000,
}
};

