const path = require('path');
const { VueLoaderPlugin } = require('vue-loader');
const webpack = require('webpack');

module.exports = {
  mode: 'development',
  entry: './main.ts', // Point this to your main Vue file (adjust as necessary)
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, './'), // Output directory
    library: 'MyComponent', // The name of the global variable
    libraryTarget: 'window', // Expose it to the window object
    globalObject: 'this',
  },
  module: {
    rules: [
      {
        test: /\.vue$/,
        loader: 'vue-loader',
      },
      {
        test: /\.ts$/, //add this garbage for ts to work
        loader: 'ts-loader',
        exclude: /node_modules/,
        options: { appendTsSuffixTo: [/\.vue$/] }, //also add this or it wont work lmao
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
        },
      },
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader'],
      },
    ],
  },
  plugins: [
    new VueLoaderPlugin(), // Make sure to include the Vue loader plugin
    new webpack.DefinePlugin({
      __VUE_OPTIONS_API__: JSON.stringify(true), // or false based on your use
      __VUE_PROD_DEVTOOLS__: JSON.stringify(false), // set to true if you want devtools in production
      __VUE_PROD_HYDRATION_MISMATCH_DETAILS__: JSON.stringify(false), // typically false
    }),
    
  ],
};
