const path = require('path');

module.exports = {
  mode: 'development', // Set the mode explicitly
  entry: './pisshitballs.tsx', // Update the entry file if needed
  output: {
    filename: 'cock.js',
    path: path.resolve(__dirname, './'), // Output directory (adjust as needed)
    library: 'cock', // Name of the global variable in the UMD bundle
    libraryTarget: 'umd', // Output format (UMD)
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'], // Add '.tsx' extension
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/, // Match '.ts' or '.tsx' files
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-react', '@babel/preset-typescript'],
          },
        },
        exclude: /node_modules/,
      },
    ],
  },
};
