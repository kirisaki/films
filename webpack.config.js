const path = require('path')

module.exports = {
  mode: 'development',
  entry: {
    main: [
      './src/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].js',
  },

  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
	loader: 'elm-webpack-loader',
	options: {
	  cwd: path.resolve(__dirname, 'src')
	}
      }
    },{
      test: /\.s[ca]ss$/,
      exclude: /node_modules/,
      use: ['style-loader', 'css-loader', 'sass-loader'],
    },{
      test:/\.html$/,
      exclude: /node_modules/,
      use: 'file-loader?name=[name].[ext]',
    }
    ],
    noParse: /\.elm$/,
  },
}
