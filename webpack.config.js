const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const merge = require('webpack-merge')
const MiniCssExtractPlugin = require("mini-css-extract-plugin")
const UglifyJsPlugin = require('uglifyjs-webpack-plugin')
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin')

const base = {
  entry: {
    app: [path.resolve(__dirname, './src/js/index.js')],
  },
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: 'index.js',
    library: '[name]',
    publicPath: '/',
  },
  resolve: {
    extensions: ['.js'],
  },
  module: {
    rules: [
      { test: /\.js$/, exclude: /node_modules/, loader: "babel-loader" },
      {
        test: /\.(eot|svg|ttf|woff|woff2)$/,
        loader: 'file-loader',
        include: /fonts/,
        options: {
          context: __dirname,
          name: 'fonts/[name].[ext]',
        },
        sideEffects: true,
      },
      {
        test: /\.styl$/,
        use: [MiniCssExtractPlugin.loader, 'css-loader', 'stylus-loader'],
        sideEffects: true,
      },
    ]
  },
  plugins: [
    new MiniCssExtractPlugin({
      filename: './styled/index.css',
    })
  ]
}


const dev = {
  mode: 'development',
  devtool: 'cheap-module-eval-source-map',
  watch: true,
  plugins: [
    new HtmlWebpackPlugin({
      template: path.resolve(__dirname, './src/index.ejs'),
      inject: false,
      templateParameters: {
        rootPath: '/',
        elmPath: 'elm.js',
      }
    }),
  ],
}


const prod = {
  mode: 'production',
  plugins: [
    new HtmlWebpackPlugin({
      template: path.resolve(__dirname, './src/index.ejs'),
      inject: false,
      templateParameters: {
        rootPath: '/',
        elmPath: 'elm.min.js',
      }
    }),
    new UglifyJsPlugin(),
    new OptimizeCssAssetsPlugin({}),
  ],
}


module.exports = (env, argv) => {
  if (argv.mode === 'production') {
    return merge(base, prod)
  }

  return merge(base, dev)
}