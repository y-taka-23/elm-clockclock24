const path = require('path');

module.exports = {
    mode: 'development',
    entry: {
        app: [
            path.resolve(__dirname, 'src', 'index.js'),
        ]
    },
    output: {
        path: path.resolve(__dirname, 'docs'),
        filename: '[name].js',
    },
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'file-loader?name=[name].[ext]',
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack-loader?verbose=true',
            },
            {
                test: /\.scss$/,
                loader: ['style-loader', 'css-loader', 'sass-loader'],
            },
        ],
        noParse: /\.elm$/
    },
    devServer: {
        inline: true,
        stats: { colors: true },
    }
};
