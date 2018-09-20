'use strict';

require('./index.html');
require('./style.scss');

const { Elm } = require('./Main.elm');

Elm.Main.init({
  node: document.getElementById('main')
});
