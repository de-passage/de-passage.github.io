// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    Array.from(document.querySelectorAll('body > div')).map(x => x.remove());
    require("./output/Main").main();
  });
}
console.log('Starting app');
require("./output/Main").main();