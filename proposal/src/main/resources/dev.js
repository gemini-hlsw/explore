import "resources/theme/semantic.less";
import "resources/less/style.less";

import "resources/conf/development.conf.json";

import App from "sjs/proposal-fastopt.js";

if (module.hot) {
  module.hot.accept();
  App.PropsTest.runIOApp();
}
