import "resources/theme/semantic.less";
import "resources/less/style.less";

import App from "sjs/observationtree-fastopt.js";

if (module.hot) {
  module.hot.accept();
  App.ObsTreeTest.runIOApp();
}
