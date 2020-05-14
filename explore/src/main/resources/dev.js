import "resources/theme/semantic.less";
import "resources/less/style.less";
import "resources/aladin.css";

import App from "sjs/explore-fastopt.js";

if (module.hot) {
  module.hot.accept();
  App.Explore.runIOApp();
}
