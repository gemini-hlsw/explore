import "resources/theme/semantic.less";
import "resources/less/style.less";
import "resources/css/aladin.css";

import App from "sjs/targeteditor-fastopt.js";

if (module.hot) {
  module.hot.accept();
  App.TargetTest.runIOApp();
}
