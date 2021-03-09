import "resources/theme/semantic.less";
import "resources/less/style.less";
import "resources/less/components/visualization.less";
import "resources/less/vendor/aladin.less";
import "resources/css/charts.scss";
import "resources/css/datepicker.scss";
import "resources/less/vendor/react-reflex.less";

import "resources/conf/staging.conf.json";
import "resources/conf/production.conf.json";

var App = require("sjs/explore-opt.js");

App.Explore.runIOApp();
