import "resources/theme/semantic.less";
import "resources/less/style.less";

import App from "sjs/conditions-fastopt.js";
import React from "react";

// Enable why did you update plugin
if (process.env.NODE_ENV !== "production") {
  const { whyDidYouUpdate } = require("why-did-you-update");
  whyDidYouUpdate(React, {
    exclude: [
      "GridItem",
      "Resizable",
      "Draggable",
      "DraggableCore",
      "MenuItem",
      "ReactGridLayout",
      "ResponsiveReactGridLayout",
      /SizeM\w/,
      "Menu"
    ]
  });
}

if (module.hot) {
  module.hot.accept();
  App.Test.runIOApp();
}
