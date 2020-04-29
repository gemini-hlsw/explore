import "resources/theme/semantic.less";
import "resources/less/style.less";
import "resources/aladin.css";

import App from "sjs/explore-fastopt.js";
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
      "Menu",
      "StreamRenderer",
      "Ref",
      "RefFindNode",
      "Sidebar",
      "Tile",
      "Button",
      "Icon",
      "DropdownItem",
      "DropdownMenu"
    ]
  });
}

if (module.hot) {
  module.hot.accept();
  App.Explore.runIOApp();
}
