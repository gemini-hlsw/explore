import "/common/theme/semantic.less";
import "/common/less/style.less";
import "react-semantic-toasts/styles/react-semantic-alert.css";

import { Explore } from "@sjs/main.js";
Explore.runIOApp();

if (import.meta.hot) {
  import.meta.hot.accept();
}
