import "/common/theme/semantic.less";
import "/common/less/style.less";
import "/common/less/components/visualization.less";
import "/common/less/vendor/aladin.less";
import "/common/sass/charts.scss";
import "/common/sass/datepicker.scss";
import "/common/less/vendor/react-reflex.less";

import { Explore } from "@sjs/main.js";
Explore.runIOApp();

if (import.meta.hot) {
  import.meta.hot.accept();
}
