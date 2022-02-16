import "/common/theme/semantic.less";
import "/common/less/style.less";
import "/common/less/components/visualization.less";
import "/common/less/vendor/aladin.less";
import "/common/sass/charts.scss";
import "/common/sass/datepicker.scss";
import "/common/less/vendor/react-reflex.less";
import "github-markdown-css/github-markdown-light.css";
import "react-circular-progressbar/dist/styles.css";

import { Explore } from "@sjs/main.js";

// Setting this here shouldn't be necessary if we get `vite-plugin-environment` to work.
// but for now we can survive setting this only on dev
if (!process) {
  process = {
    env: {},
  };
}

if (import.meta.env.DEV) {
  process.env = { CATS_EFFECT_TRACING_MODE: "none" };
}

Explore.runIOApp();

if (import.meta.hot) {
  import.meta.hot.accept();
  import.meta.hot.dispose((_) => {
    // Reset the IO runtime
    Explore.resetIOApp();
  });
}
