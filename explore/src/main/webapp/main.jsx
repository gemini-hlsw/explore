import "/common/theme/semantic.less";
import "/common/less/style.less";
import "/common/less/components/visualization.less";
import "/common/less/vendor/aladin.less";
import "/common/sass/charts.scss";
import "/common/sass/datepicker.scss";
import "/common/less/vendor/react-reflex.less";
import "github-markdown-css/github-markdown.css";
import "react-circular-progressbar/dist/styles.css";

import { Explore } from "@sjs/main.js";

// Setting this here shouldn't be necessary if we get `vite-plugin-environment` to work.
// See vite.config.js. Although configured there, the value is not getting to runtime for some reason.
// Anyway, for the moment this is not having the desired effect either, and we are downgrading cats-effect.
process.env.CATS_EFFECT_TRACING_MODE="none";
Explore.runIOApp();

if (import.meta.hot) {
  import.meta.hot.accept();
  import.meta.hot.dispose(_ => {
    // Reset the IO runtime
    Explore.resetIOApp();
  });
}
