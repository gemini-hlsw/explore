import "/common/theme/semantic.less";
import "/common/less/style.less";
import "/common/sass/visualization.scss";
import "/common/less/vendor/aladin.less";
import "/common/sass/charts.scss";
import "/common/sass/datepicker.scss";
import "/common/less/vendor/react-reflex.less";
import "github-markdown-css/github-markdown-light.css";
import "react-circular-progressbar/dist/styles.css";
import 'react-toastify/dist/ReactToastify.css';

import { Explore, ExplorePWA } from "@sjs/main.js";

import { registerSW } from "virtual:pwa-register";

// Setup the Service Worker
if ("serviceWorker" in navigator
  && !/local.lucumay.xyz/.test(window.location)) {
    ExplorePWA.runServiceWorker();
}

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
