import '/common/sass/explore-primereact.scss';
import '/lucuma-css/lucuma-ui-login.scss';
import '/lucuma-css/lucuma-ui-table.scss';
import '/lucuma-css/solar-system.scss';
import '/lucuma-css/moon.scss';
import 'primereact/resources/primereact.min.css';
import 'primeicons/primeicons.css';
import '/common/less/style.less';
import '/common/sass/visualization.scss';
import '/common/sass/tooltips.scss';
import '/common/sass/aladin.scss';
import '/common/sass/charts.scss';
import '/common/sass/datepicker.scss';
import '/common/sass/explore.scss';
import 'github-markdown-css/github-markdown-light.css';
import 'react-circular-progressbar/dist/styles.css';

import { Explore, ExplorePWA } from '@sjs/explore.js';

import { registerSW } from 'virtual:pwa-register';

// but for now we can survive setting this only on dev
if (!process) {
  process = {
    env: {},
  };
}
if (import.meta.env.DEV) {
  process.env = { CATS_EFFECT_TRACING_MODE: 'none' };
}

Explore.runIOApp();

// Setup the Service Worker, after Explore is started
if ('serviceWorker' in navigator && !/local.lucuma.xyz/.test(window.location)) {
  ExplorePWA.runServiceWorker();
}

// Setting this here shouldn't be necessary if we get `vite-plugin-environment` to work.

if (import.meta.hot) {
  import.meta.hot.accept();
}
