import 'unfonts.css';
import '/common/sass/explore-primereact.scss';
import '/lucuma-css/lucuma-ui-layout.scss';
import '/lucuma-css/lucuma-ui-login.scss';
import '/lucuma-css/lucuma-ui-sequence.scss';
import '/lucuma-css/lucuma-ui-side-tabs.scss';
import '/lucuma-css/lucuma-ui-table.scss';
import '/lucuma-css/lucuma-ui-variables-dark.scss';
import '/lucuma-css/lucuma-ui-variables-light.scss';
import '/lucuma-css/solar-system.scss';
import '/lucuma-css/moon.scss';
import 'primeicons/primeicons.css';
import '/common/css/react-resizable.css';
import '/common/css/react-grid-layout.css';
import '/common/sass/explore-grid.scss';
import '/common/sass/visualization.scss';
import '/common/sass/tooltips.scss';
import '/common/sass/aladin.scss';
import '/common/sass/charts.scss';
import '/common/sass/datepicker.scss';
import '/common/sass/explore.scss';
import '/common/css/github-markdown.css';
import 'react-circular-progressbar/dist/styles.css';

import { Explore, ExplorePWA } from '@sjs/explore.js';

import { registerSW } from 'virtual:pwa-register';

if (import.meta.env.DEV) {
  process.env = { CATS_EFFECT_TRACING_MODE: 'none' };
}

// Get dynamic enums static data from ODB rest point and into a global variable.
// ODB rest URL is resolved from environments.conf.json.
fetch('/environments.conf.json').then((response) => {
  response.json().then((environments) => {
    const getODBRestURLForHost = (host) => {
      const filtered = environments.filter((e) => e.hostName === host);
      return filtered.length ? filtered[0].odbRestURI : null;
    };

    const specificHostURL = getODBRestURLForHost(window.location.host);
    const url = specificHostURL ? specificHostURL : getODBRestURLForHost('*');

    // Suppress vite warning about dynamic imports it can't handle.
    import(/* @vite-ignore */ `${url}/export/enumMetadata`).then((enumMetadataModule) => {
      // Set it globally so it can be read in Scala code.
      window.enumMetadataString = enumMetadataModule.enumMetadata;

      // Setup the Service Worker, after Explore is started
      if ('serviceWorker' in navigator && !/local.lucuma.xyz/.test(window.location)) {
        ExplorePWA.runServiceWorker();
      }

      // IMPORTANT: Start explore **after** the PWA service worker
      // Otherwise, errors on load may swallow the service worker
      // And leave the user unable to upgrade forever
      Explore.runIOApp();

      if (import.meta.hot) {
        import.meta.hot.accept();
      }
    });
  });
});
