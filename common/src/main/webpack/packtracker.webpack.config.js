const Merge = require("webpack-merge");
const Web = require("./prod.webpack.config");
const PacktrackerPlugin = require("@packtracker/webpack-plugin");

const PackTracker = Merge(Web, {
  plugins: [
    new PacktrackerPlugin({
      project_token: '6804e495-5d60-40d7-a2cc-25875362aadc',
      upload: true,
      fail_build: true,
      branch: process.env.GITHUB_REF.split("/")[2]
    })
  ]
});

module.exports = PackTracker;
