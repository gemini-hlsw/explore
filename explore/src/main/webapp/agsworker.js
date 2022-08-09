(async () => {
  const worker = await import("@workers/exploreworkers.js");
  worker.AgsServer.runWorker();
})();
