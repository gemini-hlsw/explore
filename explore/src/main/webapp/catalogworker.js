(async () => {
  const worker = await import("@workers/exploreworkers.js");
  worker.CatalogServer.runWorker();
})();
