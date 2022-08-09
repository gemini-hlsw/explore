(async () => {
  const worker = await import("@workers/exploreworkers.js");
  worker.ItcServer.runWorker();
})();
