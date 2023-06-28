// Dynamic import for side effect only. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/import#import_a_module_for_its_side_effects_only
(async () => {
  const worker = await import('@workers/exploreworkers.js');
  worker.ItcServer.runWorker();
})();
