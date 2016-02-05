function(env, director) {
  # Add mungebits to local environment.
  mungebits <- lapply(mungebits_names <- director$find(base = "lib/mungebits"),
    function(x) director$resource(x))

  mungebits_names <- chartr("/", ".",
    sapply(mungebits_names, function(x) director:::strip_root("lib/mungebits", x)))

  if ("package:syberiaMungebits2" %in% search()) {
    for (name in ls(as.environment("package:syberiaMungebits2"))) {
      env[[name]] <- getFromNamespace(name, "syberiaMungebits2")()
    }
  }

  for (i in seq_along(mungebits)) {
    env[[mungebits_names[i]]] <- mungebits[[i]]
  }
}
