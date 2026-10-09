# Dump every registered table (with notes, figure name and write-up) into one plain-text file for review.
suppressPackageStartupMessages(library(dplyr))
root <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))
OUT <- file.path(root, "output", "2026-10-09")
files <- c("primary", "so1", "impact_bus", "impact_vs", "roadmap")
args <- commandArgs(TRUE); if (length(args)) files <- args
lines <- character()
for (f in files) { r <- readRDS(file.path(OUT, "registry", paste0(f, ".rds")))
  for (x in r) {
    lines <- c(lines, strrep("=", 110), paste0("[", x$id, "] ", x$title), paste0("Section: ", x$section, " | Figure: ", x$figure))
    tabs <- if (is.data.frame(x$table)) list(x$table) else x$table
    for (j in seq_along(tabs)) { if (!is.null(names(tabs)[j])) lines <- c(lines, paste0("-- ", names(tabs)[j]))
      lines <- c(lines, capture.output(write.table(tabs[[j]], sep = " | ", quote = FALSE, row.names = FALSE, na = ""))) }
    lines <- c(lines, "NOTES:", paste0("  - ", x$notes[nzchar(x$notes)]), "WRITE-UP:", paste0("  ", seq_along(x$writeup), ". ", x$writeup), "")
  } }
writeLines(lines, file.path(OUT, "registry", paste0("dump_", paste(files, collapse = "+"), ".txt")))
cat(length(lines), "lines\n")
