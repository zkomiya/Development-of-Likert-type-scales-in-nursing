# scripts/smoke_check.R
# ------------------------------------------------------------------
# Non-interactive smoke check for the Claude Code workflow.
# Does NOT run any analysis and does NOT touch Google Sheets / data / auth.
# Checks: (1) every *.R parses, (2) required packages are installed,
#         (3) load_scripts.R sources cleanly (only if all packages present).
# Run from the project root:
#   Rscript scripts/smoke_check.R
# This is a standalone process, so setwd() here does not affect your session.
# ------------------------------------------------------------------

# Resolve project root (the directory that contains load_scripts.R)
root <- getwd()
if (!file.exists(file.path(root, "load_scripts.R")) &&
    file.exists(file.path(root, "..", "load_scripts.R"))) {
  root <- normalizePath(file.path(root, ".."))
}
if (!file.exists(file.path(root, "load_scripts.R"))) {
  stop("Run from the project root: Rscript scripts/smoke_check.R")
}
setwd(root)
cat("Project root:", root, "\n\n")

# [1] Parse every top-level R script (no packages needed)
r_files <- list.files(root, pattern = "\\.R$", full.names = TRUE)
parse_errors <- list()
for (f in r_files) {
  msg <- tryCatch({ parse(file = f); NULL },
                  error = function(e) conditionMessage(e))
  if (!is.null(msg)) parse_errors[[basename(f)]] <- msg
}
cat(sprintf("[1] Parsed %d R files: %d error(s)\n", length(r_files), length(parse_errors)))
for (nm in names(parse_errors)) cat("    -", nm, ":", parse_errors[[nm]], "\n")

# [2] Required-package availability (matches load_scripts.R + internal deps)
required_pkgs <- c(
  "googlesheets4","tidyverse","yaml","pwr","psych","polycor","GPArotation",
  "clue","irr","boot","BlandAltmanLeh","irrCAC","lavaan","effectsize",
  "openxlsx","careless","EFA.MRFA","gridExtra","scales"
)
have <- vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
missing_pkgs <- required_pkgs[!have]
cat(sprintf("\n[2] Packages: %d/%d installed\n", sum(have), length(required_pkgs)))
if (length(missing_pkgs)) cat("    Missing:", paste(missing_pkgs, collapse = ", "), "\n")

# [3] Source load_scripts.R (defines functions + prints usage; runs NO analysis)
cat("\n[3] load_scripts.R source check: ")
if (length(parse_errors)) {
  cat("SKIPPED (parse errors above)\n")
} else if (length(missing_pkgs)) {
  cat("SKIPPED (missing packages above)\n")
} else {
  msg <- tryCatch({ sys.source("load_scripts.R", envir = new.env()); NULL },
                  error = function(e) conditionMessage(e))
  cat(if (is.null(msg)) "OK (all scripts loaded, no analysis executed)\n"
      else paste0("FAILED: ", msg, "\n"))
}

ok <- length(parse_errors) == 0
cat("\nSmoke check", if (ok) "PASSED (parse-level)" else "FAILED", "\n")
if (!ok) quit(status = 1)
