# CLAUDE.md

Guidance for Claude Code working in this repository.

## What this is
An R **script collection** (not a package) for developing/validating a 4-point
Likert patient scale in psychiatric nursing. ~59 `*.R` files in the repo root,
organized as triads: `*_calculator.R` (compute), `*_display.R` (print/plot),
`*_main.R` (controller / public `analyze_*()`). Loader: `load_scripts.R`.
Data is pulled from Google Sheets at runtime.

## Golden rules — research reproducibility first
1. Do NOT change analysis behavior; refactors must be behavior-preserving and
   existing numeric outputs must not change.
2. Do NOT alter variable names, item names, subscale/factor definitions,
   missing-value handling, factor-analysis settings, or correlation handling.
3. Do NOT edit raw/source data. Data is in Google Sheets; never commit
   `system_config.yaml` or `.secrets/`.
4. Do NOT change the `source()` order in `load_scripts.R`.
5. Use project-root-relative paths; avoid `setwd()` in analysis code.
6. Ask before adding R packages or running any `renv` command.
7. NEVER push to `main`. Stop and ask before any push.
8. Present a change plan and get explicit approval before editing.

## Execution check (Claude side — no data / no auth)
`Rscript` is not on PATH. Use the full path. **Use R-4.4.0** — that is where the
analysis packages live (user library `~/AppData/Local/R/win-library/4.4`);
R-4.5.1 is installed but has no packages. A full run is impossible here (needs
Google OAuth + the gitignored `system_config.yaml`), so the Claude-side check is
parse + package load + sourcing only:

    & "C:\Program Files\R\R-4.4.0\bin\Rscript.exe" scripts/smoke_check.R

## Tests (data-free structure guardrails)

    & "C:\Program Files\R\R-4.4.0\bin\Rscript.exe" -e "testthat::test_dir('tests/testthat')"

If you change `analysis_config.yaml` on purpose, update the matching test.

## Division of labor
- Claude Code: edit, smoke-check, testthat, review `git diff`, push to a WORK
  BRANCH (never `main`).
- RStudio: `pull`, full pipeline run, figures, final numerical verification.

## Notes
- No `renv.lock` yet — package versions are not locked (reproducibility risk).
- `.gitignore` ignores itself and `system_config.yaml`.
