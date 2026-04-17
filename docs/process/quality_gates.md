# easyROC Quality Gates

Last updated: 2026-04-17
Owner: team

## 1) Lint Gate (EASY-030)

Run:

```bash
Rscript --vanilla scripts/lint.R
```

Scope:

- Modernized module/helper files under `R/` (`mod_*`, `data_input_utils`, `shared_state`, `status_utils`, `plot_options_service`)
- All `tests/testthat/*.R` files

Notes:

- Lint profile intentionally focuses on low-noise checks during migration.
- Legacy style debt in untouched monolithic files is out of scope for this gate.

## 2) Static Checks Gate (EASY-030)

Run:

```bash
Rscript --vanilla scripts/static_checks.R
```

Rules:

- No `:::` usage in app/runtime code (`app.R`, `ui.R`, `server.R`, `R/*.R`)
- No direct `library(pROC|plyr|OptimalCutpoints)` runtime calls
- No `setwd()` usage in production app code

## 3) Full Local Quality Check

```bash
Rscript --vanilla scripts/lint.R
Rscript --vanilla scripts/static_checks.R
Rscript --vanilla tests/testthat.R
```

## 4) CI Integration

- Lint + static checks run in GitHub Actions via `.github/workflows/lint.yml`.
- Test + full PR gate enforcement is handled in `EASY-031`.
