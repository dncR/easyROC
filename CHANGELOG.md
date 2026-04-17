# Changelog

All notable changes to this project are documented in this file.

## [2026-04-18] - Sprint 7 Closure (EASY-048)

### Added

- Release evidence and monitoring docs:
  - `docs/release-evidence-rc-20260417193256.md`
  - `docs/post-release-monitoring-rc-20260417193256.md`
  - `docs/post-release-closure-rc-20260417193256.md`
- Production deployment playbook docs completed:
  - `docs/deployment.md`
  - `docs/runbook.md`
  - `docs/healthchecks.md`
  - `docs/env-secret-management.md`
  - `docs/observability.md`

### Changed

- Application architecture modernized from monolithic `ui.R/server.R` flow to R-first modular structure with `app.R` entrypoint and `mod_*` contracts.
- Test and quality workflow standardized:
  - `testthat` domain and contract coverage extended
  - `shinytest2` critical-flow smoke coverage added
  - lint + static checks integrated into CI
- Container runtime and release process standardized:
  - reproducible image build + staging/prod promote flow
  - env/secret validation gate in deploy scripts
  - readiness/health checks integrated into deploy wait logic

### Fixed

- Critical compatibility and robustness issues from early modernization sprints:
  - `status/event` type handling fixes
  - removal of non-exported `:::` package calls
  - upload/input guard improvements
  - legacy bootstrap/dependency cleanup in server orchestration
- Release-time container blocking issues:
  - missing system libs for `httpuv` compilation (`pkg-config`, `zlib1g-dev`, `curl`)
  - runtime `renv` autoload drift in container startup path

### Notes

- Release candidate: `easyroc-release:rc-20260417193256`
- Post-release monitoring outcome: no P0/P1 incident, no hotfix required.
- Known non-blocking warnings remain in runtime logs (DT deprecations and one `legend` warning path), tracked for backlog follow-up.
