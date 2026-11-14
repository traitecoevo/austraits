# austraits — agent & contributor guide

`austraits` is the R interface to AusTraits, the Australian plant trait database: a thin
AusTraits-specific layer over [`traits.use`](https://github.com/traitecoevo/traits.use), which holds
the generic code for any `traits.build` database (split out in 2026, austraits#188).

## Repo-local guidance

- **Code:** `R/load_austraits.R` (`load_austraits()`, `get_versions()`, `get_version_latest()`:
  version lookup on Zenodo, then `traits.use::load_database()`), `R/plot_locations.R` (wraps
  `traits.use::plot_locations()` with the map of Australia, `australia_map_raster` in
  `R/sysdata.rda`), `R/reexports.R` (re-exports every other traits.use export).
- **Generic code belongs in traits.use, not here.** Fix bugs in `extract_*()`, `join_*()` etc. there.
  `tests/testthat/test-reexports.R` checks austraits re-exports every traits.use export; add new
  traits.use exports to `R/reexports.R`.
- **Build/test (standard R-package workflow):** `devtools::load_all()`, `devtools::test()`,
  `devtools::check()`. The package targets `R >= 4.1.0`. Lifecycle badge is **stable**.
- **Default branch:** `master`. Docs/pkgdown site at <https://traitecoevo.github.io/austraits/>.

> Heads-up: `traits.build` `develop` installs `austraits` from branch `source-references-296`
> (`Remotes:`), not `master`, until it moves to `traits.use` (separate traits.build PR). **Do not
> delete that branch** before then: deleting it broke traits.build and austraits.build CI on
> 2026-11-14. After the move, `traits.build` no longer needs `austraits` at all.

> Heads-up: from Sept 2024 `austraits` works against any `traits.build` database (not just
> `austraits.build`). `austraits.build` versions **< 5.0 are unsupported** by the current package —
> older builds need an older tagged `austraits` (e.g. `@v2.2.2`).

---

## AusTraits family — cross-package context

`austraits` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the AusTraits-specific R package over
`traits.use` (the access/wrangle API for traits.build-format databases); loads the released `.rds`
via `load_austraits()`. Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/master/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/master/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/master/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/master/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

**Commit messages:** every family repo squash-merges, so the **PR title and body become the permanent
commit message**. Keep the subject ≤50 characters as typed and the body ≤10 lines; put the working
detail — what you tried, benchmarks, test counts, rejected alternatives — in the **first PR comment**
instead. Full convention:
[`commit-messages.md`](https://github.com/traitecoevo/austraits-meta/blob/master/governance/commit-messages.md).

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
