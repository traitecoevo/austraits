# austraits — agent & contributor guide

`austraits` is the R interface to AusTraits, the Australian plant trait database: a thin
AusTraits-specific layer over [`traits.use`](https://github.com/traitecoevo/traits.use), which holds
the generic code for any `traits.build` database (split out in 2026, austraits#188).

## Repo-local guidance

- **Code:** `R/load_austraits.R` (`load_austraits()`, `get_versions()`, `get_version_latest()`:
  the releases from `traits.use::list_versions()`, saved in `austraits_versions.json` for offline use, then
  `traits.use::load_database()`; the `austraits.json` saved by austraits 3.x is still read, so its caches load offline), `R/plot_locations.R` (wraps
  `traits.use::plot_locations()` with the map of Australia, `australia_map_raster` in
  `R/sysdata.rda`), `R/reexports.R` (re-exports every other traits.use export).
- **Generic code belongs in traits.use, not here.** Fix bugs in `extract_*()`, `join_*()` etc. there.
  `tests/testthat/test-reexports.R` checks austraits re-exports every traits.use export; add new
  traits.use exports to `R/reexports.R`.
- **Build/test (standard R-package workflow):** `devtools::load_all()`, `devtools::test()`,
  `devtools::check()`. The package targets `R >= 4.1.0`. Lifecycle badge is **stable**.
- **Test data:** the package ships no AusTraits data of its own (`R/sysdata.rda` holds only the map).
  Tests of general behaviour use `traits.use::austraits_lite`; the loading tests fake a release in a
  temporary folder, with Zenodo mocked. Only `test-load_austraits_online.R` reaches Zenodo (behind
  `skip_on_cran()`/`skip_if_offline()`): it lists the releases and downloads AusTraits 5.0.0, the oldest
  supported release, into the gitignored `tests/testthat/ignore/data/austraits`.
- **Default branch:** `master`. Docs/pkgdown site at <https://traitecoevo.github.io/austraits/>.

> `traits.build` no longer installs `austraits`: it depends on `traits.use` since traits.build#321
> (2026-11-15). Before deleting any branch here, search the family for `Remotes:` pins to it: a pin
> to `source-references-296` broke traits.build and austraits.build CI on 2026-11-14.

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
