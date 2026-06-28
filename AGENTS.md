# austraits — agent & contributor guide

`austraits` is an R package of **helpful functions to access, explore and wrangle data** from
`traits.build` relational databases. It is also the R interface to AusTraits, the Australian plant
trait database — `load_austraits()` fetches a released `.rds` build.

## Repo-local guidance

- **Code:** `R/` (functions), `tests/` (testthat), `man/` (generated docs), `NAMESPACE`,
  `vignettes/`. `data/`, `data-raw/`, and `inst/` hold packaged/raw assets (incl. the cheatsheet).
- **Access entry point:** `load_austraits()` (`R/load_austraits.R`) downloads/loads a released
  AusTraits build (`.rds`); `get_versions()` / `get_version_latest()` resolve versions.
- **Wrangle/extract:** `extract_*()` (data/dataset/taxa/trait), `join_*()` (taxa, methods,
  contributors, locations, context/location properties), `lookup_*()`, `trait_pivot_longer/wider()`,
  `summarise_database()`, `bind_databases()`, `flatten_database()`, `as_wide_table()`.
- **Build/test (standard R-package workflow):** `devtools::load_all()`, `devtools::test()`,
  `devtools::check()`. The package targets `R >= 4.0.0`; tidyverse-style Imports (dplyr, tidyr,
  purrr, etc.). Lifecycle badge is **stable**.
- **Default branch:** `master`. Docs/pkgdown site at <https://traitecoevo.github.io/austraits/>.

> Heads-up: `austraits` is the *downstream* access/wrangle layer in the data pipeline, but the
> R-package install graph runs the other way — `traits.build` **Imports `austraits`** and
> re-exports a few conversion helpers from it (`convert_df_to_list`, `convert_list_to_df1`,
> `convert_list_to_df2`, `bind_databases`, `flatten_database` — all in `NAMESPACE`). Changing those
> helpers can break `traits.build`; run its tests after touching them.

> Heads-up: from Sept 2024 `austraits` works against any `traits.build` database (not just
> `austraits.build`). `austraits.build` versions **< 5.0 are unsupported** by the current package —
> older builds need an older tagged `austraits` (e.g. `@v2.2.2`).

---

## AusTraits family — cross-package context

`austraits` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the user-facing access/wrangle API (R
package) for traits.build-format databases, including AusTraits; loads the released `.rds` via
`load_austraits()`. Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
