# Execute the imputation-gap capstone plan for the `fbiCDE` R package

## Your task
Implement `specs/plans/2026-10-15-imputation-gap.md` in this repository,
task by task (six tasks): rename `get_estimated_*()` to `get_reported_*()`,
add bundled NIBRS-estimate vocabularies, `get_nibrs_estimate()`,
`imputation_gap()`, the `estimates` vignette and term definitions, then
verify everything. Done means a `claude/*` branch with one commit per task, a
pull request on GitHub, offline and live test suites passing, `R CMD check`
clean, and the PR description reporting the test counts, the check result
and the California and national numbers from the knitted vignette.

## Environment
- You are in a Claude Code cloud session on **GitHub**:
  `github.com/civilytics/fbi_cde`. That is the public copy; the private
  primary is a Gitea repo you cannot reach. Bare `#N` numbers in the repo
  (`#42`, `#58`, ...) are **Gitea** issues. GitHub issues are written `GH-N`
  in commits (for example GH-3).
- Read these first, in this order: `CLAUDE.md` (house rules; they override
  any skill default), `specs/2026-10-09-capstone-imputation-gap-design.md`
  §3.1, §3.2, §3.3 and §6, then the plan. The plan is authoritative for what
  to build; where the spec's §4 and §6 differ, §6 wins.
- R package, `R (>= 3.5.0)`, base R; Imports are only `httr`, `jsonlite`,
  `datasets`, `utils`. Tests are testthat 3e. Tools the plan uses:
  `devtools`, `roxygen2`, `knitr`, `rmarkdown`, `pkgdown`.
- **Network:** Task 2 Step 4 (`Rscript data-raw/api_vocabularies.R`), every
  live test and Task 5 Step 1 (knitting the vignette) need
  `https://cde.ucr.cjis.gov/LATEST/`. The previous cloud session (PR #2) ran
  the live suite successfully. That this session can reach it is
  *inferred*; check first with
  `curl -s -o /dev/null -w '%{http_code}' 'https://cde.ucr.cjis.gov/LATEST/nibrs-estimation/lookup/all'`
  (expect `200`). If it fails, do Tasks 1, 3 and 4 offline work, stop, and
  report; do not fake the data or the vignette.
- PR #2's container lacked `qpdf`, `covr`, `censusapi` and `tigris`; check
  notes about those are environmental.

## Current state — verified 2026-10-15
- `main` on both Gitea and GitHub has the plan at `797017c` ("docs:
  implementation plan for the imputation-gap capstone (#42)") and this
  handoff on top of it, in one or two docs-only commits. Confirm with
  `git log --oneline -4`.
- Offline suite on `e93b58b` (the parent of the two docs commits; nothing in
  `R/` has changed since): 1,299 expectations, 0 failures, 0 errors, 36
  skipped. All the skips are live tests. Command:
  `Rscript -e 'r <- as.data.frame(devtools::test(reporter = "silent")); cat(sum(r$nb), sum(r$failed), sum(r$error), sum(r$skipped))'`.
- The fixtures the plan's tests assert against are **already committed** in
  `tests/testthat/fixtures/`, recorded verbatim from the live API on
  2026-10-15: `nibrs-estimation-lookup-all.json`,
  `nibrs-estimation-national-133-2021.json`,
  `nibrs-estimation-state-6-133-{2021,2022}.json`,
  `nibrs-estimation-state-39-133-2022.json`,
  `summarized-state-CA-V-{2021,2022}.json` and
  `nibrs-counts-state-CA-{09A,11A,11B,11C,120,13A}-{2021,2022}.json`.
- The plan's code blocks were run against those fixtures in a scratch
  session before the plan was committed. They produce exactly the plan's
  expected values: California 2022 `reported` 193,824, `reported_nibrs`
  71,625, `reported_other` 122,199, `coverage_nibrs` 0.4658594,
  `share_estimated` 0.6546212; California 2021 `reported_other` -504 and
  `share_estimated` 0.8301776; 57 estimate states, 71 indicators, years
  2021 and 2022. If a test disagrees with the plan, suspect your
  transcription first.
- Open GitHub items: issue GH-3 (stale NIBRS fields in the bundled agency
  table; **not** part of this task). No open PRs.

## What to do next
1. Create a branch `claude/imputation-gap` from `main`.
2. Execute Tasks 1 to 6 of the plan in order; each task depends on the one
   before. Commit at the end of each task with the message the plan gives.
3. Push the branch and open a PR against `main`. Title: "Imputation-gap
   capstone: get_reported_*(), get_nibrs_estimate(), imputation_gap()".
   Body: what changed, per task; test counts (offline and live); the
   `R CMD check` result; the knitted vignette's California 2021 and 2022
   and national `share_estimated` values next to the spec §3.3 values
   (83.0%, 65.5%, 30.4%, 24.1%); and anything you had to deviate from in
   the plan, with the reason.

## Traps and hard-won facts
- **The estimates API uses its own numeric IDs, not FIPS:** state 6 is
  California, 48 Texas, 39 Ohio; indicator 133 is violent crime. `?year=` is
  required. A year it does not cover answers HTTP 400 with "NIBRS Estimation
  currently only supports the years: 2021,2022". The plan deliberately does
  not check the year locally, so the API's message stays right when the FBI
  adds a year.
- **`nibrs/{level}/{code}?type=counts`** returns monthly counts in the same
  shape as `summarized/`, but NIBRS submissions only. It takes NIBRS codes
  (`09A`, `13A`, ...); a summary code such as `V` answers **all zeros**,
  not an error. `type=totals` (or no `type`) returns breakdowns instead.
  The existing `parse_summarized_response()` parses the counts form
  unchanged.
- **`summarized/` counts both reporting formats** (NIBRS and the older
  summary format); the FBI's estimate is built from NIBRS alone. That is
  why California 2022 shows 98.8% `coverage` but 46.6% `coverage_nibrs`.
  The California 2022 difference between the estimate and reported crime
  comes from this format split, **not** from counting rules. An earlier
  draft of the spec blamed counting rules and was corrected (spec §3.2).
  Do not reintroduce that explanation.
- **The counting difference is real but small:** where everything is NIBRS
  (2021), the two reported series differ by about 1% for violent crime,
  5.4% for burglary and 7.5% for homicide (national 2021, measured
  2026-10-15). So `reported_other` and `gap` can be **negative**; never
  clamp them.
- **The FBI revises these series.** California 2022's reported violent
  crime moved from 193,814 (2026-10-09) to 193,824 (2026-10-15). Offline
  tests assert against the committed fixtures; live tests use ranges, not
  exact values.
- The bundled `fbi_api_agencies$nibrs` and `$nibrs_start_date` are stale
  (GH-3): `FALSE`/`NA` for every California agency. Do not use them.
- The CDE's OpenAPI spec is embedded in the CDE web app's JavaScript bundle;
  `.../LATEST/v2/api-docs` is a 404. The bundle also lists
  `estimate/state/{state}/{offense}` and `nibrs/state/{state}/{offense}/all`.
  Both 404 when called; do not chase them.
- `tests/testthat/test-ascii-docs.R` fails on LaTeX-hostile characters in
  `man/*.Rd` (no `±`, `≈`, Unicode minus). Roxygen here uses markdown, which
  escapes `%` in ordinary text; inside a raw `\describe{}` block avoid `%`
  (the plan already does).
- `docs/` is pkgdown's output folder. Specs go in `specs/`, plans in
  `specs/plans/`; a hook and the Gitea CI reject `docs/superpowers/`. This
  overrides the superpowers skills' default locations.
- Vignettes are precomputed: edit `*.Rmd.orig`, and for text-only changes
  make the identical edit in the shipped `*.Rmd`. Knit only
  `estimates.Rmd.orig`, with the command in the plan.
- `README.md` is generated from `README.Rmd`; regenerate it with the
  plan's `rmarkdown::render` command and delete any `README.html`.

## Do NOT
- Do not merge the PR, push to `main`, or force-push. The operator lands it
  locally (signed merge) and pushes to both remotes.
- Do not re-record or edit the committed fixtures. If one looks wrong,
  report it.
- Do not run `data-raw/precompute_vignettes.R`; it re-knits every vignette
  and makes hundreds of live requests.
- Do not add packages to `Imports`, call `httr` outside `cde_request()`, or
  use `stats::` functions.
- Do not rename `get_estimated_*` inside `specs/`; those documents are
  historical.
- Do not write closing keywords (`fixes #N`, `closes GH-N`) in commits. Gitea
  #42 is closed by hand after landing.
- Do not fix GH-3 in this branch; it is separate work.
- If the knitted vignette's numbers contradict the prose around them (for
  example, if California's own 2021 count of 183,546 is no longer below the
  FBI's lower bound), stop and report. Do not reword the prose to fit.

## Open questions
- `coverage_nibrs` was added in the plan, after the spec's §6 decisions
  were recorded: it comes free in the NIBRS responses and shows what the
  FBI's estimate actually rests on. The operator was told; it is in the
  plan, so build it. If it is dropped in review, it removes one column and
  two test assertions.
- Nothing else needs a decision before you start.
