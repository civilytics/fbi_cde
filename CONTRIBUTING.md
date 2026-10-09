# Contributing to fbiCDE

## Where development happens

Development happens in a private Gitea repository. This GitHub repository is
the package's public home: [r-universe](https://civilytics.r-universe.dev/fbiCDE)
builds releases from it, and GitHub Actions gives us Windows/macOS coverage
that our Linux-only Gitea runner cannot.

Pull requests opened here are welcome. They get fetched, applied to the
Gitea repository, and pushed back here -- because that merge preserves your
commits' original SHAs, GitHub will mark the PR "Merged" on its own once the
push lands, without anyone visibly clicking Merge. That is the normal,
successful outcome, not a rejection. If it isn't going to be merged, you'll
get an actual reply saying so.

## Issues

File bugs and feature requests here on GitHub:
<https://github.com/civilytics/fbi_cde/issues>.

## House rules

See [CLAUDE.md](CLAUDE.md) for the project's architecture and conventions --
in particular, the single network seam through `cde_request()`, the
offline-fixture + live-test pairing every data path needs, and the lean
base-R dependency policy. `R CMD check` must stay clean before a PR merges.
