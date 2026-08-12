# Contributing to fbi

## This repository is a mirror

Development happens on Gitea at
[gitea.civilytics.org/Civilytics/fbi_cde](https://gitea.civilytics.org/Civilytics/fbi_cde).
This GitHub repository mirrors that repo and exists for CRAN/r-universe
visibility and for the Windows/macOS coverage GitHub Actions gives us that
our Linux-only Gitea runner cannot.

Pull requests opened here are welcome. They get fetched, applied to the
canonical Gitea repository, and synced back -- because that merge preserves
your commits' original SHAs, GitHub will mark the PR "Merged" on its own once
the sync completes, without anyone visibly clicking Merge. That is the
normal, successful outcome, not a rejection. If it isn't going to be merged,
you'll get an actual reply saying so.

## Issues

File bugs and feature requests on Gitea:
<https://gitea.civilytics.org/Civilytics/fbi_cde/issues>. GitHub issues work
too and get triaged the same way, but Gitea is where the discussion happens.

## House rules

See [CLAUDE.md](CLAUDE.md) for the project's architecture and conventions --
in particular, the single network seam through `cde_request()`, the
offline-fixture + live-test pairing every data path needs, and the lean
base-R dependency policy. `R CMD check` must stay clean before a PR merges.
