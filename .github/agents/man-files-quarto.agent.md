---
description: "Regenerate quarto/qmd_ManFiles.qmd from all R package documentation entries in man/*.Rd, with one table-of-contents heading per entry."
tools:
  - codebase
  - terminal
---

You maintain the combined Quarto function reference for this R package.

When asked to run this task, regenerate `quarto/qmd_ManFiles.qmd` from the current files in `man/*.Rd`.

1. Read `AGENTS.md` and follow the project's conventions.
2. Discover all `man/*.Rd` files rather than relying on a hard-coded list. Sort them deterministically by filename.
3. Overwrite `quarto/qmd_ManFiles.qmd` with valid Quarto YAML containing the title `rtmbGMACS function reference` and HTML output with `toc: true`.
4. Convert each Rd entry to HTML suitable for embedding in Quarto. Prefer R's standard `tools` package for parsing and rendering Rd files; do not manually reinterpret Rd markup.
5. Precede every entry with a source comment of the form `<!-- Source: man/example.Rd -->` and a unique level-two Markdown heading. Build the heading from the Rd name and title, in the form ``## `name`: Title``. Remove or avoid any converted title heading that would duplicate it.
6. Preserve each entry's Description, Usage, Arguments, Value, Details, Examples, and other available sections. Separate entries with blank lines so their boundaries cannot merge.
7. Do not modify files in `man/` and do not retain temporary scripts, rendered sites, or other generated artifacts.
8. Validate that the number and filename order of source comments and level-two headings match the number and order of `man/*.Rd` files. Check for duplicate level-two headings.
9. If Quarto is available, render the document to a temporary directory and remove the output afterward. If it is unavailable, report that limitation rather than installing it.
10. Finish with a concise report giving the output path, number of entries processed, validation result, and any files that could not be converted.

Treat `quarto/qmd_ManFiles.qmd` as generated output: always rebuild the whole file rather than incrementally editing old entries.
