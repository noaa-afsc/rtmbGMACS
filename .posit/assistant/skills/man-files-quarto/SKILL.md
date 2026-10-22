---
name: man-files-quarto
description: Regenerates quarto/qmd_ManFiles.qmd from this R package's man/*.Rd documentation, adding one table-of-contents heading per entry. Use when asked to rebuild, refresh, or update the combined Quarto function reference or qmd_ManFiles.qmd after package documentation changes.
compatibility: Designed for Posit Assistant in the rtmbGMACS project; rendering validation requires Quarto.
---

# Regenerate the combined function reference

Rebuild `quarto/qmd_ManFiles.qmd` completely from the current `man/*.Rd` files.

## Workflow

1. Read the project `AGENTS.md` and follow its conventions.
2. Discover all `man/*.Rd` files rather than relying on a hard-coded list. Sort them deterministically by filename.
3. Write valid Quarto YAML with the title `rtmbGMACS function reference` and HTML output with `toc: true`.
4. Convert each Rd file to embeddable HTML with R's standard `tools` package. Do not manually reinterpret Rd markup.
5. Precede each entry with a source comment in this exact form:

   `<!-- Source: man/example.Rd -->`

6. Add a unique level-two Markdown heading for every entry in this form:

   ``## `name`: Title``

   Derive the name and title from the Rd entry. Remove or omit any converted title heading that duplicates this heading.
7. Preserve all available documentation sections, including Description, Usage, Arguments, Value, Details, and Examples. Put blank lines between entries to prevent malformed boundaries.
8. Do not modify anything in `man/`. Do not retain temporary conversion scripts, rendered sites, or other generated artifacts.

## Validation

Confirm that:

- The source comments and level-two headings have the same count as `man/*.Rd`.
- Source comments occur in the same filename order as the sorted input files.
- Every source comment is followed by one level-two entry heading.
- Level-two entry headings are unique.

If Quarto is available, render the document to temporary storage and remove the rendered output afterward. If Quarto is unavailable, report the limitation without installing it.

Finish with the output path, number of entries processed, validation result, and names of any files that could not be converted.

Treat `quarto/qmd_ManFiles.qmd` as generated output: always rebuild the entire file rather than editing individual entries.
