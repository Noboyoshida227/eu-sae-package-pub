# How to install this `academic-docx-format` upgrade

These two files upgrade the **academic-docx-format** skill so it styles
tables in the Econometrica three-line style (with proper column widths,
small-caps headers, and padding) in addition to the headings and
equations it already handles.

I could not edit the installed skill directly because its folder is
mounted **read-only** to me, so the upgrade is provided here as drop-in
replacements.

## What changed

`scripts/format_report.py`

- **Tables** are now restyled: heavy top/bottom rules, a light rule
  under the header, no vertical or interior lines, **small-caps**
  headers (previously bold), **fixed column widths** so columns no
  longer collapse (the important fix for pandoc-generated tables), and
  generous cell padding.
- Table-level (`tblPr`) and cell-level (`tcPr`) properties are now
  sorted into canonical OOXML order, so the new borders/widths validate.
- Headings and body text are set to **black**.

`SKILL.md` — documents the table behaviour and adds table triggers.

## To install

1. Locate the installed skill folder. It is the `academic-docx-format`
   directory inside your Claude app's skills location, e.g.
   `…\Claude\…\skills-plugin\…\skills\academic-docx-format\`.
2. Replace these two files with the ones in this folder:
   - `scripts/format_report.py`
   - `SKILL.md`
3. Restart / reload Claude so the skill picks up the new files.

The script is self-contained (only `python-docx` and `lxml`) and
idempotent — running it twice gives the same result. Test it with:

```bash
python3 scripts/format_report.py input.docx output.docx
```

If editing the app-managed folder is inconvenient, share these files
with whoever maintains the skill, or re-import the skill through the
app's skill-management UI.
