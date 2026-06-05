---
name: academic-docx-format
description: Apply a consistent academic / journal-style format to a Word (.docx) document — Times New Roman body, structured Heading 1/2/3 styles, justified paragraphs, proper spacing around inline and display equations, and Econometrica-style tables (three-line rules, small-caps headers, fixed column widths). Use this whenever the user asks to "format", "clean up", "polish", "apply our report style to", "make journal-ready", "format the tables", or "tidy the headings/equations/tables in" a .docx file — especially when the document is an academic paper, working paper, technical report, or anything with equations and tables. Trigger even if the request is casual ("make this look right", "format my report", "fix the section headings") as long as the target is a Word document.
---

# Academic Word-Document Formatter

This skill applies a consistent house style to an existing Word
document. It restyles headings, body text, paragraph spacing, the
spacing around equations, and **tables** (Econometrica style) — without
altering the words.

The style is the one used in Nobuo's Spatial Price Adjustments draft
(econometrica-leaning):

| Element       | Font                  | Properties                                                          |
|---------------|-----------------------|---------------------------------------------------------------------|
| Normal body   | Times New Roman 11pt  | Justified, line spacing 1.15, 6pt after, black                      |
| Heading 1     | Times New Roman 12pt  | **Bold**, CENTERED, 24pt before / 12pt after, keep-with-next, black |
| Heading 2     | Times New Roman 11pt  | ***Bold + italic***, LEFT, 14pt before / 6pt after, keep-with-next  |
| Heading 3     | Times New Roman 11pt  | *Italic* (not bold), LEFT, 10pt before / 4pt after, keep-with-next  |

Equations:

- **Inline equations** (math embedded in a regular paragraph) get
  exactly one regular space on each side, so the math no longer runs
  into adjacent words. Punctuation that hugs the preceding token
  (period, comma, semicolon, colon, `!`, `?`, closing bracket) is left
  alone — typesetting convention is no space before punctuation.
  Likewise for opening brackets directly before the equation.
- **Display equations** (math on their own line) are centered to the
  page and sit 8pt above the following paragraph (0pt before, 8pt
  after) with lineRule="atLeast" at 240 — single line spacing for the
  equation row so it doesn't pick up the body's 1.15 leading, but the
  row can still grow as tall as the math needs without clipping.
  Inherited paragraph indentation is cleared so the centering is true
  page-center. Any empty paragraphs immediately adjacent to a display
  equation are removed. This treatment applies to both real display
  equations (m:oMathPara) AND inline-math paragraphs whose only content
  is the equation.

Tables (Econometrica style):

- **Three-line (booktabs) rules** — a heavy rule (≈1.5pt) above the
  table, a light rule (≈0.5pt) under the header row, and a heavy rule
  below the last row. **No vertical lines and no interior horizontal
  lines** between data rows.
- **Header row in SMALL CAPS** (not bold); all cell text Times New Roman.
- **Fixed layout with explicit column widths** that fill the text width
  (the stub/first column slightly narrower), so columns never collapse.
  This is essential for tables produced by pandoc, which arrive with a
  zero table width and no column grid and otherwise render with a
  crushed second column.
- **Generous cell padding** (≈60 twips top/bottom) so rows are not cramped.
- If a paragraph reading "Table 1", "Table V.2", etc. sits immediately
  above a table, it is recentered as "TABLE …" and a subtitle line just
  below it is set in small caps — the Econometrica caption convention.
  (The skill reformats existing captions; it does not invent titles.)
- A "Note:" / "Notes:" paragraph immediately after a table is set to 9pt.

## How to use it

The skill bundles a self-contained Python script. Call it with an
input and output path:

```bash
python3 <SKILL_DIR>/scripts/format_report.py <input.docx> <output.docx>
```

Pass distinct paths if you want to preserve the original; passing the
same path overwrites in place.

The script:

1. Updates / creates the **Normal**, **Heading 1**, **Heading 2**, and
   **Heading 3** style definitions in `word/styles.xml` (font, size,
   weight, alignment, spacing, black colour). Existing paragraphs that
   use these styles pick up the new look automatically.
2. Normalizes the whitespace around inline `<m:oMath>` equations.
3. Centers display `<m:oMathPara>` (and math-only) paragraphs, sets their
   spacing, and removes adjacent empty paragraphs.
4. Restyles every `<w:tbl>`: three-line rules, small-caps header,
   fixed column widths, padding; reformats any caption/subtitle above
   and any "Note:" paragraph below.
5. Runs a final pass that sorts the children of every `<w:pPr>`,
   `<w:rPr>`, `<w:tblPr>`, and `<w:tcPr>` into canonical OOXML schema
   order so the output validates against the Word schema.

## When to invoke

Whenever the user asks for any of:

- "format this Word doc / .docx"
- "apply our style", "make it journal-ready"
- "fix the heading styles / equations in <file>"
- "format the tables" / "make the tables Econometrica style"
- "clean up <file>.docx"

It's also reasonable to suggest this skill proactively if the user
hands over a draft with inconsistent heading styles, crowded inline
equations, or gridline-heavy tables.

## Important notes

- The script changes **formatting only** — body text, equation content,
  images, footnotes, and tracked changes are preserved. Tables keep
  their content and structure; only their borders, header style, column
  widths, and padding change.
- Heading levels are inferred from the document's existing
  `Heading 1` / `Heading 2` / `Heading 3` styles. If headings are named
  differently, remap them to Word's built-in heading styles first, or
  modify the `STYLE_SPECS` dict in `format_report.py`.
- Column widths are derived from the section's page size and margins
  (falling back to US-Letter, 1-inch margins → 9360 twips). The stub
  column is set slightly narrower and the remaining columns share the
  rest equally. Adjust the width logic in `_format_econometrica_table`
  if a specific layout is needed.
- The script is idempotent: running it twice produces the same file.

## Why these choices

- A single normalization-at-the-end pass for OOXML child ordering
  (paragraphs, runs, table properties, cell properties) is far more
  robust than inserting each new child at exactly the right schema
  position. Word rejects misordered children, so this safety net
  matters — and it is why borders/widths can be appended freely above.
- Fixed column widths are required because pandoc emits tables with
  `tblW=0` and no `tblGrid`; without explicit widths the second column
  collapses in both Word and LibreOffice.
- Inline-equation spacing is asymmetric on purpose: forcing a space
  before a period would produce "f(x) ." which is wrong.

## File layout

```
academic-docx-format/
├── SKILL.md
└── scripts/
    └── format_report.py
```
