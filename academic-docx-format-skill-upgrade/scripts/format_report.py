"""
Apply a consistent "academic report" style to a Word (.docx) document.

The style mirrors the formatting used in the reference report
(Spatial Price Adjustments / econometrica-style draft):

  - Body (Normal):     Times New Roman 11pt, justified, line spacing 1.15,
                       6pt space after.
  - Heading 1:         Times New Roman 12pt, bold, CENTERED,
                       24pt before, 12pt after, keep-with-next.
  - Heading 2:         Times New Roman 11pt, bold + italic, LEFT,
                       14pt before, 6pt after, keep-with-next.
  - Heading 3:         Times New Roman 11pt, italic (not bold), LEFT,
                       10pt before, 4pt after, keep-with-next.

Equations
  - Inline equations  (m:oMath inside a regular paragraph): one regular
                       space on each side, except next to closing/opening
                       punctuation.
  - Display equations (m:oMathPara): centered, 0pt before, 8pt after,
                       and any adjacent empty paragraphs are removed.

Tables (Econometrica style)
  - Caption number (e.g., "Table V.2")       -> "TABLE V.2", centered.
  - Subtitle line (e.g., "Price indices...") -> small caps, centered.
  - Borders: thick rule above the table, thin rule under the header row,
             thick rule below the last row; no vertical lines and no
             interior horizontal lines.
  - Header row text in SMALL CAPS (not bold); all cell text Times New Roman.
  - Fixed layout with column widths that fill the text width (stub column
             narrower) so columns never collapse -- essential for pandoc
             tables, which carry no widths.
  - Generous cell padding so rows are not cramped.
  - Following "Note:" / "Notes:" paragraphs set to 9pt.

Usage:
    python format_report.py <input.docx> <output.docx>
"""

from __future__ import annotations

import re
import shutil
import sys
from pathlib import Path

from docx import Document
from docx.oxml.ns import qn
from lxml import etree

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
M = "http://schemas.openxmlformats.org/officeDocument/2006/math"


def wq(tag):
    return f"{{{W}}}{tag}"


def mq(tag):
    return f"{{{M}}}{tag}"


PPR_ORDER = [
    "pStyle", "keepNext", "keepLines", "pageBreakBefore", "framePr",
    "widowControl", "numPr", "suppressLineNumbers", "pBdr", "shd",
    "tabs", "suppressAutoHyphens", "kinsoku", "wordWrap", "overflowPunct",
    "topLinePunct", "autoSpaceDE", "autoSpaceDN", "bidi", "adjustRightInd",
    "snapToGrid", "spacing", "ind", "contextualSpacing", "mirrorIndents",
    "suppressOverlap", "jc", "textDirection", "textAlignment",
    "textboxTightWrap", "outlineLvl", "divId", "cnfStyle", "rPr",
    "sectPr", "pPrChange",
]

RPR_ORDER = [
    "rStyle", "rFonts", "b", "bCs", "i", "iCs", "caps", "smallCaps",
    "strike", "dstrike", "outline", "shadow", "emboss", "imprint",
    "noProof", "snapToGrid", "vanish", "webHidden", "color", "spacing",
    "w", "kern", "position", "sz", "szCs", "highlight", "u", "effect",
    "bdr", "shd", "fitText", "vertAlign", "rtl", "cs", "em", "lang",
    "eastAsianLayout", "specVanish", "oMath",
]

# Canonical child order for table-level and cell-level properties, so the
# widths / borders / padding inserted below validate against the schema.
TBLPR_ORDER = [
    "tblStyle", "tblpPr", "tblOverlap", "bidiVisual", "tblStyleRowBandSize",
    "tblStyleColBandSize", "tblW", "tblJc", "tblCellSpacing", "tblInd",
    "tblBorders", "shd", "tblLayout", "tblCellMar", "tblLook", "tblCaption",
    "tblDescription", "tblPrChange",
]

TCPR_ORDER = [
    "cnfStyle", "tcW", "gridSpan", "hMerge", "vMerge", "tcBorders", "shd",
    "noWrap", "tcMar", "textDirection", "tcFit", "vAlign", "hideMark",
    "headers", "cellIns", "cellDel", "cellMerge", "tcPrChange",
]


STYLE_SPECS = {
    "Normal": {
        "font": "Times New Roman", "size_hpt": 22,
        "bold": False, "italic": False, "align": "both",
        "space_before": 0, "space_after": 120,
        "line": 278, "line_rule": "auto",
        "keep_with_next": False,
    },
    "Heading 1": {
        "font": "Times New Roman", "size_hpt": 24,
        "bold": True, "italic": False, "align": "center",
        "space_before": 480, "space_after": 240,
        "line": None, "line_rule": None,
        "keep_with_next": True,
    },
    "Heading 2": {
        "font": "Times New Roman", "size_hpt": 22,
        "bold": True, "italic": True, "align": "left",
        "space_before": 280, "space_after": 120,
        "line": None, "line_rule": None,
        "keep_with_next": True,
    },
    "Heading 3": {
        "font": "Times New Roman", "size_hpt": 22,
        "bold": False, "italic": True, "align": "left",
        "space_before": 200, "space_after": 80,
        "line": None, "line_rule": None,
        "keep_with_next": True,
    },
}


def _new_jc(value):
    el = etree.Element(wq("jc"))
    el.set(wq("val"), value)
    return el


def _new_spacing(before=None, after=None, line=None, line_rule=None):
    el = etree.Element(wq("spacing"))
    if before is not None:
        el.set(wq("before"), str(before))
    if after is not None:
        el.set(wq("after"), str(after))
    if line is not None:
        el.set(wq("line"), str(line))
    if line_rule is not None:
        el.set(wq("lineRule"), str(line_rule))
    return el


def _new_keep_next():
    return etree.Element(wq("keepNext"))


def _new_rfonts(font):
    el = etree.Element(wq("rFonts"))
    for attr in ("ascii", "hAnsi", "cs", "eastAsia"):
        el.set(wq(attr), font)
    return el


def _new_sz(half_points):
    el = etree.Element(wq("sz"))
    el.set(wq("val"), str(half_points))
    return el


def _new_szCs(half_points):
    el = etree.Element(wq("szCs"))
    el.set(wq("val"), str(half_points))
    return el


def _remove_local(parent, tag_local):
    for existing in parent.findall(wq(tag_local)):
        parent.remove(existing)


def apply_style_spec(style_elem, spec):
    pPr = style_elem.find(wq("pPr"))
    if pPr is None:
        pPr = etree.SubElement(style_elem, wq("pPr"))

    _remove_local(pPr, "jc")
    pPr.append(_new_jc(spec["align"]))

    _remove_local(pPr, "spacing")
    pPr.append(_new_spacing(
        before=spec.get("space_before"),
        after=spec.get("space_after"),
        line=spec.get("line"),
        line_rule=spec.get("line_rule"),
    ))

    _remove_local(pPr, "keepNext")
    if spec.get("keep_with_next"):
        pPr.append(_new_keep_next())

    rPr = style_elem.find(wq("rPr"))
    if rPr is None:
        rPr = etree.SubElement(style_elem, wq("rPr"))

    _remove_local(rPr, "rFonts")
    rPr.append(_new_rfonts(spec["font"]))

    _remove_local(rPr, "b")
    _remove_local(rPr, "bCs")
    if spec["bold"]:
        rPr.append(etree.Element(wq("b")))
        rPr.append(etree.Element(wq("bCs")))

    _remove_local(rPr, "i")
    _remove_local(rPr, "iCs")
    if spec["italic"]:
        rPr.append(etree.Element(wq("i")))
        rPr.append(etree.Element(wq("iCs")))

    _remove_local(rPr, "sz")
    _remove_local(rPr, "szCs")
    rPr.append(_new_sz(spec["size_hpt"]))
    rPr.append(_new_szCs(spec["size_hpt"]))

    # Black text (academic house style) unless the spec overrides it.
    _remove_local(rPr, "color")
    color = spec.get("color", "000000")
    if color:
        color_el = etree.Element(wq("color"))
        color_el.set(wq("val"), color)
        rPr.append(color_el)


def ensure_style(doc, name, spec):
    styles_el = doc.styles.element

    target = None
    for st in styles_el.findall(wq("style")):
        name_el = st.find(wq("name"))
        if name_el is not None and name_el.get(wq("val")) == name:
            target = st
            break

    if target is None:
        style_id = name.replace(" ", "")
        target = etree.SubElement(styles_el, wq("style"))
        target.set(wq("type"), "paragraph")
        target.set(wq("styleId"), style_id)
        name_el = etree.SubElement(target, wq("name"))
        name_el.set(wq("val"), name)

    apply_style_spec(target, spec)


def _iter_block_paragraphs(doc):
    body = doc.element.body
    for p in body.iter(wq("p")):
        yield p


def _para_pPr(p):
    pPr = p.find(wq("pPr"))
    if pPr is None:
        pPr = etree.Element(wq("pPr"))
        p.insert(0, pPr)
    return pPr


def _strip_trailing_breaks(p):
    """Display-equation paragraphs sometimes contain a trailing <w:br/>
    inside a <w:r>, which Word renders as an extra blank line inside the
    same paragraph — i.e. the "empty line" the user sees after every
    equation.  Remove any <w:br/> elements within the paragraph and drop
    runs that become empty after the break is removed.
    """
    # Collect breaks
    brs = list(p.findall(f".//{wq('br')}"))
    for br in brs:
        parent = br.getparent()
        parent.remove(br)
    # Drop runs that have no text or math inside any more
    for r in list(p.findall(wq("r"))):
        has_text = any((t.text or "") for t in r.iter(wq("t")))
        has_math = r.find(f".//{mq('oMath')}") is not None
        has_drawing = r.find(f".//{wq('drawing')}") is not None
        if not (has_text or has_math or has_drawing):
            r.getparent().remove(r)


def _make_space_run():
    """Empty w:r containing a single literal space with xml:space=preserve."""
    r = etree.Element(wq("r"))
    t = etree.SubElement(r, wq("t"))
    t.set(qn("xml:space"), "preserve")
    t.text = " "
    return r


def _unwrap_oMathPara(p):
    """Replace every <m:oMathPara> inside the paragraph with its inner
    <m:oMath> children, dropped into the paragraph at the same position.

    Why: Word renders a paragraph whose only non-pPr child is an
    <m:oMathPara> using display-math anchor centering — the equation is
    centered at the math anchor point (typically the = sign) rather than
    its geometric center, which visibly shifts asymmetric equations
    (LHS = LongRHS) right of the page midline.  LibreOffice does not
    exhibit this difference, so the issue was invisible in test renders.

    Unwrapping to bare inline <m:oMath> matches what happens when a user
    manually fixes the centering by deleting the front of the equation
    and pressing Enter again — Word treats the result as an inline math
    run inside a centered text paragraph and centers it geometrically.
    """
    for omp in list(p.iter(mq("oMathPara"))):
        parent = omp.getparent()
        idx = list(parent).index(omp)
        omaths = [c for c in omp if c.tag == mq("oMath")]
        parent.remove(omp)
        for i, omath in enumerate(omaths):
            parent.insert(idx + i, omath)


def _bracket_math_with_space_runs(p):
    """For each <m:oMath> that is a direct child of the paragraph, ensure
    there is an empty space-bearing <w:r> before and after it.  Without
    these flanking runs, Word can still fall into a display-math
    centering path for paragraphs whose only non-pPr child is bare math.
    The empty runs force Word into the inline-text-with-math centering
    path, which geometrically centers the line."""
    for child in list(p):
        if child.tag != mq("oMath"):
            continue
        idx = list(p).index(child)
        prev = p[idx - 1] if idx > 0 else None
        nxt = p[idx + 1] if idx + 1 < len(p) else None
        if prev is None or prev.tag != wq("r"):
            p.insert(idx, _make_space_run())
            idx += 1
        if nxt is None or nxt.tag != wq("r"):
            p.insert(idx + 1, _make_space_run())


def set_display_equation_paragraph(p):
    """Center a display-equation paragraph, give it 0pt before / 8pt after,
    strip any inherited indentation, line spacing, or in-paragraph line
    breaks that would push the equation off the page center, AND unwrap
    any <m:oMathPara> wrapper so Word geometric-centers the equation
    rather than anchor-centering it (which visibly shifts asymmetric
    equations to the right)."""
    # Strip <w:br/> elements first — they read as empty lines.
    _strip_trailing_breaks(p)
    # Unwrap m:oMathPara into bare inline m:oMath so Word uses
    # geometric-centering rather than anchor-point centering.
    _unwrap_oMathPara(p)
    # Bracket the now-bare m:oMath with empty space runs to keep Word
    # firmly on the inline-text centering path.
    _bracket_math_with_space_runs(p)

    pPr = _para_pPr(p)
    _remove_local(pPr, "jc")
    pPr.append(_new_jc("center"))
    _remove_local(pPr, "spacing")
    # Use lineRule="auto" with line=240 (single spacing) so the paragraph
    # does not inherit the body's 1.15 line spacing which adds visible
    # leading between the equation and the next paragraph.
    # space-after=0: any non-zero value combines with the equation
    # paragraph's natural line height (taller than text because of
    # subscripts/superscripts) to look like an empty line below it.
    pPr.append(_new_spacing(before=0, after=160, line=240, line_rule="atLeast"))
    _remove_local(pPr, "contextualSpacing")
    # Explicitly zero out indentation: removing the local <w:ind> isn't
    # enough if a parent style sets indent, so write our own zeros.
    _remove_local(pPr, "ind")
    ind = etree.SubElement(pPr, wq("ind"))
    ind.set(wq("left"), "0")
    ind.set(wq("right"), "0")
    ind.set(wq("firstLine"), "0")
    # Clear any tab stops on equation paragraphs - they can collude with
    # math layout to push the equation right of page-center.
    _remove_local(pPr, "tabs")



def is_math_only_paragraph(p):
    """True if a paragraph contains math (inline <m:oMath>) and no
    substantive surrounding text.  Authors often type an equation on
    its own line which Word saves as inline math; visually it's a
    display equation and should be centered."""
    if p.find(f".//{mq('oMathPara')}") is not None:
        return False  # already a real display equation
    if p.find(f".//{mq('oMath')}") is None:
        return False
    text = "".join(t.text or "" for t in p.iter(wq("t")))
    # Allow only whitespace, light punctuation, or simple connectors
    # ("and", "or", "where", etc. would NOT pass — those are real text).
    stripped = re.sub(r"[\s.,;:!?()\[\]{}]+", "", text)
    return stripped == ""


def is_empty_paragraph(p):
    for tag in ("oMath", "oMathPara"):
        if p.find(f".//{mq(tag)}") is not None:
            return False
    for tag in ("drawing", "pict", "object"):
        if p.find(f".//{wq(tag)}") is not None:
            return False
    text = "".join(t.text or "" for t in p.iter(wq("t")))
    return text.strip() == ""


def remove_blank_lines_around_display_equations(doc):
    body = doc.element.body
    paragraphs = [p for p in body.findall(wq("p"))]
    is_display = [
        (p.find(f".//{mq('oMathPara')}") is not None) or is_math_only_paragraph(p)
        for p in paragraphs
    ]
    to_remove = set()
    for idx, p in enumerate(paragraphs):
        if not is_display[idx]:
            continue
        if idx - 1 >= 0 and id(paragraphs[idx - 1]) not in to_remove:
            prev = paragraphs[idx - 1]
            if is_empty_paragraph(prev) and not is_display[idx - 1]:
                to_remove.add(id(prev))
        if idx + 1 < len(paragraphs):
            nxt = paragraphs[idx + 1]
            if is_empty_paragraph(nxt) and not is_display[idx + 1]:
                to_remove.add(id(nxt))
    for p in paragraphs:
        if id(p) in to_remove and p.getparent() is not None:
            p.getparent().remove(p)


_NO_SPACE_BEFORE = set(".,;:!?)]}%")
_NO_SPACE_AFTER = set("([{")
_WHITESPACE = (" ", "\t", "\xa0")


def normalize_inline_equation_spacing(doc):
    SPACE = " "

    def _ensure_trailing_space(text_el):
        text = text_el.text or ""
        if not text:
            return
        if text.endswith(_WHITESPACE):
            return
        if text[-1] in _NO_SPACE_AFTER:
            return
        text_el.text = text + SPACE
        text_el.set(qn("xml:space"), "preserve")

    def _ensure_leading_space(text_el):
        text = text_el.text or ""
        if not text:
            return
        if text.startswith(_WHITESPACE):
            return
        if text[0] in _NO_SPACE_BEFORE:
            return
        text_el.text = SPACE + text
        text_el.set(qn("xml:space"), "preserve")

    def _make_space_run():
        r = etree.Element(wq("r"))
        t = etree.SubElement(r, wq("t"))
        t.set(qn("xml:space"), "preserve")
        t.text = SPACE
        return r

    for p in _iter_block_paragraphs(doc):
        if p.find(f".//{mq('oMathPara')}") is not None:
            continue
        if is_math_only_paragraph(p):
            continue
        omaths = p.findall(f".//{mq('oMath')}")
        if not omaths:
            continue
        for omath in omaths:
            parent = omath.getparent()

            prev_sib = omath.getprevious()
            handled = False
            while prev_sib is not None and not handled:
                if prev_sib.tag == wq("r"):
                    last_t = None
                    for el in prev_sib.iter(wq("t")):
                        last_t = el
                    if last_t is not None and (last_t.text or "") != "":
                        _ensure_trailing_space(last_t)
                        handled = True
                        break
                prev_sib = prev_sib.getprevious()
            if not handled and omath.getprevious() is not None:
                new_run = _make_space_run()
                parent.insert(list(parent).index(omath), new_run)

            next_sib = omath.getnext()
            handled = False
            while next_sib is not None and not handled:
                if next_sib.tag == wq("r"):
                    first_t = next_sib.find(wq("t"))
                    if first_t is not None and (first_t.text or "") != "":
                        _ensure_leading_space(first_t)
                        handled = True
                        break
                next_sib = next_sib.getnext()
            if not handled and omath.getnext() is not None:
                new_run = _make_space_run()
                parent.insert(list(parent).index(omath) + 1, new_run)


def _sort_children(parent, order):
    known = []
    unknown = []
    for child in list(parent):
        local = etree.QName(child.tag).localname
        try:
            idx = order.index(local)
            known.append((idx, child))
        except ValueError:
            unknown.append(child)
    known.sort(key=lambda kv: kv[0])
    for child in list(parent):
        parent.remove(child)
    for _, child in known:
        parent.append(child)
    for child in unknown:
        parent.append(child)


def normalize_schema_order(doc):
    body = doc.element.body
    for p in body.iter(wq("p")):
        pPr = p.find(wq("pPr"))
        if pPr is not None:
            _sort_children(pPr, PPR_ORDER)
            inline_rPr = pPr.find(wq("rPr"))
            if inline_rPr is not None:
                _sort_children(inline_rPr, RPR_ORDER)
        for r in p.findall(wq("r")):
            rPr = r.find(wq("rPr"))
            if rPr is not None:
                _sort_children(rPr, RPR_ORDER)

    styles_el = doc.styles.element
    for st in styles_el.findall(wq("style")):
        pPr = st.find(wq("pPr"))
        if pPr is not None:
            _sort_children(pPr, PPR_ORDER)
            inline_rPr = pPr.find(wq("rPr"))
            if inline_rPr is not None:
                _sort_children(inline_rPr, RPR_ORDER)
        rPr = st.find(wq("rPr"))
        if rPr is not None:
            _sort_children(rPr, RPR_ORDER)

    for tbl in body.iter(wq("tbl")):
        tblPr = tbl.find(wq("tblPr"))
        if tblPr is not None:
            _sort_children(tblPr, TBLPR_ORDER)
    for tc in body.iter(wq("tc")):
        tcPr = tc.find(wq("tcPr"))
        if tcPr is not None:
            _sort_children(tcPr, TCPR_ORDER)



# ---------------------------------------------------------------------------
# Econometrica-style table formatter
# ---------------------------------------------------------------------------

CAPTION_RE = re.compile(r"^\s*Table\s+([A-Z0-9]+(?:\.[A-Za-z0-9]+)*)\s*$", re.I)


def _tbl_set_border(tcPr, side, sz=None):
    """Set or clear a single border on a w:tc. sz=None -> 'nil'."""
    tcBorders = tcPr.find(wq("tcBorders"))
    if tcBorders is None:
        tcBorders = etree.SubElement(tcPr, wq("tcBorders"))
    for existing in tcBorders.findall(wq(side)):
        tcBorders.remove(existing)
    el = etree.SubElement(tcBorders, wq(side))
    if sz is None:
        el.set(wq("val"), "nil")
    else:
        el.set(wq("val"), "single")
        el.set(wq("sz"), str(sz))
        el.set(wq("space"), "0")
        el.set(wq("color"), "auto")


def _tbl_set_run_props(p, **flags):
    for r in p.iter(wq("r")):
        rPr = r.find(wq("rPr"))
        if rPr is None:
            rPr = etree.Element(wq("rPr"))
            r.insert(0, rPr)
        if flags.get("bold"):
            if rPr.find(wq("b")) is None:
                etree.SubElement(rPr, wq("b"))
            if rPr.find(wq("bCs")) is None:
                etree.SubElement(rPr, wq("bCs"))
        if flags.get("italic"):
            if rPr.find(wq("i")) is None:
                etree.SubElement(rPr, wq("i"))
            if rPr.find(wq("iCs")) is None:
                etree.SubElement(rPr, wq("iCs"))
        if flags.get("smallCaps"):
            if rPr.find(wq("smallCaps")) is None:
                etree.SubElement(rPr, wq("smallCaps"))
        if flags.get("font"):
            for rf in rPr.findall(wq("rFonts")):
                rPr.remove(rf)
            rf = etree.SubElement(rPr, wq("rFonts"))
            for attr in ("ascii", "hAnsi", "cs", "eastAsia"):
                rf.set(wq(attr), flags["font"])
        if flags.get("size_hpt") is not None:
            for el in rPr.findall(wq("sz")):
                rPr.remove(el)
            for el in rPr.findall(wq("szCs")):
                rPr.remove(el)
            sz = etree.SubElement(rPr, wq("sz"))
            sz.set(wq("val"), str(flags["size_hpt"]))
            szCs = etree.SubElement(rPr, wq("szCs"))
            szCs.set(wq("val"), str(flags["size_hpt"]))


def _content_width(doc):
    """Usable text width (twips) from the section page size and margins;
    falls back to US-Letter with 1in margins (9360) if unavailable."""
    body = doc.element.body
    sectPr = body.find(wq("sectPr"))
    if sectPr is not None:
        pgSz = sectPr.find(wq("pgSz"))
        pgMar = sectPr.find(wq("pgMar"))
        if pgSz is not None and pgMar is not None:
            try:
                cw = int(pgSz.get(wq("w"))) - int(pgMar.get(wq("left"))) - int(pgMar.get(wq("right")))
                if cw > 1000:
                    return cw
            except (TypeError, ValueError):
                pass
    return 9360


def _format_econometrica_table(tbl, content_width=9360):
    """Apply booktabs borders, fixed column widths, padding, Times New Roman
    text and a small-caps header to a w:tbl."""
    tblPr = tbl.find(wq("tblPr"))
    if tblPr is None:
        tblPr = etree.Element(wq("tblPr"))
        tbl.insert(0, tblPr)
    # fixed table width + layout so columns never collapse (pandoc tables
    # arrive with tblW=0 and no grid, which makes Word/LibreOffice crush them)
    _remove_local(tblPr, "tblW")
    tblW = etree.SubElement(tblPr, wq("tblW"))
    tblW.set(wq("type"), "dxa")
    tblW.set(wq("w"), str(content_width))
    _remove_local(tblPr, "tblLayout")
    lay = etree.SubElement(tblPr, wq("tblLayout"))
    lay.set(wq("type"), "fixed")
    # table-level borders removed so cell-level borders win
    _remove_local(tblPr, "tblBorders")
    # generous cell padding
    _remove_local(tblPr, "tblCellMar")
    cm = etree.SubElement(tblPr, wq("tblCellMar"))
    for side, wd in (("top", "60"), ("left", "108"), ("bottom", "60"), ("right", "108")):
        e = etree.SubElement(cm, wq(side))
        e.set(wq("w"), wd)
        e.set(wq("type"), "dxa")

    rows = tbl.findall(wq("tr"))
    if not rows:
        return
    n_rows = len(rows)
    ncol = len(rows[0].findall(wq("tc")))
    if ncol <= 1:
        widths = [content_width]
    else:
        first = min(2880, content_width // ncol + 240)
        rest = (content_width - first) // (ncol - 1)
        widths = [first] + [rest] * (ncol - 1)
        widths[-1] += content_width - sum(widths)
    _remove_local(tbl, "tblGrid")
    grid = etree.Element(wq("tblGrid"))
    for wd in widths:
        gc = etree.SubElement(grid, wq("gridCol"))
        gc.set(wq("w"), str(wd))
    tbl.insert(list(tbl).index(tblPr) + 1, grid)

    THICK = 12  # ~1.5pt heavy rule
    THIN = 4    # ~0.5pt light rule
    for ri, tr in enumerate(rows):
        for ci, tc in enumerate(tr.findall(wq("tc"))):
            tcPr = tc.find(wq("tcPr"))
            if tcPr is None:
                tcPr = etree.Element(wq("tcPr"))
                tc.insert(0, tcPr)
            _remove_local(tcPr, "tcW")
            cw = etree.SubElement(tcPr, wq("tcW"))
            cw.set(wq("type"), "dxa")
            cw.set(wq("w"), str(widths[ci] if ci < len(widths) else widths[-1]))
            _remove_local(tcPr, "tcBorders")
            _tbl_set_border(tcPr, "left", None)
            _tbl_set_border(tcPr, "right", None)
            _tbl_set_border(tcPr, "insideV", None)
            _tbl_set_border(tcPr, "insideH", None)
            _tbl_set_border(tcPr, "top", THICK if ri == 0 else None)
            if ri == 0 and n_rows > 1:
                _tbl_set_border(tcPr, "bottom", THIN)
            elif ri == n_rows - 1:
                _tbl_set_border(tcPr, "bottom", THICK)
            else:
                _tbl_set_border(tcPr, "bottom", None)
            for p in tc.findall(wq("p")):
                _tbl_set_run_props(p, font="Times New Roman")
                if ri == 0:
                    for r in p.iter(wq("r")):
                        rPr = r.find(wq("rPr"))
                        if rPr is not None:
                            _remove_local(rPr, "b")
                            _remove_local(rPr, "bCs")
                    _tbl_set_run_props(p, smallCaps=True)


def _format_caption_pair(caption_p, subtitle_p):
    """Rewrite caption paragraph to ALL-CAPS centered, subtitle to small caps."""
    cap_text = "".join((t.text or "") for t in caption_p.iter(wq("t"))).strip()
    new_cap = cap_text.upper()
    for r in caption_p.findall(wq("r")):
        caption_p.remove(r)
    pPr = caption_p.find(wq("pPr"))
    if pPr is None:
        pPr = etree.Element(wq("pPr"))
        caption_p.insert(0, pPr)
    for jc in pPr.findall(wq("jc")):
        pPr.remove(jc)
    jc = etree.SubElement(pPr, wq("jc"))
    jc.set(wq("val"), "center")
    r = etree.SubElement(caption_p, wq("r"))
    rPr = etree.SubElement(r, wq("rPr"))
    rf = etree.SubElement(rPr, wq("rFonts"))
    for attr in ("ascii", "hAnsi", "cs", "eastAsia"):
        rf.set(wq(attr), "Times New Roman")
    t = etree.SubElement(r, wq("t"))
    t.set(qn("xml:space"), "preserve")
    t.text = new_cap

    if subtitle_p is None:
        return
    sub_text = "".join((t.text or "") for t in subtitle_p.iter(wq("t"))).strip()
    if sub_text.endswith("."):
        sub_text = sub_text[:-1]
    for r in subtitle_p.findall(wq("r")):
        subtitle_p.remove(r)
    pPr2 = subtitle_p.find(wq("pPr"))
    if pPr2 is None:
        pPr2 = etree.Element(wq("pPr"))
        subtitle_p.insert(0, pPr2)
    for jc in pPr2.findall(wq("jc")):
        pPr2.remove(jc)
    jc = etree.SubElement(pPr2, wq("jc"))
    jc.set(wq("val"), "center")
    r = etree.SubElement(subtitle_p, wq("r"))
    rPr = etree.SubElement(r, wq("rPr"))
    rf = etree.SubElement(rPr, wq("rFonts"))
    for attr in ("ascii", "hAnsi", "cs", "eastAsia"):
        rf.set(wq(attr), "Times New Roman")
    etree.SubElement(rPr, wq("smallCaps"))
    t = etree.SubElement(r, wq("t"))
    t.set(qn("xml:space"), "preserve")
    t.text = sub_text


def _format_note_paragraph(p):
    text = "".join((t.text or "") for t in p.iter(wq("t"))).strip()
    if not re.match(r"^Notes?\s*:", text, re.I):
        return False
    _tbl_set_run_props(p, font="Times New Roman", size_hpt=18)
    return True


def format_econometrica_tables(doc):
    """Walk body; for every w:tbl, format it + the caption/subtitle above + the
    Note paragraph immediately after."""
    body = doc.element.body
    elements = list(body)
    content_width = _content_width(doc)
    n_tables = 0
    for i, el in enumerate(elements):
        if etree.QName(el.tag).localname != "tbl":
            continue
        n_tables += 1
        caption_p = None
        subtitle_p = None
        if i >= 2:
            cs = elements[i-1]
            cc = elements[i-2]
            if (etree.QName(cc.tag).localname == "p"
                    and etree.QName(cs.tag).localname == "p"):
                cap_text = "".join((t.text or "") for t in cc.iter(wq("t"))).strip()
                if CAPTION_RE.match(cap_text):
                    caption_p = cc
                    subtitle_p = cs
        if caption_p is None and i >= 1:
            cand = elements[i-1]
            if etree.QName(cand.tag).localname == "p":
                cap_text = "".join((t.text or "") for t in cand.iter(wq("t"))).strip()
                if CAPTION_RE.match(cap_text):
                    caption_p = cand
                    subtitle_p = None
        if caption_p is not None:
            _format_caption_pair(caption_p, subtitle_p)
        _format_econometrica_table(el, content_width)
        if i + 1 < len(elements) and etree.QName(elements[i+1].tag).localname == "p":
            _format_note_paragraph(elements[i+1])
    return n_tables


def format_document(input_path, output_path):
    input_path = Path(input_path)
    output_path = Path(output_path)
    if output_path.resolve() != input_path.resolve():
        shutil.copyfile(input_path, output_path)
    doc = Document(str(output_path))
    for name, spec in STYLE_SPECS.items():
        ensure_style(doc, name, spec)
    normalize_inline_equation_spacing(doc)
    for p in _iter_block_paragraphs(doc):
        if p.find(f".//{mq('oMathPara')}") is not None or is_math_only_paragraph(p):
            set_display_equation_paragraph(p)
    remove_blank_lines_around_display_equations(doc)
    format_econometrica_tables(doc)
    normalize_schema_order(doc)
    doc.save(str(output_path))


def main(argv):
    if len(argv) < 2:
        print(__doc__, file=sys.stderr)
        return 1
    in_path = Path(argv[0])
    out_path = Path(argv[1])
    if not in_path.exists():
        print(f"Input not found: {in_path}", file=sys.stderr)
        return 1
    format_document(in_path, out_path)
    print(f"Wrote {out_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
