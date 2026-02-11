from __future__ import annotations

from pathlib import Path

from .models import IDXF1, IDXF2
from .utils import is_digit_like, normalize_row_width, parse_line


def import_csv_pdms(file_path: str | Path) -> list[list[str]]:
    path = Path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"PDMS temp file not found: {path}")

    rows: list[list[str]] = []
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        s = line.strip()
        if len(s) <= 3:
            continue
        if s == "EOF":
            break
        rows.append(parse_line(line, ","))

    # Keep spare columns for IDXF1/IDXF2 updates (1-based indexes in Fortran)
    return normalize_row_width(rows, min_width=max(64, IDXF2 + 1))


def trim_csv_pdms(sheet_read: list[list[str]]) -> list[list[str]]:
    out = []
    for src in sheet_read:
        row = src[:]
        if len(row) < IDXF2:
            row.extend([""] * (IDXF2 - len(row)))
        out.append(row)

    for row in out:
        # Fortran currently trims col3 only in active branch.
        if len(row) >= 3:
            row[2] = row[2].strip()

        # Fortran logic on col2: parse "X (Discharge)" -> END / flow / dir
        if len(row) >= 2:
            text = row[1].replace(")", "").replace(" (", "~")
            tokens = parse_line(text, "~")
            if len(tokens) >= 2:
                row[1] = "END"
                row[IDXF1 - 1] = tokens[0] if is_digit_like(tokens[0]) else "0"
                row[IDXF2 - 1] = tokens[1]
            else:
                row[IDXF1 - 1] = "-1"

    return out
