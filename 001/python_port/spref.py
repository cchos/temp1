from __future__ import annotations

from pathlib import Path

from .models import SprefTable
from .utils import parse_line


STOP_PREFIXES = ("EOF", "END")


def read_spref(file_path: str | Path) -> SprefTable:
    path = Path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"SPREF file not found: {path}")

    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines()
    if not lines:
        return SprefTable([])

    rows: list[list[str]] = []
    for line in lines[1:]:  # skip header
        if not line:
            continue
        prefix = line[:3].upper()
        if prefix in STOP_PREFIXES:
            break
        if line.startswith(",") or line.startswith(" "):
            continue
        rows.append(parse_line(line, ","))

    return SprefTable(rows)
