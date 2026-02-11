from __future__ import annotations


def parse_line(text: str, delim: str = ",") -> list[str]:
    """Rough equivalent of the Fortran `parse` usage in this project."""
    return [part.strip() for part in text.strip().split(delim)]


def is_digit_like(value: str) -> bool:
    """Match the legacy behavior where numeric-looking flow values are accepted."""
    v = value.strip()
    if not v:
        return False
    try:
        float(v)
        return True
    except ValueError:
        return False


def normalize_row_width(rows: list[list[str]], min_width: int = 0) -> list[list[str]]:
    width = max([min_width, *(len(r) for r in rows)] if rows else [min_width])
    return [r + [""] * (width - len(r)) for r in rows]
