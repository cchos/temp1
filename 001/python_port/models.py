from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


IDXF1 = 60
IDXF2 = 61


@dataclass
class Paths:
    ini_file: Path
    localtemp_path: Path
    server_path: str = ""
    library_path: str = ""
    etc_file: Path | None = None
    pdms_temp_file: Path | None = None


@dataclass
class SprefTable:
    rows: list[list[str]] = field(default_factory=list)


@dataclass
class PipelineState:
    paths: Paths
    spref_etc: SprefTable = field(default_factory=SprefTable)
    sheet_read: list[list[str]] = field(default_factory=list)
    tree: Any | None = None

    @property
    def nrow_sheet_read(self) -> int:
        return len(self.sheet_read)

    @property
    def ncol_sheet_read(self) -> int:
        return max((len(r) for r in self.sheet_read), default=0)
