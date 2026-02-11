from __future__ import annotations

from pathlib import Path

from .models import Paths


SECTION_KEYS = {
    "[LOCALTEMPPATH]": "localtemp",
    "[SERVERPATH]": "server",
    "[LIBRARYPATH]": "library",
}


def load_paths(ini_file: str | Path) -> Paths:
    ini_path = Path(ini_file)
    if not ini_path.exists():
        raise FileNotFoundError(f"INI file not found: {ini_path}")

    lines = [ln.rstrip("\n") for ln in ini_path.read_text(encoding="utf-8", errors="ignore").splitlines()]

    localtemp = ""
    server = ""
    library = ""

    for i, line in enumerate(lines):
        key = line.strip()
        if key in SECTION_KEYS and i + 1 < len(lines):
            value = lines[i + 1].strip()
            if SECTION_KEYS[key] == "localtemp":
                localtemp = value
            elif SECTION_KEYS[key] == "server":
                server = value
            elif SECTION_KEYS[key] == "library":
                library = value

    localtemp_path = Path(localtemp) if localtemp else Path(".")
    etc_file = localtemp_path / "spref_etc.dat" if library else None
    pdms_temp_file = localtemp_path / "tmp" / "temp.dat"

    return Paths(
        ini_file=ini_path,
        localtemp_path=localtemp_path,
        server_path=server,
        library_path=library,
        etc_file=etc_file,
        pdms_temp_file=pdms_temp_file,
    )
