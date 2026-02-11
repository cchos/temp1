from __future__ import annotations

import csv
from pathlib import Path

from .tree import TreeState


HEADER = [
    "Memo","Name","Parent","Dir","Desc","Sectype","TargetQ","Length","SecA","SecB","SecD","Rough",
    "DBNAME","Ttype","Order","P1","P2","Correction",
    "S1type","P1","P2","P3","Correction",
    "S2type","P1","P2","P3","Correction",
    "S3type","P1","P2","P3","Correction",
    "S4type","P1","P2","P3","Correction",
    "Blank","TreeID","TreeLevel","BranLevel","BranID","ElemID","Name2",
    "Blank","Blank","Spref",
    "RoomName","UnitNo","RoomPres",
    "X(Pos1)","Y(Pos1)","Z(Pos1)",
]


def _csv_join(values: list[str]) -> list[str]:
    return [v if v is not None else "" for v in values]


def export_dsheet(tree: TreeState, out_file: str | Path) -> Path:
    path = Path(out_file)
    path.parent.mkdir(parents=True, exist_ok=True)

    idx_order = tree.idx_sort_all or ([tree.root] if tree.root else [])

    with path.open("w", newline="", encoding="utf-8") as f:
        wr = csv.writer(f)
        wr.writerow(HEADER)

        for i in idx_order:
            if i <= 0:
                continue
            row = tree.source_row_by_idx.get(i, [])
            pd = tree.pdms_info2.get(i, [])
            name = tree.node_ids[i - 1] if i - 1 < len(tree.node_ids) else ""
            parent_idx = tree.parent.get(i, 0)
            parent_name = tree.node_ids[parent_idx - 1] if parent_idx > 0 and parent_idx - 1 < len(tree.node_ids) else ""
            dir_text = "SUPPLY" if tree.flow_dir.get(i, 0) == 1 else ("EXTRACT" if tree.flow_dir.get(i, 0) == -1 else "")

            # Approximate mapping aligned to export_dsheet shape.
            line = [
                str(tree.level_s2.get(i, 0)),  # Memo
                name,
                parent_name,
                dir_text,
                tree.name_by_idx.get(i, ""),
                pd[2] if len(pd) > 2 else "",   # Sectype
                f"{tree.flow.get(i, 0.0):.3f}",
                pd[3] if len(pd) > 3 else "",
                pd[4] if len(pd) > 4 else "",
                pd[5] if len(pd) > 5 else "",
                pd[6] if len(pd) > 6 else "",
                pd[9] if len(pd) > 9 else "",
                pd[17] if len(pd) > 17 else "",
                pd[12] if len(pd) > 12 else "",
                pd[13] if len(pd) > 13 else "",
                pd[14] if len(pd) > 14 else "",
                pd[15] if len(pd) > 15 else "",
                pd[16] if len(pd) > 16 else "",
                pd[18] if len(pd) > 18 else "",
                pd[19] if len(pd) > 19 else "",
                pd[20] if len(pd) > 20 else "",
                pd[21] if len(pd) > 21 else "",
                pd[22] if len(pd) > 22 else "",
                pd[23] if len(pd) > 23 else "",
                pd[24] if len(pd) > 24 else "",
                pd[25] if len(pd) > 25 else "",
                pd[26] if len(pd) > 26 else "",
                pd[27] if len(pd) > 27 else "",
                pd[28] if len(pd) > 28 else "",
                pd[29] if len(pd) > 29 else "",
                pd[30] if len(pd) > 30 else "",
                pd[31] if len(pd) > 31 else "",
                pd[32] if len(pd) > 32 else "",
                pd[33] if len(pd) > 33 else "",
                pd[34] if len(pd) > 34 else "",
                pd[35] if len(pd) > 35 else "",
                "",
                str(tree.node_code.get(i, 0)),
                str(tree.level.get(i, 0)),
                str(tree.bran_level.get(i, 0)),
                str(tree.bran_id.get(i, 0)),
                str(tree.belem_id.get(i, 0)),
                tree.name_by_idx.get(i, ""),
                "",
                "",
                pd[29] if len(pd) > 29 else "",  # Spref
                row[58].strip() if len(row) > 58 else "",
                row[60].strip() if len(row) > 60 else "",
                row[61].strip() if len(row) > 61 else "",
                row[50].strip() if len(row) > 50 else "",
                row[51].strip() if len(row) > 51 else "",
                row[52].strip() if len(row) > 52 else "",
            ]
            wr.writerow(_csv_join(line))

    return path
