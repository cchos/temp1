from __future__ import annotations

from .tree import TreeState


def trim_pdms_info2(tree: TreeState) -> None:
    """Partial Python port of trim_PDMS_info2: remove unit suffix 'm' in size fields."""
    for i, cols in tree.pdms_info2.items():
        # Fortran trims PDMS_info2(:,4:17) removing 'm'
        for k in range(3, min(len(cols), 17)):
            cols[k] = cols[k].replace("m", "").strip()
        tree.pdms_info2[i] = cols
