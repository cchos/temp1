from __future__ import annotations

from dataclasses import dataclass, field

from .models import IDXF1, IDXF2
from .utils import is_digit_like


@dataclass
class TreeState:
    node_ids: list[str] = field(default_factory=list)  # 1-based logical index
    name_by_idx: dict[int, str] = field(default_factory=dict)
    type_by_idx: dict[int, str] = field(default_factory=dict)
    parent: dict[int, int] = field(default_factory=dict)
    children: dict[int, list[int]] = field(default_factory=dict)
    level: dict[int, int] = field(default_factory=dict)
    flow: dict[int, float] = field(default_factory=dict)
    flow_dir: dict[int, int] = field(default_factory=dict)  # +1 supply, -1 extract, 0 etc
    root: int = 0
    fan: int = 0
    brothers: dict[int, list[int]] = field(default_factory=dict)
    node_code: dict[int, int] = field(default_factory=dict)

    isupp_tree: int = 0
    iextr_tree: int = 0
    idx_sort_supp: list[int] = field(default_factory=list)
    idx_sort_extr: list[int] = field(default_factory=list)
    idx_sort_all: list[int] = field(default_factory=list)
    level_s2: dict[int, int] = field(default_factory=dict)

    bran_level: dict[int, int] = field(default_factory=dict)
    bran_id: dict[int, int] = field(default_factory=dict)
    belem_id: dict[int, int] = field(default_factory=dict)
    nbran_tree: int = 0


_DIR_MAP = {
    "discharge": 1,
    "suction": -1,
    "capped": 0,
}


def _row_get(row: list[str], idx1: int) -> str:
    z = idx1 - 1
    if z < 0 or z >= len(row):
        return ""
    return row[z].strip()


def arrange_pdms(sheet_read: list[list[str]]) -> TreeState:
    state = TreeState()
    if not sheet_read:
        return state

    rows = [r[:] for r in sheet_read]
    idmy = [_row_get(r, 1) for r in rows]
    idch = [_row_get(r, 2) for r in rows]

    existing = {x for x in idmy if x}
    for child in idch:
        if child and child != "END" and child not in existing:
            new_row = [""] * max(len(rows[0]), 65)
            new_row[0] = child
            new_row[1] = ""
            new_row[2] = "UNKNOWN (W/O ATTRIBUTE)"
            new_row[3] = "UNKNOWN"
            rows.append(new_row)
            existing.add(child)

    ordered_ids: list[str] = []
    for r in rows:
        rid = _row_get(r, 1)
        if rid and rid not in ordered_ids:
            ordered_ids.append(rid)

    state.node_ids = ordered_ids
    n = len(ordered_ids)
    idx_of = {rid: i + 1 for i, rid in enumerate(ordered_ids)}

    first_row_for_idx: dict[int, int] = {}
    parent = {i: 0 for i in range(1, n + 1)}
    children = {i: [] for i in range(1, n + 1)}

    for ir, r in enumerate(rows):
        i = idx_of.get(_row_get(r, 1), 0)
        if i and i not in first_row_for_idx:
            first_row_for_idx[i] = ir
            state.name_by_idx[i] = _row_get(r, 3)
            state.type_by_idx[i] = _row_get(r, 4)
        c = _row_get(r, 2)
        if i and c and c != "END" and c in idx_of:
            ic = idx_of[c]
            parent[ic] = i

    for ic, ip in parent.items():
        if ip > 0:
            children[ip].append(ic)

    for ip in children:
        children[ip] = list(reversed(children[ip]))

    state.parent = parent
    state.children = children

    # root selection: parent==0 and has child first, else first parent==0
    root = 0
    for i in range(1, n + 1):
        if parent[i] == 0 and children[i]:
            root = i
            break
    if root == 0:
        for i in range(1, n + 1):
            if parent[i] == 0:
                root = i
                break
    state.root = root
    state.fan = root

    # levels
    level: dict[int, int] = {}
    for i in range(1, n + 1):
        ip = parent[i]
        lv = 1
        guard = 0
        while ip > 0 and guard < 100000:
            lv += 1
            ip = parent[ip]
            guard += 1
        level[i] = lv
    state.level = level
    max_level = max(level.values(), default=0)

    flow = {i: 0.0 for i in range(1, n + 1)}
    flow_dir = {i: 0 for i in range(1, n + 1)}

    for ilevel in range(max_level, 0, -1):
        for i in range(1, n + 1):
            if level[i] != ilevel:
                continue
            ir = first_row_for_idx.get(i)
            row = rows[ir] if ir is not None else []
            if _row_get(row, 2) == "END":
                sflow = _row_get(row, IDXF1)
                flow[i] = float(sflow) if is_digit_like(sflow) else 0.0
                flow_dir[i] = _DIR_MAP.get(_row_get(row, IDXF2).lower(), 0)
            elif ilevel == 1:
                ch = children[i]
                if len(ch) == 1:
                    flow[i] = flow[ch[0]]
                elif len(ch) == 2:
                    flow[i] = 0.5 * (flow[ch[0]] + flow[ch[1]])
                else:
                    flow[i] = 0.0
                flow_dir[i] = 0
            else:
                total = 0.0
                d = 0
                for ic in children[i]:
                    total += flow[ic]
                    if flow_dir[ic] not in (0, 99):
                        d = flow_dir[ic]
                flow[i] = total
                flow_dir[i] = d

    for ilevel in range(2, max_level + 1):
        for i in range(1, n + 1):
            if level[i] == ilevel:
                for ic in children[i]:
                    flow_dir[ic] = flow_dir[i]

    state.flow = flow
    state.flow_dir = flow_dir

    # brothers and node code
    brothers: dict[int, list[int]] = {i: [] for i in range(1, n + 1)}
    node_code: dict[int, int] = {}
    for i in range(1, n + 1):
        ip = parent[i]
        if ip > 0:
            brothers[i] = [x for x in children[ip] if x != i]

        d = flow_dir[i]
        if d == -1:
            code = -9
            if len(children[i]) == 0:
                code = -1
            if len(children[i]) > 1:
                code = -2
            if len(brothers[i]) >= 1:
                code = -3
        elif d == 1:
            code = 9
            if len(children[i]) == 0:
                code = 1
            if len(children[i]) > 1:
                code = 2
            if len(brothers[i]) >= 1:
                code = 3
        else:
            code = 0
        node_code[i] = code

    state.brothers = brothers
    state.node_code = node_code
    return state


def trim_twork_iChilds(sheet_read: list[list[str]], tree: TreeState) -> None:
    for i, ch in tree.children.items():
        if tree.parent.get(i, 0) == 0 or not ch:
            continue

        # Rule 1: reverse if any child type is BRCO or child ref startswith C=
        needs_reverse = False
        for ic in ch:
            ctype = tree.type_by_idx.get(ic, "").strip()
            cref = tree.node_ids[ic - 1] if 0 < ic <= len(tree.node_ids) else ""
            if ctype == "BRCO" or cref.startswith("C="):
                needs_reverse = True
                break
        if needs_reverse:
            ch = list(reversed(ch))

        # Rule 2: TfitOrder sorting from PDMS_info2(:,42) ~= sheet column (3+42)=45
        scored: list[tuple[float, int]] = []
        for j, ic in enumerate(ch, start=1):
            row = sheet_read[ic - 1] if 0 < ic <= len(sheet_read) else []
            tfit = _row_get(row, 45)
            score = 990.0
            if tfit in {"B1", "S1"}:
                score = 1.0
            elif tfit.startswith("B") and tfit[1:].isdigit():
                score = float(tfit[1:])
            elif tfit == "S":
                score = 999.0
            scored.append((score + 0.0001 * j, ic))
        scored.sort(key=lambda x: x[0])
        tree.children[i] = [ic for _, ic in scored]


def _preorder(tree: TreeState, node: int, out: list[int]) -> None:
    out.append(node)
    for child in tree.children.get(node, []):
        if child != node:
            _preorder(tree, child, out)


def sort_tree(tree: TreeState) -> None:
    iroot = tree.root
    if iroot <= 0:
        return

    isupp = 0
    iextr = 0
    for ic in tree.children.get(iroot, []):
        d = tree.flow_dir.get(ic, 0)
        if d == 1:
            isupp = ic
        elif d == -1:
            iextr = ic

    tree.isupp_tree = isupp
    tree.iextr_tree = iextr

    supp_sort: list[int] = []
    extr_sort: list[int] = []
    if isupp:
        _preorder(tree, isupp, supp_sort)
    if iextr:
        _preorder(tree, iextr, extr_sort)

    tree.idx_sort_supp = supp_sort
    tree.idx_sort_extr = extr_sort

    level_s2: dict[int, int] = {i: 0 for i in range(1, len(tree.node_ids) + 1)}
    for i, node in enumerate(supp_sort, start=1):
        level_s2[node] = i
    for i, node in enumerate(extr_sort, start=1):
        level_s2[node] = -i
    level_s2[iroot] = 0
    tree.level_s2 = level_s2

    idx_all = list(reversed(extr_sort)) + [iroot] + supp_sort
    tree.idx_sort_all = idx_all


def make_bwork(tree: TreeState) -> None:
    if not tree.root:
        return

    bran_level: dict[int, int] = {tree.root: 0}
    bran_id: dict[int, int] = {tree.root: 1}
    belem_id: dict[int, int] = {tree.root: 1}
    branch_count = 1

    def walk(sorted_nodes: list[int], start_node: int) -> None:
        nonlocal branch_count
        for i in sorted_nodes:
            nbro = len(tree.brothers.get(i, []))
            ip = tree.parent.get(i, 0)
            if ip == 0:
                continue
            if nbro > 0 or i == start_node:
                bran_level[i] = bran_level.get(ip, 0) + 1
                branch_count += 1
                bran_id[i] = branch_count
                belem_id[i] = 1
            else:
                bran_level[i] = bran_level.get(ip, 0)
                bran_id[i] = bran_id.get(ip, 1)
                belem_id[i] = belem_id.get(ip, 0) + 1

    if tree.idx_sort_supp:
        walk(tree.idx_sort_supp, tree.isupp_tree)
    if tree.idx_sort_extr:
        walk(tree.idx_sort_extr, tree.iextr_tree)

    tree.bran_level = bran_level
    tree.bran_id = bran_id
    tree.belem_id = belem_id
    tree.nbran_tree = branch_count
