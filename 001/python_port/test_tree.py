from python_port.tree import arrange_pdms, make_bwork, sort_tree, trim_twork_iChilds


def _mkrow(id1: str, child: str, name: str, typ: str, flow: str = "", direction: str = "") -> list[str]:
    row = [""] * 65
    row[0] = id1
    row[1] = child
    row[2] = name
    row[3] = typ
    row[59] = flow
    row[60] = direction
    return row


def test_tree_pipeline_basic_supply_extract_split():
    rows = [
        _mkrow("FAN", "SUP1", "fan", "EQUI"),
        _mkrow("FAN", "EXT1", "fan", "EQUI"),
        _mkrow("SUP1", "END", "s1", "DUCT", "100", "Discharge"),
        _mkrow("EXT1", "END", "e1", "DUCT", "80", "Suction"),
    ]

    tree = arrange_pdms(rows)
    trim_twork_iChilds(rows, tree)
    sort_tree(tree)
    make_bwork(tree)

    assert tree.root > 0
    assert tree.isupp_tree > 0
    assert tree.iextr_tree > 0
    assert len(tree.idx_sort_all) == len(tree.node_ids)
    assert tree.nbran_tree >= 1


def test_arrange_adds_missing_child_node():
    rows = [_mkrow("A", "B", "a", "DUCT")]
    tree = arrange_pdms(rows)
    assert "B" in tree.node_ids
