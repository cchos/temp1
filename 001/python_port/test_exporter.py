from pathlib import Path

from python_port.exporter import export_dsheet
from python_port.postprocess import trim_pdms_info2
from python_port.tree import arrange_pdms, make_bwork, sort_tree, trim_twork_iChilds


def _mkrow(id1: str, child: str, name: str, typ: str, flow: str = "", direction: str = "") -> list[str]:
    row = [""] * 65
    row[0] = id1
    row[1] = child
    row[2] = name
    row[3] = typ
    row[6] = "123m"
    row[59] = flow
    row[60] = direction
    return row


def test_trim_pdms_info2_removes_m_suffix():
    rows = [_mkrow("A", "END", "a", "DUCT", "10", "Discharge")]
    tree = arrange_pdms(rows)
    trim_pdms_info2(tree)
    # pdms_info2 index 3 == original sheet col7
    assert tree.pdms_info2[1][3] == "123"


def test_export_dsheet_writes_file(tmp_path: Path):
    rows = [
        _mkrow("FAN", "SUP1", "fan", "EQUI"),
        _mkrow("SUP1", "END", "s1", "DUCT", "100", "Discharge"),
    ]
    tree = arrange_pdms(rows)
    trim_twork_iChilds(tree)
    sort_tree(tree)
    make_bwork(tree)

    out = tmp_path / "temp_dsheet.dexp"
    export_dsheet(tree, out)

    text = out.read_text(encoding="utf-8")
    assert "Memo,Name,Parent" in text
    assert "FAN" in text
