from python_port.pdms import trim_csv_pdms


def test_trim_csv_pdms_extracts_flow_and_direction():
    rows = [["id1", "10 (SUPPLY)", " name ", "TYPE"]]
    out = trim_csv_pdms(rows)

    assert out[0][1] == "END"
    assert out[0][59] == "10"
    assert out[0][60] == "SUPPLY"
    assert out[0][2] == "name"


def test_trim_csv_pdms_sets_minus_one_when_not_parseable():
    rows = [["id1", "END", "name", "TYPE"]]
    out = trim_csv_pdms(rows)

    assert out[0][59] == "-1"
