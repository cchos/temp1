from __future__ import annotations

from pathlib import Path

from .config import load_paths
from .exporter import export_dsheet
from .models import PipelineState
from .pdms import import_csv_pdms, trim_csv_pdms
from .postprocess import trim_pdms_info2
from .spref import read_spref
from .tree import arrange_pdms, make_bwork, sort_tree, trim_twork_iChilds


def run_partial_pipeline(ini_file: str) -> PipelineState:
    paths = load_paths(ini_file)
    state = PipelineState(paths=paths)

    if paths.etc_file:
        state.spref_etc = read_spref(paths.etc_file)

    if paths.pdms_temp_file:
        state.sheet_read = trim_csv_pdms(import_csv_pdms(paths.pdms_temp_file))

    return state


def run_tree_pipeline(ini_file: str) -> PipelineState:
    state = run_partial_pipeline(ini_file)
    tree = arrange_pdms(state.sheet_read)
    trim_twork_iChilds(tree)
    sort_tree(tree)
    make_bwork(tree)
    state.tree = tree
    return state


def run_full_pipeline(ini_file: str, out_file: str | Path | None = None) -> PipelineState:
    state = run_tree_pipeline(ini_file)
    if state.tree is None:
        return state

    trim_pdms_info2(state.tree)
    output = Path(out_file) if out_file else state.paths.localtemp_path / "tmp" / "temp_dsheet.dexp"
    export_dsheet(state.tree, output)
    return state
