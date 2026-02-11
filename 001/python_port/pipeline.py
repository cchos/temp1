from __future__ import annotations

from .config import load_paths
from .models import PipelineState
from .pdms import import_csv_pdms, trim_csv_pdms
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
    trim_twork_iChilds(state.sheet_read, tree)
    sort_tree(tree)
    make_bwork(tree)
    state.tree = tree
    return state
