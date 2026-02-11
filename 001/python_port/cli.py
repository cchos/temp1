from __future__ import annotations

import argparse

from .pipeline import run_partial_pipeline, run_tree_pipeline


def main() -> None:
    parser = argparse.ArgumentParser(description="Incremental Python port for HHI duct pre-processor")
    parser.add_argument("--ini", required=True, help="Path to hhiducta.ini")
    parser.add_argument(
        "--tree",
        action="store_true",
        help="Run extended tree stages (arrange_PDMS/trim_twork_iChilds/sort_tree/make_bwork equivalents)",
    )
    args = parser.parse_args()

    state = run_tree_pipeline(args.ini) if args.tree else run_partial_pipeline(args.ini)

    print("Pipeline completed" if args.tree else "Partial pipeline completed")
    print(f"INI: {state.paths.ini_file}")
    print(f"LocalTempPath: {state.paths.localtemp_path}")
    print(f"SPREF ETC rows: {len(state.spref_etc.rows)}")
    print(f"PDMS rows: {state.nrow_sheet_read}, cols: {state.ncol_sheet_read}")

    if args.tree and state.tree is not None:
        tree = state.tree
        print(f"Tree nodes: {len(tree.node_ids)}")
        print(f"Root index: {tree.root}")
        print(f"Supply subtree nodes: {len(tree.idx_sort_supp)}")
        print(f"Extract subtree nodes: {len(tree.idx_sort_extr)}")
        print(f"Branches: {tree.nbran_tree}")


if __name__ == "__main__":
    main()
