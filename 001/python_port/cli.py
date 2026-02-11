from __future__ import annotations

import argparse

from .pipeline import run_full_pipeline, run_partial_pipeline, run_tree_pipeline


def main() -> None:
    parser = argparse.ArgumentParser(description="Incremental Python port for HHI duct pre-processor")
    parser.add_argument("--ini", required=True, help="Path to hhiducta.ini")
    parser.add_argument(
        "--tree",
        action="store_true",
        help="Run extended tree stages (arrange_PDMS/trim_twork_iChilds/sort_tree/make_bwork equivalents)",
    )
    parser.add_argument("--full", action="store_true", help="Run tree + trim_PDMS_info2 + export_dsheet stages")
    parser.add_argument("--out", default="", help="Optional output file path for --full mode")
    args = parser.parse_args()

    if args.full:
        state = run_full_pipeline(args.ini, args.out or None)
    elif args.tree:
        state = run_tree_pipeline(args.ini)
    else:
        state = run_partial_pipeline(args.ini)

    print("Full pipeline completed" if args.full else ("Pipeline completed" if args.tree else "Partial pipeline completed"))
    print(f"INI: {state.paths.ini_file}")
    print(f"LocalTempPath: {state.paths.localtemp_path}")
    print(f"SPREF ETC rows: {len(state.spref_etc.rows)}")
    print(f"PDMS rows: {state.nrow_sheet_read}, cols: {state.ncol_sheet_read}")

    if (args.tree or args.full) and state.tree is not None:
        tree = state.tree
        print(f"Tree nodes: {len(tree.node_ids)}")
        print(f"Root index: {tree.root}")
        print(f"Supply subtree nodes: {len(tree.idx_sort_supp)}")
        print(f"Extract subtree nodes: {len(tree.idx_sort_extr)}")
        print(f"Branches: {tree.nbran_tree}")


if __name__ == "__main__":
    main()
