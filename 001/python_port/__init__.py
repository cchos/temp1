"""Incremental Python port of the Fortran duct pre-processor."""

from .pipeline import run_full_pipeline, run_partial_pipeline, run_tree_pipeline

__all__ = ["run_partial_pipeline", "run_tree_pipeline", "run_full_pipeline"]
