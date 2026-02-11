"""Incremental Python port of the Fortran duct pre-processor."""

from .pipeline import run_partial_pipeline, run_tree_pipeline

__all__ = ["run_partial_pipeline", "run_tree_pipeline"]
