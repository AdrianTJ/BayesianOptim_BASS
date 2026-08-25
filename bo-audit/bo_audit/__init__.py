"""bo-audit: machinery instrumentation for any ask/tell optimizer.

Wraps the true objective so every optimizer call is decoded to its
categorical combination and checked against everything already evaluated.
Revisit counting therefore works uniformly across libraries, without
touching their internals. See core.AuditedObjective for the space schema.
"""
from bo_audit.core import AuditedObjective
from bo_audit.memo import AuditStop, MemoizedAuditedObjective

__all__ = ["AuditedObjective", "MemoizedAuditedObjective", "AuditStop"]
__version__ = "0.1.0"
