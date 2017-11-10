# Timeout handling
# Monitor				[OK]
# Link
  - What about running children?
  - What about propagating?

# Crash in engine_destroy:
  - Create engine
  - catch(thread_signal(Pid, abort), _, true),
  - engine_destroy(Pid).

# Engine aliases are not alias:
  - engine_create(x, true, Id, [alias(xx)]),
    engine_next(xx, N).
    --> Engine is not current, but recreating fails as the alias
    is not deleted.

