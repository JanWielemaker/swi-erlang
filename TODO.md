# Timeout handling
# Monitor				[OK]
# Link
  - What about running children?
  - What about propagating?

# Crash in engine_destroy:
  - Create engine
  - catch(thread_signal(Pid, abort), _, true),
  - engine_destroy(Pid).
