DEF OUTPUT PARAM op-pid AS INT NO-UNDO.

DEFINE VARIABLE intProcessHandle AS INTEGER NO-UNDO.

PROCEDURE GetCurrentProcessId EXTERNAL "KERNEL32.DLL":
   DEFINE RETURN PARAMETER intProcessHandle AS LONG.
END PROCEDURE.

RUN GetCurrentProcessId (OUTPUT intProcessHandle).
op-pid = intProcessHandle.

