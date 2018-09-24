/* hndldefs.i */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW
&ENDIF

DEFINE {&NEW} SHARED VARIABLE miscflds_reckey   AS CHARACTER.
DEFINE {&NEW} SHARED VARIABLE table_reckey      AS CHARACTER.
DEFINE {&NEW} SHARED VARIABLE Persistent-Handle AS HANDLE.
DEFINE {&NEW} SHARED VARIABLE ListLogic-Handle  AS HANDLE.
DEFINE {&NEW} SHARED VARIABLE igsSessionID      AS INTEGER.
DEFINE VARIABLE run-proc   AS CHARACTER.
DEFINE VARIABLE hsignature AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE phandle    AS HANDLE    NO-UNDO.
DEFINE VARIABLE is-running AS LOGICAL   NO-UNDO.
DEFINE VARIABLE help-page  AS INTEGER   NO-UNDO.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hTags AS HANDLE NO-UNDO.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p   PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).
RUN system/TagProcs.p   PERSISTENT SET hTags.
SESSION:ADD-SUPER-PROCEDURE (hTags).

&ENDIF
