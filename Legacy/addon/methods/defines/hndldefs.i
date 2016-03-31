/* hndldefs.i */

/* handle is declared new & set in mainmenu.w */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW
&ENDIF

DEFINE {&NEW} SHARED VARIABLE miscflds_reckey AS CHARACTER.
DEFINE {&NEW} SHARED VARIABLE table_reckey AS CHARACTER.
DEFINE {&NEW} SHARED VARIABLE Persistent-Handle AS HANDLE.
DEFINE {&NEW} SHARED VARIABLE ListLogic-Handle AS HANDLE.
DEFINE VARIABLE run-proc AS CHARACTER.
DEFINE VARIABLE hsignature AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE is-running AS LOGICAL NO-UNDO.
DEFINE VARIABLE help-page AS INTEGER NO-UNDO.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
RUN addon/nosweat/persist.p PERSISTENT SET Persistent-Handle.
RUN addon/lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
&ENDIF
