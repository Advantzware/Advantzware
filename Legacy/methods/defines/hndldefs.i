/* hndldefs.i */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW
&ENDIF

DEFINE {&NEW} SHARED VARIABLE miscflds_reckey   AS CHARACTER.
DEFINE {&NEW} SHARED VARIABLE table_reckey      AS CHARACTER.
DEFINE {&NEW} SHARED VARIABLE Persistent-Handle AS HANDLE.
DEFINE {&NEW} SHARED VARIABLE ListLogic-Handle  AS HANDLE.
DEFINE {&NEW} SHARED VARIABLE igsSessionID      AS INTEGER.

DEFINE VARIABLE run-proc     AS CHARACTER.
DEFINE VARIABLE hsignature   AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE phandle      AS HANDLE    NO-UNDO.
DEFINE VARIABLE is-running   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE help-page    AS INTEGER   NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE hdCreditProcs AS HANDLE NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE hdCommonProcs AS HANDLE NO-UNDO.

/* Note: normally, adding super procedures to the stack is done in asiLogin.w, procedure ipPreRun.
   Use the construct below ONLY if you need to instantiate these from a developer environment that
   does not use asiLogin as a starting procedure */
   
   
&IF DEFINED(UIB_is_Running) NE 0 &THEN
DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hTags    AS HANDLE NO-UNDO.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).
RUN system/TagProcs.p PERSISTENT SET hTags.
SESSION:ADD-SUPER-PROCEDURE (hTags).
&ENDIF

&IF DEFINED(CommonFile_is_Running) NE 0 &THEN
    RUN system/CreditProcs.p PERSISTENT SET hdCreditProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdCreditProcs).

    RUN system/CommonProcs.p PERSISTENT SET hdCommonProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdCommonProcs).
&ENDIF

{system/fSuperRunning.i}
