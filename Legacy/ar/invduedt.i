/* ar/invduedt.i  calculate due-date */
DEFINE VARIABLE hdCommonProcs AS HANDLE NO-UNDO.
IF NOT AVAIL terms THEN FIND FIRST terms WHERE terms.t-code = ar-inv.terms:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
IF AVAIL terms THEN do:
    RUN system/CommonProcs.p PERSISTENT SET hdCommonProcs.
        THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdCommonProcs).
        ASSIGN ar-inv.due-date:SCREEN-VALUE = STRING( DYNAMIC-FUNCTION("GetInvDueDate", date(ar-inv.inv-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}),terms.dueOnMonth,terms.dueOnDay,terms.net-days )).
        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdCommonProcs). 
END.
                           
   
