/* dynValidateProc.i - rstark - 2.27.2019 */

/* add dynamic validate procedures in alphabetical order */
/* 1. procedure has input of iphWidget AS HANDLE         */
/* 2. RUN dynValReturn (iphWidget, results of validate)  */

FUNCTION fErrorMsg RETURNS CHARACTER (iphWidget AS HANDLE):
    RETURN "Invalid Entry for " + iphWidget:LABEL + " " + iphWidget:SCREEN-VALUE.
END FUNCTION.

PROCEDURE dynValMachine:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST mach
                 WHERE mach.company EQ g_company
                   AND mach.m-code EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

/* all validate procedures should run this procedure */
PROCEDURE dynValReturn:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER iplReturn AS LOGICAL NO-UNDO.
    
    IF iplReturn THEN RETURN "".
    ELSE RETURN fErrorMsg (iphWidget).
END PROCEDURE.

PROCEDURE dynValTime:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.

    ASSIGN
        iphWidget:SCREEN-VALUE = iphWidget:SCREEN-VALUE + "0000"
        iphWidget:SCREEN-VALUE = REPLACE(iphWidget:SCREEN-VALUE," ","0")
        .
    RUN dynValReturn (iphWidget,
       (INTEGER(SUBSTR(iphWidget:SCREEN-VALUE,1,2)) EQ 24 AND
        INTEGER(SUBSTR(iphWidget:SCREEN-VALUE,4,2)) EQ 0) OR
       (INTEGER(SUBSTR(iphWidget:SCREEN-VALUE,1,2)) LE 23 AND
        INTEGER(SUBSTR(iphWidget:SCREEN-VALUE,4,2)) LE 59)
        ).
END PROCEDURE.
