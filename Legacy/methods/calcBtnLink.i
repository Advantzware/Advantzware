/* calcBtnLink.i */

DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcBtnLink AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.

RUN Get-Company IN Persistent-Handle (OUTPUT cCompany).
RUN sys/ref/nk1look.p (
    cCompany,
    "CalcBtnLink",
    "L",
    NO,
    NO,
    "",
    "",
    OUTPUT cCalcBtnLink,
    OUTPUT lFound
    ).
IF lFound AND cCalcBtnLink EQ "yes" THEN DO:       
    RUN sys/ref/nk1look.p
        (cCompany,"CalcBtnLink","C",NO,NO,"","",
         OUTPUT cCalcBtnLink,OUTPUT lFound).
    IF lFound AND cCalcBtnLink NE "" THEN DO:
        &if defined(FWD-VERSION) eq 0 &then
        OS-COMMAND NO-WAIT START VALUE (cCalcBtnLink).
        &else
           OPEN-URL(cCalcBtnLink).
        &endif
    END.
END.
ELSE RUN Get_Procedure IN Persistent-Handle ("d-frac.", OUTPUT run-proc, YES).
