/* calcBtnImage.i */

DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcBtnLink AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.

RUN Get-Company IN Persistent-Handle (OUTPUT cCompany).
RUN sys/ref/nk1look.p
    (cCompany,"CalcBtnLink","C",NO,NO,"","",
     OUTPUT cCalcBtnLink,OUTPUT lFound).
IF lFound AND cCalcBtnLink NE "" THEN DO:
    OS-COMMAND NO-WAIT START VALUE (cCalcBtnLink).
END.
ELSE RUN windows/d-frac.w.
