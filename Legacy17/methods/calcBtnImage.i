/* calcBtnImage.i */

DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcBtnImage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound        AS LOGICAL   NO-UNDO.

RUN Get-Company IN Persistent-Handle (OUTPUT cCompany).
RUN sys/ref/nk1look.p
    (cCompany,"CalcBtnImage","L",NO,NO,"","",
     OUTPUT cCalcBtnImage,OUTPUT lFound).
IF lFound AND cCalcBtnImage EQ "yes" THEN DO:       
    RUN sys/ref/nk1look.p
        (cCompany,"CalcBtnImage","C",NO,NO,"","",
         OUTPUT cCalcBtnImage,OUTPUT lFound).
    IF lFound AND SEARCH(cCalcBtnImage) NE ? THEN 
    select_frac:LOAD-IMAGE(SEARCH(cCalcBtnImage)) IN FRAME {&FRAME-NAME}.
END.
