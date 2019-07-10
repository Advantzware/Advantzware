/*------------------------------------------------------------------------
    File        : NewEst.p
    Purpose     :  Corrugated Estimate

    Syntax      :

    Description : Return a Dataset of Corrugated Estimates

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttNewEstimate NO-UNDO
    FIELD NewEst           AS CHAR FORMAT "x(8)"
    FIELD NewCust             AS CHAR FORMAT "x(8)"
     .


DEFINE DATASET dsNewEstimate FOR ttNewEstimate.

    DEFINE INPUT PARAMETER prmAction            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmUser              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComp              AS CHAR NO-UNDO.

    DEFINE OUTPUT PARAMETER cError              AS CHAR NO-UNDO.
    
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsNewEstimate.

    IF   prmAction      = ?      THEN    prmAction    = "".       
    IF   prmUser        = ?      THEN    prmUser      = "".       
    IF   prmComp        = ?      THEN    prmComp      = "".       
    
   DEF VAR prmLoc AS CHAR NO-UNDO.
   DEFINE VAR countest AS INT NO-UNDO.
    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
prmLoc  =  "MAIN" .

/*******************ADD*********************/

IF prmAction = "Est" THEN do:
  
FIND FIRST ce-ctrl WHERE ce-ctrl.company = prmComp  AND ce-ctrl.loc = prmLoc  EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL ce-ctrl  THEN DO:
            CREATE ttNewEstimate.
            ASSIGN
                ttNewEstimate.NewEst = STRING(ce-ctrl.e-num + 1)
                countest             = ce-ctrl.e-num + 1 .
        END.
IF AVAIL ce-ctrl THEN
    ASSIGN
          ce-ctrl.e-num = countest  .

END.
