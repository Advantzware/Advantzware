
/*------------------------------------------------------------------------
    File        : est.p
    Purpose     : Estimate

    Syntax      :

    Description : Return a Dataset of all Estimate Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : SEP 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsest.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.

RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).
ASSIGN
    v-qry-handle = QUERY q-estQuery:HANDLE.
    v-qry-handle:QUERY-PREPARE(v-qry-string).
    DATASET dsest:FILL().
   

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    ASSIGN
        prm-query = "FOR EACH est NO-LOCK WHERE est.company EQ " + QUOTER(prmComp).
END PROCEDURE.
