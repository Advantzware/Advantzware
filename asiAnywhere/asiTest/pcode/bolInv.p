


/*------------------------------------------------------------------------
    File        : bolInv.p
    Purpose     : BOL

    Syntax      :

    Description : Return a Dataset of all Order Inquiry invoice BOL

    Author(s)   : Kuldeep
    Created     : DEC 20 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{bolInv.i}


DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBol      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbolInv.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmOrderNum     = ? THEN ASSIGN prmOrderNum     = "".
IF prmBol     = ? THEN ASSIGN prmBol     = "".
IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".   

    
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
 FOR EACH oe-ordl WHERE oe-ordl.company EQ prmComp AND
     oe-ordl.ord-no = INT(prmOrderNum) NO-LOCK :
     FOR EACH ar-invl WHERE ar-invl.company = oe-ordl.company
                        AND ar-invl.ord-no = oe-ordl.ord-no 
                        AND ar-invl.i-no     = oe-ordl.i-no 
                        AND ar-invl.bol-no = INT(prmBol) NO-LOCK:
         FOR EACH oe-boll WHERE oe-boll.company = ar-invl.company
                            AND oe-boll.b-no = ar-invl.b-no
                            AND oe-boll.bol-no = ar-invl.bol-no
                            AND oe-boll.i-no = ar-invl.i-no  NO-LOCK:
             create ttbolInv.
             ASSIGN
                 ttbolInv.vBolNo          = oe-boll.bol-no
                 ttbolInv.vBolIno         = oe-boll.i-no
                 ttbolInv.vBolJobNo       = oe-boll.job-no
                 ttbolInv.vBolinNo2       = oe-boll.job-no2
                 ttbolInv.vBolLoc         = oe-boll.loc
                 ttbolInv.vBolLocbin      = oe-boll.loc-bin
                 ttbolInv.vBolTag         = oe-boll.tag
                 ttbolInv.vBolCases       = oe-boll.cases
                 ttbolInv.vBolQtycase     = oe-boll.qty-case
                 ttbolInv.vBolPartial     = oe-boll.partial
                 ttbolInv.vTotal          = ((oe-boll.cases * oe-boll.qty-case) + oe-boll.partial)
                 ttbolInv.vBolweight      = oe-boll.weight
                 ttbolInv.vRowid          = recid(oe-boll  ).
                 IF ttbolInv.vBolTag  <> "" THEN
                     ttbolInv.vBolTag  = SUBSTRING(ttbolInv.vBolTag, 16,5).
                 IF oe-boll.s-code = "B" THEN 
                     ASSIGN ttbolInv.vBolScode       = " Bill and Ship".
                 IF oe-boll.s-code = "I" THEN 
                     ASSIGN
                     ttbolInv.vBolScode       = " Invoice Only".
                 IF oe-boll.s-code = "S" THEN 
                     ASSIGN
                     ttbolInv.vBolScode       = " Ship Only".
                 IF oe-boll.s-code = "T" THEN 
                     ASSIGN
                     ttbolInv.vBolScode       = "Transfer"
                 .
         END.  /*FOR EACH oe-boll */
     END. /*FOR EACH ar-invl*/
 END. /*oe-ordl*/
