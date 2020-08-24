/* -------------------------------------------------- po/po-total.p 11/00 JLF */
/* update po from po lines                                                    */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-recid AS RECID NO-UNDO.

DEFINE VARIABLE dTaxAmount     AS DECIMAL NO-UNDO.
DEFINE VARIABLE dFrtTaxAmount  AS DECIMAL NO-UNDO.   

FIND po-ord WHERE RECID(po-ord) EQ v-recid NO-ERROR.

IF AVAIL po-ord THEN DO:
  ASSIGN
   po-ord.tax    = 0
   po-ord.t-cost = 0.

  /* add freight whether tax gr is blank or not */
  IF po-ord.fob-code EQ "ORIG" AND po-ord.frt-pay NE "P" THEN
      ASSIGN
       po-ord.t-cost = po-ord.t-freight.

  IF po-ord.tax-gr NE "" THEN DO:    

    IF po-ord.fob-code EQ "ORIG" AND po-ord.frt-pay NE "P" THEN
    DO:
     RUN Tax_Calculate(INPUT po-ord.company,
                           INPUT po-ord.tax-gr,
                           INPUT TRUE,
                           INPUT po-ord.t-freight,
                           INPUT "", 
                           OUTPUT dFrtTaxAmount).
    
      ASSIGN
       po-ord.tax    = dFrtTaxAmount.
    END.   
  END.

  FOR EACH po-ordl NO-LOCK
      WHERE po-ordl.company EQ po-ord.company
        AND po-ordl.po-no   EQ po-ord.po-no:

    po-ord.t-cost = po-ord.t-cost + po-ordl.t-cost.
  
    IF po-ordl.tax THEN
    DO:
       RUN Tax_Calculate(INPUT po-ord.company,
                           INPUT po-ord.tax-gr,
                           INPUT FALSE,
                           INPUT po-ordl.t-cost,
                           INPUT po-ordl.i-no, 
                           OUTPUT dTaxAmount).
      po-ord.tax = po-ord.tax + (dTaxAmount).
    END.
    
    IF po-ordl.stat EQ "U" AND
       po-ord.stat NE "H"  AND
       po-ord.opened       THEN po-ord.stat = "U".
  END.

  ASSIGN
   po-ord.tax    = ROUND(po-ord.tax,2)
   po-ord.t-cost = ROUND(po-ord.t-cost,2) + po-ord.tax.
  
END.

/* end ---------------------------------- copr. 2000  advanced software, inc. */
