/*------------------------------------------------------------------------
    File        : ItemQuantityLook.p
    Purpose     : QuantityLookUp

    Syntax      :

    Description : Return a Dataset of Quantity

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemQuantityLook NO-UNDO 
    FIELD vQty AS INTEGER FORMAT ">>>>>>>"
    FIELD vPrice AS DECIMAL FORMAT ">,>>9.99"
    FIELD vUom AS CHAR FORMAT "x(3)" 
    FIELD hjkl AS CHAR               .

DEFINE DATASET dsItemQuantityLook FOR ttItemQuantityLook.

    DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmQuote AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmItem AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustPart AS CHAR NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemQuantityLook.
IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmQuote = ?  THEN ASSIGN prmQuote = 0.

DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FOR EACH quoteitm WHERE quoteitm.company = prmComp AND (quoteitm.i-no = prmItem OR quoteitm.part-no = prmCustPart) 
    and quoteitm.q-no = prmQuote NO-LOCK, 
      EACH quoteqty WHERE quoteqty.q-no = quoteitm.q-no 
  AND quoteqty.line = quoteitm.line NO-LOCK:
    CREATE ttItemQuantityLook.
    ASSIGN
        ttItemQuantityLook.vQty = quoteqty.qty 
         ttItemQuantityLook.vPrice = quoteqty.price
         ttItemQuantityLook.vUom = quoteqty.uom.
END.
    
