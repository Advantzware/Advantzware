/* oe/updprmtx2.p copied from oe/updprmtx.p and added param ip-TransQ 
                  called from est/vp-prmtx.w */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-cust-no LIKE oe-prmtx.cust-no NO-UNDO.
DEF INPUT PARAM ip-qty AS DEC DECIMALS 10 NO-UNDO.
DEF INPUT PARAM ip-uom AS CHAR NO-UNDO.
DEF INPUT PARAM ip-prc AS DEC DECIMALS 10 NO-UNDO.
DEF INPUT PARAM ip-TransQ AS cha NO-UNDO.  /* Q for Qty or 1 form Minus 1 */

{sys/inc/VAR.i SHARED}

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR lv-qty LIKE quoteqty.qty NO-UNDO.
DEF VAR ll-msg AS LOG INIT YES NO-UNDO.
DEF VAR lv-date AS CHAR NO-UNDO.
DEF VAR vdPrevPrice AS DEC NO-UNDO.
DEFINE VARIABLE lQuotePriceMatrix AS LOGICAL NO-UNDO.
DEFINE VARIABLE cRtnChar          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound         AS LOGICAL NO-UNDO.
DEFINE VARIABLE iQuoteNo          AS INTEGER NO-UNDO.
DEFINE VARIABLE lQuoteExpireDuplicates AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdupdQuoteProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE hdQuoteProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE lRunForQuote AS LOGICAL NO-UNDO.
DEFINE VARIABLE cRunMethods AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipId AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEffectiveDate AS DATE NO-UNDO.
DEFINE VARIABLE cCustType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustCompareValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPriceMatrixPricingMethod AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtExpireDate AS DATE INIT 12/31/2099.
RUN util/updQuoteProcs.p PERSISTENT SET hdupdQuoteProcs.
RUN est/QuoteProcs.p PERSISTENT SET hdQuoteProcs.

DEF TEMP-TABLE w-matrix NO-UNDO
    FIELD qty     AS DEC
    FIELD uom     AS CHAR
    FIELD price   AS DEC
    FIELD price-m AS DEC.

DEF BUFFER b-matrix FOR w-matrix.

RUN sys/ref/nk1look.p (INPUT cocode, "QuotePriceMatrix", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lQuotePriceMatrix = logical(cRtnChar) NO-ERROR.
    
RUN sys/ref/nk1look.p (INPUT cocode, "QuoteExpireDuplicates", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lQuoteExpireDuplicates = logical(cRtnChar) NO-ERROR.  
    
RUN sys/ref/nk1look.p (INPUT cocode, "PriceMatrixPricingMethod", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cPriceMatrixPricingMethod = cRtnChar NO-ERROR.    

IF PROGRAM-NAME(2) MATCHES "*vp-prmtx.*" OR PROGRAM-NAME(2) MATCHES "*ImportQuote.*" THEN
DO:
    lRunForQuote = YES.
END.
        
SESSION:SET-WAIT-STATE ("general").
    
lv-date = STRING(YEAR(TODAY),"9999") +
          STRING(MONTH(TODAY),"99")  +
          STRING(DAY(TODAY),"99").

FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO:
  CREATE w-matrix.
  ASSIGN
   w-matrix.qty   = ip-qty
   w-matrix.uom   = ip-uom
   w-matrix.price = ip-prc.

  cCustCompareValue = ip-cust-no. 
  RUN update-matrix.
END.

ELSE DO:
  FIND quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.

  IF NOT AVAIL quotehd THEN DO:
    FIND quoteitm WHERE ROWID(quoteitm) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAIL quoteitm THEN DO:
      FIND FIRST quotehd
          WHERE quotehd.company EQ quoteitm.company
            AND quotehd.loc     EQ quoteitm.loc
            AND quotehd.q-no    EQ quoteitm.q-no
          NO-LOCK NO-ERROR.
      ll-msg = NO.
    END.
  END.
END.
        
RELEASE quoteitm.

IF AVAIL quotehd THEN DO:
  ip-cust-no = quotehd.cust-no.
  iQuoteNo   = quotehd.q-no.
  cRunMethods = quotehd.pricingMethod.   
  dtEffectiveDate = quotehd.effectiveDate.
  
  IF cRunMethods EQ "Ship To" THEN
  ASSIGN
  cCustCompareValue = quotehd.cust-no
  cShipId = quotehd.ship-id.  
  ELSE IF lQuotePriceMatrix AND cRunMethods EQ "Type" THEN
  ASSIGN
    cCustCompareValue = ""
    cShipId = "". 
  ELSE cCustCompareValue = quotehd.cust-no.
  
  FIND FIRST cust NO-LOCK
       WHERE cust.company EQ quotehd.company
       AND cust.cust-no EQ quotehd.cust-no NO-ERROR.
  IF AVAIL cust THEN
  cCustType = cust.type.
  
  FOR EACH quoteitm
      WHERE quoteitm.company EQ quotehd.company
        AND quoteitm.loc     EQ quotehd.loc
        AND quoteitm.q-no    EQ quotehd.q-no
        AND (ROWID(quoteitm) EQ ip-rowid OR
             ROWID(quotehd)  EQ ip-rowid)
      NO-LOCK,

      FIRST itemfg
      WHERE itemfg.company  EQ quoteitm.company
        AND itemfg.i-no     EQ quoteitm.i-no         
        AND (itemfg.cust-no EQ quotehd.cust-no OR
             itemfg.i-code  EQ "S")
      NO-LOCK:

    FOR EACH w-matrix:
      DELETE w-matrix.
    END.

    FOR EACH quoteqty OF quoteitm NO-LOCK:
      CREATE w-matrix.
      ASSIGN
       w-matrix.qty   = quoteqty.qty
       w-matrix.uom   = quoteqty.uom
       w-matrix.price = quoteqty.price.
    END.
         
    IF ip-TransQ = "Q" THEN RUN update-matrix.
    ELSE IF ip-TransQ = "1" THEN RUN update-matrix-minus.
    FIND CURRENT quotehd EXCLUSIVE-LOCK NO-ERROR.
    quotehd.expireDate = dtExpireDate.
    IF lQuotePriceMatrix THEN
    DO:    
      ASSIGN
       quotehd.approved = YES.       
       IF dtEffectiveDate EQ ? THEN
       quotehd.effectiveDate = TODAY.       
       RUN unApprovedDuplicateQuote IN hdQuoteProcs (ROWID(quotehd),quoteitm.part-no,quoteitm.i-no).
       
    END.   
    FIND CURRENT quotehd NO-LOCK NO-ERROR. 
     
    //IF lQuoteExpireDuplicates THEN
    //RUN UpdateExpireDate_allQuote IN hdupdQuoteProcs(ROWID(quoteitm), quotehd.effectiveDate - 1) .
     
  END.
END.

IF ll-msg AND lRunForQuote AND NOT PROGRAM-NAME(2) MATCHES "*ImportQuote.*" THEN DO:
  SESSION:SET-WAIT-STATE ("").

  MESSAGE "Create/update of FG price matrix completed..."
      VIEW-AS ALERT-BOX.
END.

RETURN.

PROCEDURE update-matrix.
  FOR EACH oe-prmtx
      WHERE oe-prmtx.company            EQ itemfg.company
        AND oe-prmtx.cust-no            EQ cCustCompareValue 
        AND oe-prmtx.i-no               BEGINS itemfg.i-no
        AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no                 
        AND oe-prmtx.custShipID         EQ cShipId 
        AND oe-prmtx.procat             EQ itemfg.procat 
        AND oe-prmtx.custype            EQ cCustType
        AND (oe-prmtx.eff-date          EQ dtEffectiveDate )        
      BY oe-prmtx.eff-date DESC:
    LEAVE.
  END.
       
  IF NOT AVAIL oe-prmtx THEN DO:
    CREATE oe-prmtx.
    ASSIGN
     oe-prmtx.company = itemfg.company      
     oe-prmtx.eff-date = TODAY
     oe-prmtx.i-no = itemfg.i-no.
     IF dtEffectiveDate NE ? THEN 
     oe-prmtx.eff-date = dtEffectiveDate.
     
     IF lQuotePriceMatrix AND cRunMethods EQ "Type" THEN
     oe-prmtx.custype = cCustType.     
     ELSE IF lQuotePriceMatrix AND cRunMethods EQ "Ship To" THEN
     ASSIGN
          oe-prmtx.cust-no = ip-cust-no
          oe-prmtx.custShipID = cShipId.
     ELSE
     ASSIGN     
     oe-prmtx.cust-no = ip-cust-no.

  END.   
  oe-prmtx.meth = YES.
  IF lQuotePriceMatrix THEN
  DO:  
      oe-prmtx.quoteID = iQuoteNo.
      RUN AddTagInfo (
                INPUT quotehd.rec_key,
                INPUT "quotehd",
                INPUT "status is set to Approved",
                INPUT ""
                ). /*From TagProcs Super Proc*/     
  END.
  oe-prmtx.exp-date = dtExpireDate.
  
  DO li = 1 TO EXTENT(oe-prmtx.qty):
    IF oe-prmtx.qty[li] NE 0 THEN DO:
      CREATE w-matrix.
      ASSIGN
       w-matrix.qty   = oe-prmtx.qty[li]
       w-matrix.uom   = oe-prmtx.uom[li]
       w-matrix.price = oe-prmtx.price[li].
    END.
  END.

  FOR EACH w-matrix
      WHERE w-matrix.qty   EQ 0
         OR w-matrix.price EQ 0:
    DELETE w-matrix.
  END.

  FOR EACH w-matrix:
    w-matrix.price-m = w-matrix.price.

    IF w-matrix.uom NE "M" THEN
      RUN sys/ref/convcuom.p (w-matrix.uom, "M", 0, 0, 0, 0,
                              w-matrix.price-m, OUTPUT w-matrix.price-m).
  END.

  FOR EACH b-matrix BY b-matrix.qty:
    FOR EACH w-matrix
        WHERE w-matrix.qty GE b-matrix.qty
          AND w-matrix.qty EQ b-matrix.qty
          AND w-matrix.price-m EQ b-matrix.price-m
/* #103075 Remove logic to filter multiple diff-qty/same-price records */          
/*          AND (w-matrix.price-m GT b-matrix.price-m OR*/
/*               w-matrix.qty EQ b-matrix.qty)          */
          AND ROWID(w-matrix) NE ROWID(b-matrix):
      DELETE w-matrix.
    END.
  END.

  ASSIGN
   oe-prmtx.qty   = 0
   oe-prmtx.uom   = ""
   oe-prmtx.price = 0
   li             = 0.

  FOR EACH w-matrix WHERE w-matrix.qty LE 0:
    DELETE w-matrix.
  END.
  
  IF cPriceMatrixPricingMethod EQ "From" THEN
  oe-prmtx.qty = 0.
  ELSE
  FOR EACH w-matrix BREAK BY w-matrix.qty:
    IF LAST(w-matrix.qty) THEN DO:
      CREATE b-matrix.
      BUFFER-COPY w-matrix TO b-matrix
      ASSIGN
       b-matrix.qty = w-matrix.qty + 1.
    END.
  END.

  FOR EACH w-matrix BREAK BY w-matrix.qty DESC:
    li = li + 1.

    IF li LE EXTENT(oe-prmtx.qty) THEN
      ASSIGN
       oe-prmtx.qty[li]  = w-matrix.qty
       oe-prmtx.uom[li]  = w-matrix.uom
       oe-prmtx.price[li]= w-matrix.price.
  END.
  
  FIND CURRENT oe-prmtx NO-LOCK NO-ERROR.
  RUN Price_ExpireOldPrice(
        INPUT oe-prmtx.company,
        INPUT oe-prmtx.i-no,
        INPUT oe-prmtx.custshipid,
        INPUT oe-prmtx.cust-no,
        INPUT oe-prmtx.custype,
        INPUT oe-prmtx.procat
        ).

END PROCEDURE.

PROCEDURE update-matrix-minus.
  FOR EACH oe-prmtx
      WHERE oe-prmtx.company            EQ itemfg.company
        AND oe-prmtx.cust-no            EQ cCustCompareValue 
        AND oe-prmtx.i-no               BEGINS itemfg.i-no
        AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no                 
        AND oe-prmtx.custShipID         EQ cShipId 
        AND oe-prmtx.procat             EQ itemfg.procat 
        AND oe-prmtx.custype            EQ cCustType
        AND (oe-prmtx.eff-date          EQ dtEffectiveDate )
      BY oe-prmtx.eff-date DESC:
    LEAVE.
  END.

  IF NOT AVAIL oe-prmtx THEN DO:
    CREATE oe-prmtx.
    ASSIGN
     oe-prmtx.company = itemfg.company       
     oe-prmtx.eff-date = TODAY
     oe-prmtx.i-no = itemfg.i-no.
     IF dtEffectiveDate NE ? THEN 
     oe-prmtx.eff-date = dtEffectiveDate.
     
     IF lQuotePriceMatrix AND cRunMethods EQ "Type" THEN
     oe-prmtx.custype = cCustType.
     ELSE IF lQuotePriceMatrix AND cRunMethods EQ "Ship To" THEN
     ASSIGN
          oe-prmtx.cust-no = ip-cust-no
          oe-prmtx.custShipID = cShipId.
     ELSE
     ASSIGN     
     oe-prmtx.cust-no = ip-cust-no.

  END.
  oe-prmtx.meth = YES.
  IF lQuotePriceMatrix THEN
  DO:
      oe-prmtx.quoteID = iQuoteNo.      
      RUN AddTagInfo (
                INPUT quotehd.rec_key,
                INPUT "quotehd",
                INPUT "The status is set to Approved ",
                INPUT ""
                ). /*From TagProcs Super Proc*/
  END.  
  oe-prmtx.exp-date = dtExpireDate. 
  
  DO li = 1 TO EXTENT(oe-prmtx.qty):
    IF oe-prmtx.qty[li] NE 0 THEN DO:
      CREATE w-matrix.
      ASSIGN
       w-matrix.qty   = oe-prmtx.qty[li]
       w-matrix.uom   = oe-prmtx.uom[li]
       w-matrix.price = oe-prmtx.price[li].
    END.
  END.

  FOR EACH w-matrix
      WHERE w-matrix.qty   EQ 0
         OR w-matrix.price EQ 0:
    DELETE w-matrix.
  END.

  FOR EACH w-matrix:
    w-matrix.price-m = w-matrix.price.

    IF w-matrix.uom NE "M" THEN
      RUN sys/ref/convcuom.p (w-matrix.uom, "M", 0, 0, 0, 0,
                              w-matrix.price-m, OUTPUT w-matrix.price-m).
  END.

  FOR EACH b-matrix BY b-matrix.qty:
    FOR EACH w-matrix
        WHERE w-matrix.qty GE b-matrix.qty
          AND w-matrix.qty EQ b-matrix.qty
          AND w-matrix.price-m EQ b-matrix.price-m
/* #103075 Remove logic to filter multiple diff-qty/same-price records */          
/*          AND (w-matrix.price-m GT b-matrix.price-m OR*/
/*               w-matrix.qty EQ b-matrix.qty)          */
          AND ROWID(w-matrix) NE ROWID(b-matrix):
      DELETE w-matrix.
    END.
  END.

  ASSIGN
   oe-prmtx.qty   = 0
   oe-prmtx.uom   = ""
   oe-prmtx.price = 0
   li             = 0.

  FOR EACH w-matrix WHERE w-matrix.qty LE 0:
    DELETE w-matrix.
  END.

  IF cPriceMatrixPricingMethod EQ "From" THEN
  oe-prmtx.qty = 0.
  ELSE
  FOR EACH w-matrix BREAK BY w-matrix.qty:
    IF LAST(w-matrix.qty) THEN DO:
      CREATE b-matrix.
      BUFFER-COPY w-matrix TO b-matrix
      ASSIGN
       b-matrix.qty = w-matrix.qty + 1.
    END.
  END.

  vdPrevPrice = 0.
  FOR EACH w-matrix BREAK BY w-matrix.qty /*DESC*/:
    li = li + 1.
    
    IF li LE EXTENT(oe-prmtx.qty) THEN DO:

       /*IF li = 1 THEN ASSIGN oe-prmtx.qty[li]  = w-matrix.qty - 1
                             oe-prmtx.uom[li]  = w-matrix.uom
                             oe-prmtx.price[li]= 0.
       ELSE */ ASSIGN oe-prmtx.qty[li]  = w-matrix.qty - 1
                   oe-prmtx.uom[li]  = w-matrix.uom
                   oe-prmtx.price[li]= vdPrevPrice.

       vdPrevPrice = w-matrix.price.
/*MESSAGE "minus: " li SKIP
    oe-prmtx.qty[li] oe-prmtx.price[li] ":" vdPrevPrice w-matrix.qty - 1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
    END.
  END.
  
  FIND CURRENT oe-prmtx NO-LOCK NO-ERROR.
  RUN Price_ExpireOldPrice(
        INPUT oe-prmtx.company,
        INPUT oe-prmtx.i-no,
        INPUT oe-prmtx.custshipid,
        INPUT oe-prmtx.cust-no,
        INPUT oe-prmtx.custype,
        INPUT oe-prmtx.procat
        ).

END PROCEDURE.
