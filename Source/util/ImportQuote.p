/*------------------------------------------------------------------------
    File        : ImportQuote.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Quote	
    Author(s)   : Sewa
    Created     : tu sep 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportQuote
    FIELD Company        AS CHARACTER 
    FIELD Location       AS CHARACTER
    FIELD QuoteGroup     AS CHARACTER 
    FIELD CustPart       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Part #" HELP "Required - Size:30" 
    FIELD CustNo         AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer" HELP "Required - Size:30"
    FIELD Quote          AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Quote#" HELP "Optional - Integer or <AUTO> to auto-number.  Use <AUTO>#### where # is a unique group number."
    FIELD Qty            AS INTEGER   FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Quantity" HELP "Required - Size:30"
    FIELD Price          AS DECIMAL   FORMAT "->>>,>>>,>>9.99" COLUMN-LABEL "Price   " HELP "Required - Decimal"
    FIELD Profit         AS DECIMAL   FORMAT "->>9.99%" COLUMN-LABEL "Profit %" HELP "Required - Decimal"
    FIELD Uom            AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "UOM" HELP "Required - Size:2" 
    FIELD ShipTo         AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "ShipTo" HELP "Optional - Size:8"
    FIELD SoldTo         AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "SoldTo" HELP "Optional - Size:8"
    FIELD QuoteDate      AS DATE      FORMAT 99/99/9999 COLUMN-LABEL "Quote Date" HELP "Optional - Date"
    FIELD DeliveryDate   AS DATE      FORMAT 99/99/9999 COLUMN-LABEL "Delivery Date" HELP "Optional - Date"
    FIELD ExpirationDate AS DATE      FORMAT 99/99/9999 COLUMN-LABEL "Expiration Date" HELP "Optional - Date"
    FIELD EstNo          AS CHARACTER FORMAT "x(6)" COLUMN-LABEL "Estimate #" HELP "Optional - Size:6"
    FIELD Contact        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Contact" HELP "Optional - Size:30" 
    FIELD SalesGroup     AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Sales Group" HELP "Optional - Size:3"
    FIELD TermsCode      AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Terms Code" HELP "Optional - Size:5"  
    FIELD Carrier        AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Carrier" HELP "Optional - Size:8"   
    FIELD Zone           AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Zone" HELP "Optional - Size:5" 
    FIELD FGItem         AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "FG Item #" HELP "Optional - Size:15"   
    FIELD ItemDscr       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Item Description" HELP "Optional - Size:30"  
    FIELD ItemDscr2      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Item Description 2" HELP "Optional - Size:30"  
    FIELD Style          AS CHARACTER FORMAT "X(6)" COLUMN-LABEL "Style" HELP "Optional - Size:6"  
    FIELD Dimensions     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Dimensions" HELP "Optional - Size:30"  
    FIELD Board          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Board" HELP "Optional - Size:30"  
    FIELD Color1         AS CHARACTER FORMAT "X(20)" COLUMN-LABEL "Color" HELP "Optional - Size:20"  
    
    .
    
DEFINE VARIABLE gcAutoIndicator AS CHARACTER NO-UNDO INITIAL "<AUTO>".
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INITIAL 3. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportQuote"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportQuote FOR ttImportQuote.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportQuote FOR ttImportQuote.

    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportQuote.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportQuote.CustNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportQuote.CustPart EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Part #".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportQuote.qty EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Quantity".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportQuote.price LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Price can not be Negative or Zero.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportQuote.uom EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Price UOM".
    END.
   
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportQuote.Quote BEGINS gcAutoIndicator THEN DO:
            opcNote = "Add Record - Auto Increment Quote"
            .
        END.
        ELSE DO:
            FIND FIRST quotehd NO-LOCK
                WHERE quotehd.company  EQ ipbf-ttImportQuote.Company
                AND quotehd.q-no     EQ INTEGER(ipbf-ttImportQuote.Quote) 
                NO-ERROR.
            IF AVAILABLE quotehd THEN 
            DO:
                IF NOT iplUpdateDuplicates THEN 
                    ASSIGN 
                        oplValid = NO
                        opcNote  = "Duplicate Exists:  Will be skipped"
                        .
                ELSE
                    ASSIGN 
                        opcNote = "Update record - All fields to be overwritten"
                        .        
            END.
            ELSE 
                ASSIGN 
                    opcNote = "Add record"
                    .
        END. /*not auto-incremented quote*/
    END. /*oplValid*/

   
    /*Field level validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:

        IF oplValid AND ipbf-ttImportQuote.CustPart NE "" THEN 
            RUN pIsValidCustPartIDNonFG (ipbf-ttImportQuote.CustPart,ipbf-ttImportQuote.CustNo, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.CustNo NE "" THEN 
            RUN pIsValidCustomerID (ipbf-ttImportQuote.CustNo, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

         IF oplValid AND ipbf-ttImportQuote.uom NE "" THEN 
            RUN pIsValidUOM (ipbf-ttImportQuote.uom, NO, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportQuote.SalesGroup NE "" THEN 
            RUN pIsValidSalesRep (ipbf-ttImportQuote.SalesGroup, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportQuote.TermsCode NE "" THEN 
            RUN pIsValidTerms (ipbf-ttImportQuote.Terms, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportQuote.Carrier NE "" THEN 
            RUN pIsValidCarrier (ipbf-ttImportQuote.Carrier, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.Carrier NE "" AND ipbf-ttImportQuote.zone NE "" THEN 
            RUN pIsValidDeliveryZone (ipbf-ttImportQuote.Carrier, ipbf-ttImportQuote.zone, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.CustPart NE "" THEN 
            RUN pIsValidShiptoID (ipbf-ttImportQuote.CustNo,ipbf-ttImportQuote.ShipTo, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.EstNo NE "" THEN 
            RUN pIsValidEstID (ipbf-ttImportQuote.EstNo, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.Style NE "" THEN 
            RUN pIsValidStyle (ipbf-ttImportQuote.Style, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.

    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
     IF ipbf-ttImportQuote.ShipTo EQ "" THEN
        ipbf-ttImportQuote.ShipTo EQ ipbf-ttImportQuote.CustNo .
    IF ipbf-ttImportQuote.SoldTo EQ "" THEN
        ipbf-ttImportQuote.SoldTo EQ ipbf-ttImportQuote.CustNo .

    IF ipbf-ttImportQuote.QuoteDate EQ ? THEN
        ipbf-ttImportQuote.QuoteDate = TODAY .
    
    ipbf-ttImportQuote.EstNo = FILL(" ",8 - LENGTH(TRIM(ipbf-ttImportQuote.EstNo))) +
                                                   TRIM(ipbf-ttImportQuote.EstNo).
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportQuote FOR ttImportQuote.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iNextLine AS INTEGER NO-UNDO.
    DEFINE VARIABLE riItemfg AS ROWID NO-UNDO.
    DEFINE VARIABLE cQuoteNumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuoteGroup AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAutoNumber AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lNewGroup AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dTotCost AS DECIMAL NO-UNDO .
    DEFINE BUFFER bQuoteItm FOR quoteitm.
    DEFINE BUFFER bf-ttImportQuote FOR ttImportQuote.
    DEFINE VARIABLE hdupdQuoteProcs AS HANDLE NO-UNDO.
    RUN util/updQuoteProcs.p PERSISTENT SET hdupdQuoteProcs.
    DEFINE BUFFER bf-quotehd FOR quotehd.
    DEFINE BUFFER bf-quoteitm FOR quoteitm.
    DEFINE BUFFER bf-quoteqty FOR quoteqty.

    ASSIGN 
        cQuoteGroup = ""
        lAutoNumber = NO
        cQuoteNumber = ipbf-ttImportQuote.Quote
        .
    IF cQuoteNumber BEGINS gcAutoIndicator THEN DO:
        /*Auto numbering logic*/
        
        /*Get the QuoteGroup as string to the right of the indicator*/
        IF LENGTH(cQuoteNumber) NE LENGTH(gcAutoIndicator) THEN 
            cQuoteGroup = SUBSTRING(cQuoteNumber,LENGTH(gcAutoIndicator) + 1, LENGTH(cQuoteNumber) - LENGTH(gcAutoIndicator)).
        IF cQuoteGroup NE "" THEN 
            FIND FIRST bf-ttImportQuote NO-LOCK
                 WHERE bf-ttImportQuote.QuoteGroup EQ cQuoteGroup
                NO-ERROR.
        IF AVAILABLE bf-ttImportQuote THEN
            cQuoteNumber = bf-ttImportQuote.Quote.
        ELSE 
            lAutoNumber = YES.
    END.
    FIND FIRST bf-quotehd EXCLUSIVE-LOCK
        WHERE bf-quotehd.company  EQ ipbf-ttImportQuote.Company
        AND bf-quotehd.q-no     EQ INTEGER(cQuoteNumber) 
        NO-ERROR.
      
    IF NOT AVAILABLE bf-quotehd THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-quotehd.
        ASSIGN 
            bf-quotehd.company = ipbf-ttImportQuote.Company
            bf-quotehd.cust-no = ipbf-ttImportQuote.CustNo
            .
        IF lAutoNumber AND cQuoteGroup NE "" THEN DO:
            FIND CURRENT ipbf-ttImportQuote EXCLUSIVE-LOCK.
            ASSIGN 
                ipbf-ttImportQuote.QuoteGroup = cQuoteGroup
                ipbf-ttImportQuote.Quote = STRING(bf-quotehd.q-no)
                .
            FIND CURRENT ipbf-ttImportQuote NO-LOCK.
        END.
    END.
    
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */

    RUN pAssignValueC (ipbf-ttImportQuote.CustNo, YES, INPUT-OUTPUT bf-quotehd.cust-no).
    RUN pAssignValueC (ipbf-ttImportQuote.ShipTo, iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.ship-id).
    RUN pAssignValueC (ipbf-ttImportQuote.SoldTo, iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.sold-id).
    RUN pAssignValueDate (DATE(ipbf-ttImportQuote.QuoteDate), iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.quo-date).
    RUN pAssignValueDate (DATE(ipbf-ttImportQuote.DeliveryDate), iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.del-date).
    RUN pAssignValueDate (DATE(ipbf-ttImportQuote.ExpirationDate), iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.expireDate).
    RUN pAssignValueC (ipbf-ttImportQuote.EstNo, iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.est-no).
    RUN pAssignValueC (ipbf-ttImportQuote.contact, iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.contact).
    RUN pAssignValueC (ipbf-ttImportQuote.SalesGroup, iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.sman).
    RUN pAssignValueC (ipbf-ttImportQuote.terms, iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.terms).
    RUN pAssignValueC (ipbf-ttImportQuote.carrier, iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.carrier).
    RUN pAssignValueC (ipbf-ttImportQuote.zone, iplIgnoreBlanks, INPUT-OUTPUT bf-quotehd.del-zone).

     FIND FIRST cust WHERE cust.company = bf-quotehd.company
            AND cust.cust-no = bf-quotehd.cust-no NO-LOCK NO-ERROR.

    IF bf-quotehd.sman EQ "" THEN
        bf-quotehd.sman = IF AVAILABLE cust THEN cust.sman ELSE "".
    IF bf-quotehd.terms EQ "" THEN
        bf-quotehd.terms = IF AVAILABLE cust THEN cust.terms ELSE "".
   IF bf-quotehd.carrier EQ "" THEN DO:
    FIND FIRST shipto NO-LOCK
        WHERE shipto.company  = bf-quotehd.company
        AND shipto.cust-no  = bf-quotehd.cust-no
        AND shipto.ship-id  = bf-quotehd.ship-id
        NO-ERROR.
    IF NOT AVAIL shipto THEN
    FIND FIRST shipto NO-LOCK
        WHERE shipto.company  = bf-quotehd.company
        AND shipto.cust-no  = bf-quotehd.cust-no
        NO-ERROR.
      IF AVAILABLE shipto THEN
          ASSIGN bf-quotehd.carrier = IF AVAILABLE shipto THEN shipto.carrier ELSE ""
                 bf-quotehd.del-zone = IF AVAILABLE shipto THEN shipto.dest-code ELSE "".
   END.
  
  FIND LAST bQuoteItm USE-INDEX q-line WHERE bQuoteItm.company = bf-quotehd.company
                                         AND bQuoteItm.loc = bf-quotehd.loc
                                         AND bQuoteItm.q-no = bf-quotehd.q-no
                 NO-LOCK NO-ERROR.
  iNextLine = IF AVAILABLE bQuoteItm THEN bQuoteItm.line + 1 ELSE 1.
    
    
    FIND FIRST bf-quoteitm OF bf-quotehd EXCLUSIVE-LOCK 
        WHERE bf-quoteitm.part-no EQ  ipbf-ttImportQuote.CustPart NO-ERROR .

    IF NOT AVAILABLE bf-quoteitm THEN DO:
       CREATE bf-quoteitm .
        ASSIGN bf-quoteitm.company = bf-quotehd.company
         bf-quoteitm.loc =  bf-quotehd.loc
         bf-quoteitm.q-no = bf-quotehd.q-no
         bf-quoteitm.line = iNextLine
         bf-quoteitm.upd-date = TODAY
         bf-quoteitm.upd-user = USERID(LDBNAME(1)) .
    END.

      RUN pAssignValueC (ipbf-ttImportQuote.CustPart, YES, INPUT-OUTPUT bf-quoteitm.part-no).
      RUN pAssignValueI (ipbf-ttImportQuote.Qty, YES, INPUT-OUTPUT bf-quoteitm.qty).
      RUN pAssignValueD (ipbf-ttImportQuote.price, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteitm.price).
      RUN pAssignValueC (ipbf-ttImportQuote.UOM, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteitm.uom).
      RUN pAssignValueC (ipbf-ttImportQuote.ItemDscr, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteitm.part-dscr1).
      RUN pAssignValueC (ipbf-ttImportQuote.ItemDscr2, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteitm.part-dscr2).
      RUN pAssignValueC (ipbf-ttImportQuote.Dimensions, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteitm.size).
      RUN pAssignValueC (ipbf-ttImportQuote.board, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteitm.i-dscr).
      RUN pAssignValueC (ipbf-ttImportQuote.color1, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteitm.i-coldscr).

      RUN custom/getcpart.p (bf-quotehd.company, bf-quotehd.cust-no,
                             INPUT-OUTPUT bf-quoteitm.part-no, INPUT-OUTPUT riItemfg).
      FIND itemfg WHERE ROWID(itemfg) EQ riItemfg NO-LOCK NO-ERROR.

      IF NOT AVAILABLE itemfg THEN
      FIND FIRST itemfg
          WHERE itemfg.company  EQ bf-quoteitm.company
            AND itemfg.part-no  EQ bf-quoteitm.part-no
            AND itemfg.part-no  NE ""
            AND (itemfg.cust-no EQ bf-quotehd.cust-no OR
                 itemfg.i-code  EQ "S")
          NO-LOCK NO-ERROR.


      IF ipbf-ttImportQuote.style NE "" THEN
          RUN pAssignValueC (ipbf-ttImportQuote.style, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteitm.style).
      ELSE
          bf-quoteitm.style = IF AVAILABLE itemfg THEN itemfg.style ELSE "" .

        bf-quoteitm.i-no = IF AVAILABLE itemfg THEN itemfg.i-no ELSE "".
         IF bf-quoteitm.part-dscr1 EQ "" THEN
             bf-quoteitm.part-dscr1 = IF AVAILABLE itemfg THEN itemfg.i-name ELSE "" .
         IF bf-quoteitm.SIZE EQ "" THEN
             bf-quoteitm.SIZE = IF AVAILABLE itemfg THEN  (STRING(itemfg.l-score[50]) + "X" + STRING(itemfg.w-score[50]) + "X" + STRING(itemfg.d-score[50])) ELSE "" .
        
      FIND FIRST bf-quoteqty EXCLUSIVE-LOCK
          WHERE bf-quoteqty.company EQ bf-quoteitm.company 
          AND bf-quoteqty.loc EQ bf-quoteitm.loc 
          AND bf-quoteqty.q-no EQ bf-quoteitm.q-no 
          AND bf-quoteqty.line EQ bf-quoteitm.line
          AND bf-quoteqty.qty EQ ipbf-ttImportQuote.Qty NO-ERROR . 

      IF NOT AVAILABLE bf-quoteqty THEN DO:
          CREATE bf-quoteqty .
          ASSIGN bf-quoteqty.company = bf-quoteitm.company
              bf-quoteqty.loc = bf-quoteitm.loc
              bf-quoteqty.q-no = bf-quoteitm.q-no
              bf-quoteqty.line = bf-quoteitm.line
              bf-quoteqty.quote-date = TODAY
              bf-quoteqty.quote-user = USERID(LDBNAME(1)) .
      END.
      
      RUN pAssignValueI (ipbf-ttImportQuote.qty, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteqty.qty).
      RUN pAssignValueD (ipbf-ttImportQuote.price, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteqty.price).
      RUN pAssignValueD (ipbf-ttImportQuote.profit, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteqty.profit).
      RUN pAssignValueC (ipbf-ttImportQuote.UOM, iplIgnoreBlanks, INPUT-OUTPUT bf-quoteqty.uom).
      
      dTotCost = bf-quoteqty.mat-cost + bf-quoteqty.lab-cost 
                 + bf-quoteqty.fo-cost
                 + bf-quoteqty.vo-cost.

      IF bf-quotehd.est-no = "" AND AVAIL itemfg AND itemfg.sell-price GT 0 THEN DO:     
         CASE bf-quoteqty.uom:
           WHEN "EA" THEN
               bf-quoteqty.profit = ((bf-quoteqty.price * 1000) - dTotCost) / (bf-quoteqty.price * 1000) * 100.
           WHEN "M" THEN
               bf-quoteqty.profit = ((bf-quoteqty.price) - dTotCost) / (bf-quoteqty.price)  * 100.
           WHEN "CS" THEN
               bf-quoteqty.profit = ((bf-quoteqty.price / itemfg.case-count * 1000) - dTotCost) / (bf-quoteqty.price / itemfg.case-count * 1000)  * 100.
           WHEN "LOT" THEN
               bf-quoteqty.profit = ((bf-quoteqty.price / bf-quoteitm.qty * 1000) - dTotCost) / (bf-quoteqty.price / bf-quoteitm.qty * 1000)  * 100.
         END CASE.           
      END. /* if est-no = "" */

      RUN UpdateExpireDate_allQuote IN hdupdQuoteProcs(ROWID(bf-quoteitm)) .
      RELEASE bf-quoteitm.
      RELEASE bf-quotehd.
      RELEASE bf-quoteqty.
END PROCEDURE.

