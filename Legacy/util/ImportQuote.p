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
    FIELD CustPart       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Part #" HELP "Required - Size:30" 
    FIELD CustNo         AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer" HELP "Required - Size:30"
    FIELD Quote          AS INTEGER   FORMAT ">>>>>>>>" COLUMN-LABEL "Quote#" HELP "Optional - Integer"
    FIELD Qty            AS INTEGER   FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Quantity" HELP "Required - Size:30"
    FIELD Price          AS DECIMAL   FORMAT "->>>,>>>,>>9.99" COLUMN-LABEL "Price" HELP "Required - Decimal"
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

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

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

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportQuote FOR ttImportQuote.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
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
        FIND FIRST quotehd NO-LOCK
            WHERE quotehd.company  EQ ipbf-ttImportQuote.Company
            AND quotehd.q-no     EQ ipbf-ttImportQuote.Quote 
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
        
    END.

   
    /*Field level validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:

        IF oplValid AND ipbf-ttImportQuote.CustPart NE "" THEN 
            RUN pIsValidCustPartID IN hdValidator (ipbf-ttImportQuote.CustPart,ipbf-ttImportQuote.CustNo, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.CustNo NE "" THEN 
            RUN pIsValidCustomerID IN hdValidator (ipbf-ttImportQuote.CustNo, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

         IF oplValid AND ipbf-ttImportQuote.uom NE "" THEN 
            RUN pIsValidUOM IN hdValidator (ipbf-ttImportQuote.uom, NO, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportQuote.SalesGroup NE "" THEN 
            RUN pIsValidSalesRep IN hdValidator (ipbf-ttImportQuote.SalesGroup, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportQuote.TermsCode NE "" THEN 
            RUN pIsValidTerms IN hdValidator (ipbf-ttImportQuote.Terms, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportQuote.Carrier NE "" THEN 
            RUN pIsValidCarrier IN hdValidator (ipbf-ttImportQuote.Carrier, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.Carrier NE "" AND ipbf-ttImportQuote.zone NE "" THEN 
            RUN pIsValidDeliveryZone IN hdValidator (ipbf-ttImportQuote.Carrier, ipbf-ttImportQuote.zone, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.CustPart NE "" THEN 
            RUN pIsValidShiptoID IN hdValidator (ipbf-ttImportQuote.CustNo,ipbf-ttImportQuote.ShipTo, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.EstNo NE "" THEN 
            RUN pIsValidEstID IN hdValidator (ipbf-ttImportQuote.EstNo, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportQuote.Style NE "" THEN 
            RUN pIsValidStyle IN hdValidator (ipbf-ttImportQuote.Style, NO, ipbf-ttImportQuote.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.

    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
     IF ipbf-ttImportQuote.ShipTo EQ "" THEN
        ipbf-ttImportQuote.ShipTo EQ ipbf-ttImportQuote.CustNo .
    IF ipbf-ttImportQuote.SoldTo EQ "" THEN
        ipbf-ttImportQuote.SoldTo EQ ipbf-ttImportQuote.CustNo .

    IF ipbf-ttImportQuote.QuoteDate EQ ? THEN
        ipbf-ttImportQuote.QuoteDate = TODAY .
    IF ipbf-ttImportQuote.DeliveryDate EQ ? THEN
        ipbf-ttImportQuote.DeliveryDate = TODAY .
    ipbf-ttImportQuote.EstNo = fill(" ",8 - LENGTH(TRIM(ipbf-ttImportQuote.EstNo))) +
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
    DEFINE VARIABLE li-next-line AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
    DEFINE BUFFER bQuoteItm FOR quoteitm.

    FIND FIRST quotehd EXCLUSIVE-LOCK
        WHERE quotehd.company  EQ ipbf-ttImportQuote.Company
          AND quotehd.q-no     EQ ipbf-ttImportQuote.Quote 
      NO-ERROR.
     
      
    IF NOT AVAILABLE quotehd THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE quotehd.
        ASSIGN 
            quotehd.company = ipbf-ttImportQuote.Company
            quotehd.cust-no = ipbf-ttImportQuote.CustNo
            .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */

    RUN pAssignValueC (ipbf-ttImportQuote.CustNo, YES, INPUT-OUTPUT quotehd.cust-no).
    RUN pAssignValueC (ipbf-ttImportQuote.ShipTo, iplIgnoreBlanks, INPUT-OUTPUT quotehd.ship-id).
    RUN pAssignValueC (ipbf-ttImportQuote.SoldTo, iplIgnoreBlanks, INPUT-OUTPUT quotehd.sold-id).
    RUN pAssignValueDate (DATE(ipbf-ttImportQuote.QuoteDate), iplIgnoreBlanks, INPUT-OUTPUT quotehd.quo-date).
    RUN pAssignValueDate (DATE(ipbf-ttImportQuote.DeliveryDate), iplIgnoreBlanks, INPUT-OUTPUT quotehd.del-date).
    RUN pAssignValueDate (DATE(ipbf-ttImportQuote.ExpirationDate), iplIgnoreBlanks, INPUT-OUTPUT quotehd.expireDate).
    RUN pAssignValueC (ipbf-ttImportQuote.EstNo, iplIgnoreBlanks, INPUT-OUTPUT quotehd.est-no).
    RUN pAssignValueC (ipbf-ttImportQuote.contact, iplIgnoreBlanks, INPUT-OUTPUT quotehd.contact).
    RUN pAssignValueC (ipbf-ttImportQuote.SalesGroup, iplIgnoreBlanks, INPUT-OUTPUT quotehd.sman).
    RUN pAssignValueC (ipbf-ttImportQuote.terms, iplIgnoreBlanks, INPUT-OUTPUT quotehd.terms).
    RUN pAssignValueC (ipbf-ttImportQuote.carrier, iplIgnoreBlanks, INPUT-OUTPUT quotehd.carrier).
    RUN pAssignValueC (ipbf-ttImportQuote.zone, iplIgnoreBlanks, INPUT-OUTPUT quotehd.del-zone).

     FIND FIRST cust WHERE cust.company = quotehd.company
            AND cust.cust-no = quotehd.cust-no NO-LOCK NO-ERROR.

    IF quotehd.sman EQ "" THEN
        quotehd.sman = IF AVAILABLE cust THEN cust.sman ELSE "".
    IF quotehd.terms EQ "" THEN
        quotehd.terms = IF AVAILABLE cust THEN cust.terms ELSE "".
   IF quotehd.carrier EQ "" THEN DO:
    FIND FIRST shipto NO-LOCK
        WHERE shipto.company  = quotehd.company
        AND shipto.cust-no  = quotehd.cust-no
        NO-ERROR.
      IF AVAILABLE shipto THEN
          ASSIGN quotehd.carrier = IF AVAILABLE shipto THEN shipto.ship-id ELSE ""
                 quotehd.del-zone = IF AVAILABLE shipto THEN shipto.carrier ELSE "".
   END.
  
  FIND LAST bQuoteItm USE-INDEX q-line WHERE bQuoteItm.company = quotehd.company
                                         AND bQuoteItm.loc = quotehd.loc
                                         AND bQuoteItm.q-no = quotehd.q-no
                 NO-LOCK NO-ERROR.
  li-next-line = IF AVAILABLE bQuoteItm THEN bQuoteItm.line + 1 ELSE 1.
    
    
    FIND FIRST quoteitm OF quotehd EXCLUSIVE-LOCK 
        WHERE quoteitm.part-no EQ  ipbf-ttImportQuote.CustPart NO-ERROR .

    IF NOT AVAILABLE quoteitm THEN DO:
       CREATE quoteitm .
        ASSIGN quoteitm.company = quotehd.company
         quoteitm.loc =  quotehd.loc
         quoteitm.q-no = quotehd.q-no
         quoteitm.line = li-next-line
         quoteitm.upd-date = TODAY
         quoteitm.upd-user = USERID(LDBNAME(1)) .
    END.

      RUN pAssignValueC (ipbf-ttImportQuote.CustPart, YES, INPUT-OUTPUT quoteitm.part-no).
      RUN pAssignValueI (ipbf-ttImportQuote.Qty, YES, INPUT-OUTPUT quoteitm.qty).
      RUN pAssignValueD (ipbf-ttImportQuote.price, iplIgnoreBlanks, INPUT-OUTPUT quoteitm.price).
      RUN pAssignValueC (ipbf-ttImportQuote.UOM, iplIgnoreBlanks, INPUT-OUTPUT quoteitm.uom).
      RUN pAssignValueC (ipbf-ttImportQuote.ItemDscr, iplIgnoreBlanks, INPUT-OUTPUT quoteitm.part-dscr1).
      RUN pAssignValueC (ipbf-ttImportQuote.ItemDscr2, iplIgnoreBlanks, INPUT-OUTPUT quoteitm.part-dscr2).
      RUN pAssignValueC (ipbf-ttImportQuote.Dimensions, iplIgnoreBlanks, INPUT-OUTPUT quoteitm.size).
      RUN pAssignValueC (ipbf-ttImportQuote.board, iplIgnoreBlanks, INPUT-OUTPUT quoteitm.i-dscr).
      RUN pAssignValueC (ipbf-ttImportQuote.color1, iplIgnoreBlanks, INPUT-OUTPUT quoteitm.i-coldscr).

      RUN custom/getcpart.p (quotehd.company, quotehd.cust-no,
                             INPUT-OUTPUT quoteitm.part-no, INPUT-OUTPUT lv-rowid).
      FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAILABLE itemfg THEN
      FIND FIRST itemfg
          WHERE itemfg.company  EQ quoteitm.company
            AND itemfg.part-no  EQ quoteitm.part-no
            AND itemfg.part-no  NE ""
            AND (itemfg.cust-no EQ quotehd.cust-no OR
                 itemfg.i-code  EQ "S")
          NO-LOCK NO-ERROR.


      IF ipbf-ttImportQuote.style NE "" THEN
          RUN pAssignValueC (ipbf-ttImportQuote.style, iplIgnoreBlanks, INPUT-OUTPUT quoteitm.style).
      ELSE
          quoteitm.style = IF AVAILABLE itemfg THEN itemfg.style ELSE "" .

        quoteitm.i-no = IF AVAILABLE itemfg THEN itemfg.i-no ELSE "".
         IF quoteitm.part-dscr1 EQ "" THEN
             quoteitm.part-dscr1 = IF AVAILABLE itemfg THEN itemfg.i-name ELSE "" .
         IF quoteitm.SIZE EQ "" THEN
             quoteitm.SIZE = IF AVAILABLE itemfg THEN  (STRING(itemfg.l-score[50]) + "X" + STRING(itemfg.w-score[50]) + "X" + STRING(itemfg.d-score[50])) ELSE "" .
        
      FIND FIRST quoteqty EXCLUSIVE-LOCK
          WHERE quoteqty.company EQ quoteitm.company 
          AND quoteqty.loc EQ quoteitm.loc 
          AND quoteqty.q-no EQ quoteitm.q-no 
          AND quoteqty.line EQ quoteitm.line
          AND quoteqty.qty EQ ipbf-ttImportQuote.Qty NO-ERROR . 

      IF NOT AVAILABLE quoteqty THEN DO:
          CREATE quoteqty .
          ASSIGN quoteqty.company = quoteitm.company
              quoteqty.loc = quoteitm.loc
              quoteqty.q-no = quoteitm.q-no
              quoteqty.line = quoteitm.line
              quoteqty.quote-date = TODAY
              quoteqty.quote-user = USERID(LDBNAME(1)) .
      END.
      
      RUN pAssignValueI (ipbf-ttImportQuote.qty, iplIgnoreBlanks, INPUT-OUTPUT quoteqty.qty).
      RUN pAssignValueD (ipbf-ttImportQuote.price, iplIgnoreBlanks, INPUT-OUTPUT quoteqty.price).
      RUN pAssignValueC (ipbf-ttImportQuote.UOM, iplIgnoreBlanks, INPUT-OUTPUT quoteqty.uom).

      
END PROCEDURE.

