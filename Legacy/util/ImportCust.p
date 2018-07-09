/*------------------------------------------------------------------------
    File        : ImportCust.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Customer	
    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportCust
    FIELD Company       AS CHARACTER 
    FIELD Location      AS CHARACTER 
    FIELD CustNo        AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Customer" HELP "Required - Size:8" 
    FIELD CustName      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Name" HELP "Optional - Size:30" 
    FIELD CustAdd1      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Addr1" HELP "Optional - Size:30"
    FIELD CustAdd2      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Addr2" HELP "Optional - Size:30"
    FIELD CustCity      AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "City" HELP "Optional - Size:15"
    FIELD CustState     AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "State" HELP "Optional - Size:2" 
    FIELD CustCountry   AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Country" HELP "Optional - Size:10"
    FIELD CustZip       AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Zip Code" HELP "Optional - Size:10"
    FIELD CustSman      AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "SalesMan" HELP "Optional - Size:3"
    FIELD CustAreaCode  AS CHARACTER FORMAT "xxx" COLUMN-LABEL "Area Code" HELP "Optional - Size:3"
    FIELD CustPhone     AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Phone" HELP "Optional - Size:7"
    FIELD CustFax       AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Fax #" HELP "Optional - Size:7"
    FIELD CreditLimit   AS DECIMAL   FORMAT ">>>,>>>,>>9.99" COLUMN-LABEL "Credit Limit" HELP "Optional - Decimal" 
    FIELD CustStatus    AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Status" HELP "Optional - Size:1"
    FIELD CreditHold    AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Credit Hold" HELP "Optional - Yes or N0"  
    FIELD CustType      AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Customer Type" HELP "Optional - Size:8"   
    FIELD Terms         AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Terms Code" HELP "Optional - Size:5" 
    FIELD FedID         AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Tax Resale#" HELP "Optional - Size:8"   
    FIELD Note1         AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Note 1" HELP "Optional - Size:30"  
    FIELD Note2         AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Note 2" HELP "Optional - Size:30"  
    FIELD Note3         AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Note 3" HELP "Optional - Size:30"  
    FIELD Note4         AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Note 4" HELP "Optional - Size:30"  
    FIELD ShipName      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "ShipTo Name" HELP "Optional - Size:30"  
    FIELD ShipAdd1      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "ShipTo Address 1" HELP "Optional - Size:30"  
    FIELD ShipAdd2      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "ShipTo Address 2" HELP "Optional - Size:30"  
    FIELD ShipCity      AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "ShipTo City" HELP "Optional - Size:15"   
    FIELD ShipState     AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "ShipTo State" HELP "Optional - Size:2"
    FIELD ShipZip       AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "ShipTo Zip" HELP "Optional - Size:10"  
    FIELD Contact       AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Contact Name" HELP "Optional - Size:25"  
    FIELD DateAdded     AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Date Added" HELP "Optional - Date"  
    FIELD CSRUser       AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "CSR" HELP "Optional - Size:10"  
    FIELD cCrUse        AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Cr. Acct #" HELP "Optional - Size:10" 
    FIELD cCRating      AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Credit Rating" HELP "Optional - Size:3" 
    FIELD dOrderLimit   AS DECIMAL   FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "Order Limit" HELP "Optional - Decimal" 
    FIELD dDiscPct      AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Discount %" HELP "Optional - Decimal" 
    FIELD cCurrency     AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Currency" HELP "Optional - Size:3" 
    FIELD cFinChrg      AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Finance Charges" HELP "Optional - Yes or N0"
    FIELD cAutoPrc      AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Auto Price" HELP "Optional - Yes or N0"
    FIELD cEdi          AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "EDI" HELP "Optional - Yes or N0"
    FIELD cFactrd       AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Factored" HELP "Optional - Yes or N0"
    FIELD iGraceDay     AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Grace Days" HELP "Optional - Integer" 
    FIELD dGraceDolr    AS DECIMAL   FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Grace $" HELP "Optional - Decimal"
    FIELD lInvPer       AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Invoice Per" HELP "Required - must be yes Or no or ?"
    FIELD cFrtPay       AS CHARACTER FORMAT "X(1)" COLUMN-LABEL "Freight Terms" HELP "Required - must be B Or C or P or T - Size:1"
    FIELD cFOB          AS CHARACTER FORMAT "X(4)" COLUMN-LABEL "Freight Terms" HELP "Required - must be DEST or ORIG - Size:4"
    FIELD cLoc          AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Location" HELP "Optional - Size:5" 
    FIELD cCarrier      AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Carrier" HELP "Optional - Size:5" 
    FIELD cDelZone      AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Delivery zone" HELP "Optional - Size:5" 
    FIELD cTerr         AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Territory" HELP "Optional - Size:5"
    FIELD iPalletId     AS INTEGER   FORMAT ">>>>>>>>9" COLUMN-LABEL "Pallet Id" HELP "Optional - Integer"
    FIELD dUnderPct     AS DECIMAL   FORMAT ">>9.99%" COLUMN-LABEL "Underrun %" HELP "Optional - Decimal" 
    FIELD dOverPct      AS DECIMAL   FORMAT ">>9.99%" COLUMN-LABEL "Overrunrun %" HELP "Optional - Decimal"
    FIELD cPallet       AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Pallet" HELP "Optional - Size:10"
    FIELD cCaseBundle   AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Case/Bundle" HELP "Optional - Size:10"
    FIELD dMarkup       AS DECIMAL   FORMAT "->9.99" COLUMN-LABEL "Mark-Up" HELP "Optional - Decimal"
    FIELD iLablePerSkip AS INTEGER   FORMAT "->,>>>,>>9" COLUMN-LABEL "# of Labels per Skid" HELP "Optional - Integer"
    FIELD iWhsDay       AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Whse Days" HELP "Optional - Integer" 
    FIELD iPalPos       AS INTEGER   FORMAT ">>>9" COLUMN-LABEL "Pallet Positions" HELP "Optional - Integer" 
    FIELD cPoMand       AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "PO# Mandatory?" HELP "Optional - Yes or N0"
    FIELD cShwSet       AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Show Set" HELP "Optional - Yes or N0"
    FIELD cPprLsInv     AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Paperless Invoice?" HELP "Optional - Yes or N0"
    FIELD cPartialShp   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Partial Ship" HELP "Optional - Yes or N0"
    FIELD cTaxable      AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Taxable" HELP "Required - must be Yes or No - Size:4"
    FIELD cTaxPrepCode  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Tax Prep Code" HELP "Optional - Size:10"
    FIELD cTaxGr        AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Tax Code" HELP "Optional - Size:10"
    FIELD cTaxResale    AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "Tax Resale#" HELP "Optional - Size:10"
    FIELD cExpDate      AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Exp Date" HELP "Optional - Date"  
    FIELD cEmail        AS CHARACTER FORMAT "X(60)" COLUMN-LABEL "Email" HELP "Optional - Date" 
    FIELD cGroup        AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Group" HELP "Optional - Character" 
    FIELD dBrkComm      AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Broker Comm%" HELP "Optional - Decimal" 
    FIELD dFltComm      AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Flat Comm%" HELP "Optional - Decimal" 
    FIELD cPrefix    AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Prefix" HELP "Optional - Size:3"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportCust"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCust FOR ttImportCust.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportCust FOR ttImportCust.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.CustNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.CustType EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer Type".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.Terms EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer Terms".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.CreditLimit LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Credit Limit can not be Negative.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.dOrderLimit LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Order Limit can not be Negative.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.dDiscPct LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Discount% can not be Negative.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.iGraceDay LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Grace Days can not be Negative.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.dGraceDolr LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Grace $ can not be Negative.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.cLoc EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Location.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.cCarrier EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Carrier.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.cDelZone EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Delivery Zone.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.iPalletId LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Pallet Id can not be Negative.".
    END.
    
    IF oplValid THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipbf-ttImportCust.Company
            AND cust.cust-no EQ ipbf-ttImportCust.CustNo
            NO-ERROR .
        IF AVAILABLE cust THEN 
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
        IF oplValid AND ipbf-ttImportCust.CustStatus NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Active", ipbf-ttImportCust.CustStatus, "A,X,S,E,I", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.CustSman NE "" THEN 
            RUN pIsValidSalesRep IN hdValidator (ipbf-ttImportCust.CustSman, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.CustType NE "" THEN 
            RUN pIsValidCustomerType IN hdValidator (ipbf-ttImportCust.CustType, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.CustState NE "" THEN 
            RUN pIsValidState IN hdValidator (ipbf-ttImportCust.CustState, NO, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.Terms NE "" THEN 
            RUN pIsValidTerms IN hdValidator (ipbf-ttImportCust.Terms, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.ShipState NE "" THEN 
            RUN pIsValidState IN hdValidator (ipbf-ttImportCust.ShipState, NO, OUTPUT oplValid, OUTPUT cValidNote). 

        IF oplValid AND ipbf-ttImportCust.CSRUser NE "" THEN 
            RUN pIsValidUserId IN hdValidator (ipbf-ttImportCust.CSRUser, NO, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cCurrency NE "" THEN 
            RUN pIsValidCurrency IN hdValidator (ipbf-ttImportCust.cCurrency, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.lInvPer NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Bol", ipbf-ttImportCust.lInvPer, "yes,no,?", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cFrtPay NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Bill", ipbf-ttImportCust.cFrtPay, "B,C,P,T", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cFOB NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Destination", ipbf-ttImportCust.cFOB, "DEST,ORIG", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cLoc NE "" THEN 
            RUN pIsValidWarehouse IN hdValidator (ipbf-ttImportCust.cLoc, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cCarrier NE "" THEN 
            RUN pIsValidCarrier IN hdValidator (ipbf-ttImportCust.cCarrier, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cCarrier NE "" AND ipbf-ttImportCust.cDelzone NE "" THEN 
            RUN pIsValidDeliveryZone IN hdValidator (ipbf-ttImportCust.cCarrier, ipbf-ttImportCust.cDelzone, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cTerr NE "" THEN 
            RUN pIsValidTerr IN hdValidator (ipbf-ttImportCust.cTerr, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cPallet NE "" THEN 
            RUN pIsValidItemForType IN hdValidator (ipbf-ttImportCust.cPallet, "D", NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cCaseBundle NE "" THEN 
            RUN pIsValidItemForType IN hdValidator (ipbf-ttImportCust.cCaseBundle, "C", NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cTaxable NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("yes", ipbf-ttImportCust.cTaxable, "yes,no", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cTaxPrepCode NE "" THEN 
            RUN pIsValidTaxGroup IN hdValidator (ipbf-ttImportCust.cTaxPrepCode, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cTaxGr NE "" THEN 
            RUN pIsValidTaxGroup IN hdValidator (ipbf-ttImportCust.cTaxGr, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCust FOR ttImportCust.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    FIND FIRST cust EXCLUSIVE-LOCK
        WHERE cust.company EQ ipbf-ttImportCust.Company
        AND cust.cust-no EQ ipbf-ttImportCust.CustNo
        NO-ERROR.  
    IF NOT AVAILABLE cust THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE cust.
        ASSIGN 
            cust.company = ipbf-ttImportCust.Company
            cust.cust-no = ipbf-ttImportCust.CustNo
            .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportCust.CustName, iplIgnoreBlanks, INPUT-OUTPUT cust.name).
    RUN pAssignValueC (ipbf-ttImportCust.CustAdd1, iplIgnoreBlanks, INPUT-OUTPUT cust.addr[1]).
    RUN pAssignValueC (ipbf-ttImportCust.CustAdd2, iplIgnoreBlanks, INPUT-OUTPUT cust.addr[2]).
    RUN pAssignValueC (ipbf-ttImportCust.CustCity, iplIgnoreBlanks, INPUT-OUTPUT cust.city).
    RUN pAssignValueC (ipbf-ttImportCust.CustState, iplIgnoreBlanks, INPUT-OUTPUT cust.state).
    RUN pAssignValueC (ipbf-ttImportCust.CustCountry, iplIgnoreBlanks, INPUT-OUTPUT cust.fax-country).
    RUN pAssignValueC (ipbf-ttImportCust.CustCountry, iplIgnoreBlanks, INPUT-OUTPUT cust.country).
    RUN pAssignValueC (ipbf-ttImportCust.CustZip, iplIgnoreBlanks, INPUT-OUTPUT cust.zip).
    RUN pAssignValueC (ipbf-ttImportCust.CustSman, iplIgnoreBlanks, INPUT-OUTPUT cust.sman).
    RUN pAssignValueC (ipbf-ttImportCust.CustAreaCode, iplIgnoreBlanks, INPUT-OUTPUT cust.area-code).
    RUN pAssignValueC (ipbf-ttImportCust.CustPhone, iplIgnoreBlanks, INPUT-OUTPUT cust.phone).  
    RUN pAssignValueC (ipbf-ttImportCust.CustFax, iplIgnoreBlanks, INPUT-OUTPUT cust.fax).
    RUN pAssignValueD (ipbf-ttImportCust.CreditLimit, iplIgnoreBlanks, INPUT-OUTPUT cust.cr-lim).
    RUN pAssignValueC (ipbf-ttImportCust.CustStatus, YES, INPUT-OUTPUT cust.active).
    RUN pAssignValueC (ipbf-ttImportCust.CreditHold, YES, INPUT-OUTPUT cust.cr-hold).
    RUN pAssignValueC (ipbf-ttImportCust.CustType, YES, INPUT-OUTPUT cust.type).
    RUN pAssignValueC (ipbf-ttImportCust.Terms, YES, INPUT-OUTPUT cust.terms).
    RUN pAssignValueC (ipbf-ttImportCust.FedID, iplIgnoreBlanks, INPUT-OUTPUT cust.tax-id).
    RUN pAssignValueC (ipbf-ttImportCust.Contact, iplIgnoreBlanks, INPUT-OUTPUT cust.contact). 
    RUN pAssignValueC (ipbf-ttImportCust.CSRUser, YES, INPUT-OUTPUT cust.csrUser_id).
    ASSIGN 
        cust.date-field = DATE(ipbf-ttImportCust.DateAdded) . 
    RUN pAssignValueC (ipbf-ttImportCust.cCrUse, iplIgnoreBlanks, INPUT-OUTPUT cust.cr-use).
    RUN pAssignValueC (ipbf-ttImportCust.cCRating, iplIgnoreBlanks, INPUT-OUTPUT cust.cr-rating).
    RUN pAssignValueD (ipbf-ttImportCust.dOrderLimit, iplIgnoreBlanks, INPUT-OUTPUT cust.ord-lim).
    RUN pAssignValueD (ipbf-ttImportCust.dDiscPct, iplIgnoreBlanks, INPUT-OUTPUT cust.disc).
    RUN pAssignValueC (ipbf-ttImportCust.cCurrency, iplIgnoreBlanks, INPUT-OUTPUT cust.curr-code).
    RUN pAssignValueC (ipbf-ttImportCust.cFinChrg, YES, INPUT-OUTPUT cust.fin-chg).
    RUN pAssignValueC (ipbf-ttImportCust.cAutoPrc, YES, INPUT-OUTPUT cust.auto-reprice).
    RUN pAssignValueC (ipbf-ttImportCust.cEdi, YES, INPUT-OUTPUT cust.an-edi-cust).
    RUN pAssignValueC (ipbf-ttImportCust.cFactrd, YES, INPUT-OUTPUT cust.factored).
    RUN pAssignValueI (ipbf-ttImportCust.iGraceDay, YES, INPUT-OUTPUT cust.cr-hold-invdays).
    RUN pAssignValueD (ipbf-ttImportCust.dGraceDolr, iplIgnoreBlanks, INPUT-OUTPUT cust.cr-hold-invdue).
    RUN pAssignValueC (ipbf-ttImportCust.lInvPer, YES, INPUT-OUTPUT cust.inv-meth).
    RUN pAssignValueC (ipbf-ttImportCust.cFrtPay, YES, INPUT-OUTPUT cust.frt-pay).
    RUN pAssignValueC (ipbf-ttImportCust.cFOB, YES, INPUT-OUTPUT cust.fob-code).
    RUN pAssignValueC (ipbf-ttImportCust.cLoc, iplIgnoreBlanks, INPUT-OUTPUT cust.loc).
    RUN pAssignValueC (ipbf-ttImportCust.cCarrier, iplIgnoreBlanks, INPUT-OUTPUT cust.carrier).
    RUN pAssignValueC (ipbf-ttImportCust.cDelZone, iplIgnoreBlanks, INPUT-OUTPUT cust.del-zone).
    RUN pAssignValueC (ipbf-ttImportCust.cterr, iplIgnoreBlanks, INPUT-OUTPUT cust.terr).
    RUN pAssignValueI (ipbf-ttImportCust.iPalletId, YES, INPUT-OUTPUT cust.spare-int-1).
    RUN pAssignValueD (ipbf-ttImportCust.dUnderPct, iplIgnoreBlanks, INPUT-OUTPUT cust.under-pct).
    RUN pAssignValueD (ipbf-ttImportCust.dOverPct, iplIgnoreBlanks, INPUT-OUTPUT cust.over-pct).
    RUN pAssignValueC (ipbf-ttImportCust.cPallet, iplIgnoreBlanks, INPUT-OUTPUT cust.pallet).
    RUN pAssignValueC (ipbf-ttImportCust.cCaseBundle, iplIgnoreBlanks, INPUT-OUTPUT cust.case-bundle).
    RUN pAssignValueD (ipbf-ttImportCust.dMarkup, iplIgnoreBlanks, INPUT-OUTPUT cust.markup).
    RUN pAssignValueI (ipbf-ttImportCust.iLablePerSkip, YES, INPUT-OUTPUT cust.int-field[1]).
    RUN pAssignValueI (ipbf-ttImportCust.iWhsDay, YES, INPUT-OUTPUT cust.ship-days).
    RUN pAssignValueI (ipbf-ttImportCust.iPalPos, YES, INPUT-OUTPUT cust.manf-day).
    RUN pAssignValueC (ipbf-ttImportCust.cPoMand, YES, INPUT-OUTPUT cust.po-mandatory).
    RUN pAssignValueC (ipbf-ttImportCust.cShwSet, YES, INPUT-OUTPUT cust.show-set).
    RUN pAssignValueC (ipbf-ttImportCust.cPprLsInv, YES, INPUT-OUTPUT cust.log-field[1]).
    RUN pAssignValueC (ipbf-ttImportCust.cPartialShp, YES, INPUT-OUTPUT cust.ship-part).
    RUN pAssignValueC (ipbf-ttImportCust.cTaxable, YES, INPUT-OUTPUT cust.sort).
    RUN pAssignValueC (ipbf-ttImportCust.cTaxPrepCode, iplIgnoreBlanks, INPUT-OUTPUT cust.spare-char-1).
    RUN pAssignValueC (ipbf-ttImportCust.cTaxGr, iplIgnoreBlanks, INPUT-OUTPUT cust.tax-gr).
    RUN pAssignValueC (ipbf-ttImportCust.cTaxResale, iplIgnoreBlanks, INPUT-OUTPUT cust.tax-id).
    IF date(ipbf-ttImportCust.cExpDate) EQ TODAY THEN
        date(ipbf-ttImportCust.cExpDate) = ?.
    ASSIGN 
        cust.date-field[2] = DATE(ipbf-ttImportCust.cExpDate) .
    RUN pAssignValueC (ipbf-ttImportCust.cEmail, iplIgnoreBlanks, INPUT-OUTPUT cust.email).
    RUN pAssignValueC (ipbf-ttImportCust.cGroup, iplIgnoreBlanks, INPUT-OUTPUT cust.spare-char-2).
    RUN pAssignValueD (ipbf-ttImportCust.dBrkComm, iplIgnoreBlanks, INPUT-OUTPUT cust.scomm).
    RUN pAssignValueD (ipbf-ttImportCust.dFltComm, iplIgnoreBlanks, INPUT-OUTPUT cust.flatCommPct).
    RUN pAssignValueC (ipbf-ttImportCust.cPrefix, iplIgnoreBlanks, INPUT-OUTPUT cust.fax-prefix).


    FIND FIRST shipto EXCLUSIVE-LOCK 
        WHERE shipto.company EQ cust.company
        AND shipto.cust-no EQ cust.cust-no
        AND shipto.ship-id EQ cust.cust-no
        NO-ERROR.
    IF NOT AVAILABLE shipto THEN 
    DO:
        CREATE shipto.
        ASSIGN 
            shipto.company = cust.company
            shipto.cust-no = cust.cust-no
            shipto.ship-id = cust.cust-no
            .
    END.
    RUN pAssignValueC (ipbf-ttImportCust.ShipName, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-name).
    RUN pAssignValueC (ipbf-ttImportCust.ShipAdd1, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-addr[1]).
    RUN pAssignValueC (ipbf-ttImportCust.ShipAdd2, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-addr[2]).
    RUN pAssignValueC (ipbf-ttImportCust.ShipCity, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-city).
    RUN pAssignValueC (ipbf-ttImportCust.ShipState, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-state).
    RUN pAssignValueC (ipbf-ttImportCust.ShipZip, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-zip).
        
    FIND FIRST soldto EXCLUSIVE-LOCK 
        WHERE soldto.company EQ cust.company
        AND soldto.cust-no EQ cust.cust-no
        AND soldto.sold-id EQ cust.cust-no
        NO-ERROR.
    IF NOT AVAILABLE soldto THEN 
    DO:
        CREATE soldto.
        ASSIGN 
            soldto.company      = cust.company
            soldto.cust-no      = cust.cust-no
            soldto.sold-id      = cust.cust-no
            soldto.sold-name    = cust.name
            soldto.sold-addr[1] = cust.addr[1]
            soldto.sold-addr[2] = cust.addr[2]
            soldto.sold-city    = cust.city
            soldto.sold-state   = cust.state
            soldto.sold-zip     = cust.zip
            .
    END.
            
    RUN pAddNote (cust.rec_key,
        ipbf-ttImportCust.Note1,
        "Misc Message 1",
        "",
        "C").
    RUN pAddNote (cust.rec_key,
        ipbf-ttImportCust.Note2,
        "Misc Message 2",
        "",
        "C").
    RUN pAddNote (cust.rec_key,
        ipbf-ttImportCust.Note3,
        "Mfg. Inst.",
        "",
        "C").
    RUN pAddNote (cust.rec_key,
        ipbf-ttImportCust.Note4,
        "B/L Message",
        "",
        "C"). 
    
END PROCEDURE.

PROCEDURE pAddNote:
    /*------------------------------------------------------------------------------
     Purpose: Adds a note to supplied rec_key and parameters
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER.
    DEFINE INPUT PARAMETER ipcText AS CHARACTER.
    DEFINE INPUT PARAMETER ipcTitle AS CHARACTER.
    DEFINE INPUT PARAMETER ipcCode AS CHARACTER.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER.

    IF ipcText NE "" THEN 
    DO:
        CREATE notes.
        ASSIGN
            notes.rec_key    = ipcRecKey
            notes.note_date  = TODAY
            notes.note_time  = TIME
            notes.note_text  = ipcText
            notes.note_title = ipcTitle
            notes.note_code  = ipcCode
            notes.user_id    = "asi"
            notes.note_type  = ipcType
            .                    
    END.                           

END PROCEDURE.
