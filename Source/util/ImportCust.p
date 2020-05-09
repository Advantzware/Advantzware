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
    FIELD CustStatus    AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Status" HELP "Optional - Size:11"
    FIELD CreditHold    AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Credit Hold" HELP "Optional - Yes or N0"  
    FIELD CustType      AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Customer Type" HELP "Required - Size:8"   
    FIELD Terms         AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Terms Code" HELP "Required - Size:5" 
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
    FIELD lInvPer       AS CHARACTER FORMAT "X(13)" COLUMN-LABEL "Invoice Per" HELP "Required - must be BOL Or PO or Group By Date - Size:13"
    FIELD cFrtPay       AS CHARACTER FORMAT "X(9)" COLUMN-LABEL "Freight Terms" HELP "Required - must be Bill Or Collect or Prepaid or 3rd Party - Size:9"
    FIELD cFOB          AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "FOB" HELP "Required - must be Destination or Origin - Size:11"
    FIELD cLoc          AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Location" HELP "Required - Size:5" 
    FIELD cCarrier      AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Carrier" HELP "Required - Size:5" 
    FIELD cDelZone      AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Delivery zone" HELP "Required - Size:5" 
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
    FIELD cPrefix       AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Prefix" HELP "Optional - Size:3"
    FIELD cCntPrice     AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Contract Pricing" HELP "Optional - Yes or N0"
    FIELD BankAcct      AS CHARACTER FORMAT "x(18)" COLUMN-LABEL "Account#" HELP "Optional - Validated - Size:18"
    FIELD SwiftBIC      AS CHARACTER FORMAT "x(11)" COLUMN-LABEL "Swift Code" HELP "Optional - Validated - Size:11"
    FIELD BankRTN       AS INTEGER FORMAT "999999999" COLUMN-LABEL "Routing" HELP "Optional - Integer"
    
    FIELD accountType   AS CHARACTER FORMAT "X(12)" COLUMN-LABEL "Account Type" HELP "Account type is used for sales reporting optional - Size:12 Split,Originated,Handed"
    FIELD splitType     AS INTEGER FORMAT "9" COLUMN-LABEL "Split Type" HELP "Split type used for sales reporting Optional - default 0"
    FIELD parentCust    AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Parent Customer" HELP "Master customer account Optional - Size:12"
    FIELD marketSegment AS CHARACTER FORMAT "x(16)" COLUMN-LABEL "Market Segment" HELP "Market segment for sales reporting Optional - Size:16"
    FIELD naicsCode     AS CHARACTER FORMAT "999999" COLUMN-LABEL "NAICS" HELP "NAICS Code, link to NaicsTable, Default = 999999"
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
        IF ipbf-ttImportCust.lInvPer EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Invoice Per.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.cFrtPay EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Freight Terms.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.cFob EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: FOB.".
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
        IF ipbf-ttImportCust.cTaxable EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Taxable.".
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
            RUN pIsValidFromList IN hdValidator ("Active", ipbf-ttImportCust.CustStatus, "Active,Inhouse,Statement,Service,Inactive", OUTPUT oplValid, OUTPUT cValidNote).

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
            RUN pIsValidFromList IN hdValidator ("Bol", ipbf-ttImportCust.lInvPer, "BOL,PO,Group by Date", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cFrtPay NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Bill", ipbf-ttImportCust.cFrtPay, "Bill,Collect,Prepaid,3rd Party", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportCust.cFOB NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Destination", ipbf-ttImportCust.cFOB, "Destination,Origin", OUTPUT oplValid, OUTPUT cValidNote).

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
            
        IF oplValid AND ipbf-ttImportCust.accountType NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Account Type", ipbf-ttImportCust.accountType, ",Split,Originated,Handed", OUTPUT oplValid, OUTPUT cValidNote).                
            
        IF oplValid AND ipbf-ttImportCust.splitType NE 0 THEN 
            RUN pIsValidFromList IN hdValidator ("Split Type", ipbf-ttImportCust.splitType, "0,1,2,3,4,5,6,7,8,9", OUTPUT oplValid, OUTPUT cValidNote).                     
        
        IF oplValid AND ipbf-ttImportCust.naicsCode NE "" THEN 
            RUN pIsValidNAICS IN hdValidator (ipbf-ttImportCust.naicsCode, YES, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    IF ipbf-ttImportCust.cFrtPay EQ "Collect" THEN 
        ipbf-ttImportCust.cFrtPay = "C".
    ELSE IF ipbf-ttImportCust.cFrtPay EQ "Bill" THEN 
        ipbf-ttImportCust.cFrtPay = "B".
    ELSE IF ipbf-ttImportCust.cFrtPay EQ "Prepaid" THEN 
        ipbf-ttImportCust.cFrtPay = "P".
    ELSE ipbf-ttImportCust.cFrtPay = "T".

    IF ipbf-ttImportCust.cFOB EQ "Destination" THEN
     ipbf-ttImportCust.cFOB = "DEST".
        ELSE 
          ipbf-ttImportCust.cFOB = "ORIG".
    IF ipbf-ttImportCust.lInvPer EQ "BOL" THEN
       ipbf-ttImportCust.lInvPer  = "NO".
    ELSE IF ipbf-ttImportCust.lInvPer EQ "PO" THEN
       ipbf-ttImportCust.lInvPer  = "yes".
    ELSE ipbf-ttImportCust.lInvPer  = "?".
    IF ipbf-ttImportCust.cTaxable EQ "yes" THEN
         ipbf-ttImportCust.cTaxable = "Y".
    ELSE ipbf-ttImportCust.cTaxable = "N".

    IF ipbf-ttImportCust.CustStatus EQ "Active" THEN 
        ipbf-ttImportCust.CustStatus = "A".
    ELSE IF ipbf-ttImportCust.CustStatus EQ "Inactive" THEN 
        ipbf-ttImportCust.CustStatus = "I".
    ELSE IF ipbf-ttImportCust.CustStatus EQ "Inhouse" THEN 
        ipbf-ttImportCust.CustStatus = "X".
    ELSE IF ipbf-ttImportCust.CustStatus EQ "Statement" THEN 
        ipbf-ttImportCust.CustStatus = "S".
    ELSE IF ipbf-ttImportCust.CustStatus EQ "Service" THEN 
        ipbf-ttImportCust.CustStatus = "E".
       
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCust FOR ttImportCust.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-cust FOR cust .
    DEFINE BUFFER bf-shipto FOR shipto .
    DEFINE BUFFER bf-soldto FOR soldto .
     
    FIND FIRST bf-cust EXCLUSIVE-LOCK
        WHERE bf-cust.company EQ ipbf-ttImportCust.Company
        AND bf-cust.cust-no EQ ipbf-ttImportCust.CustNo
        NO-ERROR.  
    IF NOT AVAILABLE bf-cust THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-cust.
        ASSIGN 
            bf-cust.company = ipbf-ttImportCust.Company
            bf-cust.cust-no = ipbf-ttImportCust.CustNo
            .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportCust.CustName, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.name).
    RUN pAssignValueC (ipbf-ttImportCust.CustAdd1, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.addr[1]).
    RUN pAssignValueC (ipbf-ttImportCust.CustAdd2, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.addr[2]).
    RUN pAssignValueC (ipbf-ttImportCust.CustCity, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.city).
    RUN pAssignValueC (ipbf-ttImportCust.CustState, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.state).
    RUN pAssignValueC (ipbf-ttImportCust.CustCountry, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.fax-country).
    RUN pAssignValueC (ipbf-ttImportCust.CustCountry, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.country).
    RUN pAssignValueC (ipbf-ttImportCust.CustZip, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.zip).
    RUN pAssignValueC (ipbf-ttImportCust.CustSman, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.sman).
    RUN pAssignValueC (ipbf-ttImportCust.CustAreaCode, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.area-code).
    RUN pAssignValueC (ipbf-ttImportCust.CustPhone, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.phone).  
    RUN pAssignValueC (ipbf-ttImportCust.CustFax, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.fax).
    RUN pAssignValueD (ipbf-ttImportCust.CreditLimit, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.cr-lim).
    RUN pAssignValueC (ipbf-ttImportCust.CustStatus, YES, INPUT-OUTPUT bf-cust.active).
    RUN pAssignValueC (ipbf-ttImportCust.CreditHold, YES, INPUT-OUTPUT bf-cust.cr-hold).
    RUN pAssignValueC (ipbf-ttImportCust.CustType, YES, INPUT-OUTPUT bf-cust.type).
    RUN pAssignValueC (ipbf-ttImportCust.Terms, YES, INPUT-OUTPUT bf-cust.terms).
    RUN pAssignValueC (ipbf-ttImportCust.FedID, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.tax-id).
    RUN pAssignValueC (ipbf-ttImportCust.Contact, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.contact). 
    RUN pAssignValueC (ipbf-ttImportCust.CSRUser, YES, INPUT-OUTPUT bf-cust.csrUser_id).
    RUN pAssignValueCToDt (ipbf-ttImportCust.DateAdded, YES, INPUT-OUTPUT bf-cust.date-field[1]).
    RUN pAssignValueC (ipbf-ttImportCust.cCrUse, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.cr-use).
    RUN pAssignValueC (ipbf-ttImportCust.cCRating, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.cr-rating).
    RUN pAssignValueD (ipbf-ttImportCust.dOrderLimit, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.ord-lim).
    RUN pAssignValueD (ipbf-ttImportCust.dDiscPct, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.disc).
    RUN pAssignValueC (ipbf-ttImportCust.cCurrency, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.curr-code).
    RUN pAssignValueC (ipbf-ttImportCust.cFinChrg, YES, INPUT-OUTPUT bf-cust.fin-chg).
    RUN pAssignValueC (ipbf-ttImportCust.cAutoPrc, YES, INPUT-OUTPUT bf-cust.auto-reprice).
    RUN pAssignValueC (ipbf-ttImportCust.cEdi, YES, INPUT-OUTPUT bf-cust.an-edi-cust).
    RUN pAssignValueC (ipbf-ttImportCust.cFactrd, YES, INPUT-OUTPUT bf-cust.factored).
    RUN pAssignValueI (ipbf-ttImportCust.iGraceDay, YES, INPUT-OUTPUT bf-cust.cr-hold-invdays).
    RUN pAssignValueD (ipbf-ttImportCust.dGraceDolr, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.cr-hold-invdue).
    RUN pAssignValueC (ipbf-ttImportCust.lInvPer, YES, INPUT-OUTPUT bf-cust.inv-meth).
    RUN pAssignValueC (ipbf-ttImportCust.cFrtPay, YES, INPUT-OUTPUT bf-cust.frt-pay).
    RUN pAssignValueC (ipbf-ttImportCust.cFOB, YES, INPUT-OUTPUT bf-cust.fob-code).
    RUN pAssignValueC (ipbf-ttImportCust.cLoc, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.loc).
    RUN pAssignValueC (ipbf-ttImportCust.cCarrier, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.carrier).
    RUN pAssignValueC (ipbf-ttImportCust.cDelZone, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.del-zone).
    RUN pAssignValueC (ipbf-ttImportCust.cterr, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.terr).
    RUN pAssignValueI (ipbf-ttImportCust.iPalletId, YES, INPUT-OUTPUT bf-cust.spare-int-1).
    RUN pAssignValueD (ipbf-ttImportCust.dUnderPct, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.under-pct).
    RUN pAssignValueD (ipbf-ttImportCust.dOverPct, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.over-pct).
    RUN pAssignValueC (ipbf-ttImportCust.cPallet, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.pallet).
    RUN pAssignValueC (ipbf-ttImportCust.cCaseBundle, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.case-bundle).
    RUN pAssignValueD (ipbf-ttImportCust.dMarkup, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.markup).
    RUN pAssignValueI (ipbf-ttImportCust.iLablePerSkip, YES, INPUT-OUTPUT bf-cust.int-field[1]).
    RUN pAssignValueI (ipbf-ttImportCust.iWhsDay, YES, INPUT-OUTPUT bf-cust.ship-days).
    RUN pAssignValueI (ipbf-ttImportCust.iPalPos, YES, INPUT-OUTPUT bf-cust.manf-day).
    RUN pAssignValueC (ipbf-ttImportCust.cPoMand, YES, INPUT-OUTPUT bf-cust.po-mandatory).
    RUN pAssignValueC (ipbf-ttImportCust.cShwSet, YES, INPUT-OUTPUT bf-cust.show-set).
    RUN pAssignValueC (ipbf-ttImportCust.cPprLsInv, YES, INPUT-OUTPUT bf-cust.log-field[1]).
    RUN pAssignValueC (ipbf-ttImportCust.cPartialShp, YES, INPUT-OUTPUT bf-cust.ship-part).
    RUN pAssignValueC (ipbf-ttImportCust.cTaxable, YES, INPUT-OUTPUT bf-cust.sort).
    RUN pAssignValueC (ipbf-ttImportCust.cTaxPrepCode, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.spare-char-1).
    RUN pAssignValueC (ipbf-ttImportCust.cTaxGr, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.tax-gr).
    RUN pAssignValueC (ipbf-ttImportCust.cTaxResale, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.tax-id).
    RUN pAssignValueCToDt (ipbf-ttImportCust.cExpDate, YES, INPUT-OUTPUT bf-cust.date-field[2]).
   IF date(ipbf-ttImportCust.cExpDate) EQ TODAY THEN
        bf-cust.date-field[2] = ?.
    RUN pAssignValueC (ipbf-ttImportCust.cEmail, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.email).
    RUN pAssignValueC (ipbf-ttImportCust.cGroup, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.spare-char-2).
    RUN pAssignValueD (ipbf-ttImportCust.dBrkComm, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.scomm).
    RUN pAssignValueD (ipbf-ttImportCust.dFltComm, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.flatCommPct).
    RUN pAssignValueC (ipbf-ttImportCust.cPrefix, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.fax-prefix).
    RUN pAssignValueC (ipbf-ttImportCust.cCntPrice, YES, INPUT-OUTPUT bf-cust.imported).
    RUN pAssignValueC (ipbf-ttImportCust.BankAcct, YES, INPUT-OUTPUT bf-cust.Bank-Acct).
    RUN pAssignValueC (ipbf-ttImportCust.SwiftBIC, YES, INPUT-OUTPUT bf-cust.SwiftBIC).
    RUN pAssignValueC (ipbf-ttImportCust.BankRTN, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.Bank-RTN).
    RUN pAssignValueC (ipbf-ttImportCust.accountType, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.accountType).
    RUN pAssignValueI (ipbf-ttImportCust.splitType, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.splitType).
    RUN pAssignValueC (ipbf-ttImportCust.parentCust, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.parentCust).
    RUN pAssignValueC (ipbf-ttImportCust.marketSegment, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.marketSegment).
    RUN pAssignValueC (ipbf-ttImportCust.naicsCode, iplIgnoreBlanks, INPUT-OUTPUT bf-cust.naicsCode).

    FIND FIRST bf-shipto EXCLUSIVE-LOCK 
        WHERE bf-shipto.company EQ bf-cust.company
        AND bf-shipto.cust-no EQ bf-cust.cust-no
        AND bf-shipto.ship-id EQ bf-cust.cust-no
        NO-ERROR.
    IF NOT AVAILABLE bf-shipto THEN 
    DO:
        CREATE bf-shipto.
        ASSIGN 
            bf-shipto.company = bf-cust.company
            bf-shipto.cust-no = bf-cust.cust-no
            bf-shipto.ship-id = bf-cust.cust-no
            bf-shipto.tax-code = bf-cust.tax-gr
            bf-shipto.tax-mandatory = bf-cust.sort EQ "Y"
            .
    END.
    RUN pAssignValueC (ipbf-ttImportCust.ShipName, iplIgnoreBlanks, INPUT-OUTPUT bf-shipto.ship-name).
    RUN pAssignValueC (ipbf-ttImportCust.ShipAdd1, iplIgnoreBlanks, INPUT-OUTPUT bf-shipto.ship-addr[1]).
    RUN pAssignValueC (ipbf-ttImportCust.ShipAdd2, iplIgnoreBlanks, INPUT-OUTPUT bf-shipto.ship-addr[2]).
    RUN pAssignValueC (ipbf-ttImportCust.ShipCity, iplIgnoreBlanks, INPUT-OUTPUT bf-shipto.ship-city).
    RUN pAssignValueC (ipbf-ttImportCust.ShipState, iplIgnoreBlanks, INPUT-OUTPUT bf-shipto.ship-state).
    RUN pAssignValueC (ipbf-ttImportCust.ShipZip, iplIgnoreBlanks, INPUT-OUTPUT bf-shipto.ship-zip).
        
    FIND FIRST bf-soldto EXCLUSIVE-LOCK 
        WHERE bf-soldto.company EQ bf-cust.company
        AND bf-soldto.cust-no EQ bf-cust.cust-no
        AND bf-soldto.sold-id EQ bf-cust.cust-no
        NO-ERROR.
    IF NOT AVAILABLE bf-soldto THEN 
    DO:
        CREATE bf-soldto.
        ASSIGN 
            bf-soldto.company      = bf-cust.company
            bf-soldto.cust-no      = bf-cust.cust-no
            bf-soldto.sold-id      = bf-cust.cust-no
            bf-soldto.sold-name    = bf-cust.name
            bf-soldto.sold-addr[1] = bf-cust.addr[1]
            bf-soldto.sold-addr[2] = bf-cust.addr[2]
            bf-soldto.sold-city    = bf-cust.city
            bf-soldto.sold-state   = bf-cust.state
            bf-soldto.sold-zip     = bf-cust.zip
            .
    END.
            
    RUN pAddNote (bf-cust.rec_key,
        ipbf-ttImportCust.Note1,
        "Misc Message 1",
        "",
        "C").
    RUN pAddNote (bf-cust.rec_key,
        ipbf-ttImportCust.Note2,
        "Misc Message 2",
        "",
        "C").
    RUN pAddNote (bf-cust.rec_key,
        ipbf-ttImportCust.Note3,
        "Mfg. Inst.",
        "",
        "C").
    RUN pAddNote (bf-cust.rec_key,
        ipbf-ttImportCust.Note4,
        "B/L Message",
        "",
        "C"). 
    RELEASE bf-cust.
    RELEASE bf-shipto.
    RELEASE bf-soldto .
    
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
