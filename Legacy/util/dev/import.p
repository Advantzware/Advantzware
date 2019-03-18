
/*------------------------------------------------------------------------
    File        : import.p
    Purpose     : Runs an import for various tables in the system

    Syntax      : run util\import.p (cocode, filename, logonly (Y/N), type, header file y/n, abort y/n)
                    Valid Types:  AR, AP, CUST, VEND, GL, FG
    Description : Runs an import based on a .csv file passed in as argument

    Author(s)   : BV
    Created     : Mon Sep 18 22:21:28 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcImportFile  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLogFile     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplLogOnly     AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER ipcType        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplHeader      AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER iplAbortIfInvalid AS LOGICAL NO-UNDO.


/*To be Exposed to support mass field updates based on type*/
DEFINE VARIABLE cField AS CHARACTER NO-UNDO INIT 'Note'.


DEFINE TEMP-TABLE ttCustUpdate
    FIELD cCustNo        AS CHARACTER
    FIELD cField         AS CHARACTER
    FIELD lValid         AS LOGICAL
    FIELD cInvalidReason AS CHARACTER
    FIELD iCount         AS INTEGER 
    .
DEFINE TEMP-TABLE ttImportedInvoicesAP
    FIELD cVendNo          AS CHARACTER 
    FIELD cInvNo           AS CHARACTER 
    FIELD cInvDate         AS CHARACTER 
    FIELD cDueDate         AS CHARACTER 
    FIELD cTaxCode         AS CHARACTER 
    FIELD cDiscount        AS CHARACTER 
    FIELD cDays            AS CHARACTER 
    FIELD cLinePONumber    AS CHARACTER 
    FIELD cLinePOLine      AS CHARACTER 
    FIELD cLineAccount     AS CHARACTER 
    FIELD cLineQuantity    AS CHARACTER 
    FIELD cLineQuantityUom AS CHARACTER 
    FIELD cLinePrice       AS CHARACTER 
    FIELD cLinePriceUom    AS CHARACTER 
    FIELD cLineAmount      AS CHARACTER 
    FIELD cLineTax         AS CHARACTER
    FIELD lValid           AS LOGICAL
    FIELD cInvalidReason   AS CHARACTER
    FIELD iCount           AS INTEGER
    .
    
DEFINE TEMP-TABLE ttImportedInvoicesAR
    FIELD cCustNo                  AS CHARACTER 
    FIELD cShipTo                  AS CHARACTER 
    FIELD cInvNo                   AS CHARACTER 
    FIELD cPONum                   AS CHARACTER 
    FIELD cInvDate                 AS CHARACTER 
    FIELD cDueDate                 AS CHARACTER 
    FIELD cTaxCode                 AS CHARACTER 
    FIELD cTermsCode               AS CHARACTER 
    FIELD cDiscount                AS CHARACTER 
    FIELD cDiscountDays            AS CHARACTER 
    FIELD cCarrier                 AS CHARACTER 
    FIELD cFreight                 AS CHARACTER 
    FIELD cLine                    AS CHARACTER 
    FIELD cLineAccount             AS CHARACTER 
    FIELD cLineItemNo              AS CHARACTER 
    FIELD cLineItemName            AS CHARACTER 
    FIELD cLineItemDescription     AS CHARACTER 
    FIELD cLineCustomerLotNo       AS CHARACTER 
    FIELD cLineQuantity            AS CHARACTER 
    FIELD cLineQuantityUom         AS CHARACTER 
    FIELD cLinePrice               AS CHARACTER 
    FIELD cLinePriceUom            AS CHARACTER 
    FIELD cLineAmount              AS CHARACTER 
    FIELD cLineDiscount            AS CHARACTER 
    FIELD cLineCost                AS CHARACTER 
    FIELD cLineCostUom             AS CHARACTER 
    FIELD cLineSalesman1           AS CHARACTER 
    FIELD cLineSalesman1Percent    AS CHARACTER 
    FIELD cLineSalesman1Commission AS CHARACTER
    FIELD cLineSalesman2           AS CHARACTER 
    FIELD cLineSalesman2Percent    AS CHARACTER 
    FIELD cLineSalesman2Commission AS CHARACTER
    FIELD cLineSalesman3           AS CHARACTER 
    FIELD cLineSalesman3Percent    AS CHARACTER 
    FIELD cLineSalesman3Commission AS CHARACTER
    FIELD cLineTax                 AS CHARACTER  
    FIELD lValid                   AS LOGICAL
    FIELD cInvalidReason           AS CHARACTER
    FIELD iCount                   AS INTEGER
    .
DEFINE TEMP-TABLE ttImportedVendors
    FIELD cVendNo          AS CHARACTER
    FIELD cVendName        AS CHARACTER
    FIELD cVendAdd1        AS CHARACTER
    FIELD cVendAdd2        AS CHARACTER
    FIELD cVendCity        AS CHARACTER
    FIELD cVendState       AS CHARACTER
    FIELD cVendCountry     AS CHARACTER
    FIELD cVendPostal      AS CHARACTER
    FIELD cVendZip         AS CHARACTER
    FIELD cVendRemit       AS CHARACTER 
    FIELD cVendRAdd1       AS CHARACTER
    FIELD cVendRAdd2       AS CHARACTER
    FIELD cVendRCity       AS CHARACTER
    FIELD cVendRState      AS CHARACTER
    FIELD cVendRPostal     AS CHARACTER
    FIELD cVendRZip        AS CHARACTER
    FIELD cTerms           AS CHARACTER
    FIELD cGL              AS CHARACTER
    FIELD cVendAreaCode    AS CHARACTER
    FIELD cVendPhone       AS CHARACTER
    FIELD cVendFaxAreaCode AS CHARACTER
    FIELD cVendFax         AS CHARACTER
    FIELD c1099            AS CHARACTER 
    FIELD cFedID           AS CHARACTER 
    FIELD cType            AS CHARACTER
    FIELD cTaxGroup        AS CHARACTER
    FIELD cCarrier         AS CHARACTER 
    FIELD lValid           AS LOGICAL
    FIELD cInvalidReason   AS CHARACTER
    FIELD iCount           AS INTEGER
    INDEX VendNo IS PRIMARY cVendNo
    INDEX Valid             lValid.


DEFINE TEMP-TABLE ttImportedCustomers
    FIELD cCustNo        AS CHARACTER
    FIELD cCustName      AS CHARACTER
    FIELD cCustAdd1      AS CHARACTER
    FIELD cCustAdd2      AS CHARACTER
    FIELD cCustCity      AS CHARACTER
    FIELD cCustState     AS CHARACTER
    FIELD cCustCountry   AS CHARACTER
    FIELD cCustZip       AS CHARACTER
    FIELD cCustSman      AS CHARACTER
    FIELD cCustAreaCode  AS CHARACTER
    FIELD cCustPhone     AS CHARACTER
    FIELD cCustFax       AS CHARACTER
    FIELD cCreditLimit   AS CHARACTER
    FIELD cStatus        AS CHARACTER
    FIELD cCreditHold    AS CHARACTER
    FIELD cCustType      AS CHARACTER
    FIELD cTerms         AS CHARACTER
    FIELD cFedID         AS CHARACTER
    FIELD cNote1         AS CHARACTER
    FIELD cNote2         AS CHARACTER
    FIELD cNote3         AS CHARACTER
    FIELD cNote4         AS CHARACTER
    FIELD cShipName      AS CHARACTER
    FIELD cShipAdd1      AS CHARACTER
    FIELD cShipAdd2      AS CHARACTER
    FIELD cShipCity      AS CHARACTER
    FIELD cShipState     AS CHARACTER
    FIELD cShipZip       AS CHARACTER
    FIELD cContact       AS CHARACTER 
    FIELD cDateAdded     AS CHARACTER 
    FIELD lValid         AS LOGICAL
    FIELD cInvalidReason AS CHARACTER
    FIELD iCount         AS INTEGER
    INDEX CustNo IS PRIMARY cCustNo
    INDEX Valid             lValid.

DEFINE TEMP-TABLE ttImportedFGItems
    FIELD cINo           AS CHARACTER
    FIELD cCustPart      AS CHARACTER
    FIELD cAvgCost       AS CHARACTER
    FIELD cLastCost      AS CHARACTER
    FIELD cStdMatl       AS CHARACTER
    FIELD cTotalCost     AS CHARACTER
    FIELD cCostUom       AS CHARACTER
    FIELD cCustId        AS CHARACTER
    FIELD cCategory      AS CHARACTER
    FIELD cWhs           AS CHARACTER
    FIELD cBin           AS CHARACTER
    FIELD cPurMan        AS CHARACTER
    FIELD cCasePal       AS CHARACTER
    FIELD cStockCust     AS CHARACTER
    FIELD cStyle         AS CHARACTER
    FIELD cName          AS CHARACTER
    FIELD cDesc1         AS CHARACTER
    FIELD cDesc2         AS CHARACTER
    FIELD cDesc3         AS CHARACTER
    FIELD cL             AS CHARACTER
    FIELD cW             AS CHARACTER
    FIELD cD             AS CHARACTER
    FIELD cSqFt          AS CHARACTER
    FIELD cCount         AS CHARACTER
    FIELD cUnPal         AS CHARACTER
    FIELD cBlankW        AS CHARACTER
    FIELD cBlankL        AS CHARACTER
    FIELD cNoteEst       AS CHARACTER
    FIELD cNotePick      AS CHARACTER
    FIELD lValid         AS LOGICAL
    FIELD cInvalidReason AS CHARACTER
    FIELD iCount         AS INTEGER
    INDEX INo IS PRIMARY cINo
    INDEX Valid          lValid.

DEFINE TEMP-TABLE ttImportedAccounts
    FIELD cAccount       AS CHARACTER
    FIELD cAccountDesc   AS CHARACTER
    FIELD cAccountType   AS CHARACTER
    FIELD lValid         AS LOGICAL
    FIELD cInvalidReason AS CHARACTER
    FIELD iCount         AS INTEGER
    INDEX Account IS PRIMARY cAccount
    INDEX Valid              lValid.


DEFINE STREAM sRow.
DEFINE STREAM sLog.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fValidateImportVend RETURNS LOGICAL 
    (  ) FORWARD.

FUNCTION fValidateImportCust RETURNS LOGICAL 
    (  ) FORWARD.

FUNCTION fValidateImportCustUpd RETURNS LOGICAL 
    (  ) FORWARD.

FUNCTION fValidateImportAccount RETURNS LOGICAL 
    (  ) FORWARD.

FUNCTION fValidateImportFG RETURNS LOGICAL 
    (  ) FORWARD.
    
FUNCTION fValidateImportInvoiceAP RETURNS LOGICAL 
    (  ) FORWARD.   

FUNCTION fValidateImportInvoiceAR RETURNS LOGICAL 
    (  ) FORWARD.   
       
     
/* ***************************  Main Block  *************************** */
CASE ipcType:
    WHEN "CUSTUPD" THEN 
        DO:
            RUN pImportFileCustUpd.
            IF fValidateImportCustUpd() OR NOT iplAbortIfInvalid THEN
                RUN pProcessChangesCustUpd.
            ELSE DO:
                iplLogOnly = YES.
                RUN pProcessChangesCustUpd.
            END.
        END.
    WHEN "CUST" THEN 
        DO:
            RUN pImportFileCust.
            IF fValidateImportCust() OR NOT iplAbortIfInvalid THEN
                RUN pProcessChangesCust.
            ELSE DO:
                iplLogOnly = YES.
                RUN pProcessChangesCust.
            END.
        END.
    WHEN "VEND" THEN 
        DO:
            RUN pImportFileVend.
            IF fValidateImportVend() OR NOT iplAbortIfInvalid THEN
                RUN pProcessChangesVend.
            ELSE DO:
                iplLogOnly = YES.
                RUN pProcessChangesVend.
            END.
        END.
    WHEN "GL" THEN 
        DO:
            RUN pImportFileAccount.
            IF fValidateImportAccount() OR NOT iplAbortIfInvalid THEN
                RUN pProcessChangesAccount.
            ELSE DO:
                iplLogOnly = YES.
                RUN pProcessChangesAccount.
            END.
        END.    
    WHEN "FG" THEN 
        DO:
            RUN pImportFileFG.
            IF fValidateImportFG() OR NOT iplAbortIfInvalid THEN
                RUN pProcessChangesFG.
            ELSE DO:
                iplLogOnly = YES.
                RUN pProcessChangesFG.
            END.
        END.
    WHEN "AP" THEN 
        DO:
            RUN pImportFileInvoicesAP.
            IF fValidateImportInvoiceAP() OR NOT iplAbortIfInvalid THEN
                RUN pProcessChangesInvociesAP.
            ELSE DO:
                iplLogOnly = YES.
                RUN pProcessChangesInvociesAP.
            END.
        END.
    WHEN "AR" THEN 
        DO:
            RUN pImportFileInvoicesAR.
            IF fValidateImportInvoiceAR() OR NOT iplAbortIfInvalid THEN
                RUN pProcessChangesInvociesAR.
            ELSE DO:
                iplLogOnly = YES.
                RUN pProcessChangesInvociesAR.
            END.
        END.
        
END CASE.
MESSAGE "Import Process Completed.  Check LogFile at " ipcLogFile " for results"
    VIEW-AS ALERT-BOX.
    
/* **********************  Internal Procedures  *********************** */

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

PROCEDURE pCreateNewInvoiceAP:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AP invoice, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    DEFINE INPUT PARAMETER ipcVendor AS CHARACTER.
    DEFINE INPUT PARAMETER ipcInvoice AS CHARACTER.
    DEFINE INPUT PARAMETER ipcInvDate AS CHARACTER.
    DEFINE OUTPUT PARAMETER opriAPInv AS ROWID.
    
    CREATE ap-inv.
    ASSIGN
        ap-inv.company  = ipcCompany
        ap-inv.inv-no   = ipcInvoice
        ap-inv.inv-date = TODAY
        ap-inv.vend-no  = ipcVendor
        .
    IF ipcInvDate NE "" THEN 
        ap-inv.inv-date = DATE(ipcInvDate).                     
    FIND FIRST vend NO-LOCK 
        WHERE vend.company EQ ipcCompany
        AND vend.vend-no EQ ipcVendor
        NO-ERROR.
    IF AVAILABLE vend THEN 
    DO:
        ASSIGN
            ap-inv.disc-%    = vend.disc-%
            ap-inv.disc-days = vend.disc-days
            ap-inv.tax-gr    = vend.tax-gr
            .  
        
        FIND FIRST currency NO-LOCK  
            WHERE currency.company EQ ipcCompany
            AND currency.c-code EQ  vend.curr-code
            NO-ERROR.
        FIND FIRST company NO-LOCK 
            WHERE company.company EQ ipcCompany
            NO-ERROR.
        IF NOT AVAILABLE currency AND AVAILABLE company THEN 
            FIND FIRST currency NO-LOCK 
                WHERE currency.company EQ ipcCompany
                AND currency.c-code EQ company.curr-code
                NO-ERROR.
        IF AVAILABLE currency THEN
            ASSIGN 
                ap-inv.ex-rate      = currency.ex-rate
                ap-inv.curr-code[1] = currency.c-code
                .
        ELSE 
            ASSIGN 
                ap-inv.ex-rate      = 1
                ap-inv.curr-code[1] = 'USD'
                .                        
        
        FIND FIRST terms WHERE terms.t-code EQ vend.terms NO-LOCK NO-ERROR.
        IF AVAILABLE terms THEN
            ASSIGN
                ap-inv.disc-%    = terms.disc-rate
                ap-inv.disc-days = terms.disc-days
                .
        ap-inv.due-date = IF AVAILABLE terms THEN terms.net-day + ap-inv.inv-date
        ELSE ap-inv.inv-date.
                                                   
    END.
    opriAPInv = ROWID(ap-inv).
    RELEASE ap-inv.

END PROCEDURE.

PROCEDURE pCreateNewInvoiceAR:
    /*------------------------------------------------------------------------------
         Purpose: Creates a new AR invoice, setting defaults based on key values
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER.
    DEFINE INPUT PARAMETER ipcInvDate AS CHARACTER.
    DEFINE OUTPUT PARAMETER opriARInv AS ROWID.
    
    DEFINE VARIABLE iNextInvoiceNumber     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNextInvoiceLinkNumber AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ar-inv FOR ar-inv.
    
    ASSIGN
        iNextInvoiceNumber     = 0
        iNextInvoiceLinkNumber = 0.
    FIND LAST bf-ar-inv NO-LOCK 
        USE-INDEX x-no 
        NO-ERROR.
    iNextInvoiceLinkNumber = IF AVAILABLE bf-ar-inv THEN bf-ar-inv.x-no + 1 ELSE 1.
    FIND FIRST ar-ctrl NO-LOCK  
        WHERE ar-ctrl.company EQ ipcCompany
        NO-ERROR.
    iNextInvoiceNumber = IF AVAILABLE ar-ctrl THEN ar-ctrl.last-inv + 1 ELSE 1.
    DO WHILE TRUE:
        FIND FIRST bf-ar-inv NO-LOCK 
            WHERE bf-ar-inv.company EQ ipcCompany
            AND bf-ar-inv.inv-no  EQ iNextInvoiceLinkNumber
            NO-ERROR.
        FIND FIRST inv-head NO-LOCK 
            WHERE inv-head.company EQ ipcCompany
            AND inv-head.inv-no  EQ iNextInvoiceLinkNumber
            NO-ERROR.
        IF NOT AVAILABLE bf-ar-inv AND NOT AVAILABLE inv-head THEN LEAVE.
        iNextInvoiceLinkNumber = iNextInvoiceLinkNumber + 1.
    END. 
    CREATE ar-inv.
    ASSIGN
        ar-inv.company  = ipcCompany
        ar-inv.inv-no   = iNextInvoiceNumber 
        ar-inv.x-no     = iNextInvoiceLinkNumber 
        ar-inv.inv-date = TODAY 
        ar-inv.cust-no  = ipcCustomer
        .
    IF ipcInvDate NE "" THEN ar-inv.inv-date = DATE(ipcInvDate).
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipcCompany
        AND cust.cust-no EQ ar-inv.cust-no
        NO-ERROR.    
    IF AVAILABLE cust THEN 
    DO:
        ASSIGN 
            ar-inv.cust-name    = cust.NAME
            ar-inv.terms        = cust.terms
            ar-inv.carrier      = cust.carrier
            ar-inv.tax-code     = cust.tax-gr
            ar-inv.curr-code[1] = cust.curr-code.
        IF cust.curr-code = "" THEN 
        DO:
            FIND company NO-LOCK 
                WHERE company.company EQ ipcCompany
                NO-ERROR.
            IF AVAILABLE company THEN 
                ar-inv.curr-code[1] = company.curr-code.
        END.
        FIND FIRST shipto NO-LOCK 
            WHERE shipto.company EQ ipcCompany 
            AND shipto.cust-no EQ cust.cust-no
            NO-ERROR.
        IF AVAILABLE shipto THEN 
            ASSIGN 
                ar-inv.ship-id = shipto.ship-id
                .                               .
        FIND currency NO-LOCK 
            WHERE currency.company EQ ipcCompany
            AND currency.c-code EQ cust.curr-code 
            NO-ERROR.
        IF AVAILABLE currency THEN 
            ar-inv.ex-rate = currency.ex-rate.
        FIND FIRST terms NO-LOCK 
            WHERE terms.t-code EQ cust.terms
            NO-ERROR.
        IF AVAILABLE terms THEN 
            ASSIGN 
                ar-inv.terms-d   = terms.dscr
                ar-inv.due-date  = ar-inv.inv-date + terms.net-days
                ar-inv.disc-%    = terms.disc-rate
                ar-inv.disc-days = terms.disc-days
                .
    END.
    
    opriARInv = ROWID(ar-inv).
    RELEASE ar-inv.
    
END PROCEDURE.

PROCEDURE pCreateNewInvoiceLineAP:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AP invoice line, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriAPInv AS ROWID.
    DEFINE OUTPUT PARAMETER opriAPInvl AS ROWID.
    
    DEFINE VARIABLE cAccount AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ap-invl FOR ap-invl.
    
    FIND ap-inv NO-LOCK 
        WHERE ROWID(ap-inv) EQ ipriAPInv
        NO-ERROR.
    IF NOT AVAILABLE ap-inv THEN RETURN.
    FIND FIRST ap-ctrl NO-LOCK
        WHERE ap-ctrl.company EQ ap-inv.company
        NO-ERROR.
    FIND FIRST vend NO-LOCK 
        WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
        NO-ERROR.
    IF NOT AVAILABLE vend THEN RETURN.
    IF vend.actnum NE "" THEN
        cAccount = vend.actnum.
    ELSE IF AVAILABLE ap-ctrl THEN
            cAccount = ap-ctrl.purchases.     
                
    FIND LAST bf-ap-invl NO-LOCK 
        WHERE bf-ap-invl.i-no  EQ ap-inv.i-no
        AND bf-ap-invl.po-no EQ 0
        USE-INDEX i-no 
        NO-ERROR.
                    
    CREATE ap-invl.
    ASSIGN
        ap-invl.i-no       = ap-inv.i-no
        ap-invl.company    = ap-inv.company
        ap-invl.vend-no    = ap-inv.vend-no
        ap-invl.line       = (IF AVAILABLE bf-ap-invl THEN bf-ap-invl.line ELSE 0) + 1
        ap-invl.actnum     = cAccount
        ap-invl.loc        = ap-inv.loc
        ap-invl.period     = ap-inv.period
        ap-invl.posted     = ap-inv.posted
        ap-invl.cons-uom   = "EA"
        ap-invl.pr-qty-uom = "EA"
        ap-invl.tax        = ap-inv.tax-gr NE ""
        .
    opriAPInvl = ROWID(ap-invl).
    RELEASE ap-invl.
    
END PROCEDURE.

PROCEDURE pCreateNewInvoiceLineAR:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AP invoice line, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriARInv AS ROWID.
    DEFINE OUTPUT PARAMETER opriARInvl AS ROWID.
    
    DEFINE VARIABLE iNextLineNumber AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    FIND ar-inv NO-LOCK 
        WHERE ROWID(ar-inv) EQ ipriARInv
        NO-ERROR.
    IF NOT AVAILABLE ar-inv THEN RETURN.    
    FIND LAST bf-ar-invl NO-LOCK  
        WHERE bf-ar-invl.x-no EQ ar-inv.x-no 
        USE-INDEX x-no 
        NO-ERROR.
    iNextLineNumber = IF AVAILABLE bf-ar-invl THEN bf-ar-invl.LINE + 1 ELSE 1.
    FIND FIRST ar-ctrl NO-LOCK 
        WHERE ar-ctrl.company EQ ar-inv.company 
        NO-ERROR.
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ar-inv.company 
        AND cust.cust-no EQ ar-inv.cust-no
        NO-ERROR.
    CREATE ar-invl.
    ASSIGN       
        ar-invl.x-no       = ar-inv.x-no
        ar-invl.company    = ar-inv.company
        ar-invl.cust-no    = ar-inv.cust-no
        ar-invl.inv-no     = ar-inv.inv-no
        ar-invl.LINE       = iNextLineNumber
        ar-invl.po-no      = ar-inv.po-no
        ar-invl.pr-qty-uom = "EA"
        ar-invl.cons-uom   = "EA"
        ar-invl.dscr[1]    = "EA"
        ar-invl.actnum     = IF AVAILABLE ar-ctrl THEN ar-ctrl.sales ELSE ""
        ar-invl.sman[1]    = IF AVAILABLE cust THEN cust.sman ELSE ""
        ar-invl.s-pct[1]   = IF ar-invl.sman[1] NE "" THEN 100 ELSE 0             
        .
    opriARInvl = ROWID(ar-invl).
    RELEASE ar-invl.
    
END PROCEDURE.


PROCEDURE pImportFileInvoicesAP:
    /*------------------------------------------------------------------------------
     Purpose: Reads the contents of an Excel File into a temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportedInvoicesAP.
    cFile = ipcImportFile.
    iCount = 0.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM sRow FROM VALUE(cFile).
        REPEAT:
            iCount = iCount + 1.
            CREATE ttImportedInvoicesAP.
            IMPORT STREAM sRow DELIMITER ","
                ttImportedInvoicesAP.cInvNo
                ttImportedInvoicesAP.cVendNo
                ttImportedInvoicesAP.cInvDate
                ttImportedInvoicesAP.cLineAmount               
                ttImportedInvoicesAP.cDueDate
                ttImportedInvoicesAP.cLineTax
                ttImportedInvoicesAP.cLineAccount
                ttImportedInvoicesAP.cTaxCode
                ttImportedInvoicesAP.cDiscount
                ttImportedInvoicesAP.cDays
                ttImportedInvoicesAP.cLinePONumber
                ttImportedInvoicesAP.cLinePOLine
                ttImportedInvoicesAP.cLineQuantity
                ttImportedInvoicesAP.cLineQuantityUom
                ttImportedInvoicesAP.cLinePrice
                ttImportedInvoicesAP.cLinePriceUom
                . 
            ttImportedInvoicesAP.iCount = iCount.
        END.
        OUTPUT STREAM sRow CLOSE.
    END.
    FOR EACH ttImportedInvoicesAP:
        IF TRIM(ttImportedInvoicesAP.cInvNo) EQ "" AND ttImportedInvoicesAP.iCount > 1  THEN
            DELETE ttImportedInvoicesAP.
    END.

END PROCEDURE.

PROCEDURE pImportFileInvoicesAR:
    /*------------------------------------------------------------------------------
     Purpose: Reads the contents of an Excel File into a temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportedInvoicesAR.
    cFile = ipcImportFile.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM sRow FROM VALUE(cFile).
        REPEAT:
            iCount = iCount + 1.
            CREATE ttImportedInvoicesAR.
            IMPORT STREAM sRow DELIMITER ","
                ttImportedInvoicesAR.cInvNo
                ttImportedInvoicesAR.cCustNo                  
                ttImportedInvoicesAR.cInvDate                 
                ttImportedInvoicesAR.cLineAmount
                ttImportedInvoicesAR.cLineTax
                ttImportedInvoicesAR.cLineAccount              
                ttImportedInvoicesAR.cLine                    
                ttImportedInvoicesAR.cShipTo                  
                ttImportedInvoicesAR.cPONum                   
                ttImportedInvoicesAR.cDueDate                 
                ttImportedInvoicesAR.cTaxCode                 
                ttImportedInvoicesAR.cTermsCode               
                ttImportedInvoicesAR.cDiscount                
                ttImportedInvoicesAR.cDiscountDays            
                ttImportedInvoicesAR.cCarrier                 
                ttImportedInvoicesAR.cFreight                 
                ttImportedInvoicesAR.cLineItemNo              
                ttImportedInvoicesAR.cLineItemName            
                ttImportedInvoicesAR.cLineItemDescription     
                ttImportedInvoicesAR.cLineCustomerLotNo       
                ttImportedInvoicesAR.cLineQuantity            
                ttImportedInvoicesAR.cLineQuantityUom         
                ttImportedInvoicesAR.cLinePrice               
                ttImportedInvoicesAR.cLinePriceUom            
                ttImportedInvoicesAR.cLineDiscount            
                ttImportedInvoicesAR.cLineCost                
                ttImportedInvoicesAR.cLineCostUom             
                ttImportedInvoicesAR.cLineSalesman1           
                ttImportedInvoicesAR.cLineSalesman1Percent    
                ttImportedInvoicesAR.cLineSalesman1Commission
                ttImportedInvoicesAR.cLineSalesman2           
                ttImportedInvoicesAR.cLineSalesman2Percent    
                ttImportedInvoicesAR.cLineSalesman2Commission
                ttImportedInvoicesAR.cLineSalesman3           
                ttImportedInvoicesAR.cLineSalesman3Percent    
                ttImportedInvoicesAR.cLineSalesman3Commission
                .
            ttImportedInvoicesAR.iCount = iCount. 
        END.
        OUTPUT STREAM sRow CLOSE.
    END.
    FOR EACH ttImportedInvoicesAR:
        IF TRIM(ttImportedInvoicesAR.cInvNo) EQ "" AND (ttImportedInvoicesAR.iCount > 1 or ttImportedInvoicesAR.iCount = 0) THEN
            DELETE ttImportedInvoicesAR.
    END.

END PROCEDURE.


PROCEDURE pImportFileAccount:
    /*------------------------------------------------------------------------------
     Purpose: Reads the contents of an Excel File into a temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportedAccounts.
    cFile = ipcImportFile.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM sRow FROM VALUE(cFile).
        REPEAT:
            iCount = iCount + 1.
            CREATE ttImportedAccounts.
            IMPORT STREAM sRow DELIMITER ","
                ttImportedAccounts.cAccount
                ttImportedAccounts.cAccountDesc
                ttImportedAccounts.cAccountType
                . 
            ttImportedAccounts.iCount = iCount.
        END.
        OUTPUT STREAM sRow CLOSE.
    END.
    FOR EACH ttImportedAccounts:
        IF TRIM(ttImportedAccounts.cAccount) EQ "" AND ttImportedAccounts.iCount > 1 THEN
            DELETE ttImportedAccounts.
    END.

END PROCEDURE.

PROCEDURE pImportFileFG:
    /*------------------------------------------------------------------------------
     Purpose: Reads the contents of an Excel File into a temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportedFGItems.
    cFile = ipcImportFile.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM sRow FROM VALUE(cFile).
        REPEAT:
            iCount = iCount + 1.
            CREATE ttImportedFGItems.
            IMPORT STREAM sRow DELIMITER ","
                ttImportedFGItems.cINo
                ttImportedFGItems.cCustPart
                ttImportedFGItems.cAvgCost       
                ttImportedFGItems.cLastCost      
                ttImportedFGItems.cStdMatl       
                ttImportedFGItems.cTotalCost     
                ttImportedFGItems.cCostUom       
                ttImportedFGItems.cCustId        
                ttImportedFGItems.cCategory      
                ttImportedFGItems.cWhs           
                ttImportedFGItems.cBin           
                ttImportedFGItems.cPurMan        
                ttImportedFGItems.cCasePal 
                ttImportedFGItems.cStockCust      
                ttImportedFGItems.cStyle         
                ttImportedFGItems.cName         
                ttImportedFGItems.cDesc1        
                ttImportedFGItems.cDesc2        
                ttImportedFGItems.cDesc3        
                ttImportedFGItems.cL             
                ttImportedFGItems.cW             
                ttImportedFGItems.cD             
                ttImportedFGItems.cSqFt          
                ttImportedFGItems.cCount         
                ttImportedFGItems.cUnPal         
                ttImportedFGItems.cBlankW        
                ttImportedFGItems.cBlankL        
                ttImportedFGItems.cNoteEst       
                ttImportedFGItems.cNotePick      
                . 
            ttImportedFGItems.iCount = iCount.
        END.
        OUTPUT STREAM sRow CLOSE.
    END.
    FOR EACH ttImportedFGItems:
        IF TRIM(ttImportedFGItems.cINo) EQ "" AND ttImportedFGItems.iCount > 1 THEN
            DELETE ttImportedFGItems.
    END.

END PROCEDURE.


PROCEDURE pImportFileVend:
    /*------------------------------------------------------------------------------
     Purpose: Reads the contents of an Excel File into a temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportedVendors.
    cFile = ipcImportFile.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM sRow FROM VALUE(cFile).
        REPEAT:
            iCount = iCount + 1.
            CREATE ttImportedVendors.
            IMPORT STREAM sRow DELIMITER ","
                ttImportedVendors.cVendNo
                ttImportedVendors.cVendName
                ttImportedVendors.cVendAdd1
                ttImportedVendors.cVendAdd2
                ttImportedVendors.cVendCity
                ttImportedVendors.cVendState
                ttImportedVendors.cVendPostal
                ttImportedVendors.cVendZip
                ttImportedVendors.cTerms
                ttImportedVendors.cGL
                ttImportedVendors.cVendRemit
                ttImportedVendors.cVendRAdd1
                ttImportedVendors.cVendRAdd2
                ttImportedVendors.cVendRCity
                ttImportedVendors.cVendRState
                ttImportedVendors.cVendRPostal
                ttImportedVendors.cVendRZip
                ttImportedVendors.cVendCountry
                ttImportedVendors.cVendAreaCode
                ttImportedVendors.cVendPhone
                ttImportedVendors.cVendFaxAreaCode
                ttImportedVendors.cVendFax
                ttImportedVendors.c1099
                ttImportedVendors.cFedID
                ttImportedVendors.cType
                ttImportedVendors.cTaxGroup
                ttImportedVendors.cCarrier
                .
            ttImportedVendors.iCount = iCount.
        END.
        OUTPUT STREAM sRow CLOSE.
    END.
    FOR EACH ttImportedVendors:
        IF TRIM(ttImportedVendors.cVendNo) EQ "" AND ttImportedVendors.iCount > 1 THEN
            DELETE ttImportedVendors.
    END.

END PROCEDURE.

PROCEDURE pImportFileCust:
    /*------------------------------------------------------------------------------
     Purpose: Reads the contents of an Excel File into a temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO. 
    
    EMPTY TEMP-TABLE ttImportedCustomers.
    cFile = ipcImportFile.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM sRow FROM VALUE(cFile).
        REPEAT:
            iCount = iCount + 1.
            CREATE ttImportedCustomers.
            IMPORT STREAM sRow DELIMITER ","
                ttImportedCustomers.cCustNo
                ttImportedCustomers.cCustName
                ttImportedCustomers.cCustAdd1
                ttImportedCustomers.cCustAdd2
                ttImportedCustomers.cCustCity
                ttImportedCustomers.cCustState
                ttImportedCustomers.cCustZip
                ttImportedCustomers.cCustCountry
                ttImportedCustomers.cCustSman
                ttImportedCustomers.cContact
                ttImportedCustomers.cDateAdded
                ttImportedCustomers.cCustAreaCode
                ttImportedCustomers.cCustPhone
                ttImportedCustomers.cCustFax
                ttImportedCustomers.cCreditLimit
                ttImportedCustomers.cStatus
                ttImportedCustomers.cCreditHold
                ttImportedCustomers.cCustType
                ttImportedCustomers.cTerms
                ttImportedCustomers.cFedID
                ttImportedCustomers.cNote1
                ttImportedCustomers.cNote2
                ttImportedCustomers.cNote3
                ttImportedCustomers.cNote4
                ttImportedCustomers.cShipName
                ttImportedCustomers.cShipAdd1
                ttImportedCustomers.cShipAdd2
                ttImportedCustomers.cShipCity
                ttImportedCustomers.cShipState
                ttImportedCustomers.cShipZip
                
                . 
            ttImportedCustomers.iCount = iCount.
        END.
        OUTPUT STREAM sRow CLOSE.
    END.
    FOR EACH ttImportedCustomers:
        IF TRIM(ttImportedCustomers.cCustNo) EQ "" AND ttImportedCustomers.iCount > 1 THEN
            DELETE ttImportedCustomers.
    END.

END PROCEDURE.

PROCEDURE pImportFileCustUpd:
    /*------------------------------------------------------------------------------
     Purpose: Reads the contents of an Excel File into a temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustUpdate.
    cFile = ipcImportFile.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM sRow FROM VALUE(cFile).
        REPEAT:
            iCount = iCount + 1.
            CREATE ttCustUpdate.
            IMPORT STREAM sRow DELIMITER ","
                ttCustUpdate.cCustNo
                ttCustUpdate.cField
                . 
            ttCustUpdate.iCount = iCount.
        END.
        OUTPUT STREAM sRow CLOSE.
    END.
    FOR EACH ttCustUpdate:
        IF TRIM(ttCustUpdate.cCustNo) EQ "" AND ttCustUpdate.iCount > 1 THEN
            DELETE ttCustUpdate.
    END.

END PROCEDURE.

PROCEDURE pProcessChangesAccount:
    /*------------------------------------------------------------------------------
     Purpose:  Processes completely valid temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttImportedAccounts NO-LOCK
        WHERE ttImportedAccounts.lValid EQ YES 
        :
        FIND FIRST account NO-LOCK
            WHERE account.company EQ ipcCompany
            AND account.actnum EQ ttImportedAccounts.cAccount
            NO-ERROR.
        IF NOT AVAILABLE account THEN 
        DO:
                       
            IF NOT iplLogOnly THEN 
            DO:

                CREATE account.
                ASSIGN
                    account.company = ipcCompany
                    account.actnum  = ttImportedAccounts.cAccount
                    account.dscr    = ttImportedAccounts.cAccountDesc
                    account.type    = ttImportedAccounts.cAccountType
                    .
                    
            END.
            EXPORT STREAM sLog DELIMITER "," 
                "Added Account"
                ttImportedAccounts.cAccount
                ttImportedAccounts.cAccountDesc
                ttImportedAccounts.cAccountType
                .                    
        END.
         
    END.
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.
PROCEDURE pProcessChangesInvociesAR:
    /*------------------------------------------------------------------------------
     Purpose:  Processes completely valid temp table
     Notes:
    ------------------------------------------------------------------------------*/
   
    DEFINE VARIABLE riARInv  AS ROWID .
    DEFINE VARIABLE riARInvl AS ROWID.
    DEFINE BUFFER bf-ar-inv FOR ar-inv.
    
    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttImportedInvoicesAR NO-LOCK:
        IF ttImportedInvoicesAR.lValid EQ YES THEN 
        DO:
            IF NOT iplLogOnly THEN 
            DO:
                FIND FIRST ar-inv NO-LOCK
                    WHERE ar-inv.company EQ ipcCompany
                    AND ar-inv.inv-no EQ INT(ttImportedInvoicesAR.cInvNo)
                    NO-ERROR.
                IF NOT AVAILABLE ar-inv THEN 
                DO:
                    RUN pCreateNewInvoiceAR (ipcCompany,ttImportedInvoicesAR.cCustNo, ttImportedInvoicesAR.cInvDate, OUTPUT riARInv).
                    FIND ar-inv EXCLUSIVE-LOCK 
                        WHERE ROWID(ar-inv) EQ riARInv
                        NO-ERROR.
                    IF NOT AVAILABLE ar-inv THEN NEXT.
                    /*Override defaults with values from import*/
                    IF ttImportedInvoicesAR.cInvNo NE "" THEN 
                        ar-inv.inv-no = INTEGER(ttImportedInvoicesAR.cInvNo).
                    IF ttImportedInvoicesAR.cInvDate NE "" THEN 
                        ar-inv.inv-date = DATE(ttImportedInvoicesAR.cInvDate).
                    IF ttImportedInvoicesAR.cDueDate NE "" THEN 
                        ar-inv.due-date = DATE(ttImportedInvoicesAR.cDueDate).
                    IF ttImportedInvoicesAR.cShipto NE "" THEN 
                        ar-inv.ship-id = ttImportedInvoicesAR.cShipTo.
                    IF ttImportedInvoicesAR.cPONum NE "" THEN 
                        ar-inv.po-no = ttImportedInvoicesAR.cPONum.
                    IF ttImportedInvoicesAR.cTaxCode NE "" THEN
                    DO:
                        FIND FIRST stax NO-LOCK 
                            WHERE stax.company EQ ipcCompany
                            AND stax.tax-group EQ ttImportedInvoicesAR.cTaxCode
                            NO-ERROR.
                        IF AVAILABLE stax THEN 
                            ar-inv.tax-code = ttImportedInvoicesAR.cTaxCode.
                    END.
                    IF ttImportedInvoicesAR.cTermsCode NE "" THEN 
                        ar-inv.terms = ttImportedInvoicesAR.cTermsCode.
                    IF ttImportedInvoicesAR.cDiscount NE "" THEN 
                        ar-inv.disc-% = DECIMAL(ttImportedInvoicesAR.cDiscount).               
                    IF ttImportedInvoicesAR.cDiscountDays NE "" THEN 
                        ar-inv.disc-days = DECIMAL(ttImportedInvoicesAR.cDiscountDays).
                    IF ttImportedInvoicesAR.cCarrier NE "" THEN 
                        ar-inv.carrier = ttImportedInvoicesAR.cCarrier.
                    IF ttImportedInvoicesAR.cFreight NE "" THEN 
                        ar-inv.freight = DECIMAL(ttImportedInvoicesAR.cFreight).                                     
                END. /*Not avail ar-inv*/
                RUN pCreateNewInvoiceLineAR (ROWID(ar-inv), OUTPUT riARInvl).
                FIND ar-invl EXCLUSIVE-LOCK 
                    WHERE ROWID(ar-invl) EQ riARInvl
                    NO-ERROR.
                IF NOT AVAILABLE ar-invl THEN NEXT.
                /*Override defaults with values from import*/
                ASSIGN 
                    ar-invl.tax = ttImportedInvoicesAR.cLineTax EQ 'Y'
                    ar-invl.amt = DECIMAL(ttImportedInvoicesAR.cLineAmount)
                    .
                IF TRIM(ttImportedInvoicesAR.cLineQuantity) NE "" THEN 
                    ar-invl.qty = DECIMAL(ttImportedInvoicesAR.cLineQuantity).
                ELSE 
                    ar-invl.qty = 1.
                ar-invl.inv-qty = ar-invl.qty.
                IF ttImportedInvoicesAR.cLineQuantityUom NE "" THEN 
                    ar-invl.cons-uom = ttImportedInvoicesAR.cLineQuantityUOM.
                ELSE 
                    ar-invl.cons-uom = "EA".
                IF ttImportedInvoicesAR.cLinePrice NE "" THEN 
                    ar-invl.unit-pr = DECIMAL(ttImportedInvoicesAR.cLinePrice).
                ELSE 
                    ar-invl.unit-pr = ar-invl.amt.
                IF ttImportedInvoicesAR.cLinePriceUom NE "" THEN 
                    ar-invl.pr-qty-uom = ttImportedInvoicesAR.cLinePriceUom.
                ELSE 
                    ar-invl.pr-qty-uom = "EA".
                IF ttImportedInvoicesAR.cLineAccount NE "" THEN 
                    ar-invl.actnum = ttImportedInvoicesAR.cLineAccount.
                IF ttImportedInvoicesAR.cLine NE "" THEN 
                    ar-invl.line = INTEGER(ttImportedInvoicesAR.cLine).
                IF ttImportedInvoicesAR.cLineItemNo NE "" THEN 
                    ar-invl.i-no = ttImportedInvoicesAR.cLineItemNo.
                IF ttImportedInvoicesAR.cLineItemName NE "" THEN 
                    ar-invl.i-name = ttImportedInvoicesAR.cLineItemName.
                IF ttImportedInvoicesAR.cLineItemDescription NE "" THEN                                 
                    ar-invl.i-dscr = ttImportedInvoicesAR.cLineItemDescription.     
                IF ttImportedInvoicesAR.cLineCustomerLotNo NE "" THEN 
                    ar-invl.lot-no = ttImportedInvoicesAR.cLineCustomerLotNo.
                IF ttImportedInvoicesAR.cLineDiscount NE "" THEN                            
                    ar-invl.disc = DECIMAL(ttImportedInvoicesAR.cLineDiscount).            
                IF ttImportedInvoicesAR.cLineCost NE "" THEN 
                    ar-invl.cost = DECIMAL(ttImportedInvoicesAR.cLineCost).                
                IF ttImportedInvoicesAR.cLineCostUom NE "" THEN 
                    ar-invl.cons-uom = ttImportedInvoicesAR.cLineCostUom.             
                IF ttImportedInvoicesAR.cLineSalesman1 NE "" THEN 
                    ar-invl.sman[1] = ttImportedInvoicesAR.cLineSalesman1.
                IF ttImportedInvoicesAR.cLineSalesman1Percent NE "" THEN 
                    ar-invl.s-pct[1] = DECIMAL(ttImportedInvoicesAR.cLineSalesman1Percent).
                IF ttImportedInvoicesAR.cLineSalesman1Commission NE "" THEN 
                    ar-invl.s-comm[1] = DECIMAL(ttImportedInvoicesAR.cLineSalesman1Commission).
                IF ttImportedInvoicesAR.cLineSalesman2 NE "" THEN 
                    ar-invl.sman[2] = ttImportedInvoicesAR.cLineSalesman2.
                IF ttImportedInvoicesAR.cLineSalesman2Percent NE "" THEN 
                    ar-invl.s-pct[2] = DECIMAL(ttImportedInvoicesAR.cLineSalesman2Percent).
                IF ttImportedInvoicesAR.cLineSalesman2Commission NE "" THEN 
                    ar-invl.s-comm[3] = DECIMAL(ttImportedInvoicesAR.cLineSalesman3Commission).
                IF ttImportedInvoicesAR.cLineSalesman3 NE "" THEN 
                    ar-invl.sman[3] = ttImportedInvoicesAR.cLineSalesman3.
                IF ttImportedInvoicesAR.cLineSalesman3Percent NE "" THEN 
                    ar-invl.s-pct[3] = DECIMAL(ttImportedInvoicesAR.cLineSalesman3Percent).
                IF ttImportedInvoicesAR.cLineSalesman3Commission NE "" THEN 
                    ar-invl.s-comm[3] = DECIMAL(ttImportedInvoicesAR.cLineSalesman3Commission).
                                
                RUN pRecalculateARInvoiceHeader(ROWID(ar-inv), ar-invl.amt).
            /*Override defaults with imported values for line*/                                     
            END. /*not iplLogOnly*/
            EXPORT STREAM sLog DELIMITER "," 
                "Added AR Invoice"
                ttImportedInvoicesAR.cInvNo
                ttImportedInvoicesAR.cCustNo                  
                ttImportedInvoicesAR.cInvDate                 
                ttImportedInvoicesAR.cLineAmount              
                ttImportedInvoicesAR.cLine                    
                ttImportedInvoicesAR.cShipTo                  
                ttImportedInvoicesAR.cPONum                   
                ttImportedInvoicesAR.cDueDate                 
                ttImportedInvoicesAR.cTaxCode                 
                ttImportedInvoicesAR.cTermsCode               
                ttImportedInvoicesAR.cDiscount                
                ttImportedInvoicesAR.cDiscountDays            
                ttImportedInvoicesAR.cCarrier                 
                ttImportedInvoicesAR.cFreight                 
                ttImportedInvoicesAR.cLineAccount             
                ttImportedInvoicesAR.cLineItemNo              
                ttImportedInvoicesAR.cLineItemName            
                ttImportedInvoicesAR.cLineItemDescription     
                ttImportedInvoicesAR.cLineCustomerLotNo       
                ttImportedInvoicesAR.cLineQuantity            
                ttImportedInvoicesAR.cLineQuantityUom         
                ttImportedInvoicesAR.cLinePrice               
                ttImportedInvoicesAR.cLinePriceUom            
                ttImportedInvoicesAR.cLineDiscount            
                ttImportedInvoicesAR.cLineCost                
                ttImportedInvoicesAR.cLineCostUom             
                ttImportedInvoicesAR.cLineSalesman1           
                ttImportedInvoicesAR.cLineSalesman1Percent    
                ttImportedInvoicesAR.cLineSalesman1Commission
                ttImportedInvoicesAR.cLineSalesman2           
                ttImportedInvoicesAR.cLineSalesman2Percent    
                ttImportedInvoicesAR.cLineSalesman2Commission
                ttImportedInvoicesAR.cLineSalesman3           
                ttImportedInvoicesAR.cLineSalesman3Percent    
                ttImportedInvoicesAR.cLineSalesman3Commission
                .
        END. /*Valid Temp Table*/
        ELSE
            EXPORT STREAM sLog DELIMITER "," 
                ttImportedInvoicesAR.cInvalidReason
                ttImportedInvoicesAR.cInvNo
                ttImportedInvoicesAR.cCustNo                  
                ttImportedInvoicesAR.cInvDate                 
                ttImportedInvoicesAR.cLineAmount              
                ttImportedInvoicesAR.cLine                    
                ttImportedInvoicesAR.cShipTo                  
                ttImportedInvoicesAR.cPONum                   
                ttImportedInvoicesAR.cDueDate                 
                ttImportedInvoicesAR.cTaxCode                 
                ttImportedInvoicesAR.cTermsCode               
                ttImportedInvoicesAR.cDiscount                
                ttImportedInvoicesAR.cDiscountDays            
                ttImportedInvoicesAR.cCarrier                 
                ttImportedInvoicesAR.cFreight                 
                ttImportedInvoicesAR.cLineAccount             
                ttImportedInvoicesAR.cLineItemNo              
                ttImportedInvoicesAR.cLineItemName            
                ttImportedInvoicesAR.cLineItemDescription     
                ttImportedInvoicesAR.cLineCustomerLotNo       
                ttImportedInvoicesAR.cLineQuantity            
                ttImportedInvoicesAR.cLineQuantityUom         
                ttImportedInvoicesAR.cLinePrice               
                ttImportedInvoicesAR.cLinePriceUom            
                ttImportedInvoicesAR.cLineDiscount            
                ttImportedInvoicesAR.cLineCost                
                ttImportedInvoicesAR.cLineCostUom             
                ttImportedInvoicesAR.cLineSalesman1           
                ttImportedInvoicesAR.cLineSalesman1Percent    
                ttImportedInvoicesAR.cLineSalesman1Commission
                ttImportedInvoicesAR.cLineSalesman2           
                ttImportedInvoicesAR.cLineSalesman2Percent    
                ttImportedInvoicesAR.cLineSalesman2Commission
                ttImportedInvoicesAR.cLineSalesman3           
                ttImportedInvoicesAR.cLineSalesman3Percent    
                ttImportedInvoicesAR.cLineSalesman3Commission
                .                    
         
    END. /*each tt*/
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.

PROCEDURE pProcessChangesInvociesAP:
    /*------------------------------------------------------------------------------
     Purpose:  Processes completely valid temp table
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE riAPInv  AS ROWID. 
    DEFINE VARIABLE riAPInvl AS ROWID. 
    
    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttImportedInvoicesAP NO-LOCK:
        IF ttImportedInvoicesAP.lValid EQ YES THEN 
        DO:
            IF NOT iplLogOnly THEN 
            DO:
                /*if found, add another line to existing header - otherwise, create a new header*/
                FIND FIRST ap-inv NO-LOCK
                    WHERE ap-inv.company EQ ipcCompany
                    AND ap-inv.inv-no EQ ttImportedInvoicesAP.cInvNo
                    AND ap-inv.vend-no EQ ttImportedInvoicesAP.cVendNo
                    NO-ERROR.
                IF NOT AVAILABLE ap-inv THEN /*create a new one*/
                DO:
                    RUN pCreateNewInvoiceAP (ipcCompany, ttImportedInvoicesAP.cVendNo, ttimportedInvoicesAP.cInvNo, ttImportedInvoicesAP.cInvDate, OUTPUT riApInv).
                    FIND ap-inv EXCLUSIVE-LOCK
                        WHERE ROWID(ap-inv) EQ riAPInv
                        NO-ERROR.
                    IF NOT AVAILABLE ap-inv THEN NEXT.    
                    
                    /*Override defaults with imported values for header*/
                    IF ttimportedInvoicesAP.cInvDate NE "" THEN 
                        ap-inv.inv-date =  DATE(ttImportedInvoicesAP.cInvDate).
                    IF ttImportedInvoicesAP.cTaxCode NE "" THEN  
                    DO:
                        FIND FIRST stax NO-LOCK 
                            WHERE stax.company EQ ipcCompany
                            AND stax.tax-group EQ ttImportedInvoicesAP.cTaxCode
                            NO-ERROR.
                        IF AVAILABLE stax THEN 
                            ap-inv.tax-gr = ttImportedInvoicesAP.cTaxCode.
                    END.
                    IF ttImportedInvoicesAP.cDueDate NE "" THEN 
                        ap-inv.due-date = DATE(ttImportedInvoicesAP.cDueDate).
                    IF ttImportedInvoicesAP.cDays NE "" THEN 
                        ap-inv.disc-days = INTEGER(ttImportedInvoicesAP.cDays).
                    IF ttImportedInvoicesAP.cDiscount NE "" THEN 
                        ap-inv.disc-% = DECIMAL(ttImportedInvoicesAP.cDiscount).
    
                END. /*not available ap-inv*/
                RUN pCreateNewInvoiceLineAP (ROWID(ap-inv), OUTPUT riAPInvl).
                FIND ap-invl EXCLUSIVE-LOCK 
                    WHERE ROWID(ap-invl) EQ riAPInvl
                    NO-ERROR.
                IF NOT AVAILABLE ap-invl THEN NEXT.
                
                /*Override defaults with imported values for line*/ 
                ASSIGN 
                    ap-invl.tax = ttImportedInvoicesAP.cLineTax EQ 'Y'
                    ap-invl.amt = DECIMAL(ttImportedInvoicesAP.cLineAmount)
                    .
                IF ttImportedInvoicesAP.cLineQuantity NE "" THEN 
                    ap-invl.qty = DECIMAL(ttImportedInvoicesAP.cLineQuantity).
                ELSE 
                    ap-invl.qty = 1.
                IF ttImportedInvoicesAP.cLineQuantityUom NE "" THEN 
                    ap-invl.cons-uom = ttImportedInvoicesAP.cLineQuantityUOM.
                ELSE 
                    ap-invl.cons-uom = "EA".
                IF ttImportedInvoicesAP.cLinePrice NE "" THEN 
                    ap-invl.unit-pr = DECIMAL(ttImportedInvoicesAP.cLinePrice).
                ELSE 
                    ap-invl.unit-pr = ap-invl.amt.
                IF ttImportedInvoicesAP.cLinePriceUom NE "" THEN 
                    ap-invl.pr-qty-uom = ttImportedInvoicesAP.cLinePriceUom.
                ELSE 
                    ap-invl.pr-qty-uom = "EA".
                IF ttImportedInvoicesAP.cLinePONumber EQ "" THEN
                    ap-invl.po-no = INTEGER(ttImportedInvoicesAP.cLinePONumber).
                IF ttImportedInvoicesAP.cLinePOline EQ "" THEN
                    ap-invl.po-line = INTEGER(ttImportedInvoicesAP.cLinePONumber).
                IF ttImportedInvoicesAP.cLineAccount NE "" THEN 
                    ap-invl.actnum = ttImportedInvoicesAP.cLineAccount.
                                                            
                RUN pRecalculateAPInvoiceHeader (ROWID(ap-inv), NO).      
               
            END. /*Not iplLogOnly*/
            EXPORT STREAM sLog DELIMITER "," 
                "Added AP Invoice"
                ttImportedInvoicesAP.cInvNo
                ttImportedInvoicesAP.cVendNo
                ttImportedInvoicesAP.cInvDate
                ttImportedInvoicesAP.cLineAmount               
                ttImportedInvoicesAP.cDueDate
                ttImportedInvoicesAP.cLineTax
                ttImportedInvoicesAP.cLineAccount
                ttImportedInvoicesAP.cTaxCode
                ttImportedInvoicesAP.cDiscount
                ttImportedInvoicesAP.cDays
                ttImportedInvoicesAP.cLinePONumber
                ttImportedInvoicesAP.cLinePOLine
                ttImportedInvoicesAP.cLineQuantity
                ttImportedInvoicesAP.cLineQuantityUom
                ttImportedInvoicesAP.cLinePrice
                ttImportedInvoicesAP.cLinePriceUom
                .        
                            
        END. /*Valid Record*/
        ELSE
            EXPORT STREAM sLog DELIMITER "," 
                ttImportedInvoicesAP.cInvalidReason
                ttImportedInvoicesAP.cInvNo
                ttImportedInvoicesAP.cVendNo
                ttImportedInvoicesAP.cInvDate
                ttImportedInvoicesAP.cLineAmount               
                ttImportedInvoicesAP.cDueDate
                ttImportedInvoicesAP.cLineTax
                ttImportedInvoicesAP.cLineAccount
                ttImportedInvoicesAP.cTaxCode
                ttImportedInvoicesAP.cDiscount
                ttImportedInvoicesAP.cDays
                ttImportedInvoicesAP.cLinePONumber
                ttImportedInvoicesAP.cLinePOLine
                ttImportedInvoicesAP.cLineQuantity
                ttImportedInvoicesAP.cLineQuantityUom
                ttImportedInvoicesAP.cLinePrice
                ttImportedInvoicesAP.cLinePriceUom
                .
     
    END. /*each tt*/
    
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.

PROCEDURE pProcessChangesFG:
    /*------------------------------------------------------------------------------
     Purpose:  Processes completely valid temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttImportedFGItems NO-LOCK
        WHERE ttImportedFGItems.lValid EQ YES 
        :
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ipcCompany
            AND itemfg.i-no EQ ttImportedFGItems.cINo
            NO-ERROR.
        IF NOT AVAILABLE itemfg THEN 
        DO:
                      
            IF NOT iplLogOnly THEN 
            DO:

                CREATE itemfg.
                ASSIGN
                    itemfg.company        = ipcCompany
                    itemfg.i-no           = ttImportedFGItems.cINo
                    itemfg.part-no        = ttImportedFGItems.cCustPart
                    itemfg.avg-cost       = DECIMAL(ttImportedFGItems.cAvgCost)       
                    itemfg.last-cost      = DECIMAL(ttImportedFGItems.cLastCost)      
                    itemfg.std-mat-cost   = DECIMAL(ttImportedFGItems.cStdMatl)       
                    itemfg.total-std-cost = DECIMAL(ttImportedFGItems.cTotalCost)     
                    itemfg.prod-uom       = ttImportedFGItems.cCostUom       
                    itemfg.cust-no        = ttImportedFGItems.cCustId        
                    itemfg.procat         = ttImportedFGItems.cCategory      
                    itemfg.loc            = ttImportedFGItems.cWhs           
                    itemfg.def-loc        = ttImportedFGItems.cWhs 
                    itemfg.def-loc-bin    = ttImportedFGItems.cBin           
                    itemfg.pur-man        = (ttImportedFGItems.cPurMan EQ "P")        
                    itemfg.ship-meth      = (ttImportedFGItems.cCasePal EQ "C") 
                    itemfg.i-code         = ttImportedFGItems.cStockCust      
                    itemfg.style          = ttImportedFGItems.cStyle         
                    itemfg.i-name         = ttImportedFGItems.cName         
                    itemfg.part-dscr1     = ttImportedFGItems.cDesc1        
                    itemfg.part-dscr2     = ttImportedFGItems.cDesc2        
                    itemfg.part-dscr3     = ttImportedFGItems.cDesc3        
                    itemfg.l-score        = DECIMAL(ttImportedFGItems.cL)             
                    itemfg.w-score        = DECIMAL(ttImportedFGItems.cW)             
                    itemfg.d-score        = DECIMAL(ttImportedFGItems.cD)             
                    itemfg.t-sqft         = DECIMAL(ttImportedFGItems.cSqFt)          
                    itemfg.t-sqin         = DECIMAL(ttImportedFGItems.cSqFt) * 144
                    itemfg.case-count     = INTEGER(ttImportedFGItems.cCount)         
                    itemfg.case-pall      = INTEGER(ttImportedFGItems.cUnPal)         
                    itemfg.t-wid          = DECIMAL(ttImportedFGItems.cBlankW)        
                    itemfg.t-len          = DECIMAL(ttImportedFGItems.cBlankL)        
                    .
                RUN pAddNote (itemfg.rec_key,
                    ttImportedFGItems.cNoteEst,
                    "Pkg Instruction",
                    "EST",
                    "S").

                RUN pAddNote (itemfg.rec_key,
                    ttImportedFGItems.cNotePick,
                    "Ship Instruction",
                    "PT",
                    "S").
            END.                    
        END.
        EXPORT STREAM sLog DELIMITER "," 
            "Added FG Item"
            ttImportedFGItems.cINo
            ttImportedFGItems.cCustPart
            ttImportedFGItems.cAvgCost       
            ttImportedFGItems.cLastCost      
            ttImportedFGItems.cStdMatl       
            ttImportedFGItems.cTotalCost     
            ttImportedFGItems.cCostUom       
            ttImportedFGItems.cCustId        
            ttImportedFGItems.cCategory      
            ttImportedFGItems.cWhs           
            ttImportedFGItems.cBin           
            ttImportedFGItems.cPurMan        
            ttImportedFGItems.cCasePal 
            ttImportedFGItems.cStockCust      
            ttImportedFGItems.cStyle         
            ttImportedFGItems.cName         
            ttImportedFGItems.cDesc1        
            ttImportedFGItems.cDesc2        
            ttImportedFGItems.cDesc3        
            ttImportedFGItems.cL             
            ttImportedFGItems.cW             
            ttImportedFGItems.cD             
            ttImportedFGItems.cSqFt          
            ttImportedFGItems.cCount         
            ttImportedFGItems.cUnPal         
            ttImportedFGItems.cBlankW        
            ttImportedFGItems.cBlankL        
            ttImportedFGItems.cNoteEst       
            ttImportedFGItems.cNotePick      
            .                    
    END.
        
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.

PROCEDURE pProcessChangesVend:
    /*------------------------------------------------------------------------------
     Purpose:  Processes completely valid temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttImportedVendors NO-LOCK
        WHERE ttImportedVendors.lValid EQ YES 
        :
        FIND FIRST vend NO-LOCK
            WHERE vend.company EQ ipcCompany
            AND vend.vend-no EQ ttImportedVendors.cVendNo
            NO-ERROR.
        IF NOT AVAILABLE vend THEN 
        DO:
                       
            IF NOT iplLogOnly THEN 
            DO:
                CREATE vend.
                ASSIGN
                    vend.company      = ipcCompany
                    vend.vend-no      = ttImportedVendors.cVendNo
                    vend.name         = ttImportedVendors.cVendName
                    vend.remit        = ttImportedVendors.cVendName
                    vend.add1         = ttImportedVendors.cVendAdd1
                    vend.r-add1       = ttImportedVendors.cVendRAdd1
                    vend.add2         = ttImportedVendors.cVendAdd2
                    vend.r-add2       = ttImportedVendors.cVendRAdd2
                    vend.city         = ttImportedVendors.cVendCity
                    vend.r-city       = ttImportedVendors.cVendRCity
                    vend.state        = ttImportedVendors.cVendState
                    vend.r-state      = ttImportedVendors.cVendRState
                    vend.country      = ttImportedVendors.cVendCountry
                    vend.r-country    = ttImportedVendors.cVendCountry
                    vend.postal       = ttImportedVendors.cVendPostal
                    vend.r-postal     = ttImportedVendors.cVendRPostal
                    vend.zip          = ttImportedVendors.cVendZip
                    vend.r-zip        = ttImportedVendors.cVendRZip
                    vend.area-code    = ttImportedVendors.cVendAreaCode
                    vend.phone        = REPLACE(ttImportedVendors.cVendPhone,"-","")
                    vend.fax-area     = ttImportedVendors.cVendFaxAreaCode
                    vend.fax          = REPLACE(ttImportedVendors.cVendFax,"-","")
                    vend.tax-gr       = ttImportedVendors.cTaxGroup
                    vend.terms        = ttImportedVendors.cTerms
                    vend.code-1099    = ttImportedVendors.c1099
                    vend.loc          = "MAIN"
                    vend.frt-pay      = "P"
                    vend.carrier      = ttImportedVendors.cCarrier
                    vend.actnum       = ttImportedVendors.cGL
                    vend.tax-id       = ttImportedVendors.cFedID
                    vend.payment-type = "Check"
                    .
                IF vend.country = "Canada" THEN 
                    vend.curr-code = "CAD".
                ELSE 
                    vend.curr-code = "USD".

            END.
            EXPORT STREAM sLog DELIMITER "," 
                "Added Vendor"
                ttImportedVendors.cVendNo
                ttImportedVendors.cVendName
                ttImportedVendors.cVendAdd1
                ttImportedVendors.cVendCity
                ttImportedVendors.cVendState
                ttImportedVendors.cVendCountry
                ttImportedVendors.cVendPostal
                ttImportedVendors.cVendZip
                ttImportedVendors.cVendAreaCode
                ttImportedVendors.cVendPhone
                ttImportedVendors.cVendFaxAreaCode
                ttImportedVendors.cVendFax
                .    
        END.
         
    END.
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.

PROCEDURE pProcessChangesCust:
    /*------------------------------------------------------------------------------
     Purpose:  Processes completely valid temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttImportedCustomers NO-LOCK
        WHERE ttImportedCustomers.lValid EQ YES 
        :
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipcCompany
            AND cust.cust-no EQ ttImportedCustomers.cCustNo
            NO-ERROR.
        IF NOT AVAILABLE cust THEN 
        DO:
                       
            IF NOT iplLogOnly THEN 
            DO:

                CREATE cust.
                ASSIGN
                    cust.company     = ipcCompany
                    cust.cust-no     = ttImportedCustomers.cCustNo
                    cust.name        = ttImportedCustomers.cCustName
                    cust.addr[1]     = ttImportedCustomers.cCustAdd1
                    cust.addr[2]     = ttImportedCustomers.cCustAdd2
                    cust.city        = ttImportedCustomers.cCustCity
                    cust.state       = ttImportedCustomers.cCustState
                    cust.fax-country = ttImportedCustomers.cCustCountry
                    cust.country     = ttImportedCustomers.cCustCountry
                    cust.zip         = ttImportedCustomers.cCustZip
                    cust.sman        = ttImportedCustomers.cCustSman
                    cust.area-code   = ttImportedCustomers.cCustAreaCode
                    cust.phone       = REPLACE(ttImportedCustomers.cCustPhone,"-","")
                    cust.fax         = REPLACE(ttImportedCustomers.cCustFax,"-","")
                    cust.cr-lim      = DECIMAL(ttImportedCustomers.cCreditLimit)
                    cust.active      = ttImportedCustomers.cStatus
                    cust.cr-hold     = (ttImportedCustomers.cCreditHold = "YES")
                    cust.type        = ttImportedCustomers.cCustType
                    cust.terms       = ttImportedCustomers.cTerms
                    cust.tax-id      = ttImportedCustomers.cFedID
                    cust.contact     = ttImportedCustomers.cContact
                    cust.date-field  = DATE(ttImportedCustomers.cDateAdded) 
                    .
                FIND FIRST shipto EXCLUSIVE-LOCK 
                    WHERE shipto.company EQ ipcCompany
                    AND shipto.cust-no EQ ttImportedCustomers.cCustNo
                    AND shipto.ship-id EQ ttImportedCustomers.cCustNo
                    NO-ERROR.
                IF NOT AVAILABLE shipto THEN 
                DO:
                    CREATE shipto.
                    ASSIGN 
                        shipto.company   = ipcCompany
                        shipto.cust-no   = ttImportedCustomers.cCustNo
                        shipto.ship-id   = ttImportedCustomers.cCustNo
                        shipto.ship-name = ttImportedCustomers.cShipName
                        .
                END.
                ASSIGN 
                    shipto.ship-addr[1] = ttImportedCustomers.cShipAdd1
                    shipto.ship-addr[2] = ttImportedCustomers.cShipAdd2
                    shipto.ship-city    = ttImportedCustomers.cShipCity
                    shipto.ship-state   = ttImportedCustomers.cShipState
                    shipto.ship-zip     = ttImportedCustomers.cShipZip
                    .
                FIND FIRST soldto EXCLUSIVE-LOCK 
                    WHERE soldto.company EQ ipcCompany
                    AND soldto.cust-no EQ ttImportedCustomers.cCustNo
                    AND soldto.sold-id EQ ttImportedCustomers.cCustNo
                    NO-ERROR.
                IF NOT AVAILABLE soldto THEN 
                DO:
                    CREATE soldto.
                    ASSIGN 
                        soldto.company   = ipcCompany
                        soldto.cust-no   = ttImportedCustomers.cCustNo
                        soldto.sold-id   = ttImportedCustomers.cCustNo
                        soldto.sold-name = ttImportedCustomers.cCustName
                        .
                END.
                ASSIGN 
                    soldto.sold-addr[1] = ttImportedCustomers.cCustAdd1
                    soldto.sold-addr[2] = ttImportedCustomers.cCustAdd2
                    soldto.sold-city    = ttImportedCustomers.cCustCity
                    soldto.sold-state   = ttImportedCustomers.cCustState
                    soldto.sold-zip     = ttImportedCustomers.cCustZip
                    .
                RUN pAddNote (cust.rec_key,
                    ttImportedCustomers.cNote1,
                    "Misc Message 1",
                    "",
                    "C").
                RUN pAddNote (cust.rec_key,
                    ttImportedCustomers.cNote2,
                    "Misc Message 2",
                    "",
                    "C").
                RUN pAddNote (cust.rec_key,
                    ttImportedCustomers.cNote3,
                    "Mfg. Inst.",
                    "",
                    "C").
                RUN pAddNote (cust.rec_key,
                    ttImportedCustomers.cNote4,
                    "B/L Message",
                    "",
                    "C").  
            END.
            EXPORT STREAM sLog DELIMITER "," 
                "Added Customer"
                ttImportedCustomers.cCustNo
                ttImportedCustomers.cCustName
                ttImportedCustomers.cCustAdd1
                ttImportedCustomers.cCustAdd2
                ttImportedCustomers.cCustCity
                ttImportedCustomers.cCustState
                ttImportedCustomers.cCustCountry
                ttImportedCustomers.cCustZip
                ttImportedCustomers.cCustSman
                ttImportedCustomers.cCustAreaCode
                ttImportedCustomers.cCustPhone
                ttImportedCustomers.cCustFax
                ttImportedCustomers.cCreditLimit
                ttImportedCustomers.cStatus
                ttImportedCustomers.cCreditHold
                ttImportedCustomers.cCustType
                ttImportedCustomers.cTerms
                ttImportedCustomers.cFedID
                ttImportedCustomers.cNote1
                ttImportedCustomers.cNote2
                ttImportedCustomers.cShipAdd1
                ttImportedCustomers.cShipAdd2
                ttImportedCustomers.cShipCity
                ttImportedCustomers.cShipState
                ttImportedCustomers.cShipZip

                .    
        END.
         
    END.
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.

PROCEDURE pProcessChangesCustUpd:
    /*------------------------------------------------------------------------------
     Purpose:  Processes completely valid temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttCustUpdate NO-LOCK
        WHERE ttCustUpdate.lValid EQ YES 
        :
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipcCompany
            AND cust.cust-no EQ ttCustUpdate.cCustNo
            NO-ERROR.
        IF AVAILABLE cust THEN 
        DO:           
            IF NOT iplLogOnly THEN 
            DO:
                CASE cField:
                    WHEN "Note" THEN 
                        DO:
                            RUN pAddNote (cust.rec_key,
                                ttCustUpdate.cField,
                                "OldID",
                                "",
                                "C").  
                        END.
                END CASE.            
            END.
            EXPORT STREAM sLog DELIMITER "," 
                "Updated Customer"
                ttCustUpdate.cCustNo
                ttCustUpdate.cField
                .    
        END.
         
    END.
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.

PROCEDURE pRecalculateAPInvoiceHeader:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriAPInv AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplOverwriteTax AS LOGICAL NO-UNDO.

    DEFINE VARIABLE dFreight        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxRate        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxRateFreight AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-ap-inv  FOR ap-inv.
    DEFINE BUFFER bf-ap-invl FOR ap-invl.


    FIND bf-ap-inv EXCLUSIVE-LOCK  
        WHERE ROWID(bf-ap-inv) EQ ipriAPInv NO-ERROR.

    IF AVAILABLE bf-ap-inv THEN 
    DO:

        IF NOT iplOverwriteTax THEN
            bf-ap-inv.tax-amt = 0.

        ASSIGN
            bf-ap-inv.net     = 0
            bf-ap-inv.freight = 0.

        IF bf-ap-inv.tax-gr NE "" THEN
            RUN ar/cctaxrt.p (bf-ap-inv.company, bf-ap-inv.tax-gr,
                OUTPUT dTaxRate, OUTPUT dTaxRateFreight).

        FOR EACH bf-ap-invl WHERE bf-ap-invl.i-no EQ bf-ap-inv.i-no NO-LOCK:
            bf-ap-inv.net = bf-ap-inv.net + bf-ap-invl.amt.

            IF bf-ap-invl.tax AND NOT iplOverwriteTax THEN
                bf-ap-inv.tax-amt = bf-ap-inv.tax-amt +
                    ROUND((bf-ap-invl.amt * dTaxRate / 100),2).

            IF bf-ap-invl.po-no NE 0 THEN 
            DO:
                FIND FIRST po-ordl NO-LOCK 
                    WHERE po-ordl.company EQ  bf-ap-invl.company
                    AND po-ordl.po-no   EQ (IF bf-ap-invl.po-no EQ 0 THEN bf-ap-inv.po-no
                    ELSE bf-ap-invl.po-no)
                    AND po-ordl.line    EQ (bf-ap-invl.line + (bf-ap-invl.po-no * 1000 * -1)) 
                    USE-INDEX po-no NO-ERROR.

                IF AVAILABLE po-ordl THEN 
                DO:
                    RUN po/getfrtcs.p (ROWID(po-ordl), bf-ap-invl.qty, OUTPUT dFreight).
                    bf-ap-inv.freight = bf-ap-inv.freight + dFreight.
                END.
            END.
        END.

        ASSIGN
            bf-ap-inv.tax-amt = bf-ap-inv.tax-amt +
                        ROUND((bf-ap-inv.freight * dTaxRateFreight / 100),2)
            bf-ap-inv.net     = bf-ap-inv.net + bf-ap-inv.tax-amt
            bf-ap-inv.due     = bf-ap-inv.net - bf-ap-inv.disc-taken -
                        bf-ap-inv.paid + bf-ap-inv.freight.
    END.

    FIND CURRENT bf-ap-inv NO-LOCK.

END PROCEDURE.

PROCEDURE pRecalculateARInvoiceHeader:
    /*------------------------------------------------------------------------------
     Purpose: Recaculates the balances on the AR header
     Notes: from ar/ar-invk.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriARInv AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdLineAmount AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dSubTotal   AS DECIMAL NO-UNDO.   
    DEFINE VARIABLE dTax        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxFreight AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ar-inv  FOR ar-inv.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.


    FIND bf-ar-inv EXCLUSIVE-LOCK  
        WHERE ROWID(bf-ar-inv) EQ ipriARInv NO-ERROR.

    IF AVAILABLE bf-ar-inv THEN 
    DO:
        bf-ar-inv.net = bf-ar-inv.net + ipdLineAmount.
        FIND FIRST cust NO-LOCK  
            WHERE cust.company EQ bf-ar-inv.company
            AND cust.cust-no EQ bf-ar-inv.cust-no 
            NO-ERROR.
            
        ASSIGN
            dSubTotal         = bf-ar-inv.net + bf-ar-inv.freight
            bf-ar-inv.tax-amt = 0
            dTax              = 0
            dTaxFreight       = 0.
   
        IF bf-ar-inv.tax-code NE "" AND cust.sort EQ "Y" THEN 
        DO:
            /*            RUN ar/calctax2.p (bf-ar-inv.tax-code,*/
            /*                NO,                               */
            /*                bf-ar-inv.net,                    */
            /*                cust.company,                     */
            /*                "", /* item */                    */
            /*                OUTPUT dTax).                     */
            /*                                                  */
            /*            RUN ar/calctax2.p (bf-ar-inv.tax-code,*/
            /*                YES,                              */
            /*                bf-ar-inv.freight,                */
            /*                cust.company,                     */
            /*                "", /* item */                    */
            /*                OUTPUT dTaxFreight).              */

            bf-ar-inv.tax-amt = dTax + dTaxFreight.
        END.

        ASSIGN
            bf-ar-inv.gross = dSubTotal + bf-ar-inv.tax-amt
            bf-ar-inv.due   = bf-ar-inv.gross - bf-ar-inv.paid - bf-ar-inv.disc-taken.
        
    END.
    FIND CURRENT bf-ar-inv NO-LOCK.
    RELEASE bf-ar-inv.

END PROCEDURE.

/* ************************  Function Implementations ***************** */
FUNCTION fValidateImportFG RETURNS LOGICAL 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Seaches each machine to make sure the machine codes are valid
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER NO-UNDO.
    
    FOR EACH ttImportedFGItems:
        ttImportedFGItems.lValid = YES.
        IF ttImportedFGItems.iCount EQ 1 AND iplHeader THEN 
        DO: 
            ASSIGN 
                ttImportedFGItems.lValid         = NO 
                ttImportedFGItems.cInvalidReason = "Header"
                .
            NEXT.
        END.
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ipcCompany
            AND itemfg.i-no EQ ttImportedFGItems.cINo
            NO-ERROR.
        FIND FIRST style NO-LOCK
            WHERE style.company EQ ipcCompany
            AND style.style EQ ttImportedFGItems.cStyle
            NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO:
            ASSIGN 
                ttImportedFGItems.lValid         = NO
                ttImportedFGItems.cInvalidReason = "FG Item: " + ttImportedFGItems.cINo + " already exists."
                .
        END.
        ELSE IF NOT AVAILABLE style THEN 
            DO:
                ASSIGN 
                    ttImportedFGItems.lValid         = NO
                    ttImportedFGItems.cInvalidReason = "Style: " + ttImportedFGItems.cStyle + " does not exist."
                    .
            END.
    END.
    
    lAllValid = NOT CAN-FIND(FIRST ttImportedFGItems WHERE ttImportedFGItems.lValid EQ NO AND ttImportedFGItems.cInvalidReason NE "Header").
    RETURN lAllValid.

        
END FUNCTION.

FUNCTION fValidateImportAccount RETURNS LOGICAL 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Seaches each machine to make sure the machine codes are valid
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER NO-UNDO.
    
    FOR EACH ttImportedAccounts:
        ttImportedAccounts.lValid = YES.
        IF ttImportedAccounts.iCount EQ 1 AND iplHeader THEN 
        DO: 
            ASSIGN 
                ttImportedAccounts.lValid         = NO 
                ttImportedAccounts.cInvalidReason = "Header"
                .
            NEXT.
        END.
        FIND FIRST account NO-LOCK
            WHERE account.company EQ ipcCompany
            AND account.actnum EQ ttImportedAccounts.cAccount
            NO-ERROR.
        IF AVAILABLE account THEN 
        DO:
            ASSIGN 
                ttImportedAccounts.lValid         = NO
                ttImportedAccounts.cInvalidReason = "Account: " + ttImportedAccounts.cAccount + " already exists."
                .
        END.
    END.

    lAllValid = NOT CAN-FIND(FIRST ttImportedAccounts WHERE ttImportedAccounts.lValid EQ NO AND ttImportedAccounts.cInvalidReason NE "Header").
    RETURN lAllValid.

        
END FUNCTION.



FUNCTION fValidateImportVend RETURNS LOGICAL 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Seaches each machine to make sure the machine codes are valid
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER NO-UNDO.
    
    FOR EACH ttImportedVendors:
        ttImportedVendors.lValid = YES.
        IF ttImportedVendors.iCount EQ 1 AND iplHeader THEN 
        DO: 
            ASSIGN 
                ttImportedVendors.lValid         = NO 
                ttImportedVendors.cInvalidReason = "Header"
                .
            NEXT.
        END.
        FIND FIRST vend NO-LOCK
            WHERE vend.company EQ ipcCompany
            AND vend.vend-no EQ ttImportedVendors.cVendNo
            NO-ERROR.
        IF AVAILABLE vend THEN 
        DO:
            ASSIGN 
                ttImportedVendors.lValid         = NO
                ttImportedVendors.cInvalidReason = "Vendor: " + ttImportedVendors.cVendNo + " already exists."
                .
        END.
    END.
     
    lAllValid = NOT CAN-FIND(FIRST ttImportedVendors WHERE ttImportedVendors.lValid EQ NO AND ttImportedVendors.cInvalidReason NE "Header").
    RETURN lAllValid.

        
END FUNCTION.


FUNCTION fValidateImportCust RETURNS LOGICAL 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Seaches each machine to make sure the machine codes are valid
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER NO-UNDO.
    
    FOR EACH ttImportedCustomers:
        ttImportedCustomers.lValid = YES.
        IF ttImportedCustomers.iCount EQ 1 AND iplHeader THEN 
        DO: 
            ASSIGN 
                ttImportedCustomers.lValid         = NO 
                ttImportedCustomers.cInvalidReason = "Header"
                .
            NEXT.
        END.
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipcCompany
            AND cust.cust-no EQ ttImportedCustomers.cCustNo
            NO-ERROR.
        IF AVAILABLE cust THEN 
        DO:
            ASSIGN 
                ttImportedCustomers.lValid         = NO
                ttImportedCustomers.cInvalidReason = "Customer: " + ttImportedCustomers.cCustNo + " already exists."
                .
        END.
    END.
    
    lAllValid = NOT CAN-FIND(FIRST ttImportedCustomers WHERE ttImportedCustomers.lValid EQ NO AND ttImportedCustomers.cInvalidReason NE "Header").

    RETURN lAllValid.

        
END FUNCTION.
FUNCTION fValidateImportCustUpd RETURNS LOGICAL 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Seaches each machine to make sure the machine codes are valid
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER NO-UNDO.
    
    FOR EACH ttCustUpdate:
        ttCustUpdate.lValid = YES.
        IF ttCustUpdate.iCount EQ 1 AND iplHeader THEN 
        DO: 
            ASSIGN 
                ttCustUpdate.lValid         = NO 
                ttCustUpdate.cInvalidReason = "Header"
                .
            NEXT.
        END.
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipcCompany
            AND cust.cust-no EQ ttCustUpdate.cCustNo
            NO-ERROR.
        IF NOT AVAILABLE cust THEN 
        DO:
            ASSIGN 
                ttCustUpdate.lValid         = NO
                ttCustUpdate.cInvalidReason = "Customer: " + ttCustUpdate.cCustNo + " not found."
                .
        END.
    END.
    
    lAllValid = NOT CAN-FIND(FIRST ttCustUpdate WHERE ttCustUpdate.lValid EQ NO AND ttCustUpdate.cInvalidReason NE "Header").

    RETURN lAllValid.

        
END FUNCTION.
FUNCTION fValidateImportInvoiceAP RETURNS LOGICAL 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Seaches each machine to make sure the machine codes are valid
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER NO-UNDO.
    
    FOR EACH ttImportedInvoicesAP:
        ttImportedInvoicesAP.lValid = YES.
        IF ttImportedInvoicesAP.iCount EQ 1 AND iplHeader THEN 
        DO: 
            ASSIGN 
                ttImportedInvoicesAP.lValid         = NO 
                ttImportedInvoicesAP.cInvalidReason = "Header"
                .
            NEXT.
        END.
        FIND FIRST vend NO-LOCK
            WHERE vend.company EQ ipcCompany
            AND vend.vend-no EQ ttImportedInvoicesAP.cVendNo
            NO-ERROR.
        IF NOT AVAILABLE vend THEN 
        DO:
            ASSIGN 
                ttImportedInvoicesAP.lValid         = NO
                ttImportedInvoicesAP.cInvalidReason = "Vendor: " + ttImportedInvoicesAP.cVendNo + " not found. "
                .
        END.
        IF ttImportedInvoicesAP.cLineAccount NE "" THEN 
        DO:
            FIND FIRST account NO-LOCK
                WHERE account.company EQ ipcCompany
                AND account.actnum EQ ttImportedInvoicesAP.cLineAccount
                NO-ERROR.
            IF NOT AVAILABLE account THEN 
            DO:
                ASSIGN 
                    ttImportedInvoicesAP.lValid         = NO
                    ttImportedInvoicesAP.cInvalidReason = ttImportedInvoicesAP.cInvalidReason + "Account: " + ttImportedInvoicesAP.cLineAccount + " not found."
                    .
            END.
        END.
    END.
    
    lAllValid = NOT CAN-FIND(FIRST ttImportedInvoicesAP WHERE ttImportedInvoicesAP.lValid EQ NO AND ttImportedInvoicesAP.cInvalidReason NE "Header").

    RETURN lAllValid.

        
END FUNCTION.
FUNCTION fValidateImportInvoiceAR RETURNS LOGICAL 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Seaches each machine to make sure the machine codes are valid
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER NO-UNDO.
    
    FOR EACH ttImportedInvoicesAR:
        ttImportedInvoicesAR.lValid = YES.
        IF ttImportedInvoicesAR.iCount EQ 1 AND iplHeader THEN 
        DO: 
            ASSIGN 
                ttImportedInvoicesAR.lValid         = NO 
                ttImportedInvoicesAR.cInvalidReason = "Header"
                .
            NEXT.
        END.
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipcCompany
            AND cust.cust-no EQ ttImportedInvoicesAR.cCustNo
            NO-ERROR.
        IF NOT AVAILABLE cust THEN 
        DO:
            ASSIGN 
                ttImportedInvoicesAR.lValid         = NO
                ttImportedInvoicesAR.cInvalidReason = "Customer: " + ttImportedInvoicesAR.cCustNo + " not found."
                .
        END.
    END.
    
    lAllValid = NOT CAN-FIND(FIRST ttImportedInvoicesAR WHERE ttImportedInvoicesAR.lValid EQ NO AND ttImportedInvoicesAR.cInvalidReason NE "Header").

    RETURN lAllValid.

        
END FUNCTION.
