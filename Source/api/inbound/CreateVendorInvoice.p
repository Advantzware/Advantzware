/*------------------------------------------------------------------------
    File        : api\inbound\CreateVendorInvoice.p
    Purpose     : Processes request data

    Syntax      :

    Description : Processes request data

    Author(s)   : Vishnu Vellanki
    Created     : Thu Jan 02 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
{inventory/ttinventory.i "NEW SHARED"}.
{jc/jcgl-sh.i  NEW}

DEFINE INPUT  PARAMETER ipcCompany                                  AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER ipcVendorID                                 AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorInvoiceNumber                      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorInvoiceDate                        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorInvoiceDueDate                     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdVendorInvoiceDiscountPercentage          AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipdVendorInvoiceTotalAmount                 AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipiVendorInvoiceLinePurchaseOrderNumber     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiVendorInvoiceLinePurchaseOrderLineNumber AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorInvoiceLineAccountNumber           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdVendorInvoiceLineQuantity                AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorInvoiceLineQuantityUOM             AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdVendorInvoiceLinePrice                   AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorInvoiceLinePriceUOM                AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdVendorInvoiceLineSqFt                    AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipdVendorInvoiceLineAmount                  AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorInvoiceLineDescription             AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                                  AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                                  AS CHARACTER NO-UNDO.

{api/inbound/ttRctd.i}

/* This will eventually move to setsession - START >>>*/
&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTags    AS HANDLE NO-UNDO.

g_company=ipcCompany.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).
RUN system/TagProcs.p PERSISTENT SET hdTags.
SESSION:ADD-SUPER-PROCEDURE (hdTags).
{sys/inc/var.i "new shared"}
/* END <<<*/

DEFINE VARIABLE cVendorInvoiceLineEachUOM AS CHARACTER NO-UNDO INITIAL "EA".
DEFINE VARIABLE hdCommonProcs             AS HANDLE    NO-UNDO.
DEFINE VARIABLE iDiscDays                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTaxGroup                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPOActNum                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE riapinv                   AS ROWID     NO-UNDO.
DEFINE VARIABLE cVendActNum               AS CHARACTER NO-UNDO.
DEFINE VARIABLE riapinvl                  AS ROWID     NO-UNDO.
DEFINE VARIABLE lItemType                 AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iIndex                    AS INTEGER   NO-UNDO. 
DEFINE VARIABLE hdTagProcs                AS HANDLE    NO-UNDO. 
DEFINE VARIABLE lHold                     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cHoldMessage              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cAPInvLineDescr           AS CHARACTER NO-UNDO. 

DEFINE BUFFER bf-ap-inv FOR ap-inv. 

RUN system/TagProcs.p PERSISTENT SET hdTagProcs.                    

ASSIGN
    oplSuccess                      = YES
    cocode                          = ipcCompany
    ipcVendorInvoiceLineQuantityUOM = IF ipcVendorInvoiceLineQuantityUOM EQ "" THEN
                                          cVendorInvoiceLineEachUOM
                                      ELSE
                                          ipcVendorInvoiceLineQuantityUOM
    ipcVendorInvoiceLinePriceUOM    = IF ipcVendorInvoiceLinePriceUOM EQ "" THEN
                                          cVendorInvoiceLineEachUOM
                                      ELSE
                                          ipcVendorInvoiceLinePriceUOM
    .

RUN system/CommonProcs.p PERSISTENT SET hdCommonProcs.

/* Validates all required fields */
RUN pValidations (
    INPUT  ipcCompany,
    INPUT  ipcVendorID,
    INPUT  ipcVendorInvoiceNumber,
    INPUT  ipcVendorInvoiceDate,
    INPUT  ipdVendorInvoiceTotalAmount,
    INPUT  ipdVendorInvoiceLineQuantity,
    INPUT  ipdVendorInvoiceLinePrice,
    INPUT  ipdVendorInvoiceLineAmount,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).
    
IF NOT oplSuccess THEN
    RETURN.
    
/* Validates PO number and line for non-zero values */
IF ipiVendorInvoiceLinePurchaseOrderNumber NE 0 THEN
    RUN pValidatePODetails (
        INPUT  ipcCompany,
        INPUT  ipiVendorInvoiceLinePurchaseOrderNumber,
        INPUT  ipiVendorInvoiceLinePurchaseOrderLineNumber,
        OUTPUT cPOActNum,
        OUTPUT cAPInvLineDescr,
        OUTPUT lItemType,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).

IF NOT oplSuccess THEN
    RETURN.

/* checks whether invoice exists or not */  
FIND FIRST ap-inv NO-LOCK
     WHERE ap-inv.company EQ ipcCompany
       AND ap-inv.inv-no  EQ ipcVendorInvoiceNumber
     NO-ERROR.
IF NOT AVAILABLE ap-inv THEN /* create a new invoice*/
    RUN pCreateNewInvoice (
        INPUT  ipcCompany, 
        INPUT  ipcVendorID, 
        INPUT  ipcVendorInvoiceNumber, 
        INPUT  ipcVendorInvoiceDate,
        INPUT  ipcVendorInvoiceDueDate,
        INPUT  ipdVendorInvoiceDiscountPercentage
        ).
     
/* if po value is zero then account number comes from vendor */
IF ipiVendorInvoiceLinePurchaseOrderNumber EQ 0 THEN DO:
    FIND FIRST vend NO-LOCK 
         WHERE vend.company EQ ipcCompany
           AND vend.vend-no EQ ipcVendorID
         NO-ERROR. 
    IF AVAILABLE vend THEN 
        cVendActNum = vend.actnum.
END.
         
/* VendorInvoiceLineAccountNumber gets value from purchase order line if it is available else from vendor */     
IF ipcVendorInvoiceLineAccountNumber EQ "" THEN
    ipcVendorInvoiceLineAccountNumber = IF ipiVendorInvoiceLinePurchaseOrderNumber NE 0 THEN
                                            cPOActNum
                                        ELSE 
                                            cVendActNum. 

/* ipcVendorInvoiceLineDescription gets value from purchase order line if it is not provided */ 
IF ipcVendorInvoiceLineDescription EQ "" THEN
    ipcVendorInvoiceLineDescription = cAPInvLineDescr.
                                        
/* Checks whether invoice created or not */                                         
FIND FIRST ap-inv NO-LOCK
     WHERE ap-inv.company EQ ipcCompany
       AND ap-inv.inv-no  EQ ipcVendorInvoiceNumber
       AND ap-inv.vend-no EQ ipcVendorID
     NO-ERROR.
          
IF AVAILABLE ap-inv THEN DO:
    IF ipiVendorInvoiceLinePurchaseOrderNumber EQ 0 THEN DO:
        FIND LAST  ap-invl NO-LOCK 
             WHERE ap-invl.company EQ ap-inv.company
               AND ap-invl.i-no    EQ ap-inv.i-no
               AND ap-invl.po-no   EQ ipiVendorInvoiceLinePurchaseOrderNumber
             NO-ERROR.
        ipiVendorInvoiceLinePurchaseOrderLineNumber = IF AVAILABLE ap-invl THEN 
                                                          ap-invl.line + 1
                                                      ELSE
                                                          ipiVendorInvoiceLinePurchaseOrderLineNumber + 1  
                                                      .             
    END. 
    /* Checks whether invoice line exists or not */         
    FIND FIRST ap-invl NO-LOCK 
         WHERE ap-invl.company EQ ap-inv.company
           AND ap-invl.i-no    EQ ap-inv.i-no
           AND ap-invl.po-no   EQ ipiVendorInvoiceLinePurchaseOrderNumber
           AND ap-invl.po-line EQ ipiVendorInvoiceLinePurchaseOrderLineNumber
         NO-ERROR.
    IF NOT AVAILABLE ap-invl THEN DO:
        RUN pCreateNewInvoiceLine (
            INPUT  ROWID(ap-inv), 
            INPUT  ipiVendorInvoiceLinePurchaseOrderNumber,
            INPUT  ipiVendorInvoiceLinePurchaseOrderLineNumber,
            INPUT  ipcVendorInvoiceLineAccountNumber,
            INPUT  ipdVendorInvoiceLineQuantity,
            INPUT  ipdVendorInvoiceLinePrice,
            INPUT  ipdVendorInvoiceLineAmount,
            INPUT  ipdVendorInvoiceLineSqFt,
            INPUT  ipcVendorInvoiceLineQuantityUOM,
            INPUT  ipcVendorInvoiceLinePriceUOM,
            INPUT  ipcVendorInvoiceLineDescription,
            OUTPUT riAPInvl
            ) NO-ERROR.

        IF NOT ERROR-STATUS:ERROR THEN DO:
            IF NOT lItemType THEN
                RUN pValidateFGItemReceiptQtyPrice (
                    INPUT        riapinvl,
                    INPUT        ipcCompany,
                    INPUT        ipiVendorInvoiceLinePurchaseOrderNumber,
                    INPUT        ipiVendorInvoiceLinePurchaseOrderLineNumber,
                    INPUT        ipdVendorInvoiceLineQuantity,
                    INPUT        ipdVendorInvoiceLinePrice,
                    INPUT-OUTPUT lHold,
                    INPUT-OUTPUT cHoldMessage
                    ).
            ELSE
                RUN pValidateRMItemReceiptQtyPrice (
                    INPUT        riapinvl,
                    INPUT        ipcCompany,
                    INPUT        ipiVendorInvoiceLinePurchaseOrderNumber,
                    INPUT        ipiVendorInvoiceLinePurchaseOrderLineNumber,
                    INPUT        ipdVendorInvoiceLineQuantity,
                    INPUT        ipdVendorInvoiceLinePrice,
                    INPUT-OUTPUT lHold,
                    INPUT-OUTPUT cHoldMessage
                    ). 
        END.
    END.

    IF lHold THEN DO:
        FIND FIRST bf-ap-inv EXCLUSIVE-LOCK
             WHERE bf-ap-inv.company EQ ipcCompany
               AND bf-ap-inv.inv-no  EQ ipcVendorInvoiceNumber
               AND bf-ap-inv.vend-no EQ ipcVendorID
             NO-ERROR.
        IF AVAILABLE bf-ap-inv THEN
            bf-ap-inv.stat = "H". /* Hold */
        DO iIndex = 1 TO NUM-ENTRIES(cHoldMessage,"|"): 
            IF VALID-HANDLE(hdTagProcs) AND ENTRY(iIndex, cHoldMessage, "|") NE "" THEN DO:
                RUN AddTagHold IN hdTagProcs (
                    INPUT ap-inv.REC_KEY,
                    INPUT "ap-inv",
                    INPUT ENTRY(iIndex, cHoldMessage, "|")
                    ).
            END.
        END.
        RELEASE bf-ap-inv.
    END.
    RUN pRecalculateInvoiceHeader (
        INPUT ROWID(ap-inv), 
        INPUT NO
        ).  
END.

/* Validates required fields */
PROCEDURE pValidations:
    /*------------------------------------------------------------------------------
     Purpose: Validates required fields
     Notes:
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT  PARAMETER  ipcCompany                   AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER  ipcVendorID                  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcVendorInvoiceNumber       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcVendorInvoiceDate         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipdVendorInvoiceTotalAmount  AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipdVendorInvoiceLineQuantity AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipdVendorInvoiceLinePrice    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipdVendorInvoiceLineAmount   AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER  oplSuccess                   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER  opcMessage                   AS CHARACTER NO-UNDO. 

    DEFINE VARIABLE lValidInvoiceDate AS LOGICAL NO-UNDO.
    
    ASSIGN
        lValidInvoiceDate = ipcVendorInvoiceDate NE ""
        oplSuccess        = YES
        .
   
    /* Validate vendor invoice number */
    IF ipcVendorInvoiceNumber EQ "" THEN DO:
        ASSIGN 
            opcMessage = "Invalid Vendor Invoice Number (" + ipcVendorInvoiceNumber + ")"
            oplSuccess = NO
            .
            
        RETURN.
    END.

    /* Validate company */
    IF NOT CAN-FIND(FIRST company NO-LOCK
                    WHERE company.company EQ ipcCompany) THEN DO:
        ASSIGN 
            opcMessage = "Invalid Company (" + ipcCompany + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Validate vendorid */
    IF NOT CAN-FIND(FIRST vend NO-LOCK
                    WHERE vend.company EQ ipcCompany
                      AND vend.vend-no EQ ipcVendorID) THEN DO:
        ASSIGN 
            opcMessage = "Invalid VendorID (" + ipcVendorID + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Validate vendor invoice date for non-zero values  */
    IF lValidInvoiceDate THEN
        RUN spCommon_ValidateValueByDataType (
            INPUT  ipcVendorInvoiceDate,
            INPUT  "DATE",
            OUTPUT lValidInvoiceDate
            ) NO-ERROR.
            
    IF NOT lValidInvoiceDate THEN DO:
        ASSIGN
            opcMessage = "Invalid Vendor Invoice Date (" + ipcVendorInvoiceDate + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            oplSuccess = NO
            .
            
        RETURN.    
    END.
    
    /* Validate vendor invoice total amount */
    IF ipdVendorInvoiceTotalAmount EQ 0 THEN DO:
        ASSIGN 
            opcMessage = "Invalid Vendor Invoice Total Amount (" + STRING(ipdVendorInvoiceTotalAmount) + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Validate vendor invoice line quantity */
    IF ipdVendorInvoiceLineQuantity EQ 0 THEN DO:
        ASSIGN 
            opcMessage = "Invalid Vendor Invoice Line Quantity (" + STRING(ipdVendorInvoiceLineQuantity) + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Validate vendor invoice line price */
    IF ipdVendorInvoiceLinePrice EQ 0 THEN DO:
        ASSIGN 
            opcMessage = "Invalid Vendor Invoice Line Price (" + STRING(ipdVendorInvoiceLinePrice) + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Validate vendor invoice line amount */
    IF ipdVendorInvoiceLineAmount EQ 0 THEN DO:
        ASSIGN 
            opcMessage = "Invalid Vendor Invoice Line Amount (" + STRING(ipdVendorInvoiceLineAmount) + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            oplSuccess = NO
            .
            
        RETURN.
    END.
END PROCEDURE.

PROCEDURE pCreateNewInvoice:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AP invoice
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER  ipcCompany                         AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER  ipcVendorID                        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcVendorInvoiceNumber             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcVendorInvoiceDate               AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcVendorInvoiceDueDate            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipdVendorInvoiceDiscountPercentage AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE iExRate   AS INTEGER   NO-UNDO INITIAL 1.
    DEFINE VARIABLE cCurrCode AS CHARACTER NO-UNDO INITIAL "USD".  
    
    CREATE ap-inv.
    ASSIGN
        ap-inv.company     = ipcCompany
        ap-inv.inv-no      = ipcVendorInvoiceNumber
        ap-inv.vend-no     = ipcVendorID
        ap-inv.inv-date    = DATE(ipcVendorInvoiceDate)
        .
                    
    FIND FIRST vend NO-LOCK 
         WHERE vend.company EQ ipcCompany
           AND vend.vend-no EQ ipcVendorID
         NO-ERROR.
    IF AVAILABLE vend THEN DO:
        ASSIGN
            ap-inv.disc-%    = IF ipdVendorInvoiceDiscountPercentage EQ 0 THEN
                                   vend.disc-%
                               ELSE
                                   ipdVendorInvoiceDiscountPercentage
            ap-inv.disc-days = vend.disc-days
            ap-inv.tax-gr    = vend.tax-gr
            .  
        
        FIND FIRST currency NO-LOCK  
             WHERE currency.company EQ ipcCompany
               AND currency.c-code  EQ vend.curr-code
             NO-ERROR.
        FIND FIRST company NO-LOCK 
             WHERE company.company EQ ipcCompany
             NO-ERROR.
        IF NOT AVAILABLE currency AND AVAILABLE company THEN 
            FIND FIRST currency NO-LOCK 
                 WHERE currency.company EQ ipcCompany
                   AND currency.c-code  EQ company.curr-code
                 NO-ERROR.
        IF AVAILABLE currency THEN
            ASSIGN 
                ap-inv.ex-rate      = currency.ex-rate
                ap-inv.curr-code[1] = currency.c-code
                .
        ELSE 
            ASSIGN 
                ap-inv.ex-rate      = iExRate   /* dafault exchange rate */
                ap-inv.curr-code[1] = cCurrCode /* dafault currency code */
                .                        
        
        FIND FIRST terms NO-LOCK
             WHERE terms.t-code EQ vend.terms 
             NO-ERROR.
        IF AVAILABLE terms THEN
            ASSIGN
                ap-inv.disc-%    = terms.disc-rate
                ap-inv.disc-days = terms.disc-days
                .
        ap-inv.due-date = IF ipcVendorInvoiceDueDate NE "" AND ipcVendorinvoiceDueDate NE ? THEN
                              DATE(ipcVendorInvoiceDueDate)
                          ELSE IF AVAILABLE terms THEN 
                              terms.net-day + ap-inv.inv-date
                          ELSE
                              ap-inv.inv-date.
                                                   
    END.
    RELEASE ap-inv.

END PROCEDURE.

PROCEDURE pCreateNewInvoiceLine:
    /*------------------------------------------------------------------------------
        Purpose: Creates a new AP invoice line
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriAPInv                       AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo                         AS INTEGER   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipiPoLine                       AS INTEGER   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcLineAccount                  AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipdQuantity                     AS DECIMAL   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipdPrice                        AS DECIMAL   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipdTotalAmount                  AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdSquareFeet                   AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendorInvoiceLineQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendorInvoiceLinePriceUOM    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendorInvoiceLineDescription AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriapinvl                      AS ROWID     NO-UNDO.
    
    FIND FIRST ap-inv NO-LOCK 
         WHERE ROWID(ap-inv) EQ ipriAPInv
         NO-ERROR.
        
    IF NOT AVAILABLE ap-inv THEN 
        RETURN.
                      
    CREATE ap-invl.
    ASSIGN
        ap-invl.i-no       = ap-inv.i-no
        ap-invl.company    = ap-inv.company
        ap-invl.vend-no    = ap-inv.vend-no
        ap-invl.line       = (ipiPONo * 1000) + ipiPOLine
        ap-invl.loc        = ap-inv.loc
        ap-invl.period     = ap-inv.period
        ap-invl.posted     = ap-inv.posted
        ap-invl.cons-uom   = ipcVendorInvoiceLineQuantityUOM
        ap-invl.pr-qty-uom = ipcVendorInvoiceLinePriceUOM
        ap-invl.tax        = ap-inv.tax-gr NE ""
        ap-invl.amt        = ipdTotalAmount 
        ap-invl.qty        = ipdQuantity
        ap-invl.unit-pr    = ipdPrice
        ap-invl.po-no      = ipiPoNo       
        ap-invl.po-line    = ipiPOLine
        ap-invl.dscr       = ipcVendorInvoiceLineDescription
        ap-invl.actnum     = ipcLineAccount
        ap-invl.sf-sht     = ipdSquareFeet
        .
    opriapinvl = ROWID(ap-invl).
    RELEASE ap-invl.

END PROCEDURE.

PROCEDURE pValidatePODetails:
    /*------------------------------------------------------------------------------
        Purpose: Validates PO details
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany                                  AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipiVendorInvoiceLinePurchaseOrderNumber     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiVendorInvoiceLinePurchaseOrderLineNumber AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPOActNum                                 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPOLineDescr                              AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplItemType                                 AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess                                  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                                  AS CHARACTER NO-UNDO. 
    
    oplSuccess = YES.
    
    IF NOT CAN-FIND(FIRST po-ord NO-LOCK
                    WHERE po-ord.company EQ ipcCompany
                      AND po-ord.po-no   EQ ipiVendorInvoiceLinePurchaseOrderNumber) THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "Invalid PO Number (" + STRING(ipiVendorInvoiceLinePurchaseOrderNumber) + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            .
        
        RETURN.    
    END.
   
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ ipiVendorInvoiceLinePurchaseOrderNumber
           AND po-ordl.line    EQ ipiVendorInvoiceLinePurchaseOrderLineNumber
          NO-ERROR.
    IF NOT AVAILABLE po-ordl THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "Invalid PO Line (" + STRING(ipiVendorInvoiceLinePurchaseOrderLineNumber) + ") entered for Invoice (" + ipcVendorInvoiceNumber + ")"
            .
            
        RETURN.    
    END.
        
    ASSIGN 
        opcPOActNum    = po-ordl.actnum
        oplItemType    = po-ordl.item-type
        opcPOLineDescr = po-ordl.i-name
        .
            
END PROCEDURE.

PROCEDURE pRecalculateInvoiceHeader:
    /*------------------------------------------------------------------------------
       Purpose: recalucaltes invoice header details
       Notes:
      ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriAPInv AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplOverwriteTax AS LOGICAL NO-UNDO.

    DEFINE VARIABLE dFreight        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxRate        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxRateFreight AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-ap-inv  FOR ap-inv.
    DEFINE BUFFER bf-ap-invl FOR ap-invl.

    FIND FIRST bf-ap-inv EXCLUSIVE-LOCK  
         WHERE ROWID(bf-ap-inv) EQ ipriAPInv 
         NO-ERROR.

    IF AVAILABLE bf-ap-inv THEN DO: 
        IF NOT iplOverwriteTax THEN
            bf-ap-inv.tax-amt = 0.

        ASSIGN
            bf-ap-inv.net     = 0
            bf-ap-inv.freight = 0
            .

        IF bf-ap-inv.tax-gr NE "" THEN
            RUN ar/cctaxrt.p (
                INPUT  bf-ap-inv.company, 
                INPUT  bf-ap-inv.tax-gr,
                OUTPUT dTaxRate, 
                OUTPUT dTaxRateFreight
                ).

        FOR EACH bf-ap-invl NO-LOCK 
            WHERE bf-ap-invl.i-no EQ bf-ap-inv.i-no:
            bf-ap-inv.net = bf-ap-inv.net + bf-ap-invl.amt.

            IF bf-ap-invl.tax AND NOT iplOverwriteTax THEN
                bf-ap-inv.tax-amt = bf-ap-inv.tax-amt +
                                    ROUND((bf-ap-invl.amt * dTaxRate / 100),2).

            IF bf-ap-invl.po-no NE 0 THEN DO: 
                FIND FIRST po-ordl NO-LOCK 
                     WHERE po-ordl.company EQ  bf-ap-invl.company
                       AND po-ordl.po-no   EQ (IF bf-ap-invl.po-no EQ 0 THEN bf-ap-inv.po-no
                     ELSE bf-ap-invl.po-no)
                      AND po-ordl.line     EQ (bf-ap-invl.line + (bf-ap-invl.po-no * 1000 * -1)) 
                     USE-INDEX po-no NO-ERROR.

                IF AVAILABLE po-ordl THEN DO: 
                    RUN po/getfrtcs.p (
                        INPUT ROWID(po-ordl), 
                        INPUT bf-ap-invl.qty, 
                        OUTPUT dFreight
                        ).
                    bf-ap-inv.freight = bf-ap-inv.freight + dFreight.
                END.
            END.
        END.

        ASSIGN
            bf-ap-inv.tax-amt = bf-ap-inv.tax-amt +
                                ROUND((bf-ap-inv.freight * dTaxRateFreight / 100),2)
            bf-ap-inv.net     = bf-ap-inv.net + bf-ap-inv.tax-amt
            bf-ap-inv.due     = bf-ap-inv.net - bf-ap-inv.disc-taken -
                                bf-ap-inv.paid + bf-ap-inv.freight
            .
    END.

    FIND CURRENT bf-ap-inv NO-LOCK.

END PROCEDURE.

PROCEDURE pValidateFGItemReceiptQtyPrice:
    /*------------------------------------------------------------------------------
       Purpose: FG Item Receipt quantity and price validation 
       Notes:
      ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipriAPInvl   AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPoNo      AS INTEGER   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipiPoLine    AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipdQuantity  AS INTEGER   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipdPrice     AS INTEGER   NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER ioplHold     AS LOGICAL   NO-UNDO.    
    DEFINE INPUT-OUTPUT PARAMETER iopcHoldNote AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantity         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPriceMatch       AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostFromHistory  AS DECIMAL   NO-UNDO.
    
    FIND FIRST po-ord NO-LOCK
         WHERE po-ord.company EQ ipcCompany
           AND po-ord.po-no   EQ ipiPoNo
         NO-ERROR.
    
    IF NOT AVAILABLE po-ord THEN
        RETURN.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ po-ord.po-no
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF NOT AVAILABLE po-ordl THEN
        RETURN.
    
    FIND FIRST ap-invl NO-LOCK
         WHERE ROWID(ap-invl) EQ ipriAPInvl NO-ERROR.
    IF NOT AVAILABLE ap-invl THEN
        RETURN.
        
    FOR EACH fg-rcpth NO-LOCK
       WHERE fg-rcpth.company   EQ ipcCompany
         AND fg-rcpth.i-no      EQ po-ordl.i-no
         AND fg-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
         AND fg-rcpth.rita-code EQ "R"
       USE-INDEX item-po,
       EACH fg-rdtlh EXCLUSIVE-LOCK
       WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
         AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
         AND NOT CAN-FIND(
                          FIRST ap-invl 
                          WHERE ap-invl.i-no EQ INT(SUBSTR(fg-rdtlh.receiver-no,1,10))
                            AND ap-invl.line EQ (po-ordl.po-no * 1000) + po-ordl.line
                         ):
        ASSIGN 
            dQuantity = dQuantity + fg-rdtlh.qty
            dCostFromHistory = fg-rdtlh.cost
            .
        IF dQuantity GT ipdQuantity THEN
            LEAVE.        
    END.
    IF dQuantity EQ ipdQuantity THEN DO:
        IF dCostFromHistory EQ ipdPrice THEN
            lPriceMatch = TRUE.
        ELSE
            lPriceMatch = FALSE.
            
        lQtyMatch = TRUE.
        
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company   EQ ipcCompany
              AND fg-rcpth.i-no      EQ po-ordl.i-no
              AND fg-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
              AND fg-rcpth.rita-code EQ "R"
            USE-INDEX item-po,
            EACH fg-rdtlh EXCLUSIVE-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
              AND NOT CAN-FIND(
                          FIRST ap-invl 
                          WHERE ap-invl.i-no EQ INT(SUBSTR(fg-rdtlh.receiver-no,1,10))
                            AND ap-invl.line EQ (po-ordl.po-no * 1000) + po-ordl.line
                         ):
            fg-rdtlh.receiver-no = (STRING(ap-invl.i-no,"9999999999") +
                                    STRING(ipdQuantity,"-9999999999.99999")).           
            
        END.
    END.
    IF NOT lQtyMatch THEN
        ASSIGN
            ioplHold    = YES
            cMessage    = "PO # " + STRING(ipiPoNo)  + 
                          "-" + STRING(po-ordl.line) + " Quantity (" + STRING(ipdQuantity) + 
                          ") does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            .
        
    IF NOT lPriceMatch THEN
        ASSIGN
            ioplHold     = YES
            cMessage     = "PO # " + STRING(ipiPoNo) + 
                           "-" + STRING(po-ordl.line) + " Price (" + STRING(ipdPrice)  + 
                           ") does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            .
    
END PROCEDURE.

PROCEDURE pValidateRMItemReceiptQtyPrice:
    /*------------------------------------------------------------------------------
       Purpose: RM Item Receipt quantity and price validation
       Notes:
      ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipriAPInvl   AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPoNo      AS INTEGER   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipiPoLine    AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipdQuantity  AS DECIMAL   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipdPrice     AS DECIMAL   NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER ioplHold     AS LOGICAL   NO-UNDO.    
    DEFINE INPUT-OUTPUT PARAMETER iopcHoldNote AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantity   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPriceMatch AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    
    FIND FIRST po-ord NO-LOCK
         WHERE po-ord.company EQ ipcCompany
           AND po-ord.po-no   EQ ipiPoNo
         NO-ERROR.
    
    IF NOT AVAILABLE po-ord THEN
        RETURN.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ po-ord.po-no
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF NOT AVAILABLE po-ordl THEN
        RETURN.
    
    FIND FIRST ap-invl NO-LOCK
         WHERE ROWID(ap-invl) EQ ipriAPInvl NO-ERROR.
    IF NOT AVAILABLE ap-invl THEN
        RETURN.
        
    FOR EACH rm-rcpth NO-LOCK
       WHERE rm-rcpth.company   EQ ipcCompany
         AND rm-rcpth.i-no      EQ po-ordl.i-no
         AND rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
         AND rm-rcpth.job-no    EQ po-ordl.job-no
         AND rm-rcpth.job-no2   EQ po-ordl.job-no2
         AND rm-rcpth.rita-code EQ "R"
       USE-INDEX item-po,
       EACH rm-rdtlh EXCLUSIVE-LOCK
       WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
         AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
         AND (rm-rdtlh.s-num    EQ po-ordl.s-num OR po-ordl.s-num EQ 0):
         
        dQuantity = dQuantity + rm-rdtlh.qty.
        
        IF dQuantity EQ ipdQuantity THEN DO:
       
            IF rm-rdtlh.cost EQ ipdPrice THEN
                lPriceMatch = TRUE.
                
            ASSIGN
                lQtyMatch            = TRUE
                rm-rdtlh.receiver-no = (STRING(ap-invl.i-no,"9999999999") +
                                        STRING(ipdQuantity,"-9999999999.99999"))
                .           
            LEAVE.
        END.        
    END.

    IF NOT lQtyMatch THEN
        ASSIGN
            ioplHold    = YES
            cMessage    = "PO # " + STRING(ipiPoNo)  + 
                          "-" + STRING(po-ordl.line,"999") + ", Quantity " + STRING(ipdQuantity) + 
                          " does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            .
        
    IF NOT lPriceMatch THEN
        ASSIGN
            ioplHold     = YES
            cMessage     = "PO # " + STRING(ipiPoNo) + 
                           "-" + STRING(po-ordl.line,"999") + ", Price " + STRING(ipdPrice)  + 
                           " does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            .
    
END PROCEDURE.


    
    


