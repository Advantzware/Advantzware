
/*------------------------------------------------------------------------
    File        : ImportAP.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for AP Invoices	

    Author(s)   : BV
    Created     : Fri Nov 24 16:18:38 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportAP
    FIELD Company         AS CHARACTER FORMAT "x(3)"
    FIELD InvoiceNo       AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Invoice #"
    FIELD VendorID        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Vendor ID"
    FIELD InvoiceDate     AS DATE FORMAT "99/99/99" COLUMN-LABEL "Inv Date"
    FIELD DueDate         AS DATE FORMAT "99/99/99" COLUMN-LABEL "Due Date"
    FIELD LineAmount      AS DECIMAL FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "$ Amount" 
    FIELD LineQuantity    AS DECIMAL FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity"
    FIELD LineQuantityUom AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Qty UOM"
    FIELD LinePrice       AS DECIMAL FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price"
    FIELD LinePriceUom    AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Price UOM"
    FIELD LineTax         AS LOGICAL FORMAT "Y/N" COLUMN-LABEL "Tax (Line)"
    FIELD LineAccount     AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "GL Accnt #"
    FIELD TaxGroup        AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Tax Group"
    FIELD Discount        AS DECIMAL FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Discount"
    FIELD DiscountDays    AS INTEGER FORMAT ">>9" COLUMN-LABEL "Disc Days"
    FIELD LinePONumber    AS INTEGER FORMAT ">>>>>9" COLUMN-LABEL "PO #"
    FIELD LinePOLine      AS INTEGER FORMAT ">>9" COLUMN-LABEL "PO Line"
    .
    

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddRecord:
    /*------------------------------------------------------------------------------
     Purpose: Accepts a Data Array, validates it and adds a temp-table record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcData AS CHARACTER NO-UNDO EXTENT.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdTempTableBuffer AS HANDLE.
    DEFINE VARIABLE cData             AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportAP FOR ttImportAP.

    oplValid = YES.
    CREATE ttImportAP.
    ASSIGN 
        ttImportAP.Company = ipcCompany.
    FOR EACH ttImportMap
        WHERE ttImportMap.cType EQ 'AP':
        cData = ipcData[ttImportMap.iImportIndex].
        hdTempTableBuffer = TEMP-TABLE ttImportAP:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ttImportMap.iIndex + giIndexOffset):HANDLE.
        CASE ttImportMap.cDataType:
            WHEN "integer" THEN 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = INT(cData).
            WHEN "logical" THEN 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = cData BEGINS "Y".
            WHEN "decimal" THEN 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = DEC(cDaTa).
            WHEN "date" THEN 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = DATE(cData). 
            OTHERWISE 
            ASSIGN 
                hdTempTableBuffer:BUFFER-VALUE = cData.
        END CASE.              
    END.
    IF oplValid THEN 
    DO:
        IF ttImportAP.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportAP.VendorID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: VendorID".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportAP.InvoiceNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Invoice#".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST vend NO-LOCK 
            WHERE vend.company EQ ttImportAP.Company
            AND vend.vend-no EQ ttImportAP.VendorID
            NO-ERROR. 
        IF NOT AVAILABLE vend THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Key Field Invalid: VendorID"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportAP NO-LOCK 
            WHERE bf-ttImportAP.Company EQ ttImportAP.Company
            AND bf-ttImportAP.VendorID EQ ttImportAP.VendorID
            AND bf-ttImportAP.InvoiceNo EQ ttImportAP.InvoiceNo
            AND bf-ttImportAP.LinePONumber EQ ttImportAP.LinePONumber 
            AND bf-ttImportAP.LinePOLine EQ ttImportAP.LinePOLine
            AND ROWID(bf-ttImportAP) NE ROWID(ttImportAP)
            NO-ERROR.
        IF AVAILABLE bf-ttImportAP THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST ap-inv NO-LOCK 
            WHERE ap-inv.company EQ ttImportAP.Company
            AND ap-inv.vend-no EQ ttImportAP.VendorID
            AND ap-inv.inv-no EQ ttImportAP.InvoiceNo
            NO-ERROR .
        IF AVAILABLE ap-inv THEN 
            FIND FIRST ap-invl NO-LOCK 
                WHERE ap-invl.company EQ ap-inv.company
                AND ap-invl.i-no EQ ap-inv.i-no
                AND ap-invl.po-no EQ ttImportAP.LinePONumber
                AND ap-invl.po-line EQ ttImportAP.LinePOLine
                NO-ERROR.
        IF AVAILABLE ap-inv AND AVAILABLE ap-invl THEN 
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
        ELSE IF AVAILABLE ap-inv AND NOT AVAILABLE ap-invl THEN 
                ASSIGN 
                    opcNote = "Add record - New Line to Existing Invoice"
                    .
            ELSE 
                ASSIGN 
                    opcNote = "Add record"
                    .
        
    END.
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ttImportAP.TaxGroup NE "" THEN 
        DO:
            FIND FIRST stax NO-LOCK 
                WHERE stax.company EQ ttImportAP.Company
                AND stax.tax-group EQ ttImportAP.TaxGroup
                NO-ERROR.
            IF NOT AVAILABLE stax THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid TaxGroup"
                    .
                    
        END.
    END.
    IF NOT oplValid THEN DELETE ttImportAP.
    
END PROCEDURE.

PROCEDURE pCreateNewInvoice:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AP invoice, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    DEFINE INPUT PARAMETER ipcVendor AS CHARACTER.
    DEFINE INPUT PARAMETER ipcInvoice AS CHARACTER.
    DEFINE INPUT PARAMETER ipdtInvDate AS DATE.
    DEFINE OUTPUT PARAMETER opriAPInv AS ROWID.
    
    CREATE ap-inv.
    ASSIGN
        ap-inv.company  = ipcCompany
        ap-inv.inv-no   = ipcInvoice
        ap-inv.inv-date = TODAY
        ap-inv.vend-no  = ipcVendor
        .
    IF ipdtInvDate NE ? THEN 
        ap-inv.inv-date = DATE(ipdtInvDate).                     
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

PROCEDURE pCreateNewInvoiceLine:
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

PROCEDURE pExportData:
/*------------------------------------------------------------------------------
 Purpose:  Runs the Export Data Program for AP
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE pInitialize:
    /*------------------------------------------------------------------------------
     Purpose: Initializes the specific Column Mapping for APs   
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLoadFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFields     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabels     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataTypes  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidths     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormats    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndexStart AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportAP.
    EMPTY TEMP-TABLE ttImportMap.
    
    iIndexStart = 1 + giIndexOffset.
    cWidths    = "60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60".
    
    IF ipcLoadFile EQ '' THEN 
    DO:
        ASSIGN 
            cFields    = ""
            cDataTypes = ""
            cFormats   = ""
            cLabels    = ""
            .
        DO iIndex = iIndexStart TO TEMP-TABLE ttImportAP:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
            ASSIGN 
                cFields    = cFields + TEMP-TABLE ttImportAP:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):NAME + ","
                cDataTypes = cDataTypes + TEMP-TABLE ttImportAP:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):DATA-TYPE + ","
                cFormats   = cFormats + TEMP-TABLE ttImportAP:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):FORMAT + ","
                cLabels    = cLabels + TEMP-TABLE ttImportAP:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):COLUMN-LABEL + ","
                .
            
        
        END.
        ASSIGN 
            cFields    = TRIM(cFields,",")
            cDataTypes = TRIM(cDataTypes,",")
            cFormats   = TRIM(cFormats,",")
            cLabels    = TRIM(cLabels,",")
            .
        DO iIndex = 1 TO NUM-ENTRIES(cFields):
            CREATE ttImportMap.
            ASSIGN 
                ttImportMap.cType         = "AP"
                ttImportMap.cLabel        = ENTRY(iIndex,cFields)
                ttImportMap.iIndex        = iIndex
                ttImportMap.iImportIndex  = iIndex
                ttImportMap.cDataType     = ENTRY(iIndex,cDataTypes)
                ttImportMap.cColumnLabel  = ENTRY(iIndex,cLabels)
                ttImportMap.cColumnFormat = ENTRY(iIndex,cFormats)
                .
            IF iIndex LE NUM-ENTRIES(cWidths)  THEN 
                ttImportMap.iColumnWidth = INT(ENTRY(iIndex,cWidths)).
        END. 
    
    END.
    ELSE 
    DO:
    /*Load from Config File provided*/
    END.

END PROCEDURE.

PROCEDURE pProcessImport:
    /*------------------------------------------------------------------------------
     Purpose: Processes the temp-table already loaded and returns counts
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiUpdated AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAdded AS INTEGER NO-UNDO.

    DEFINE VARIABLE riAPInv  AS ROWID. 
    DEFINE VARIABLE riAPInvl AS ROWID. 
    
    FOR EACH ttImportAP NO-LOCK:
        IF ttImportAP.VendorID EQ "" THEN NEXT.
        IF ttImportAP.InvoiceNo EQ "" THEN NEXT.
        
        opiAdded = opiAdded + 1.
        
        /*if found, add another line to existing header - otherwise, create a new header*/
        FIND FIRST ap-inv NO-LOCK
            WHERE ap-inv.company EQ ttImportAP.Company
            AND ap-inv.inv-no EQ ttImportAP.InvoiceNo
            AND ap-inv.vend-no EQ ttImportAP.VendorID
            NO-ERROR.
        IF NOT AVAILABLE ap-inv THEN /*create a new one*/
        DO:
            RUN pCreateNewInvoice (ttImportAP.Company, ttImportAP.VendorID, ttImportAP.InvoiceNo, ttImportAP.InvoiceDate, OUTPUT riApInv).
            FIND ap-inv EXCLUSIVE-LOCK
                WHERE ROWID(ap-inv) EQ riAPInv
                NO-ERROR.
            IF NOT AVAILABLE ap-inv THEN NEXT.    
                    
            /*Override defaults with imported values for header*/
            IF ttImportAP.InvoiceDate NE ?THEN 
                ap-inv.inv-date =  ttImportAP.InvoiceDate.
            IF ttImportAP.TaxGroup NE "" THEN  
            DO:
                FIND FIRST stax NO-LOCK 
                    WHERE stax.company EQ ttImportAP.Company
                    AND stax.tax-group EQ ttImportAP.TaxGroup
                    NO-ERROR.
                IF AVAILABLE stax THEN 
                    ap-inv.tax-gr = ttImportAP.TaxGroup.
            END.
            IF ttImportAP.DueDate NE ? THEN 
                ap-inv.due-date = ttImportAP.DueDate.
            IF ttImportAP.DiscountDays NE 0 THEN 
                ap-inv.disc-days = ttImportAP.DiscountDays.
            IF ttImportAP.Discount NE 0 THEN 
                ap-inv.disc-% = ttImportAP.Discount.
    
        END. /*not available ap-inv*/
        RUN pCreateNewInvoiceLine (ROWID(ap-inv), OUTPUT riAPInvl).
        FIND ap-invl EXCLUSIVE-LOCK 
            WHERE ROWID(ap-invl) EQ riAPInvl
            NO-ERROR.
        IF NOT AVAILABLE ap-invl THEN NEXT.
                
        /*Override defaults with imported values for line*/ 
        ASSIGN 
            ap-invl.tax = ttImportAP.LineTax
            ap-invl.amt = ttImportAP.LineAmount
            .
        IF ttImportAP.LineQuantity NE 0 THEN 
            ap-invl.qty = ttImportAP.LineQuantity.
        ELSE 
            ap-invl.qty = 1.
        IF ttImportAP.LineQuantityUom NE "" THEN 
            ap-invl.cons-uom = ttImportAP.LineQuantityUOM.
        ELSE 
            ap-invl.cons-uom = "EA".
        IF ttImportAP.LinePrice NE 0 THEN 
            ap-invl.unit-pr = ttImportAP.LinePrice.
        ELSE 
            ap-invl.unit-pr = ap-invl.amt.
        IF ttImportAP.LinePriceUom NE "" THEN 
            ap-invl.pr-qty-uom = ttImportAP.LinePriceUom.
        ELSE 
            ap-invl.pr-qty-uom = "EA".
        IF ttImportAP.LinePONumber NE 0 THEN 
        DO:
            ap-invl.po-no = ttImportAP.LinePONumber.
            RUN pUpdateInvoiceLineFromPO(ROWID(ap-inv)).
        END.
        IF ttImportAP.LinePOLine NE 0 THEN
            ap-invl.po-line = ttImportAP.LinePOLine.
        IF ttImportAP.LineAccount NE "" THEN 
            ap-invl.actnum = ttImportAP.LineAccount.
                                                            
        RUN pRecalculateInvoiceHeader (ROWID(ap-inv), NO).   
    END.
    

END PROCEDURE.

PROCEDURE pRecalculateInvoiceHeader:
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

PROCEDURE pUpdateInvoiceLineFromPO:
    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriAPInvl AS ROWID NO-UNDO.
        
/*    DEFINE VARIABLE v-qty        AS DECIMAL   NO-UNDO.                                          */
/*    DEFINE VARIABLE cAccountVend AS CHARACTER NO-UNDO.                                          */
/*    DEFINE VARIABLE v-dscr       AS CHARACTER NO-UNDO.                                          */
/*    DEFINE VARIABLE v-tmp-qty    LIKE ap-invl.qty NO-UNDO.                                      */
/*                                                                                                */
/*    DEFINE VARIABLE v-ext-cost   AS DECIMAL   NO-UNDO.                                          */
/*    DEFINE VARIABLE v-qty-uom    AS CHARACTER NO-UNDO.                                          */
/*    DEFINE VARIABLE v-out-qty    AS DECIMAL   NO-UNDO.                                          */
/*    DEFINE VARIABLE v-out-cost   AS DECIMAL   NO-UNDO.                                          */
/*    DEFINE VARIABLE v-setup-per  AS DECIMAL   NO-UNDO.                                          */
/*                                                                                                */
/*    DEFINE VARIABLE lSetup       AS LOGICAL   NO-UNDO.                                          */
/*                                                                                                */
/*    DEFINE BUFFER bf-ap-inv  FOR ap-inv.                                                        */
/*    DEFINE BUFFER bf-ap-invl FOR ap-invl.                                                       */
/*                                                                                                */
/*    DEFINE VARIABLE v-corr AS LOG.                                                              */
/*                                                                                                */
/*                                                                                                */
/*    FIND bf-ap-invl EXCLUSIVE-LOCK                                                              */
/*        WHERE ROWID(bf-ap-invl) EQ ipriAPInvl NO-ERROR.                                         */
/*    FIND FIRST bf-ap-inv OF bf-ap-invl no-lock                                                  */
/*    FIND FIRST vend NO-LOCK                                                                     */
/*        WHERE vend.company EQ bf-ap-invl.company                                                */
/*        AND vend.vend-no EQ bf-ap-inv.vend-no                                                   */
/*        NO-ERROR.                                                                               */
/*                                                                                                */
/*    FIND FIRST sys-ctrl                                                                         */
/*        WHERE sys-ctrl.company EQ bf-ap-invl.company                                            */
/*        AND sys-ctrl.name    EQ "MSFCALC"                                                       */
/*        NO-LOCK NO-ERROR.                                                                       */
/*    ASSIGN                                                                                      */
/*        cAccountVend = IF AVAILABLE vend THEN vend.actnum ELSE ""                               */
/*        v-dscr       = "".                                                                      */
/*                                                                                                */
/*    IF cAccountVend EQ "" THEN                                                                  */
/*        cAccountVend = v-ap-pur.                                                                */
/*                                                                                                */
/*    FOR EACH tt-pol                                                                             */
/*        WHERE tt-pol.selekt                                                                     */
/*        AND tt-pol.qty-to-inv NE 0,                                                             */
/*        FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK                             */
/*        BREAK BY po-ordl.po-no:                                                                 */
/*                                                                                                */
/*        FIND FIRST po-ord NO-LOCK                                                               */
/*            WHERE po-ord.company EQ po-ordl.company                                             */
/*            AND po-ord.po-no   EQ po-ordl.po-no                                                 */
/*            NO-ERROR.                                                                           */
/*                                                                                                */
/*        CREATE tt-ap-invl.                                                                      */
/*        ASSIGN                                                                                  */
/*            tt-ap-invl.i-no    = ap-inv.i-no                                                    */
/*            tt-ap-invl.actnum  = cAccountVend                                                   */
/*            tt-ap-invl.company = ap-inv.company                                                 */
/*            tt-ap-invl.vend-no = ap-inv.vend-no                                                 */
/*            tt-ap-invl.dscr    = v-dscr                                                         */
/*            tt-ap-invl.loc     = ap-inv.loc                                                     */
/*            tt-ap-invl.period  = ap-inv.period                                                  */
/*            tt-ap-invl.posted  = ap-inv.posted                                                  */
/*            tt-ap-invl.tax     = ap-inv.tax-gr NE ""                                            */
/*            .                                                                                   */
/*        IF aptax-chr = "ITEM" THEN                                                              */
/*        DO:                                                                                     */
/*            FIND ITEM WHERE ITEM.company = g_company                                            */
/*                AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.                                  */
/*            IF AVAILABLE ITEM THEN tt-ap-invl.tax = ITEM.tax-rcpt .                             */
/*        END.                                                                                    */
/*                                                                                                */
/*        ASSIGN                                                                                  */
/*            tt-ap-invl.po-no      = (po-ord.po-no)                                              */
/*            tt-ap-invl.LINE       = (po-ordl.LINE + (po-ord.po-no * 1000) ) /* ap/invline.i 1 */*/
/*            tt-ap-invl.dscr       = po-ordl.i-name                                              */
/*            tt-ap-invl.unit-pr    = (po-ordl.cost)                                              */
/*            tt-ap-invl.pr-qty-uom = po-ordl.pr-uom                                              */
/*            tt-ap-invl.cons-uom   = po-ordl.pr-qty-uom                                          */
/*            v-wid                 = po-ordl.s-wid                                               */
/*            v-len                 = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len  */
/*            v-dep                 = 0                                                           */
/*            v-bwt                 = 0.                                                          */
/*                                                                                                */
/*        IF tt-ap-invl.cons-uom EQ "ROLL" THEN tt-ap-invl.cons-uom = "LF".                       */
/*                                                                                                */
/*        IF po-ordl.item-type AND appaper-chr NE "PO UOM" AND                                    */
/*            CAN-FIND(FIRST item                                                                 */
/*            WHERE item.company EQ cocode                                                        */
/*            AND item.i-no    EQ po-ordl.i-no                                                    */
/*            AND item.mat-type EQ "P")          THEN                                             */
/*            tt-ap-invl.cons-uom = appaper-chr.                                                  */
/*                                                                                                */
/*        RELEASE prod.                                                                           */
/*        RELEASE costtype.                                                                       */
/*                                                                                                */
/*        IF po-ordl.item-type EQ NO                          AND                                 */
/*            (fgpostgl EQ "AllItems" OR fgpostgl EQ "POOnly") THEN                               */
/*        DO:                                                                                     */
/*            FIND FIRST itemfg                                                                   */
/*                WHERE itemfg.company EQ po-ordl.company                                         */
/*                AND itemfg.i-no    EQ po-ordl.i-no                                              */
/*                NO-LOCK NO-ERROR.                                                               */
/*                                                                                                */
/*            IF AVAILABLE itemfg THEN                                                            */
/*                FIND FIRST prodl                                                                */
/*                    WHERE prodl.company EQ itemfg.company                                       */
/*                    AND prodl.procat  EQ itemfg.procat                                          */
/*                    AND CAN-FIND(FIRST prod                                                     */
/*                    WHERE prod.company EQ cocode                                                */
/*                    AND prod.prolin  EQ prodl.prolin)                                           */
/*                    NO-LOCK NO-ERROR.                                                           */
/*                                                                                                */
/*            IF AVAILABLE prodl THEN                                                             */
/*                FIND FIRST prod                                                                 */
/*                    WHERE prod.company EQ prodl.company                                         */
/*                    AND prod.prolin  EQ prodl.prolin                                            */
/*                    NO-LOCK NO-ERROR.                                                           */
/*        END.                                                                                    */
/*                                                                                                */
/*        ELSE                                                                                    */
/*            IF po-ordl.item-type AND v-rmpostgl-char EQ "ALLITEMS" THEN                         */
/*            DO:                                                                                 */
/*                FIND FIRST item                                                                 */
/*                    WHERE item.company EQ cocode                                                */
/*                    AND item.i-no    EQ po-ordl.i-no                                            */
/*                    NO-LOCK NO-ERROR.                                                           */
/*                                                                                                */
/*                IF AVAILABLE item AND item.stocked THEN                                         */
/*                    FIND FIRST costtype                                                         */
/*                        WHERE costtype.company   EQ cocode                                      */
/*                        AND costtype.cost-type EQ item.cost-type                                */
/*                        NO-LOCK NO-ERROR.                                                       */
/*            END.                                                                                */
/*                                                                                                */
/*        IF AVAILABLE prod AND prod.wip-mat NE "" THEN                                           */
/*            tt-ap-invl.actnum = prod.wip-mat.                                                   */
/*        ELSE                                                                                    */
/*            IF AVAILABLE costtype AND costtype.ap-accrued NE "" THEN                            */
/*                tt-ap-invl.actnum = costtype.ap-accrued.                                        */
/*            ELSE                                                                                */
/*                IF lv-po-glnum THEN tt-ap-invl.actnum = po-ordl.actnum.                         */
/*                ELSE                                                                            */
/*                DO:                                                                             */
/*                    IF v-vend-actnum EQ "" THEN                                                 */
/*                    DO:                                                                         */
/*                        FIND FIRST vend WHERE vend.company EQ cocode                            */
/*                            AND vend.vend-no EQ po-ord.vend-no                                  */
/*                            NO-LOCK NO-ERROR.                                                   */
/*                        IF AVAILABLE vend THEN v-vend-actnum = vend.actnum.                     */
/*                    END.                                                                        */
/*                    tt-ap-invl.actnum = v-vend-actnum.                                          */
/*                END.                                                                            */
/*                                                                                                */
/*        IF v-vend-actnum EQ "" THEN                                                             */
/*            v-vend-actnum = v-ap-pur.                                                           */
/*                                                                                                */
/*        IF tt-ap-invl.actnum EQ "" THEN tt-ap-invl.actnum = v-vend-actnum.                      */
/*                                                                                                */
/*        FIND FIRST ITEM WHERE item.company EQ cocode                                            */
/*            AND item.i-no    EQ po-ordl.i-no                                                    */
/*            AND po-ordl.item-type                                                               */
/*            NO-LOCK NO-ERROR.                                                                   */
/*        IF AVAILABLE item THEN                                                                  */
/*        DO:                                                                                     */
/*            v-dep = item.s-dep.                                                                 */
/*            {po/pol-dims.i}                                                                     */
/*        END.                                                                                    */
/*                                                                                                */
/*        IF NOT po-ordl.item-type AND tt-ap-invl.cons-uom NE "EA" THEN                           */
/*            RUN sys/ref/convquom.p ("EA", tt-ap-invl.cons-uom,                                  */
/*                v-bwt, v-len, v-wid, v-dep,                                                     */
/*                tt-pol.qty-to-inv, OUTPUT tt-pol.qty-to-inv).                                   */
/*                                                                                                */
/*        ASSIGN                                                                                  */
/*            tt-ap-invl.qty = tt-pol.qty-to-inv.                                                 */
/*                                                                                                */
/*        IF tt-pol.qty-to-inv-uom NE "" AND                                                      */
/*            tt-pol.qty-to-inv-uom NE po-ordl.pr-qty-uom THEN                                    */
/*            RUN sys/ref/convquom.p (tt-pol.qty-to-inv-uom, po-ordl.pr-qty-uom,                  */
/*                v-bwt, v-len, v-wid, v-dep,                                                     */
/*                tt-ap-invl.qty, OUTPUT v-tmp-qty).                                              */
/*        ELSE                                                                                    */
/*            v-tmp-qty = tt-ap-invl.qty.                                                         */
/*        IF LOOKUP(po-ordl.pr-uom,"L,LOT") GT 0 THEN                                             */
/*        DO:                                                                                     */
/*            ASSIGN                                                                              */
/*                tt-ap-invl.amt        = po-ordl.t-cost                                          */
/*                tt-ap-invl.unit-pr    = po-ordl.cost                                            */
/*                tt-ap-invl.pr-qty-uom = po-ordl.pr-uom.                                         */
/*        END.                                                                                    */
/*        ELSE                                                                                    */
/*        DO:                                                                                     */
/*            /*Calculate proportionate amt based on total cost, not including setup*/            */
/*            tt-ap-invl.amt = (po-ordl.t-cost - po-ordl.setup) * ( v-tmp-qty / po-ordl.ord-qty ).*/
/*                                                                                                */
/*            /*Add setup charges back only if no other ap-invl exist for this po line*/          */
/*            lSetup = NO.                                                                        */
/*            FIND FIRST bf-ap-invl                                                               */
/*                WHERE bf-ap-invl.company EQ po-ordl.company                                     */
/*                AND bf-ap-invl.po-no EQ po-ordl.po-no                                           */
/*                AND bf-ap-invl.po-line EQ po-ordl.LINE                                          */
/*                AND bf-ap-invl.item-no EQ po-ordl.i-no                                          */
/*                NO-LOCK NO-ERROR.                                                               */
/*            IF NOT AVAILABLE bf-ap-invl THEN lSetup = YES.                                      */
/*            IF lSetup THEN tt-ap-invl.amt = tt-ap-invl.amt + po-ordl.setup.                     */
/*                                                                                                */
/*            tt-ap-invl.unit-pr = tt-ap-invl.amt / tt-ap-invl.qty.                               */
/*                                                                                                */
/*            IF tt-ap-invl.cons-uom NE tt-ap-invl.pr-qty-uom THEN                                */
/*                RUN sys/ref/convcuom.p (tt-ap-invl.cons-uom, tt-ap-invl.pr-qty-uom,             */
/*                    v-bwt, v-len, v-wid, v-dep,                                                 */
/*                    tt-ap-invl.unit-pr, OUTPUT tt-ap-invl.unit-pr).                             */
/*            tt-ap-invl.unit-pr = ROUND(tt-ap-invl.unit-pr, 2).                                  */
/*        END.                                                                                    */
/*        IF v-len EQ 0 THEN v-len = 12.                                                          */
/*        IF v-wid EQ 0 THEN v-wid = 12.                                                          */
/*                                                                                                */
/*        tt-ap-invl.sf-sht = IF v-corr THEN (v-len * v-wid * .007)                               */
/*        ELSE (v-len * v-wid / 144).                                                             */
/*                                                                                                */
/*        IF NOT AVAILABLE item             AND                                                   */
/*            (v-len EQ 0 OR v-wid EQ 0) THEN                                                     */
/*        DO:                                                                                     */
/*            FIND FIRST itemfg                                                                   */
/*                WHERE itemfg.company EQ cocode                                                  */
/*                AND itemfg.i-no    EQ po-ordl.i-no                                              */
/*                AND NOT po-ordl.item-type                                                       */
/*                NO-LOCK NO-ERROR.                                                               */
/*            IF AVAILABLE itemfg THEN tt-ap-invl.sf-sht = (itemfg.t-sqft).                       */
/*        END.                                                                                    */
/*                                                                                                */
/*        IF tt-ap-invl.cons-uom EQ "EA" THEN v-qty = tt-ap-invl.qty.                             */
/*        ELSE                                                                                    */
/*            RUN sys/ref/convquom.p(tt-ap-invl.cons-uom, "EA",                                   */
/*                v-bwt, v-len, v-wid, v-dep,                                                     */
/*                tt-ap-invl.qty, OUTPUT v-qty).                                                  */
/*                                                                                                */
/*        tt-ap-invl.amt-msf = (tt-ap-invl.sf-sht * v-qty / 1000).                                */
/*                                                                                                */
/*    END.                                                                                        */

END PROCEDURE.

