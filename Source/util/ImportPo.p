
/*------------------------------------------------------------------------
    File        : ImportPrep.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Prep and Die	

    Author(s)   : Sewa Singh
    Created     : Fri May 8 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportPo
    FIELD Company                 AS CHARACTER 
    FIELD Location                AS CHARACTER
    FIELD PoNoGroup               AS CHARACTER
    FIELD vend-no                 AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Vendor #" HELP "Required - Size:1"
    FIELD po-no                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "PO #" HELP "Optional - Integer or <AUTO> to auto-number.  Use <AUTO>#### where # is a unique group number. " 
    FIELD iline                   AS INTEGER FORMAT ">>>" COLUMN-LABEL "Po Line" HELP "Optional - Integer"
    FIELD due-date                AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Due Date" HELP "Optional - Date"
    FIELD ship-id                 AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Ship ID" HELP "Optional - Size:8"
    FIELD ship-name               AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Ship Name" HELP "Optional - Size:30"
    FIELD ship-addr1              AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Ship Address 1" HELP "Optional - Size:30"
    FIELD ship-addr2              AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Ship Address 2" HELP "Optional - Size:30"
    FIELD ship-city               AS CHARACTER FORMAT "X(16)" COLUMN-LABEL "Ship City" HELP "Optional - Size:16"
    FIELD ship-state              AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Ship State" HELP "Optional - Size:2"
    FIELD ship-zip                AS CHARACTER FORMAT "xxxxx-xxxx" COLUMN-LABEL "Ship Zip" HELP "Optional - Size:9"
    FIELD carrier                 AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Shipping Carrier" HELP "Optional - Size:5"
    FIELD t-freight               AS DECIMAL FORMAT "->>,>>9.99" COLUMN-LABEL "Total Freight" HELP "Optional - Decimal"
    FIELD frt-pay                 AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Freight Payment" HELP "Required - P,C or B"
    FIELD fob-code                AS CHARACTER   FORMAT "X(10)" COLUMN-LABEL "FOB" HELP "Required - Dest or ORIG"  
    FIELD tax-gr                  AS CHARACTER   FORMAT "x(3)" COLUMN-LABEL "Tax Code" HELP "Optional - Size:3"
    FIELD tax                     AS DECIMAL FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Tax" HELP "Optional - decimal"
    FIELD line-tax                AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Taxable" HELP "Optional - Yes or No(Blank No)"
    FIELD terms                   AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Payment Terms" HELP "Required - Size:5"
    FIELD t-cost                  AS DECIMAL FORMAT "->,>>>,>>9.99<<" COLUMN-LABEL "Total Cost" HELP "Optional - Decimal" 
    FIELD job-no                  AS CHARACTER FORMAT "X(6)" COLUMN-LABEL "Job #" HELP "Optional - Size:6"
    FIELD item-type               AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Item Type" HELP "Required - RM or FG"
    FIELD i-no                    AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Item #" HELP "Optional - Size:15"
    FIELD i-name                  AS CHARACTER   FORMAT "x(30)" COLUMN-LABEL "Item Name" HELP "Optional - Size:30"
    FIELD s-wid                   AS DECIMAL FORMAT ">>9.9999" COLUMN-LABEL "Width" HELP "Optional - Decimal"  
    FIELD s-len                   AS DECIMAL FORMAT ">>9.9999" COLUMN-LABEL "Length" HELP "Optional - Decimal"
    FIELD s-num                   AS INTEGER FORMAT ">9" COLUMN-LABEL "Sheet #" HELP "Optional - Integer"
    FIELD b-num                   AS INTEGER FORMAT ">9" COLUMN-LABEL "Blank #" HELP "Optional - Integer"
    FIELD dscr                    AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Description 1" HELP "Optional - Size:30"
    FIELD dscr2                   AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Description 2" HELP "Optional - Size:30"
    FIELD vend-i-no               AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Vendor Item #" HELP "Optional - Size:15"
    FIELD ord-qty                 AS DECIMAL FORMAT "->>>,>>>,>>9.9<<<<<" COLUMN-LABEL "Order Qty" HELP "Optional - Decimal"
    FIELD pr-qty-uom              AS CHARACTER   FORMAT "x(3)" COLUMN-LABEL "Order UOM" HELP "Optional - Size:3"
    FIELD t-rec-qty               AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<<<<" COLUMN-LABEL "Qty Received" HELP "Optional - Decimal"
    FIELD cons-uom                AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Rec. UOM" HELP "Optional - Size:3"   
    FIELD cost                    AS DECIMAL   FORMAT "->,>>>,>>9.99<<<<" COLUMN-LABEL "Item Cost" HELP "Optional - Decimal"
    FIELD pr-uom                  AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM" HELP "Optional - Size:3"
    FIELD buyer                   AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Buyer" HELP "Optional - Size:10"
    FIELD stat                    AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Status" HELP "Optional - Size:10"
    FIELD linestat                AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Item Status" HELP "Optional - Size:2"
    FIELD printed                 AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Printed" HELP "Optional - Yes Or No(Blank N)"
    FIELD opened                  AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Opened" HELP "Optional - Yes or No (Blank- No)"
    FIELD type                    AS CHARACTER   FORMAT "x(10)" COLUMN-LABEL "Type" HELP "Required - R,D or S"
    FIELD contact                 AS CHARACTER   FORMAT "x(25)" COLUMN-LABEL "Contact" HELP "Optional - Size:25"
    FIELD po-date                 AS CHARACTER   FORMAT "x(10)" COLUMN-LABEL "PO Date" HELP "Optional - Date"
    FIELD last-ship-date          AS CHARACTER   FORMAT "x(10)" COLUMN-LABEL "Last Ship Date" HELP "Optional - Date"
    FIELD setup                   AS DECIMAL FORMAT ">>,>>9.99" COLUMN-LABEL "Setup" HELP "Optional - Decimal"
    FIELD disc                    AS DECIMAL FORMAT "->>>,>>9.99" COLUMN-LABEL "Discount" HELP "Optional - Decimal"
    FIELD actnum                  AS CHARACTER FORMAT "x(25)" COLUMN-LABEL "GL Number" HELP "Optional - Size:25"
    FIELD over-pct                AS DECIMAL FORMAT ">>9.99%" COLUMN-LABEL "Overrun" HELP "Optional - Decimal"
    FIELD under-pct               AS DECIMAL   FORMAT ">>9.99%" COLUMN-LABEL "Underrun" HELP "Optional - Decimal"
    FIELD cust-no                 AS CHARACTER   FORMAT "x(8)" COLUMN-LABEL "Customer #" HELP "Optional - Size:8"
    FIELD ord-no                  AS INTEGER   FORMAT ">>>>>9" COLUMN-LABEL "Order #" HELP "Optional - Integer"
       
    
    .
DEFINE VARIABLE gcAutoIndicator AS CHARACTER NO-UNDO INITIAL "<AUTO>".    
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 3. /*Set to 3 to skip Company Location and PoNoGroup field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportPo"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iNextPo LIKE po-ctrl.next-po-no NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ttImportPo FOR ttImportPo.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cPoNumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoNoGroup AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAutoNumber AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lNewGroup AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dTotCost AS DECIMAL NO-UNDO .
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.
    DEFINE BUFFER bf-po-ord FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    DEFINE BUFFER bf-ttImportPo FOR ttImportPo.
    
     ASSIGN 
        cPoNoGroup = ""
        lAutoNumber = NO
        cPoNumber = ipbf-ttImportPo.po-no
        .
    IF cPoNumber BEGINS gcAutoIndicator THEN DO:
        /*Auto numbering logic*/
        
        /*Get the PoNoGroup as string to the right of the indicator*/
        IF LENGTH(cPoNumber) NE LENGTH(gcAutoIndicator) THEN 
            cPoNoGroup = SUBSTRING(cPoNumber,LENGTH(gcAutoIndicator) + 1, LENGTH(cPoNumber) - LENGTH(gcAutoIndicator)).
        IF cPoNoGroup NE "" THEN 
            FIND FIRST bf-ttImportPo NO-LOCK
                 WHERE bf-ttImportPo.PoNoGroup EQ cPoNoGroup
                NO-ERROR.
        IF AVAILABLE bf-ttImportPo THEN
            cPoNumber = bf-ttImportPo.po-no.
        ELSE 
            lAutoNumber = YES.
    END.    

    FIND FIRST bf-po-ord EXCLUSIVE-LOCK 
        WHERE bf-po-ord.company EQ ipbf-ttImportPo.Company
        AND bf-po-ord.po-no EQ integer(cPoNumber)
        NO-ERROR.

    IF NOT AVAILABLE bf-po-ord THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE bf-po-ord.
        
        RUN sys/ref/asiseq.p (ipbf-ttImportPo.Company,'po_seq',OUTPUT iNextPO) NO-ERROR.

        ASSIGN bf-po-ord.company        = ipbf-ttImportPo.Company
               bf-po-ord.po-no          = inextPO         
               bf-po-ord.po-date        = TODAY
               bf-po-ord.loc            = ipbf-ttImportPo.Location
               bf-po-ord.buyer          = USERID(LDBNAME(1))
               bf-po-ord.under-pct      = 10
               bf-po-ord.over-pct       = 10         
               bf-po-ord.due-date       = bf-po-ord.po-date + 
                                       IF WEEKDAY(bf-po-ord.po-date) EQ 6 THEN 3 ELSE 1
               bf-po-ord.last-ship-date = bf-po-ord.due-date                 
               bf-po-ord.user-id        = USERID(LDBNAME(1)) .  
               
               IF lAutoNumber AND cPoNoGroup NE "" THEN DO:
                 FIND CURRENT ipbf-ttImportPo EXCLUSIVE-LOCK.
                   ASSIGN 
                ipbf-ttImportpo.PoNoGroup = cPoNoGroup
                ipbf-ttImportpo.po-no = STRING(bf-po-ord.po-no)
                .
               FIND CURRENT ipbf-ttImportPo NO-LOCK.
        END.
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportPo.vend-no, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.vend-no).                                                   
    RUN pAssignValueC (ipbf-ttImportPo.ship-id, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.ship-id).                                                   
    RUN pAssignValueC (ipbf-ttImportPo.ship-name, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.ship-name).                               
    RUN pAssignValueC (ipbf-ttImportPo.ship-addr1, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.ship-addr[1]).                                                   
    RUN pAssignValueC (ipbf-ttImportPo.ship-addr2, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.ship-addr[2]).                                                   
    RUN pAssignValueC (ipbf-ttImportPo.ship-city, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.ship-city).                                       
    RUN pAssignValueC (ipbf-ttImportPo.ship-state, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.ship-state).                                         
    RUN pAssignValueC (ipbf-ttImportPo.ship-zip, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.ship-zip).                      
    RUN pAssignValueC (ipbf-ttImportPo.carrier, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.carrier).                                 
    RUN pAssignValueD (ipbf-ttImportPo.t-freight, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.t-freight).                                     
    RUN pAssignValueC (ipbf-ttImportPo.frt-pay, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.frt-pay).                                                 
    RUN pAssignValueC (ipbf-ttImportPo.fob-code, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.fob-code).                   
    RUN pAssignValueC (ipbf-ttImportPo.tax-gr, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.tax-gr).                                         
    RUN pAssignValueC (ipbf-ttImportPo.tax, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.tax).                                 
    RUN pAssignValueC (ipbf-ttImportPo.terms, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.terms).                                       
    RUN pAssignValueD (ipbf-ttImportPo.t-cost, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.t-cost).                                 
    RUN pAssignValueC (ipbf-ttImportPo.buyer, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.buyer).     
    RUN pAssignValueC (ipbf-ttImportPo.stat, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.stat).                                      
    RUN pAssignValueC (ipbf-ttImportPo.type, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.type).                             
    RUN pAssignValueC (ipbf-ttImportPo.contact, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.contact).                                 
    RUN pAssignValueCToDt (ipbf-ttImportPo.po-date, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.po-date).                                          
    RUN pAssignValueCToDt (ipbf-ttImportPo.last-ship-date, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.last-ship-date).
    RUN pAssignValueD (ipbf-ttImportPo.over-pct, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.over-pct).                                           
    RUN pAssignValueD (ipbf-ttImportPo.under-pct, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ord.under-pct).
    
    FIND FIRST bf-po-ordl EXCLUSIVE-LOCK
         WHERE bf-po-ordl.company EQ ipbf-ttImportPo.Company AND
               bf-po-ordl.po-no EQ bf-po-ord.po-no AND
               bf-po-ordl.LINE EQ ipbf-ttImportPo.iline NO-ERROR.
    IF NOT AVAIL bf-po-ordl THEN DO:
       FIND LAST po-ordl NO-LOCK WHERE
            po-ordl.company EQ bf-po-ord.company AND
            po-ordl.po-no EQ bf-po-ord.po-no
            NO-ERROR.

        iCount = IF AVAILABLE po-ordl THEN po-ordl.line + 1 ELSE 1.
   
        CREATE bf-po-ordl. 
        
        ASSIGN
            bf-po-ordl.company   = bf-po-ord.company
            bf-po-ordl.po-no     = bf-po-ord.po-no
            bf-po-ordl.stat      = "O"
            bf-po-ordl.ord-qty   = 1
            bf-po-ordl.cons-qty  = 1
            bf-po-ordl.line      = iCount
            bf-po-ordl.due-date  = bf-po-ord.due-date
            bf-po-ordl.over-pct  = bf-po-ord.over-pct
            bf-po-ordl.under-pct = bf-po-ord.under-pct
            bf-po-ordl.vend-no   = bf-po-ord.vend-no.

    END.       
        
    RUN pAssignValueCToDt (ipbf-ttImportPo.due-date, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.due-date).                         
    RUN pAssignValueC (ipbf-ttImportPo.job-no, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.job-no).                                 
    RUN pAssignValueC (ipbf-ttImportPo.i-no, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.i-no).                                          
    RUN pAssignValueC (ipbf-ttImportPo.i-name, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.i-name).                                     
    RUN pAssignValueD (ipbf-ttImportPo.s-wid, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.s-wid).                             
    RUN pAssignValueD (ipbf-ttImportPo.s-len, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.s-len).                                           
    RUN pAssignValueI (ipbf-ttImportPo.s-num, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.s-num).                                                 
    RUN pAssignValueI (ipbf-ttImportPo.b-num, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.b-num).                             
    RUN pAssignValueC (ipbf-ttImportPo.dscr, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.dscr[1]).                             
    RUN pAssignValueC (ipbf-ttImportPo.dscr2, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.dscr[2]).                             
    RUN pAssignValueC (ipbf-ttImportPo.vend-i-no, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.vend-i-no).                                           
    RUN pAssignValueD (ipbf-ttImportPo.ord-qty, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.ord-qty).                                                 
    RUN pAssignValueC (ipbf-ttImportPo.pr-qty-uom, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.pr-qty-uom).                                         
    RUN pAssignValueD (ipbf-ttImportPo.t-rec-qty, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.t-rec-qty).                     
    RUN pAssignValueC (ipbf-ttImportPo.cons-uom, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.cons-uom).                                           
    RUN pAssignValueD (ipbf-ttImportPo.cost, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.cost).                         
    RUN pAssignValueC (ipbf-ttImportPo.pr-uom, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.pr-uom).                 
    RUN pAssignValueC (ipbf-ttImportPo.stat, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.stat).                                           
    RUN pAssignValueD (ipbf-ttImportPo.setup, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.setup).                         
    RUN pAssignValueD (ipbf-ttImportPo.disc, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.disc).                                 
    RUN pAssignValueC (ipbf-ttImportPo.actnum, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.actnum).  
    
    RUN pAssignValueD (ipbf-ttImportPo.over-pct, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.over-pct).                                           
    RUN pAssignValueD (ipbf-ttImportPo.under-pct, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.under-pct).                         
    RUN pAssignValueC (ipbf-ttImportPo.cust-no, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.cust-no).                 
    RUN pAssignValueI (ipbf-ttImportPo.ord-no, iplIgnoreBlanks, INPUT-OUTPUT bf-po-ordl.ord-no).                         
    
    
    IF ipbf-ttImportPo.printed EQ "Yes" THEN
        ASSIGN bf-po-ord.printed = YES.
    ELSE bf-po-ord.printed = NO .
    IF ipbf-ttImportPo.opened EQ "Yes" THEN
        ASSIGN bf-po-ordl.opened = YES.
    ELSE bf-po-ordl.opened = NO .
    IF ipbf-ttImportPo.item-type EQ "RM" THEN
        ASSIGN bf-po-ordl.item-type = YES.
    ELSE bf-po-ordl.item-type = NO .


   RELEASE bf-po-ord .
   RELEASE bf-po-ordl .                                                                                                                                   
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportPo FOR ttImportPo.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE uom-list AS CHARACTER INIT "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
    DEFINE BUFFER bf-ttImportPo FOR ttImportPo.


    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportPo.vend-no EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Vendor is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportPo.terms EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Terms is Blank".
    END.   
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportPo.item-type EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Item Type is Blank(must be FG or RM)".
    END.
    
    /*Determine if Add or Update*/ 
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportPo.po-no BEGINS gcAutoIndicator THEN DO:
            opcNote = "Add Record - Auto Increment Po#"
            .
        END.          
        ELSE do:
            FIND FIRST bf-ttImportPo NO-LOCK 
                WHERE bf-ttImportPo.Company EQ ipbf-ttImportPo.Company
                AND bf-ttImportPo.po-no EQ ipbf-ttImportPo.po-no 
                AND bf-ttImportPo.iline EQ ipbf-ttImportPo.iline
                AND ROWID(bf-ttImportPo) NE ROWID(ipbf-ttImportPo)
                NO-ERROR.
            IF AVAILABLE bf-ttImportPo THEN 
              ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
        IF oplValid THEN do:
            FIND FIRST po-ord NO-LOCK 
                WHERE po-ord.company EQ ipbf-ttImportPo.Company
                AND po-ord.po-no EQ integer(ipbf-ttImportPo.po-no)
                NO-ERROR .
            IF AVAIL po-ord THEN
            DO: 
                IF NOT iplUpdateDuplicates THEN 
                    ASSIGN 
                        oplValid = NO
                        opcNote  = "Duplicate record exists"
                        .
                ELSE
                    ASSIGN 
                        oplValid = YES
                        opcNote = "Update existing record"
                        .        
            END.             
            ELSE
                ASSIGN
                    oplValid = NO
                    opcNote = "Add record" .
            END. 
        END.
    END.
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid THEN 
            RUN pIsValidFromList ("FOB", ipbf-ttImportPo.fob-code, "ORIG,Dest", OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid THEN 
            RUN pIsValidFromList ("Freight Payment", ipbf-ttImportPo.frt-pay, "P,C,B", OUTPUT oplValid, OUTPUT cValidNote).     
            
        IF oplValid THEN 
            RUN pIsValidFromList("Po# Type", ipbf-ttImportPo.TYPE, "R,D,S", OUTPUT oplValid, OUTPUT cValidNote).          
            
        IF oplValid THEN 
            RUN pIsValidFromList ("Po# Status", ipbf-ttImportPo.stat, "H,N,O,C,A,U,P", OUTPUT oplValid, OUTPUT cValidNote).              
            
        IF oplValid THEN 
            RUN pIsValidFromList ("Item Status", ipbf-ttImportPo.linestat, "H,N,O,C,A,U,P", OUTPUT oplValid, OUTPUT cValidNote).                          
            
       IF oplValid THEN 
            RUN pIsValidFromList ("Item Type", ipbf-ttImportPo.item-type, "RM,FG", OUTPUT oplValid, OUTPUT cValidNote).                               
            
        IF oplValid AND ipbf-ttImportPo.Carrier NE "" THEN 
            RUN pIsValidCarrier (ipbf-ttImportPo.Carrier, NO, ipbf-ttImportPo.Company, OUTPUT oplValid, OUTPUT cValidNote).            
            
        IF oplValid AND ipbf-ttImportPo.tax-gr NE "" THEN 
            RUN pIsValidTaxGroup (ipbf-ttImportPo.tax-gr, NO, ipbf-ttImportPo.Company, OUTPUT oplValid, OUTPUT cValidNote).            
            
        IF oplValid AND ipbf-ttImportPo.terms NE "" THEN 
            RUN pIsValidTerms (ipbf-ttImportPo.terms, NO, ipbf-ttImportPo.Company, OUTPUT oplValid, OUTPUT cValidNote).            
                
        IF oplValid AND ipbf-ttImportPo.actnum NE "" THEN 
            RUN pIsValidGLAccount (ipbf-ttImportPo.actnum, NO, ipbf-ttImportPo.Company, OUTPUT oplValid, OUTPUT cValidNote).        
            
        IF oplValid AND ipbf-ttImportPo.i-no NE "" AND ipbf-ttImportPo.item-type EQ "RM" THEN 
            RUN pIsValidRMITemID (ipbf-ttImportPo.i-no, NO, ipbf-ttImportPo.Company, OUTPUT oplValid, OUTPUT cValidNote).    

        IF oplValid AND ipbf-ttImportPo.i-no NE "" AND ipbf-ttImportPo.item-type EQ "FG" THEN 
            RUN pIsValidFGITemID (ipbf-ttImportPo.i-no, NO, ipbf-ttImportPo.Company, OUTPUT oplValid, OUTPUT cValidNote).        
        
        IF oplValid AND ipbf-ttImportPo.vend-no NE "" THEN 
            RUN pIsValidVendor (ipbf-ttImportPo.vend-no, NO, ipbf-ttImportPo.Company, OUTPUT oplValid, OUTPUT cValidNote).        
            
        IF ipbf-ttImportPo.item-type EQ "RM" THEN DO:
            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.company EQ ipbf-ttImportPo.Company
                AND ITEM.i-no EQ  ipbf-ttImportPo.i-no NO-ERROR.
            IF AVAILABLE ITEM THEN
                RUN sys/ref/uom-rm.p  (ITEM.mat-type, OUTPUT uom-list).
            END.
            ELSE DO:
                RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).
            END.

        IF oplValid THEN 
            RUN pIsValidUOM (ipbf-ttImportPo.pr-qty-uom, YES, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid THEN 
            RUN pIsValidFromList ("UOM",ipbf-ttImportPo.pr-qty-uom,uom-list, OUTPUT oplValid, OUTPUT cValidNote).     
            
        IF oplValid THEN 
            RUN pIsValidUOM (ipbf-ttImportPo.pr-uom, YES, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid THEN 
            RUN pIsValidFromList ("UOM",ipbf-ttImportPo.pr-uom,uom-list, OUTPUT oplValid, OUTPUT cValidNote).         

                
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
END PROCEDURE.


