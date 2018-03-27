
/*------------------------------------------------------------------------
    File        : ImportFG.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for FG Items	

    Author(s)   : BV
    Created     : Tues Mar 12  10:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportFG
    FIELD Company                 AS CHARACTER 
    FIELD Location                AS CHARACTER 
    FIELD FGItemID                AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "FG Item # [R15]" HELP "Required - Size:15" 
    FIELD CustomerID              AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Cust ID [RM8]" HELP "Required & Must Be Valid - Size:8"
    FIELD PartID                  AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Cust Part #[X15]" HELP "Defaults to FG Item # - Size:15"
    FIELD PartName                AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Name [30]" HELP "Optional - Size:30"
    FIELD PartDescription1        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Desc 1 [30]" HELP "Optional - Size:30"
    FIELD PartDescription2        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Desc 2 [30]" HELP "Optional - Size:30"
    FIELD PartDescription3        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Desc 3 [30]" HELP "Optional - Size:30"
    FIELD FGItemGroup             AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Group [15]" HELP "Optional - Size:15"
    FIELD ExemptFromDiscount      AS LOGICAL   FORMAT "Y/N" COLUMN-LABEL "Exempt from Discount [L]" HELP "Optional - Logical"
    FIELD Style                   AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Style [V10]" HELP "Optional - Field Validated - Size:10"
    FIELD DieID                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Die # [20]" HELP "Optional - Size:20"
    FIELD PlateID                 AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Plate # [20]" HELP "Optional - Size:20"
    FIELD CadID                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "CAD # [20]" HELP "Optional - Size:20"
    FIELD QCID                    AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "QC # [20]" HELP "Optional - Size:20"
    FIELD UPCID                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "UPC # [20]" HELP "Optional - Size:20"
    FIELD ReleaseSequence         AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Release Sequence [I]" HELP "Optional - Integer"
    FIELD Taxable                 AS LOGICAL   FORMAT "Y/N" COLUMN-LABEL "Taxable [L]" HELP "Optional - Logical"
    FIELD Varied                  AS LOGICAL   FORMAT "Y/N" COLUMN-LABEL "Varied [L]" HELP "Optional - Logical"
    FIELD ActiveStatus            AS CHARACTER FORMAT "x" COLUMN-LABEL "Active [X1]" HELP "Defaults to A (Active) vs. I (Inactive) - Size:1"
    FIELD Purchased               AS LOGICAL   FORMAT "Y/N" COLUMN-LABEL "Purchased [L]" HELP "Optional - Logical (Y=Purchased N=Manufactured)" 
    FIELD ShipByCase              AS LOGICAL   FORMAT "Y/N" COLUMN-LABEL "Ship By Case [L]" HELP "Optional - Logical (Y=Case N=Pallet)"
    FIELD StockItem               AS CHARACTER FORMAT "X" COLUMN-LABEL "Stock Item [X1]" HELP "Defaults based on rules (S=Stock Item C=Custom Box) Size:1"
    FIELD SellPrice               AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Price [N]" HELP "Optional - Decimal"
    FIELD SellPriceUOM            AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Price UOM [XV3]" HELP "Defaults to M - Field Validated - Size:3"  
    FIELD Currency                AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Currency [XV3]" HELP "Defaults to USD - Field Validated - Size:3"
    FIELD Category                AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Category [V5]" HELP "Optional - Field Validated - Size:5"
    FIELD OrderType               AS CHARACTER FORMAT "x" COLUMN-LABEL "Order Type [1]" HELP "Defauts to 'O'"
    FIELD Warehouse               AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse [XV5]" HELP "Defaults - Field Validated - Size:5"
    FIELD Bin                     AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Bin [XV8]" HELP "Defaults - Field Validated - Size:8"
    FIELD InventoryClass          AS CHARACTER FORMAT "x" COLUMN-LABEL "Inventory Class [1]" HELP "Optional - Size:1"
    FIELD CycleCountCode          AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Cycle Count Code [2]" HELP "Optional - Size:2"
    FIELD UnitCount               AS INTEGER   FORMAT ">>>>9" COLUMN-LABEL "Case Count [XI]" HELP "Defaults to 1 - Integer"
    FIELD UnitsPerPallet          AS INTEGER   FORMAT ">>>>9" COLUMN-LABEL "Units/Pall [XI]" HELP "Defaults to 1 - Integer"
    FIELD ProductionCode          AS CHARACTER FORMAT "x(6)" COLUMN-LABEL "Prod Code [6]" HELP "Optional - Size:6"   
    FIELD LbsPer100               AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Lbs/100 [XD]" HELP "Optional - Decimal"
    FIELD PackingNote             AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Pk Note [20]" HELP "Optional - Size:20"
    FIELD StdCostMaterial         AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Std Mat'l Cost [N]" HELP "Optional - Decimal"
    FIELD StdCostLabor            AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Std Labor Cost [N]" HELP "Optional - Decimal"
    FIELD StdCostVariableOverhead AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Std Var OH Cost [N]" HELP "Optional - Decimal"
    FIELD StdCostFixedOverhead    AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Std Fix OH Cost [N]" HELP "Optional - Decimal"
    FIELD StdCostFull             AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Full Cost [N]" HELP "Optional - Decimal"
    FIELD StdCostUOM              AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Cost UOM [XV3]" HELP "Defaults to M - Field Validated - Size:3"  
    FIELD Stocked                 AS LOGICAL   FORMAT "Y/N" COLUMN-LABEL "Stocked [L]" HELP "Optional - Logical"
    FIELD LengthBox               AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Length [N]" HELP "Optional - Decimal"
    FIELD WidthBox                AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Width [N]" HELP "Optional - Decimal"
    FIELD DepthBox                AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Depth [N]" HELP "Optional - Decimal"
    FIELD LengthBlank             AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Style Sq. In. Length [N]" HELP "Optional - Decimal"
    FIELD WidthBlank              AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Style Sq. In. Width [N]" HELP "Optional - Decimal"
    FIELD FactorInvoice           AS LOGICAL   FORMAT "Y/N" COLUMN-LABEL "Factor Invoice [L]" HELP "Optional - Logical"
    FIELD ReorderPoint            AS LOGICAL   FORMAT "Y/N" COLUMN-LABEL "Reorder Point [L]" HELP "Optional - Logical (Y=Reorder Point N=Lot Controlled)"
    FIELD ReorderLevel            AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Reorder Level [I]" HELP "Optional - Integer"
    FIELD OrderMinimum            AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Minimum Order [I]" HELP "Optional - Integer"
    FIELD OrderMaximum            AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Maximum Order [I]" HELP "Optional - Integer"
    FIELD PurchasedQuantityUOM    AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Purchased Quantity UOM [XV3]" HELP "Defaults - Field Validated - Size:3"  
    FIELD LeadTimeDays            AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Lead Time in Days [I]" HELP "Optional - Integer"
    FIELD BeginningDate           AS DATETIME  FORMAT "99/99/9999" COLUMN-LABEL "Beginning Date [D]" HELP "Optional - Date in MM/DD/YYYY"
    FIELD SalesRepID              AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Sales Rep [V3]" HELP "Optional - Field Validated - Size:3" /*Defaults to Customer SalesmanID*/
    FIELD SpecNote1Group          AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 1 Group [3]" HELP "Optional - Size:3"
    FIELD SpecNote1Title          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 1 Title [60]" HELP "Optional - Size:60"  
    FIELD SpecNote1Note           AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 1 Note [Large]" HELP "Optional - Size:Large"
    FIELD SpecNote2Group          AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 2 Group [3]" HELP "Optional - Size:3"
    FIELD SpecNote2Title          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 2 Title [60]" HELP "Optional - Size:60" 
    FIELD SpecNote2Note           AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 2 Note [Large]" HELP "Optional - Size:Large"
    FIELD SpecNote3Group          AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 3 Group [3]" HELP "Optional - Size:3"
    FIELD SpecNote3Title          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 3 Title [60]" HELP "Optional - Size:60" 
    FIELD SpecNote3Note           AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 3 Note [Large]" HELP "Optional - Size:Large"
    FIELD SpecNote4Group          AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 4 Group [3]" HELP "Optional - Size:3"
    FIELD SpecNote4Title          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 4 Title [60]" HELP "Optional - Size:60" 
    FIELD SpecNote4Note           AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 4 Note [Large]" HELP "Optional - Size:Large"
    FIELD SpecNote5Group          AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 5 Group [3]" HELP "Optional - Size:3"
    FIELD SpecNote5Title          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 5 Title [60]" HELP "Optional - Size:60" 
    FIELD SpecNote5Note           AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 5 Note [Large]" HELP "Optional - Size:Large"
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportFG"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFG FOR ttImportFG.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    DEFINE VARIABLE riNote AS ROWID NO-UNDO.

    FIND FIRST itemfg EXCLUSIVE-LOCK 
        WHERE itemfg.company EQ ipbf-ttImportFG.Company
        AND itemfg.i-no EQ ipbf-ttImportFG.FGItemID
        NO-ERROR.

    IF NOT AVAILABLE itemfg THEN 
    DO:
        CREATE itemfg.
        ASSIGN 
            iopiAdded        = iopiAdded + 1
            itemfg.company   = ipbf-ttImportFG.Company
            itemfg.loc       = ipbf-ttImportFG.Location
            itemfg.i-no      = ipbf-ttImportFG.FGItemID
        
            /*Refactor - Default values should come from rules in create.trg for itemfg*/
            itemfg.stat      = "A"
            itemfg.sell-uom  = "M"
            itemfg.curr-code = "USD"
            itemfg.type-code = "O"
            .
    END.
    ASSIGN 
        itemfg.cust-no      = ipbf-ttImportFG.CustomerID
        itemfg.part-no      = IF ipbf-ttImportFG.PartID EQ "" THEN itemfg.i-no ELSE ipbf-ttImportFG.PartID
        itemfg.i-name       = ipbf-ttImportFG.PartName
        itemfg.part-dscr1   = ipbf-ttImportFG.PartDescription1
        itemfg.part-dscr2   = ipbf-ttImportFG.PartDescription2
        itemfg.part-dscr3   = ipbf-ttImportFG.PartDescription3    
        itemfg.spare-char-1 = ipbf-ttImportFG.FGItemGroup
        itemfg.exempt-disc  = ipbf-ttImportFG.ExemptFromDiscount
        itemfg.style        = ipbf-ttImportFG.Style
        itemfg.die-no       = ipbf-ttImportFG.DieID
        itemfg.cad-no       = ipbf-ttImportFG.CadID
        itemfg.plate-no     = ipbf-ttImportFG.PlateID
        itemfg.spc-no       = ipbf-ttImportFG.QCID
        itemfg.upc-no       = ipbf-ttImportFG.UPCID
        itemfg.spare-int-2  = ipbf-ttImportFG.ReleaseSequence
        itemfg.taxable      = ipbf-ttImportFG.Taxable
        itemfg.spare-char-2 = IF ipbf-ttImportFG.Varied THEN "YES" ELSE "NO"
        itemfg.stat         = IF ipbf-ttImportFG.ActiveStatus EQ "I" THEN "I" ELSE "A" 
        itemfg.pur-man      = ipbf-ttImportFG.Purchased
        itemfg.ship-meth    = ipbf-ttImportFG.ShipByCase        
        itemfg.i-code       = IF ipbf-ttImportFG.StockItem EQ "S" THEN "S" ELSE "C"
        itemfg.sell-price   = ipbf-ttImportFG.SellPrice
        itemfg.sell-uom     = IF ipbf-ttImportFG.SellPriceUOM EQ "" THEN itemfg.sell-uom ELSE ipbf-ttImportFG.SellPriceUOM
        itemfg.curr-code[1] = IF ipbf-ttImportFG.Currency EQ "" THEN itemfg.curr-code[1] ELSE ipbf-ttImportFG.Currency
        itemfg.procat       = ipbf-ttImportFG.Category
        itemfg.type-code    = IF ipbf-ttImportFG.OrderType EQ "" THEN itemfg.type-code ELSE ipbf-ttImportFG.OrderType
        itemfg.def-loc      = IF ipbf-ttImportFG.Warehouse EQ "" THEN itemfg.def-loc ELSE ipbf-ttImportFG.Warehouse
        itemfg.def-loc-bin  = IF ipbf-ttImportFG.Bin EQ "" THEN itemfg.def-loc-bin ELSE ipbf-ttImportFG.Bin
        itemfg.class        = ipbf-ttImportFG.InventoryClass
        itemfg.cc-code      = ipbf-ttImportFG.CycleCountCode
        itemfg.case-count   = MAX(ipbf-ttImportFG.UnitCount,1)
        itemfg.case-pall    = MAX(ipbf-ttImportFG.UnitsPerPallet,1)
        itemfg.prod-code    = ipbf-ttImportFG.ProductionCode
        itemfg.weight-100   = ipbf-ttImportFG.LbsPer100
        itemfg.prod-notes   = ipbf-ttImportFG.PackingNote
        itemfg.std-mat-cost = ipbf-ttImportFG.StdCostMaterial
        itemfg.std-lab-cost = ipbf-ttImportFG.StdCostLabor
        itemfg.std-var-cost = ipbf-ttImportFG.StdCostVariableOverhead
        itemfg.std-fix-cost = ipbf-ttImportFG.StdCostFixedOverhead
        itemfg.std-tot-cost = itemfg.std-mat-cost + itemfg.std-lab-cost + itemfg.std-var-cost + itemfg.std-fix-cost
        itemfg.spare-dec-1  = ipbf-ttImportFG.StdCostFull
        itemfg.prod-uom     = IF ipbf-ttImportFG.StdCostUom EQ "" THEN itemfg.prod-uom ELSE ipbf-ttImportFG.StdCostUOM
        itemfg.stocked      = ipbf-ttImportFG.Stocked
        itemfg.l-score[50]  = ipbf-ttImportFG.LengthBox
        itemfg.w-score[50]  = ipbf-ttImportFG.WidthBox
        itemfg.d-score[50]  = ipbf-ttImportFG.DepthBox
        itemfg.t-len        = ipbf-ttImportFG.LengthBlank
        itemfg.t-wid        = ipbf-ttImportFG.WidthBlank
        itemfg.t-sqin       = itemfg.t-len * itemfg.t-wid
        itemfg.t-sqft       = itemfg.t-sqin / 144
        itemfg.spare-char-3 = ipbf-ttImportFG.SalesRepID
        itemfg.factored     = ipbf-ttImportFG.FactorInvoice
        itemfg.ord-policy   = ipbf-ttImportFG.ReorderPoint
        itemfg.ord-level    = ipbf-ttImportFG.ReorderLevel
        itemfg.ord-min      = ipbf-ttImportFG.OrderMinimum
        itemfg.ord-max      = ipbf-ttImportFG.OrderMaximum
        itemfg.pur-uom      = ipbf-ttImportFG.PurchasedQuantityUOM
        itemfg.lead-days    = ipbf-ttImportFG.LeadTimeDays
        itemfg.beg-date     = ipbf-ttImportFG.BeginningDate   
        .

    IF ipbf-ttImportFG.Category NE "" THEN 
    DO:
        FIND FIRST fgcat NO-LOCK 
            WHERE fgcat.company EQ ipbf-ttImportFG.Company
            AND fgcat.procat EQ ipbf-ttImportFG.Category
            NO-ERROR.
        IF AVAILABLE fgcat THEN 
            itemfg.procat-desc = fgcat.dscr.
    END.    

    IF ipbf-ttImportFG.SpecNote1Note NE "" THEN 
        RUN util/AddNote.p (itemfg.rec_key, ipbf-ttImportFG.SpecNote1Note, ipbf-ttImportFG.SpecNote1Title, ipbf-ttImportFG.SpecNote1Group, "S", OUTPUT riNote).
    IF ipbf-ttImportFG.SpecNote2Note NE "" THEN 
        RUN util/AddNote.p (itemfg.rec_key, ipbf-ttImportFG.SpecNote2Note, ipbf-ttImportFG.SpecNote2Title, ipbf-ttImportFG.SpecNote2Group, "S", OUTPUT riNote).
    IF ipbf-ttImportFG.SpecNote3Note NE "" THEN 
        RUN util/AddNote.p (itemfg.rec_key, ipbf-ttImportFG.SpecNote3Note, ipbf-ttImportFG.SpecNote3Title, ipbf-ttImportFG.SpecNote3Group, "S", OUTPUT riNote).
    IF ipbf-ttImportFG.SpecNote4Note NE "" THEN 
        RUN util/AddNote.p (itemfg.rec_key, ipbf-ttImportFG.SpecNote4Note, ipbf-ttImportFG.SpecNote4Title, ipbf-ttImportFG.SpecNote4Group, "S", OUTPUT riNote).
    IF ipbf-ttImportFG.SpecNote5Note NE "" THEN 
        RUN util/AddNote.p (itemfg.rec_key, ipbf-ttImportFG.SpecNote5Note, ipbf-ttImportFG.SpecNote5Title, ipbf-ttImportFG.SpecNote5Group, "S", OUTPUT riNote).

END PROCEDURE.

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFG FOR ttImportFG.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE NO-UNDO.
    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportFG FOR ttImportFG.

    RUN util/Validate.p PERSISTENT SET hdValidator.

    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFG.FGItemID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "FG Item ID Blank".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportFG NO-LOCK 
            WHERE bf-ttImportFG.Company EQ ipbf-ttImportFG.Company
            AND bf-ttImportFG.FGItemID EQ ipbf-ttImportFG.FGItemID
            AND ROWID(bf-ttImportFG) NE ROWID(ipbf-ttImportFG)
            NO-ERROR.
        IF AVAILABLE bf-ttImportFG THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        RUN pIsValidFGItemID IN hdValidator (ipbf-ttImportFG.FGItemID, YES, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid THEN 
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
                oplValid = YES
                opcNote = "Add record"
                .
        
    END.

    /*Validate Required Fields*/    
    IF oplValid THEN 
        RUN pIsValidCustomerID IN hdValidator (ipbf-ttImportFG.CustomerID,
            YES,
            ipbf-ttImportFG.Company,  
            OUTPUT oplValid, 
            OUTPUT cValidNote).
  
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportFG.Style NE "" THEN 
            RUN pIsValidStyle IN hdValidator (ipbf-ttImportFG.Style, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportFG.SellPriceUOM NE "" THEN 
            RUN pIsValidUOM IN hdValidator (ipbf-ttImportFG.SellPriceUOM, NO, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportFG.StdCostUOM NE "" THEN 
            RUN pIsValidUOM IN hdValidator (ipbf-ttImportFG.StdCostUOM, NO, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportFG.PurchasedQuantityUOM NE "" THEN 
            RUN pIsValidUOM IN hdValidator (ipbf-ttImportFG.PurchasedQuantityUOM, NO, OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid AND ipbf-ttImportFG.Currency NE "" THEN 
            RUN pIsValidCurrency IN hdValidator (ipbf-ttImportFG.Currency, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
                
        IF oplValid AND ipbf-ttImportFG.Category NE "" THEN 
            RUN pIsValidFGCategory IN hdValidator (ipbf-ttImportFG.Category, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
           
        IF oplValid AND ipbf-ttImportFG.Warehouse NE "" THEN 
            RUN pIsValidWarehouse IN hdValidator (ipbf-ttImportFG.Warehouse, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportFG.Bin NE "" THEN 
            RUN pIsValidFGBin IN hdValidator (ipbf-ttImportFG.Bin, "", NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid AND ipbf-ttImportFG.SalesRepID NE "" THEN 
            RUN pIsValidSalesRep IN hdValidator (ipbf-ttImportFG.SalesRepID, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.

END PROCEDURE.

