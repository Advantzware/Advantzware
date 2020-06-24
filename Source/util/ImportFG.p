
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
    FIELD FGItemID                AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "FG Item #" HELP "Required - Size:15" 
    FIELD CustomerID              AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Cust ID" HELP "Required & Must Be Valid - Size:8"
    FIELD PartID                  AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Cust Part #" HELP "Defaults to FG Item # - Size:15"
    FIELD PartName                AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Name" HELP "Optional - Size:30"
    FIELD PartDescription1        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Desc 1" HELP "Optional - Size:30"
    FIELD PartDescription2        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Desc 2" HELP "Optional - Size:30"
    FIELD PartDescription3        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Desc 3" HELP "Optional - Size:30"
    FIELD FGItemGroup             AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Group" HELP "Optional - Size:15"
    FIELD ExemptFromDiscount      AS CHARACTER FORMAT "X" COLUMN-LABEL "Exempt from Discount" HELP "Optional - Y or N (blank=N)"
    FIELD Style                   AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Style" HELP "Optional - Field Validated - Size:10"
    FIELD DieID                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Die #" HELP "Optional - Size:20"
    FIELD PlateID                 AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Plate #" HELP "Optional - Size:20"
    FIELD CadID                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "CAD #" HELP "Optional - Size:20"
    FIELD QCID                    AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "QC #" HELP "Optional - Size:20"
    FIELD UPCID                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "UPC #" HELP "Optional - Size:20"
    FIELD ReleaseSequence         AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Release Sequence" HELP "Optional - Integer"
    FIELD Taxable                 AS CHARACTER FORMAT "X" COLUMN-LABEL "Taxable" HELP "Optional - Y or N (blank=N)"
    FIELD Varied                  AS CHARACTER FORMAT "X" COLUMN-LABEL "Varied" HELP "Optional - Y or N (blank=N)"
    FIELD ActiveStatus            AS CHARACTER FORMAT "X" COLUMN-LABEL "Active" HELP "Defaults to A (Active) vs. I (Inactive) - Size:1"
    FIELD Purchased               AS CHARACTER FORMAT "X" COLUMN-LABEL "Purchased" HELP "Optional - P=Purchased or M=Manufactured (blank=M)" 
    FIELD ShipByCase              AS CHARACTER FORMAT "X" COLUMN-LABEL "Ship By Case" HELP "Optional - C=Case or P=Pallet (blank=P)"
    FIELD StockItem               AS CHARACTER FORMAT "X" COLUMN-LABEL "Stock Item" HELP "Defaults based on rules (S=Stock Item C=Custom Box) Size:1"
    FIELD SellPrice               AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Price" HELP "Optional - Decimal"
    FIELD SellPriceUOM            AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Price UOM" HELP "Defaults to M - Field Validated - Size:3"  
    FIELD Currency                AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Currency" HELP "Defaults to USD - Field Validated - Size:3"
    FIELD Category                AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Category" HELP "Optional - Field Validated - Size:5"
    FIELD OrderType               AS CHARACTER FORMAT "x" COLUMN-LABEL "Order Type" HELP "Defauts to 'O'"
    FIELD Warehouse               AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse" HELP "Defaults - Field Validated - Size:5"
    FIELD Bin                     AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Bin      " HELP "Defaults - Field Validated - Size:8"
    FIELD InventoryClass          AS CHARACTER FORMAT "x" COLUMN-LABEL "Inventory Class" HELP "Optional - Size:1"
    FIELD CycleCountCode          AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Cycle Count Code" HELP "Optional - Size:2"
    FIELD UnitCount               AS INTEGER   FORMAT ">>>>9" COLUMN-LABEL "Case Count" HELP "Defaults to 1 - Integer"
    FIELD UnitsPerPallet          AS INTEGER   FORMAT ">>>>9" COLUMN-LABEL "Units/Pall" HELP "Defaults to 1 - Integer"
    FIELD ProductionCode          AS CHARACTER FORMAT "x(6)" COLUMN-LABEL "Prod Code" HELP "Optional - Size:6"   
    FIELD LbsPer100               AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Lbs/100" HELP "Optional - Decimal"
    FIELD PackingNote             AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Pk Note" HELP "Optional - Size:20"
    FIELD FrtClass                AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Freight Class" HELP "Optional - Size:8"
    FIELD FrtClassDscr            AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Freight Class Desc" HELP "Optional - Size:30"
    FIELD TrNo                    AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Pallet#" HELP "Optional - Size:10"
    FIELD Zone                    AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Zone" HELP "Optional - Size:12"
    FIELD StackHeight             AS INTEGER   FORMAT "->>>>>9" COLUMN-LABEL "Stack Height" HELP "Optional - Integer"
    FIELD PalletLen               AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Pallet (L)" HELP "Optional - Decimal"
    FIELD PalletWid               AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Pallet (W)" HELP "Optional - Decimal"
    FIELD PalletDep               AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Pallet (D)" HELP "Optional - Decimal"
    FIELD StdPalletVol            AS DECIMAL   FORMAT "->>>>>>>9.99" COLUMN-LABEL "Std. Pallet Volume(In3)" HELP "Optional - Decimal"
    FIELD StdCostMaterial         AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Std Mat'l Cost" HELP "Optional - Decimal"
    FIELD StdCostLabor            AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Std Labor Cost" HELP "Optional - Decimal"
    FIELD StdCostVariableOverhead AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Std Var OH Cost" HELP "Optional - Decimal"
    FIELD StdCostFixedOverhead    AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Std Fix OH Cost" HELP "Optional - Decimal"
    FIELD StdCostFull             AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Full Cost" HELP "Optional - Decimal"
    FIELD StdCostUOM              AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Cost UOM " HELP "Defaults to M - Field Validated - Size:3"  
    FIELD Stocked                 AS CHARACTER FORMAT "X" COLUMN-LABEL "Stocked " HELP "Optional - Y or N (blank=N)"
    FIELD LengthBox               AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Length " HELP "Optional - Decimal"
    FIELD WidthBox                AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Width " HELP "Optional - Decimal"
    FIELD DepthBox                AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Depth " HELP "Optional - Decimal"
    FIELD LengthBlank             AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Style Sq. In. Length" HELP "Optional - Decimal"
    FIELD WidthBlank              AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Style Sq. In. Width" HELP "Optional - Decimal"
    FIELD FactorInvoice           AS CHARACTER FORMAT "X" COLUMN-LABEL "Factor Invoice" HELP "Optional - Y or N (blank=N)"
    FIELD ReorderPoint            AS CHARACTER FORMAT "X" COLUMN-LABEL "Reorder Point" HELP "Optional - Y=Reorder Point N=Lot Controlled (blank=N)"
    FIELD ReorderLevel            AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Reorder Level" HELP "Optional - Integer"
    FIELD OrderMinimum            AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Minimum Order" HELP "Optional - Integer"
    FIELD OrderMaximum            AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Maximum Order" HELP "Optional - Integer"
    FIELD PurchasedQuantityUOM    AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Purchased Quantity UOM" HELP "Defaults - Field Validated - Size:3"  
    FIELD LeadTimeDays            AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Lead Time in Days" HELP "Optional - Integer"
    FIELD BeginningDate           AS DATETIME  FORMAT "99/99/9999" COLUMN-LABEL "Beginning Date" HELP "Optional - Date in MM/DD/YYYY"
    FIELD SalesRepID              AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Sales Rep" HELP "Optional - Field Validated - Size:3" /*Defaults to Customer SalesmanID*/
    FIELD SpecNote1Group          AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 1 Group" HELP "Optional - Size:3"
    FIELD SpecNote1Title          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 1 Title" HELP "Optional - Size:60"  
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
    FIELD productTaxClass         AS CHARACTER FORMAT "X(18)" COLUMN-LABEL "Product Tax Class" HELP "Optional - Size:18" 
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
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.
    DEFINE BUFFER bf-itemfg FOR itemfg.

    FIND FIRST bf-itemfg EXCLUSIVE-LOCK 
        WHERE bf-itemfg.company EQ ipbf-ttImportFG.Company
        AND bf-itemfg.i-no EQ ipbf-ttImportFG.FGItemID
        NO-ERROR.

    IF NOT AVAILABLE bf-itemfg THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE bf-itemfg.
        ASSIGN 
            bf-itemfg.company   = ipbf-ttImportFG.Company
            bf-itemfg.loc       = ipbf-ttImportFG.Location
            bf-itemfg.i-no      = ipbf-ttImportFG.FGItemID
            bf-itemfg.part-no   = IF ipbf-ttImportFG.PartID EQ "" THEN bf-itemfg.i-no ELSE ipbf-ttImportFG.PartID
        
            /*Refactor - Default values should come from rules in create.trg for bf-itemfg*/
            bf-itemfg.stat      = "A"
            bf-itemfg.sell-uom  = "M"
            bf-itemfg.curr-code = "USD"
            bf-itemfg.type-code = "O"
            .
    END.
    IF ipbf-ttImportFG.UnitCount EQ 0 AND NOT iplIgnoreBlanks THEN
        ASSIGN ipbf-ttImportFG.UnitCount = 1 . 
    IF ipbf-ttImportFG.UnitsPerPallet EQ 0 AND NOT iplIgnoreBlanks THEN
        ASSIGN ipbf-ttImportFG.UnitsPerPallet = 1 .
        
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportFG.CustomerID, YES, INPUT-OUTPUT bf-itemfg.cust-no).
    RUN pAssignValueC (ipbf-ttImportFG.PartID, YES, INPUT-OUTPUT bf-itemfg.part-no).
    RUN pAssignValueC (ipbf-ttImportFG.PartName, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.i-name).
    RUN pAssignValueC (ipbf-ttImportFG.PartDescription1, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.part-dscr1). 
    RUN pAssignValueC (ipbf-ttImportFG.PartDescription2, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.part-dscr2). 
    RUN pAssignValueC (ipbf-ttImportFG.PartDescription3, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.part-dscr3).  
    RUN pAssignValueC (ipbf-ttImportFG.FGItemGroup, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.spare-char-1).
    RUN pAssignValueC (ipbf-ttImportFG.Style, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.style).
    RUN pAssignValueC (ipbf-ttImportFG.DieID, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.die-no).
    RUN pAssignValueC (ipbf-ttImportFG.CadID, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.cad-no).
    RUN pAssignValueC (ipbf-ttImportFG.PlateID, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.plate-no).        
    RUN pAssignValueC (ipbf-ttImportFG.QCID, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.spc-no).    
    RUN pAssignValueC (ipbf-ttImportFG.UPCID, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.upc-no).     
    RUN pAssignValueC (ipbf-ttImportFG.Category, YES, INPUT-OUTPUT bf-itemfg.procat).
    RUN pAssignValueCToL (ipbf-ttImportFG.ExemptFromDiscount, "Y", iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.exempt-disc).
    RUN pAssignValueCToL (ipbf-ttImportFG.Taxable, "Y", iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.taxable).
    RUN pAssignValueC (ipbf-ttImportFG.Varied, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.spare-char-2).
    RUN pAssignValueI (ipbf-ttImportFG.ReleaseSequence, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.spare-int-2).
    RUN pAssignValueC (ipbf-ttImportFG.ActiveStatus, YES, INPUT-OUTPUT bf-itemfg.stat).
    RUN pAssignValueCToL (ipbf-ttImportFG.Purchased, "P", iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.pur-man).      
    RUN pAssignValueCToL (ipbf-ttImportFG.ShipByCas, "C", iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.ship-meth).
    RUN pAssignValueC (ipbf-ttImportFG.StockItem, YES, INPUT-OUTPUT bf-itemfg.i-code).
    RUN pAssignValueD (ipbf-ttImportFG.SellPrice, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.sell-price).
    RUN pAssignValueC (ipbf-ttImportFG.SellPriceUOM, YES, INPUT-OUTPUT bf-itemfg.sell-uom).
    RUN pAssignValueC (ipbf-ttImportFG.Currency, YES, INPUT-OUTPUT bf-itemfg.curr-code[1]).
    RUN pAssignValueC (ipbf-ttImportFG.OrderType, YES, INPUT-OUTPUT bf-itemfg.type-code).
    RUN pAssignValueC (ipbf-ttImportFG.Warehouse, YES, INPUT-OUTPUT bf-itemfg.def-loc).
    RUN pAssignValueC (ipbf-ttImportFG.Bin, YES, INPUT-OUTPUT bf-itemfg.def-loc-bin).
    RUN pAssignValueC (ipbf-ttImportFG.InventoryClass, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.class).
    RUN pAssignValueC (ipbf-ttImportFG.CycleCountCode, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.cc-code).
    RUN pAssignValueI (ipbf-ttImportFG.UnitCount, YES, INPUT-OUTPUT bf-itemfg.case-count).
    RUN pAssignValueI (ipbf-ttImportFG.UnitsPerPallet, YES, INPUT-OUTPUT bf-itemfg.case-pall).
    RUN pAssignValueC (ipbf-ttImportFG.ProductionCode, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.prod-code).
    RUN pAssignValueD (ipbf-ttImportFG.LbsPer100, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.weight-100).   
    RUN pAssignValueC (ipbf-ttImportFG.PackingNote, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.prod-notes).
    RUN pAssignValueD (ipbf-ttImportFG.StdCostMaterial, YES, INPUT-OUTPUT bf-itemfg.std-mat-cost).
    RUN pAssignValueD (ipbf-ttImportFG.StdCostLabor, YES, INPUT-OUTPUT bf-itemfg.std-lab-cost).         
    RUN pAssignValueD (ipbf-ttImportFG.StdCostVariableOverhead, YES, INPUT-OUTPUT bf-itemfg.std-var-cost).
    RUN pAssignValueD (ipbf-ttImportFG.StdCostFixedOverhead, YES, INPUT-OUTPUT bf-itemfg.std-fix-cost).
    RUN pAssignValueD (ipbf-ttImportFG.StdCostFull, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.spare-dec-1).
    RUN pAssignValueC (ipbf-ttImportFG.StdCostUOM, YES, INPUT-OUTPUT bf-itemfg.prod-uom).
    RUN pAssignValueCToL (ipbf-ttImportFG.Stocked, "Y", iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.stocked).
    RUN pAssignValueD (ipbf-ttImportFG.LengthBox, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.l-score[50]).
    RUN pAssignValueD (ipbf-ttImportFG.WidthBox, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.w-score[50]).
    RUN pAssignValueD (ipbf-ttImportFG.DepthBox, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.d-score[50]).
    RUN pAssignValueD (ipbf-ttImportFG.LengthBlank, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.t-len).
    RUN pAssignValueD (ipbf-ttImportFG.WidthBlank, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.t-wid).
    RUN pAssignValueC (ipbf-ttImportFG.SalesRepID, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.spare-char-3).
    RUN pAssignValueCToL (ipbf-ttImportFG.FactorInvoice, "Y", iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.factored).
    RUN pAssignValueCToL (ipbf-ttImportFG.ReorderPoint, "Y", iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.ord-policy).
    RUN pAssignValueI (ipbf-ttImportFG.ReorderLevel, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.ord-level).
    RUN pAssignValueI (ipbf-ttImportFG.OrderMinimum, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.ord-min).
    RUN pAssignValueI (ipbf-ttImportFG.OrderMaximum, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.ord-max).
    RUN pAssignValueC (ipbf-ttImportFG.PurchasedQuantityUOM, YES, INPUT-OUTPUT bf-itemfg.pur-uom).
    RUN pAssignValueI (ipbf-ttImportFG.LeadTimeDays, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.lead-days).
    RUN pAssignValueDate (ipbf-ttImportFG.BeginningDate, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.beg-date).  

    RUN pAssignValueC (ipbf-ttImportFG.FrtClass, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.frt-class).
    RUN pAssignValueC (ipbf-ttImportFG.FrtClassDscr, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.frt-class-dscr).
    RUN pAssignValueC (ipbf-ttImportFG.TrNo, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.trno).
    RUN pAssignValueC (ipbf-ttImportFG.Zone, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.spare-char-4).
    RUN pAssignValueI (ipbf-ttImportFG.StackHeight, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.stackHeight).
    RUN pAssignValueD (ipbf-ttImportFG.PalletLen, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.unitLength).
    RUN pAssignValueD (ipbf-ttImportFG.PalletWid, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.unitWidth).
    RUN pAssignValueD (ipbf-ttImportFG.PalletDep, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.unitHeight).
    RUN pAssignValueD (ipbf-ttImportFG.StdPalletVol, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.palletVolume).
    RUN pAssignValueC (ipbf-ttImportFG.productTaxClass, iplIgnoreBlanks, INPUT-OUTPUT bf-itemfg.productTaxClass).
    /*Recalculate derived values*/
    ASSIGN 
        bf-itemfg.std-tot-cost = bf-itemfg.std-mat-cost + bf-itemfg.std-lab-cost + bf-itemfg.std-var-cost + bf-itemfg.std-fix-cost
        bf-itemfg.t-sqin       = bf-itemfg.t-len * bf-itemfg.t-wid
        bf-itemfg.t-sqft       = bf-itemfg.t-sqin / 144
        .
        
    IF ipbf-ttImportFG.Category NE "" THEN 
    DO:
        FIND FIRST fgcat NO-LOCK 
            WHERE fgcat.company EQ ipbf-ttImportFG.Company
            AND fgcat.procat EQ ipbf-ttImportFG.Category
            NO-ERROR.
        IF AVAILABLE fgcat THEN 
            bf-itemfg.procat-desc = fgcat.dscr.
    END.    

    IF ipbf-ttImportFG.SpecNote1Note NE "" THEN 
        RUN util/Dev/AddNote.p (bf-itemfg.rec_key, ipbf-ttImportFG.SpecNote1Note, ipbf-ttImportFG.SpecNote1Title, ipbf-ttImportFG.SpecNote1Group, "S", OUTPUT riNote).
    IF ipbf-ttImportFG.SpecNote2Note NE "" THEN 
        RUN util/Dev/AddNote.p (bf-itemfg.rec_key, ipbf-ttImportFG.SpecNote2Note, ipbf-ttImportFG.SpecNote2Title, ipbf-ttImportFG.SpecNote2Group, "S", OUTPUT riNote).
    IF ipbf-ttImportFG.SpecNote3Note NE "" THEN 
        RUN util/Dev/AddNote.p (bf-itemfg.rec_key, ipbf-ttImportFG.SpecNote3Note, ipbf-ttImportFG.SpecNote3Title, ipbf-ttImportFG.SpecNote3Group, "S", OUTPUT riNote).
    IF ipbf-ttImportFG.SpecNote4Note NE "" THEN 
        RUN util/Dev/AddNote.p (bf-itemfg.rec_key, ipbf-ttImportFG.SpecNote4Note, ipbf-ttImportFG.SpecNote4Title, ipbf-ttImportFG.SpecNote4Group, "S", OUTPUT riNote).
    IF ipbf-ttImportFG.SpecNote5Note NE "" THEN 
        RUN util/Dev/AddNote.p (bf-itemfg.rec_key, ipbf-ttImportFG.SpecNote5Note, ipbf-ttImportFG.SpecNote5Title, ipbf-ttImportFG.SpecNote5Group, "S", OUTPUT riNote).
    
   RELEASE bf-itemfg .
    
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

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportFG FOR ttImportFG.


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
        RUN pIsValidFGItemID (ipbf-ttImportFG.FGItemID, YES, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
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
        RUN pIsValidCustomerID (ipbf-ttImportFG.CustomerID,
            YES,
            ipbf-ttImportFG.Company,  
            OUTPUT oplValid, 
            OUTPUT cValidNote).
  
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportFG.Style NE "" THEN 
            RUN pIsValidStyle (ipbf-ttImportFG.Style, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportFG.SellPriceUOM NE "" THEN 
            RUN pIsValidUOM (ipbf-ttImportFG.SellPriceUOM, NO, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportFG.StdCostUOM NE "" THEN 
            RUN pIsValidUOM (ipbf-ttImportFG.StdCostUOM, NO, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportFG.PurchasedQuantityUOM NE "" THEN 
            RUN pIsValidUOM (ipbf-ttImportFG.PurchasedQuantityUOM, NO, OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid AND ipbf-ttImportFG.Currency NE "" THEN 
            RUN pIsValidCurrency (ipbf-ttImportFG.Currency, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
                
        IF oplValid AND ipbf-ttImportFG.Category NE "" THEN 
            RUN pIsValidFGCategory (ipbf-ttImportFG.Category, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
           
        IF oplValid AND ipbf-ttImportFG.Warehouse NE "" THEN 
            RUN pIsValidWarehouse (ipbf-ttImportFG.Warehouse, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportFG.Bin NE "" THEN 
            RUN pIsValidFGBin (ipbf-ttImportFG.Bin, "", NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportFG.Bin NE "" THEN 
            RUN pIsValidFGBinForLoc (ipbf-ttImportFG.Bin, ipbf-ttImportFG.Warehouse, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid AND ipbf-ttImportFG.SalesRepID NE "" THEN 
            RUN pIsValidSalesRep (ipbf-ttImportFG.SalesRepID, NO, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportFG.ActiveStatus NE "" THEN   
            RUN pIsValidFromList ("Active", ipbf-ttImportFG.ActiveStatus, "A,I", OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportFG.StockItem NE "" THEN   
            RUN pIsValidFromList ("Stock Item", ipbf-ttImportFG.StockItem, "S,C", OUTPUT oplValid, OUTPUT cValidNote).            

        IF oplValid AND ipbf-ttImportFG.TrNo NE "" THEN   
            RUN pIsValidItemForType (ipbf-ttImportFG.TrNo,"D", YES, ipbf-ttImportFG.Company, OUTPUT oplValid, OUTPUT cValidNote).            
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    

END PROCEDURE.

