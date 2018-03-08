
/*------------------------------------------------------------------------
    File        : ImportEstimate.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Estimates	

    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 
DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef  FOR ef.
DEFINE NEW SHARED BUFFER xeb  FOR eb.
DEFINE NEW SHARED BUFFER xqty FOR est-qty.
        
DEFINE TEMP-TABLE ttImportEstimate
    FIELD Company         AS CHARACTER 
    FIELD riParentEst     AS ROWID   /*Can be the row-id of the est table or blank for new estimate*/
    FIELD Industry        AS CHARACTER FORMAT "X" COLUMN-LABEL "Industry" /*C or F*/
    FIELD EstimateNo      AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Est #" /*Defaults to auto-incremented*/
    FIELD FormNo          AS INTEGER   FORMAT ">>" COLUMN-LABEL "Form #" /*Defaults to 1*/
    FIELD BlankNo         AS INTEGER   FORMAT ">>" COLUMN-LABEL "Blank #" /*Defaults to 1*/
    FIELD CustomerID      AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Cust ID"/*Required - Validated*/
    FIELD ShipToID        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "ShipTo ID" /*Defaults to CustomerID if not provided - validated if entered*/
    FIELD PartID          AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Cust Part #" /*Required*/
    FIELD PartName        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Name" /*Optional*/
    FIELD PartDescription AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Desc"  /*Optional*/
    FIELD FGItemID        AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "FG Item #" 
    FIELD Style           AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Style" /*Required and validated*/
    FIELD Flute           AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Flute" /*Requied and validated*/
    FIELD Test            AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Test" /*Required and validated*/
    FIELD BoardID         AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Board" /*Can be derived from CFlute and cTest*/
    FIELD GlueID          AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Glue"
    FIELD TabInOut        AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Tab In/Out" /*Defaults to In*/
    FIELD Category        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Category" /*Required and validated*/
    FIELD LengthBox       AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "L" /*Required*/
    FIELD WidthBox        AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "W" /*Required*/
    FIELD DepthBox        AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "D" /*Optional*/
    FIELD FPanel          AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "FPanel" /*Optional*/
    FIELD BPanel          AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "BPanel" /*Optional*/
    FIELD LengthBlank     AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Blank L"/*Optional - If provided will override the calculated blank*/
    FIELD WidthBlank      AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Blank W" /*Optional - If provided will override the calculated blank*/
    FIELD Purchased       AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Purchased" /*Defaults to No*/
    FIELD CadID           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "CAD" /*Optional*/
    FIELD DieID           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "DIE" /*Optional*/
    FIELD Quantity        AS INTEGER   FORMAT ">>>>>>>" COLUMN-LABEL "Quantity"/*Required*/
    FIELD DieInches       AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Die Inches" /*Optional - defaults to 0*/
    FIELD SalesManID      AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Salesman" /*Defaults to Customer SalesmanID*/
    FIELD QuantityYield   AS INTEGER   FORMAT ">>>>>>>" COLUMN-LABEL "Yield Quantity" /*Defaults to 1*/
    FIELD Designer        AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Designer"   /*Optional*/    
    FIELD WidthDie        AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Die W" /*Defaults to WidthBlank*/  
    FIELD LengthDie       AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Die L" /*Defaults to LengthBlank*/
    FIELD InkDescription  AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Ink Description" /*Optional*/
    FIELD InkColors       AS INTEGER   FORMAT ">>" COLUMN-LABEL "Colors"  /*Optional - If not provided, derived from provided Ink Codes or 0*/
    FIELD Ink1Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 1 Code" /*Optional - Array of Inks - Validated RM Codes - Defaults to Style Inks or Control Inks if Count is non-zero*/
    FIELD Ink1Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 1 Coverage" /*Optional - Array of Coverages - Defaults to Style % or Control % if not provided for corresponding code*/
    FIELD Ink1Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 1 Unit"
    FIELD Ink1Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 1 Pass"
    FIELD Ink1Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 1 Side"
    FIELD Ink2Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 2 Code" /*Optional - Array of Inks - Validated RM Codes - Defaults to Style Inks or Control Inks if Count is non-zero*/
    FIELD Ink2Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 2 Coverage" /*Optional - Array of Coverages - Defaults to Style % or Control % if not provided for corresponding code*/
    FIELD Ink2Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 2 Unit"
    FIELD Ink2Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 2 Pass"
    FIELD Ink2Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 2 Side"
    FIELD Ink3Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 3 Code" /*Optional - Array of Inks - Validated RM Codes - Defaults to Style Inks or Control Inks if Count is non-zero*/
    FIELD Ink3Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 3 Coverage" /*Optional - Array of Coverages - Defaults to Style % or Control % if not provided for corresponding code*/
    FIELD Ink3Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 3 Unit"
    FIELD Ink3Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 3 Pass"
    FIELD Ink3Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 3 Side"
    FIELD Ink4Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 4 Code" /*Optional - Array of Inks - Validated RM Codes - Defaults to Style Inks or Control Inks if Count is non-zero*/
    FIELD Ink4Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 4 Coverage" /*Optional - Array of Coverages - Defaults to Style % or Control % if not provided for corresponding code*/
    FIELD Ink4Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 4 Unit"
    FIELD Ink4Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 4 Pass"
    FIELD Ink4Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 4 Side"
    FIELD Ink5Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 5 Code" /*Optional - Array of Inks - Validated RM Codes - Defaults to Style Inks or Control Inks if Count is non-zero*/
    FIELD Ink5Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 5 Coverage" /*Optional - Array of Coverages - Defaults to Style % or Control % if not provided for corresponding code*/
    FIELD Ink5Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 5 Unit"
    FIELD Ink5Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 5 Pass"
    FIELD Ink5Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 5 Side"   
    FIELD EstNote1Group   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Est Note 1 Group"
    FIELD EstNote1Title   AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Est Note 1 Title" 
    FIELD EstNote1Note    AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Est Note 1 Note"
    FIELD EstNote2Group   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Est Note 2 Group"
    FIELD EstNote2Title   AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Est Note 2 Title" 
    FIELD EstNote2Note    AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Est Note 2 Note"
    FIELD EstNote3Group   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Est Note 3 Group"
    FIELD EstNote3Title   AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Est Note 3 Title" 
    FIELD EstNote3Note    AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Est Note 3 Note"
    FIELD SpecNote1Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 1 Group"
    FIELD SpecNote1Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 1 Title" 
    FIELD SpecNote1Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 1 Note"
    FIELD SpecNote2Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 2 Group"
    FIELD SpecNote2Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 2 Title" 
    FIELD SpecNote2Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 2 Note"
    FIELD SpecNote3Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 3 Group"
    FIELD SpecNote3Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 3 Title" 
    FIELD SpecNote3Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 3 Note"
    FIELD SpecNote4Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 4 Group"
    FIELD SpecNote4Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 4 Title" 
    FIELD SpecNote4Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 4 Note"
    FIELD SpecNote5Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 5 Group"
    FIELD SpecNote5Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 5 Title" 
    FIELD SpecNote5Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 5 Note"
    FIELD VendorID        AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Vendor ID"
    FIELD VendorCost      AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Vendor Cost"
    FIELD VendorCostUOM   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Vendor Cost UOM"
    FIELD VendorItemID    AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "Vendor Item"
    .
DEFINE VARIABLE giIndexOffset AS INTEGER   NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/
DEFINE VARIABLE gcWidths      AS CHARACTER NO-UNDO INIT "60,60,40,40,60,60, 100,150,150,100,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,100,100,60,60,60,60,100,60,60,150,40,60,60,40,40,40,60,60,40,40,40,60,60,40,40,40,60,60,40,40,40,60,60,40,40,40,40,150,150,40,150,150,40,150,150,40,150,150,40,150,150,40,150,150,40,150,150,40,150,150,40,150,150,60,60,60,100". 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
PROCEDURE pAddFarm:
/*------------------------------------------------------------------------------
 Purpose: Builds the Farm tab for a Finished Good Item and Estimate
 Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-ttImportEstimate FOR ttImportEstimate.
DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
DEFINE VARIABLE dQty1 AS DECIMAL.
DEFINE VARIABLE dQty2 AS DECIMAL.
DEFINE VARIABLE dPrice1 AS DECIMAL.
DEFINE VARIABLE dPrice2 AS DECIMAL.


IF ipbf-ttImportEstimate.Quantity GT 0 THEN 
    ASSIGN 
        dQty1 = ipbf-ttImportEstimate.Quantity - 0.001
        dQty2 = 9999999.9
        dPrice1 = 99999.99
        dPrice2 = ipbf-ttImportEstimate.VendorCost
        .
ELSE
ASSIGN 
        dQty1 = 9999999.9
        dQty2 = 0
        dPrice1 = ipbf-ttImportEstimate.VendorCost
        dPrice2 = 0
        . 
        
IF ipbf-ttImportEstimate.VendorID NE "" THEN DO:
    IF NOT CAN-FIND(FIRST e-itemfg WHERE e-itemfg.company EQ ipbf-ttImportEstimate.Company AND e-itemfg.i-no EQ ipcFGItemID) THEN DO:
        CREATE e-itemfg.
        ASSIGN 
            e-itemfg.company = ipbf-ttImportEstimate.Company
            e-itemfg.i-no = ipcFGItemID
            e-itemfg.std-uom = IF ipbf-ttImportEstimate.VendorCostUOM EQ "" THEN "EA" ELSE ipbf-ttImportEstimate.VendorCostUOM 
            .
    END.
    IF NOT CAN-FIND(FIRST e-itemfg-vend WHERE e-itemfg-vend.company EQ ipbf-ttImportEstimate.Company AND e-itemfg-vend.i-no EQ ipcFGItemID AND e-itemfg-vend.vend-no EQ "") THEN DO:
        CREATE e-itemfg-vend.
        ASSIGN 
            e-itemfg-vend.company = ipbf-ttImportEstimate.Company
            e-itemfg-vend.i-no = ipcFGItemID
            e-itemfg-vend.run-qty[1] = dQty1
            e-itemfg-vend.run-qty[2] = dQty2
            e-itemfg-vend.run-cost[1] = dPrice1
            e-itemfg-vend.run-cost[2] = dPrice2        
            e-itemfg-vend.form-no = ipbf-eb.form-no
            e-itemfg-vend.blank-no = ipbf-eb.blank-no
            e-itemfg-vend.eqty = ipbf-eb.eqty
            .
    END.
    CREATE e-itemfg-vend.
    ASSIGN 
        e-itemfg-vend.company = ipbf-ttImportEstimate.Company
        e-itemfg-vend.i-no = ipcFGItemID
        e-itemfg-vend.vend-no = ipbf-ttImportEstimate.VendorID
        e-itemfg-vend.vend-item = ipbf-ttImportEstimate.VendorItemID
        e-itemfg-vend.run-qty[1] = dQty1
        e-itemfg-vend.run-qty[2] = dQty2
        e-itemfg-vend.run-cost[1] = dPrice1
        e-itemfg-vend.run-cost[2] = dPrice2        
        e-itemfg-vend.form-no = ipbf-eb.form-no
        e-itemfg-vend.blank-no = ipbf-eb.blank-no
        e-itemfg-vend.eqty = ipbf-eb.eqty
        .
    CREATE e-itemfg-vend.
    ASSIGN 
        e-itemfg-vend.company = ipbf-ttImportEstimate.Company
        e-itemfg-vend.i-no = ipcFGItemID
        e-itemfg-vend.vend-no = ipbf-ttImportEstimate.VendorID
        e-itemfg-vend.vend-item = ipbf-ttImportEstimate.VendorItemID
        e-itemfg-vend.run-qty[1] = dQty1
        e-itemfg-vend.run-qty[2] = dQty2
        e-itemfg-vend.run-cost[1] = dPrice1
        e-itemfg-vend.run-cost[2] = dPrice2        
        e-itemfg-vend.est-no = ipbf-eb.est-no
        e-itemfg-vend.form-no = ipbf-eb.form-no
        e-itemfg-vend.blank-no = ipbf-eb.blank-no
        e-itemfg-vend.eqty = ipbf-eb.eqty
        .
END.    

END PROCEDURE.

PROCEDURE pAddNotes:
    /*------------------------------------------------------------------------------
     Purpose:  Adds estimates to re 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportEstimate FOR ttImportEstimate.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cNoteType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.
    
    IF ipcType EQ "Estimate" THEN 
    DO:
        cNoteType = "D".
        IF ipbf-ttImportEstimate.EstNote1Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.EstNote1Note, ipbf-ttImportEstimate.EstNote1Title, ipbf-ttImportEstimate.EstNote1Group, cNoteType, OUTPUT riNote).
        IF ipbf-ttImportEstimate.EstNote2Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.EstNote2Note, ipbf-ttImportEstimate.EstNote2Title, ipbf-ttImportEstimate.EstNote2Group, cNoteType, OUTPUT riNote).
        IF ipbf-ttImportEstimate.EstNote3Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.EstNote3Note, ipbf-ttImportEstimate.EstNote3Title, ipbf-ttImportEstimate.EstNote3Group, cNoteType, OUTPUT riNote).
        
        FIND FIRST notes EXCLUSIVE-LOCK 
            WHERE ROWID(notes) EQ riNote
            NO-ERROR.
            IF AVAILABLE notes THEN notes.note_form_no = 1.     
                                       
    END.    
    ELSE DO:
        cNoteType = "S".
        IF ipbf-ttImportEstimate.SpecNote1Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.SpecNote1Note, ipbf-ttImportEstimate.SpecNote1Title, ipbf-ttImportEstimate.SpecNote1Group, cNoteType, OUTPUT riNote).
        IF ipbf-ttImportEstimate.SpecNote2Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.SpecNote2Note, ipbf-ttImportEstimate.SpecNote2Title, ipbf-ttImportEstimate.SpecNote2Group, cNoteType, OUTPUT riNote).
        IF ipbf-ttImportEstimate.SpecNote3Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.SpecNote3Note, ipbf-ttImportEstimate.SpecNote3Title, ipbf-ttImportEstimate.SpecNote3Group, cNoteType, OUTPUT riNote).
    END.
     

END PROCEDURE.

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
    DEFINE BUFFER bf-ttImportEstimate FOR ttImportEstimate.

    oplValid = YES.
    CREATE ttImportEstimate.
    ASSIGN 
        ttImportEstimate.Company = ipcCompany.
    FOR EACH ttImportMap
        WHERE ttImportMap.cType EQ 'Est':
        cData = ipcData[ttImportMap.iImportIndex].
        hdTempTableBuffer = TEMP-TABLE ttImportEstimate:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ttImportMap.iIndex + giIndexOffset):HANDLE.
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
        IF ttImportEstimate.CustomerID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank CustomerID".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportEstimate.Style EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Style".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportEstimate.Category EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Category".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportEstimate.Quantity LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Quantity".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportEstimate.LengthBox LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Box Length".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportEstimate.WidthBox LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Box Width".
    END.
    IF oplValid THEN 
    DO:
        IF LOOKUP(ttImportEstimate.Industry,"C,F") EQ 0 THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Industry not C or F"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ttImportEstimate.Company
            AND cust.cust-no EQ ttImportEstimate.CustomerID
            NO-ERROR. 
        IF NOT AVAILABLE cust THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Invalid CustomerID"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportEstimate NO-LOCK 
            WHERE bf-ttImportEstimate.Company EQ ttImportEstimate.Company
            AND bf-ttImportEstimate.CustomerID EQ ttImportEstimate.CustomerID
            AND bf-ttImportEstimate.PartID EQ ttImportEstimate.PartID
            AND ROWID(bf-ttImportEstimate) NE ROWID(ttImportEstimate)
            NO-ERROR.
        IF AVAILABLE bf-ttImportEstimate THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        IF ttImportEstimate.EstimateNo NE "" THEN 
            FIND FIRST est NO-LOCK 
                WHERE est.company EQ ttImportEstimate.Company
                AND est.est-no EQ ttImportEstimate.EstimateNo
                NO-ERROR .
        IF AVAILABLE est THEN 
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
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF ttImportEstimate.ShipToID NE "" THEN 
        DO:
            FIND FIRST shipto NO-LOCK 
                WHERE shipto.company EQ ttImportEstimate.Company
                AND shipto.ship-id EQ ttImportEstimate.ShipToID
                NO-ERROR.
            IF NOT AVAILABLE shipto THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid ShipTo ID"
                    .
        END.
        IF oplValid AND ttImportEstimate.FGItemID NE "" AND ttImportEstimate.FGItemID NE "<AUTO>" THEN 
        DO:
            FIND FIRST itemfg NO-LOCK 
                WHERE itemfg.company EQ ttImportEstimate.Company
                AND itemfg.i-no EQ ttImportEstimate.FGItemID
                NO-ERROR.
            IF NOT AVAILABLE itemfg THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid FG Item"
                    .
        END.
        IF oplValid AND ttImportEstimate.Style NE "" THEN 
        DO:
            FIND FIRST style NO-LOCK 
                WHERE style.company EQ ttImportEstimate.Company
                AND style.style EQ ttImportEstimate.Style
                NO-ERROR.
            IF NOT AVAILABLE style THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Style"
                    .
        END.
        IF oplValid AND ttImportEstimate.Flute NE "" THEN 
        DO:
            FIND FIRST flute NO-LOCK 
                WHERE flute.company EQ ttImportEstimate.Company
                AND flute.code EQ ttImportEstimate.Flute
                NO-ERROR.
            IF NOT AVAILABLE flute THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Flute"
                    .
        END.
        IF oplValid AND ttImportEstimate.BoardID NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ttImportEstimate.Company
                AND item.i-no EQ ttImportEstimate.BoardID
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Board ID"
                    .
        END.
        IF oplValid AND ttImportEstimate.GlueID NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ttImportEstimate.Company
                AND item.i-no EQ ttImportEstimate.GlueID
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Glue ID"
                    .
        END.
        IF oplValid AND ttImportEstimate.Category NE "" THEN 
        DO:
            FIND FIRST fgcat NO-LOCK 
                WHERE fgcat.company EQ ttImportEstimate.Company
                AND fgcat.procat EQ ttImportEstimate.Category
                NO-ERROR.
            IF NOT AVAILABLE fgcat THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Category"
                    .
        END.
        IF oplValid AND ttImportEstimate.Ink1Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ttImportEstimate.Company
                AND item.i-no EQ ttImportEstimate.Ink1Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 1 Code"
                    .
        END.
        IF oplValid AND ttImportEstimate.Ink2Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ttImportEstimate.Company
                AND item.i-no EQ ttImportEstimate.Ink2Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 2 Code"
                    .
        END.
        IF oplValid AND ttImportEstimate.Ink3Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ttImportEstimate.Company
                AND item.i-no EQ ttImportEstimate.Ink3Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 3 Code"
                    .
        END.
        IF oplValid AND ttImportEstimate.Ink4Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ttImportEstimate.Company
                AND item.i-no EQ ttImportEstimate.Ink4Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 4 Code"
                    .
        END.
        IF oplValid AND ttImportEstimate.Ink5Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ttImportEstimate.Company
                AND item.i-no EQ ttImportEstimate.Ink5Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 5 Code"
                    .
        END.
        IF oplValid AND ttImportEstimate.SalesManID NE "" THEN 
        DO:
            FIND FIRST sman NO-LOCK 
                WHERE sman.company EQ ttImportEstimate.Company
                AND sman.sman EQ ttImportEstimate.SalesManID
                NO-ERROR.
            IF NOT AVAILABLE sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Salesman"
                    .    
        END.    
        IF oplValid AND ttImportEstimate.VendorID NE "" THEN 
        DO:
            FIND FIRST vend NO-LOCK 
                WHERE vend.company EQ ttImportEstimate.Company
                AND vend.vend-no EQ ttImportEstimate.VendorID
                NO-ERROR.
            IF NOT AVAILABLE vend THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Vendor"
                    .
        END.
    END.
    IF NOT oplValid THEN DELETE ttImportEstimate.
    
END PROCEDURE.

PROCEDURE pAssignInks:
    /*------------------------------------------------------------------------------
     Purpose:  Writes the inks to the estimate and calculates color count
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb               FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttImportEstimate FOR ttImportEstimate.
    
    DEFINE VARIABLE iCount AS INTEGER.
    
    IF ipbf-ttImportEstimate.Ink1Code NE "" THEN 
        ASSIGN 
            iCount                  = iCount + 1
            ipbf-eb.i-code[iCount]  = ipbf-ttImportEstimate.Ink1Code
            ipbf-eb.i-code2[iCount] = ipbf-ttImportEstimate.Ink1Code
            ipbf-eb.i-%[iCount]     = ipbf-ttImportEstimate.Ink1Coverage
            ipbf-eb.i-%2[iCount]    = ipbf-ttImportEstimate.Ink1Coverage
            ipbf-eb.i-ps[iCount]    = MAX(ipbf-ttImportEstimate.Ink1Pass,1)
            ipbf-eb.i-ps2[iCount]   = MAX(ipbf-ttImportEstimate.Ink1Pass,1)
            /*Refactor - Need to import code for sides and units*/
            .
    IF ipbf-ttImportEstimate.Ink2Code NE "" THEN 
        ASSIGN 
            iCount                  = iCount + 1
            ipbf-eb.i-code[iCount]  = ipbf-ttImportEstimate.Ink2Code
            ipbf-eb.i-code2[iCount] = ipbf-ttImportEstimate.Ink2Code
            ipbf-eb.i-%[iCount]     = ipbf-ttImportEstimate.Ink2Coverage
            ipbf-eb.i-%2[iCount]    = ipbf-ttImportEstimate.Ink2Coverage
            ipbf-eb.i-ps[iCount]    = MAX(ipbf-ttImportEstimate.Ink2Pass,1)
            ipbf-eb.i-ps2[iCount]   = MAX(ipbf-ttImportEstimate.Ink2Pass,1)
            /*Refactor - Need to import code for sides and units*/
            .
    IF ipbf-ttImportEstimate.Ink3Code NE "" THEN 
        ASSIGN 
            iCount                  = iCount + 1
            ipbf-eb.i-code[iCount]  = ipbf-ttImportEstimate.Ink3Code
            ipbf-eb.i-code2[iCount] = ipbf-ttImportEstimate.Ink3Code
            ipbf-eb.i-%[iCount]     = ipbf-ttImportEstimate.Ink3Coverage
            ipbf-eb.i-%2[iCount]    = ipbf-ttImportEstimate.Ink3Coverage
            ipbf-eb.i-ps[iCount]    = MAX(ipbf-ttImportEstimate.Ink3Pass,1)
            ipbf-eb.i-ps2[iCount]   = MAX(ipbf-ttImportEstimate.Ink3Pass,1)
            /*Refactor - Need to import code for sides and units*/
            .
    IF ipbf-ttImportEstimate.Ink4Code NE "" THEN 
        ASSIGN 
            iCount                  = iCount + 1
            ipbf-eb.i-code[iCount]  = ipbf-ttImportEstimate.Ink4Code
            ipbf-eb.i-code2[iCount] = ipbf-ttImportEstimate.Ink4Code
            ipbf-eb.i-%[iCount]     = ipbf-ttImportEstimate.Ink4Coverage
            ipbf-eb.i-%2[iCount]    = ipbf-ttImportEstimate.Ink4Coverage
            ipbf-eb.i-ps[iCount]    = MAX(ipbf-ttImportEstimate.Ink4Pass,1)
            ipbf-eb.i-ps2[iCount]   = MAX(ipbf-ttImportEstimate.Ink4Pass,1)
            /*Refactor - Need to import code for sides and units*/
            .
    IF ipbf-ttImportEstimate.Ink5Code NE "" THEN 
        ASSIGN 
            iCount                  = iCount + 1
            ipbf-eb.i-code[iCount]  = ipbf-ttImportEstimate.Ink5Code
            ipbf-eb.i-code2[iCount] = ipbf-ttImportEstimate.Ink5Code
            ipbf-eb.i-%[iCount]     = ipbf-ttImportEstimate.Ink5Coverage
            ipbf-eb.i-%2[iCount]    = ipbf-ttImportEstimate.Ink5Coverage
            ipbf-eb.i-ps[iCount]    = MAX(ipbf-ttImportEstimate.Ink5Pass,1)
            ipbf-eb.i-ps2[iCount]   = MAX(ipbf-ttImportEstimate.Ink5Pass,1)
            /*Refactor - Need to import code for sides and units*/
            .
    IF iCount > 0 THEN 
        ipbf-eb.i-col = iCount.
    ELSE 
        ipbf-eb.i-col = ipbf-ttImportEstimate.InkColors.

END PROCEDURE.

PROCEDURE pExportData:
    /*------------------------------------------------------------------------------
     Purpose:  Runs the Export Data Program for Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriContext AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iopcFile AS CHARACTER NO-UNDO.


END PROCEDURE.

PROCEDURE pInitialize:
    /*------------------------------------------------------------------------------
     Purpose: Initializes the specific Column Mapping for Estimates   
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLoadFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFields     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabels     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataTypes  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormats    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndexStart AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportEstimate.
    EMPTY TEMP-TABLE ttImportMap.
    
    iIndexStart = 1 + giIndexOffset.
    
    IF ipcLoadFile EQ '' THEN 
    DO:
        ASSIGN 
            cFields    = ""
            cDataTypes = ""
            cFormats   = ""
            cLabels    = ""
            .
        DO iIndex = iIndexStart TO TEMP-TABLE ttImportEstimate:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
            ASSIGN 
                cFields    = cFields + TEMP-TABLE ttImportEstimate:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):NAME + ","
                cDataTypes = cDataTypes + TEMP-TABLE ttImportEstimate:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):DATA-TYPE + ","
                cFormats   = cFormats + TEMP-TABLE ttImportEstimate:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):FORMAT + ","
                cLabels    = cLabels + TEMP-TABLE ttImportEstimate:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):COLUMN-LABEL + ","
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
                ttImportMap.cType         = "Est"
                ttImportMap.cLabel        = ENTRY(iIndex,cFields)
                ttImportMap.iIndex        = iIndex
                ttImportMap.iImportIndex  = iIndex
                ttImportMap.cDataType     = ENTRY(iIndex,cDataTypes)
                ttImportMap.cColumnLabel  = ENTRY(iIndex,cLabels)
                ttImportMap.cColumnFormat = ENTRY(iIndex,cFormats)
                .
            IF iIndex LE NUM-ENTRIES(gcWidths)  THEN 
                ttImportMap.iColumnWidth = INT(ENTRY(iIndex,gcWidths)).
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

    DEFINE VARIABLE riEb      AS ROWID     NO-UNDO.
    DEFINE VARIABLE cIndustry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEstType  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE riItemfg  AS ROWID     NO-UNDO.
        
    FOR EACH ttImportEstimate NO-LOCK:
        opiUpdated = opiUpdated + 1.
        ASSIGN 
            cIndustry = ttImportEstimate.Industry
            .
        IF cIndustry EQ "C" THEN 
            iEstType = 5. 
        ELSE 
            iEstType = 1.
        FIND FIRST est NO-LOCK 
            WHERE ROWID(est) EQ ttImportEstimate.riParentEst NO-ERROR.
        IF NOT AVAILABLE est THEN 
        DO:
            RUN est/NewEstimate.p (cIndustry, iEstType, OUTPUT riEb).
        END.
        ELSE 
        DO:
            RUN est/NewEstimateForm.p (cIndustry, ROWID(est), OUTPUT riEb).
        END.
        opiAdded = opiAdded + 1.
        FIND eb 
            WHERE ROWID(eb) EQ riEb  
            NO-ERROR.
        FIND FIRST ef OF eb
            NO-ERROR.
        FIND FIRST est OF eb  
            NO-ERROR.
        FIND est-qty 
            WHERE est-qty.company EQ ef.company
            AND est-qty.est-no EQ ef.est-no
            AND est-qty.eqty EQ ef.eqty 
            NO-ERROR.
        
        IF eb.eqty EQ 0 THEN 
            eb.eqty         = ttImportEstimate.Quantity.
        ASSIGN 
            est-qty.eqty   = eb.eqty
            est.est-qty[1] = eb.eqty
            ef.eqty        = eb.eqty
            .

        ASSIGN 
            eb.part-no      = ttImportEstimate.PartID
            eb.part-dscr1   = ttImportEstimate.PartName
            eb.part-dscr2   = ttImportEstimate.PartDescription
            eb.style        = ttImportEstimate.Style
            eb.sman         = ttImportEstimate.SalesManID
            eb.die-in       = ttImportEstimate.DieInches
            eb.cad-no       = ttImportEstimate.CadID
            eb.die-no       = ttImportEstimate.DieID
            eb.num-up       = 1
            eb.num-wid      = 1
            eb.num-len      = 1
            eb.pur-man      = ttImportEstimate.Purchased EQ "YES" OR ttImportEstimate.Purchased EQ "P"
            eb.spare-char-1 = ttImportEstimate.Designer
            eb.len          = ttImportEstimate.LengthBox
            eb.wid          = ttImportEstimate.WidthBox
            eb.dep          = ttImportEstimate.DepthBox
            eb.i-coldscr    = ttImportEstimate.InkDescription
            ef.board        = ttImportEstimate.Board     
            ef.nc           = NOT eb.pur-man   
            ef.blank-qty    = 1
            ef.trim-w       = ttImportEstimate.WidthDie
            ef.trim-l       = ttImportEstimate.LengthDie
            .
            
        IF ttImportEstimate.Category NE '' THEN 
            eb.procat       = ttImportEstimate.Category.
        IF ttImportEstimate.QuantityYield GT 0 THEN 
            eb.yld-qty      = ttImportEstimate.QuantityYield.
        ELSE 
            eb.yld-qty      = 1.
        
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ  eb.company 
            AND cust.cust-no EQ ttImportEstimate.CustomerID
            NO-ERROR.

        IF AVAILABLE cust THEN 
        DO: 
            eb.cust-no = cust.cust-no.
            IF ttImportEstimate.ShipToID NE '' THEN 
                FIND FIRST shipto NO-LOCK 
                    WHERE shipto.company EQ cust.company
                    AND shipto.cust-no EQ cust.cust-no
                    AND shipto.ship-id EQ ttImportEstimate.ShipToID
                    NO-ERROR .
            IF NOT AVAILABLE shipto THEN 
                FIND FIRST shipto NO-LOCK 
                    WHERE shipto.company EQ cust.company
                    AND shipto.cust-no EQ cust.cust-no
                    AND shipto.ship-id EQ cust.cust-no
                    NO-ERROR.
            IF NOT AVAILABLE shipto THEN 
                FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
            IF AVAILABLE shipto THEN
                eb.ship-id = shipto.ship-id.
            ELSE 
                eb.ship-id = cust.cust-no.
            eb.sman = cust.sman.
        END.
        IF ttImportEstimate.SalesManID NE '' THEN 
            eb.sman = ttImportEstimate.SalesManID.
            
        IF ttImportEstimate.TabInOut EQ '' THEN 
            eb.tab-in = YES.
        ELSE 
            eb.tab-in = ttImportEstimate.TabInOut EQ "In".
        
        eb.dust = ttImportEstimate.FPanel.
        eb.fpanel = ttImportEstimate.BPanel.
        
        RUN est/CalcBlankSize.p (cIndustry, ROWID(eb)).
   
        IF ttImportEstimate.LengthBlank GT 0 THEN 
            eb.t-len = ttImportEstimate.LengthBlank.
        IF ttImportEstimate.WidthBlank GT 0 THEN 
            eb.t-wid = ttImportEstimate.WidthBlank.    
    
        RUN pAssignInks (BUFFER eb, BUFFER ttImportEstimate).

        IF ef.board EQ '' THEN 
        DO:
            FIND FIRST ITEM NO-LOCK 
                WHERE item.company EQ ef.company
                AND  item.flute EQ ttImportEstimate.Flute
                AND item.reg-no EQ ttImportEstimate.Test
                NO-ERROR.
            IF AVAILABLE ITEM THEN 
                ef.board = item.i-no.
        END.
        FIND FIRST item NO-LOCK 
            WHERE item.company EQ ef.company 
            AND item.i-no  EQ ef.board
            NO-ERROR.
        IF AVAILABLE item THEN
            ASSIGN 
                ef.board = item.i-no
                ef.cal   = item.cal
                eb.flute = item.flute
                eb.test  = item.reg-no
                .
        RUN est/CalcLayout.p (cIndustry,
            ROWID(ef),
            ROWID(eb),
            YES,  /*New Layout vs. Recalculation*/
            NO, /*Prompt to Reset*/
            YES /*Recalc dimensions - Refactor - should be no if Style is foam*/).
        
        RUN est/BuildDefaultPreps.p (BUFFER est,
            BUFFER ef,
            INPUT eb.form-no,
            INPUT 0).
  
        RUN pAddNotes(BUFFER ttImportEstimate, "Estimate", est.rec_key).

        RUN est/CalcPacking.p(ROWID(eb)).
               
        IF ttImportEstimate.FGItemID NE "" THEN 
        DO:
            IF ttImportEstimate.FGItemID EQ "<AUTO>" THEN 
            DO: 
                RUN est/NewFGFromEst.p(ROWID(eb), "", OUTPUT riItemFG).
                FIND FIRST itemfg NO-LOCK 
                    WHERE ROWID(itemfg) EQ riItemFG
                    NO-ERROR.
            END.
            ELSE 
            DO:
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ eb.company
                    AND itemfg.i-no EQ ttImportEstimate.FGItemID
                    NO-ERROR.
            END.  
        END.
        
        IF AVAILABLE itemfg THEN 
        DO:
            eb.stock-no = itemfg.i-no.
            RUN pAddNotes(BUFFER ttImportEstimate, "Spec", itemfg.rec_key).
            RUN pAddFarm(BUFFER ttImportEstimate, BUFFER eb, itemfg.i-no).
            
        END. 
    END.
    opiUpdated = opiUpdated - opiAdded.

END PROCEDURE.

