
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
    FIELD Location        AS CHARACTER 
    FIELD riParentEst     AS ROWID   /*Can be the row-id of the est table or blank for new estimate*/
    FIELD Industry        AS CHARACTER FORMAT "X" COLUMN-LABEL "Industry" HELP "Required - Must Be F or C - Size:1"
    FIELD EstimateNo      AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Est #" HELP "Auto-incremented if not provided - Size:8"
    FIELD Quantity        AS INTEGER   FORMAT ">>>>>>>" COLUMN-LABEL "Quantity" HELP "Required - Integer"
    FIELD FormNo          AS INTEGER   FORMAT ">9" COLUMN-LABEL "Form #" HELP "Defaults to 1 if blank - Integer"
    FIELD BlankNo         AS INTEGER   FORMAT ">9" COLUMN-LABEL "Blank #" HELP "Defaults to 1 if blank - Integer"
    FIELD CustomerID      AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Cust ID" HELP "Required - Must be valid - Size:8"
    FIELD ShipToID        AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "ShipTo ID" HELP "Defaults to Cust ID if blank - Field validated - Size:8"
    FIELD PartID          AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Cust Part #" HELP "Required - Size:15"
    FIELD PartName        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Name" HELP "Optional - Size:30"
    FIELD PartDescription AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Part Desc" HELP "Optional - Size:30"
    FIELD FGItemID        AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "FG Item #" HELP "Optional - Use <AUTO> to auto create - Size:15"
    FIELD Style           AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Style" HELP "Required - Must be valid - Size:5"
    FIELD Flute           AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Flute" HELP "Required - Must be valid - Size:5"
    FIELD Test            AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Test" HELP "Required - Must be valid - Size:"
    FIELD BoardID         AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Board" HELP "Defaults based on Flute and Test - Field Validated - Size:10"
    FIELD GlueID          AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Glue" HELP "Defaults based on Style - Field Validated - Size:10"
    FIELD TabInOut        AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Tab In/Out" HELP "Defaults to In - Size:3"
    FIELD Category        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Category" HELP "Required - Must be valid"
    FIELD LengthBox       AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Length" HELP "Required (Use 0.5 for 1/2) - Decimal"
    FIELD WidthBox        AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Width" HELP "Required (Use 0.5 for 1/2) - Decimal"
    FIELD DepthBox        AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Box Depth" HELP "Optional (Use 0.5 for 1/2) - Decimal"
    FIELD FPanel          AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "FPanel" HELP "Optional - Decimal"
    FIELD BPanel          AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "BPanel" HELP "Optional - Decimal"
    FIELD LengthBlank     AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Blank Length" HELP "Defaults to Style Calculation - Decimal"
    FIELD WidthBlank      AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Blank Width" HELP "Defaults to Style Calculation - Decimal "
    FIELD Purchased       AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Purchased" HELP "Optional - Enter P or Y for Purchased"
    FIELD CadID           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "CAD #     " HELP "Optional - Size:20"
    FIELD DieID           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "DIE #     " HELP "Optional - Size:20"
    FIELD DieInches       AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Die Inches" HELP "Optional - Decimal"
    FIELD SalesManID      AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Salesman" HELP "Defaults to Customer Sales Rep - Size:5"
    FIELD QuantityYield   AS INTEGER   FORMAT ">>>>>>>" COLUMN-LABEL "Yield Quantity" HELP "Defaults to 1 - Integer"
    FIELD Designer        AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Designer" HELP "Optional - Size:20"    
    FIELD WidthDie        AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Die W" HELP "Optional - Decimal"
    FIELD LengthDie       AS DECIMAL   FORMAT ">>>>>.99" COLUMN-LABEL "Die L" HELP "Optional - Decimal"
    FIELD InkDescription  AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Ink Description" HELP "Optional - Size:30"
    FIELD InkColors       AS INTEGER   FORMAT ">>" COLUMN-LABEL "Colors"  HELP "Optional - If not provided, derived from provided Ink Codes or 0"
    FIELD Ink1Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 1 Code" HELP "Optional - Field Validated - Size:10"
    FIELD Ink1Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 1 Coverage" HELP "Optional - Decimal"
    FIELD Ink1Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 1 Unit" HELP "Optional - Integer"
    FIELD Ink1Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 1 Pass" HELP "Optional - Integer"
    FIELD Ink1Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 1 Side" HELP "Optional - F or B"
    FIELD Ink2Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 2 Code" HELP "Optional - Field Validated - Size:10"
    FIELD Ink2Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 2 Coverage" HELP "Optional - Decimal"
    FIELD Ink2Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 2 Unit" HELP "Optional - Integer"
    FIELD Ink2Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 2 Pass" HELP "Optional - Integer"
    FIELD Ink2Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 2 Side" HELP "Optional - F or B"
    FIELD Ink3Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 3 Code" HELP "Optional - Field Validated - Size:10"
    FIELD Ink3Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 3 Coverage" HELP "Optional - Decimal"
    FIELD Ink3Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 3 Unit" HELP "Optional - Integer"
    FIELD Ink3Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 3 Pass" HELP "Optional - Integer"
    FIELD Ink3Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 3 Side" HELP "Optional - F or B"
    FIELD Ink4Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 4 Code" HELP "Optional - Field Validated - Size:10"
    FIELD Ink4Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 4 Coverage" HELP "Optional - Decimal"
    FIELD Ink4Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 4 Unit" HELP "Optional - Integer"
    FIELD Ink4Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 4 Pass" HELP "Optional - Integer"
    FIELD Ink4Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 4 Side" HELP "Optional - F or B"
    FIELD Ink5Code        AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ink 5 Code" HELP "Optional - Field Validated - Size:10"
    FIELD Ink5Coverage    AS DECIMAL   FORMAT ">>.99" COLUMN-LABEL "Ink 5 Coverage" HELP "Optional - Decimal"
    FIELD Ink5Unit        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 5 Unit" HELP "Optional - Integer"
    FIELD Ink5Pass        AS INTEGER   FORMAT ">>" COLUMN-LABEL "Ink 5 Pass" HELP "Optional - Integer"
    FIELD Ink5Side        AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Ink 5 Side"  HELP "Optional - F or B"  
    FIELD EstNote1Group   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Est Note 1 Group" HELP "Optional - Size:3"
    FIELD EstNote1Title   AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Est Note 1 Title" HELP "Optional - Size:60"
    FIELD EstNote1Note    AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Est Note 1 Note" HELP "Optional - Size:Large"
    FIELD EstNote2Group   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Est Note 2 Group" HELP "Optional - Size:3"
    FIELD EstNote2Title   AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Est Note 2 Title" HELP "Optional - Size:60"
    FIELD EstNote2Note    AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Est Note 2 Note" HELP "Optional - Size:Large"
    FIELD EstNote3Group   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Est Note 3 Group" HELP "Optional - Size:3"
    FIELD EstNote3Title   AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Est Note 3 Title" HELP "Optional - Size:60"
    FIELD EstNote3Note    AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Est Note 3 Note" HELP "Optional - Size:Large"
    FIELD SpecNote1Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 1 Group" HELP "Optional - Size:3"
    FIELD SpecNote1Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 1 Title" HELP "Optional - Size:60"
    FIELD SpecNote1Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 1 Note" HELP "Optional - Size:Large"
    FIELD SpecNote2Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 2 Group" HELP "Optional - Size:3"
    FIELD SpecNote2Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 2 Title" HELP "Optional - Size:60"
    FIELD SpecNote2Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 2 Note" HELP "Optional - Size:Large"
    FIELD SpecNote3Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 3 Group" HELP "Optional - Size:3"
    FIELD SpecNote3Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 3 Title" HELP "Optional - Size:60"
    FIELD SpecNote3Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 3 Note" HELP "Optional - Size:Large"
    FIELD SpecNote4Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 4 Group" HELP "Optional - Size:3"
    FIELD SpecNote4Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 4 Title" HELP "Optional - Size:60"
    FIELD SpecNote4Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 4 Note" HELP "Optional - Size:Large"
    FIELD SpecNote5Group  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Spec Note 5 Group" HELP "Optional - Size:3"
    FIELD SpecNote5Title  AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Spec Note 5 Title" HELP "Optional - Size:60"
    FIELD SpecNote5Note   AS CHARACTER FORMAT "X(200)" COLUMN-LABEL "Spec Note 5 Note" HELP "Optional - Size:Large"
    FIELD VendorID        AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Vendor ID" HELP "Optional - Field Validated - Size:10"
    FIELD VendorCost      AS DECIMAL   FORMAT ">>>>>>.99" COLUMN-LABEL "Vendor Cost" HELP "Optional - Decimal"
    FIELD VendorCostUOM   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Vendor Cost UOM" HELP "Optional - Field Validate - Size:3"
    FIELD VendorItemID    AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "Vendor Item" HELP "Optional - Size:15"
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 3. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Include Initializes the ttImportMap based on the temp-table definition - Procedure pInitialize*/
{util/ImportProcs.i &ImportTempTable = "ttImportEstimate"}

PROCEDURE pAddFarm:
    /*------------------------------------------------------------------------------
     Purpose: Builds the Farm tab for a Finished Good Item and Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportEstimate FOR ttImportEstimate.
    DEFINE PARAMETER BUFFER ipbf-eb               FOR eb.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE VARIABLE dQty1   AS DECIMAL.
    DEFINE VARIABLE dQty2   AS DECIMAL.
    DEFINE VARIABLE dPrice1 AS DECIMAL.
    DEFINE VARIABLE dPrice2 AS DECIMAL.


    IF ipbf-ttImportEstimate.Quantity GT 0 THEN 
        ASSIGN 
            dQty1   = ipbf-ttImportEstimate.Quantity - 0.001
            dQty2   = 9999999.9
            dPrice1 = 99999.99
            dPrice2 = ipbf-ttImportEstimate.VendorCost
            .
    ELSE
        ASSIGN 
            dQty1   = 9999999.9
            dQty2   = 0
            dPrice1 = ipbf-ttImportEstimate.VendorCost
            dPrice2 = 0
            . 
        
    IF ipbf-ttImportEstimate.VendorID NE "" THEN 
    DO:
        FIND FIRST e-itemfg EXCLUSIVE-LOCK 
            WHERE e-itemfg.company EQ ipbf-ttImportEstimate.Company 
            AND e-itemfg.i-no EQ ipcFGItemID
            NO-ERROR.
        IF NOT AVAILABLE e-itemfg THEN 
        DO:
            CREATE e-itemfg.
            ASSIGN 
                e-itemfg.company = ipbf-ttImportEstimate.Company
                e-itemfg.i-no    = ipcFGItemID
                e-itemfg.std-uom = IF ipbf-ttImportEstimate.VendorCostUOM EQ "" THEN "EA" ELSE ipbf-ttImportEstimate.VendorCostUOM 
                .
        END.
        
        /*find blank Vend on IF1*/
        FIND FIRST e-itemfg-vend NO-LOCK  
            WHERE e-itemfg-vend.company EQ ipbf-ttImportEstimate.Company 
            AND e-itemfg-vend.i-no EQ ipcFGItemID
            AND e-itemfg-vend.est-no EQ ""
            AND e-itemfg-vend.form-no EQ ipbf-eb.form-no
            AND e-itemfg-vend.blank-no EQ ipbf-eb.blank-no
            AND e-itemfg-vend.eqty EQ  ipbf-eb.eqty 
            AND e-itemfg-vend.vend-no EQ ""
            NO-ERROR.
        IF NOT AVAILABLE e-itemfg-vend THEN 
        DO:
            CREATE e-itemfg-vend.
            ASSIGN 
                e-itemfg-vend.company     = ipbf-ttImportEstimate.Company
                e-itemfg-vend.i-no        = ipcFGItemID
                e-itemfg-vend.run-qty[1]  = dQty1
                e-itemfg-vend.run-qty[2]  = dQty2
                e-itemfg-vend.run-cost[1] = dPrice1
                e-itemfg-vend.run-cost[2] = dPrice2     
                e-itemfg-vend.est-no      = ""  
                e-itemfg-vend.form-no     = ipbf-eb.form-no
                e-itemfg-vend.blank-no    = ipbf-eb.blank-no
                e-itemfg-vend.eqty        = ipbf-eb.eqty
                e-itemfg-vend.std-uom     = ipbf-ttImportEstimate.VendorCostUOM
                e-itemfg-vend.vend-item   = ipbf-ttImportEstimate.VendorItemID
                e-itemfg-vend.vend-no     = ""
                e-itemfg-vend.item-type   = NO
                .
        END.
        RELEASE e-itemfg-vend.
        
        /*find blank Vend on est*/
        FIND FIRST e-itemfg-vend NO-LOCK  
            WHERE e-itemfg-vend.company EQ ipbf-ttImportEstimate.Company 
            AND e-itemfg-vend.i-no EQ ipcFGItemID
            AND e-itemfg-vend.est-no EQ ipbf-eb.est-no
            AND e-itemfg-vend.form-no EQ ipbf-eb.form-no
            AND e-itemfg-vend.blank-no EQ ipbf-eb.blank-no
            AND e-itemfg-vend.eqty EQ  ipbf-eb.eqty 
            AND e-itemfg-vend.vend-no EQ ""
            NO-ERROR.
        IF NOT AVAILABLE e-itemfg-vend THEN 
        DO:
            CREATE e-itemfg-vend.
            ASSIGN 
                e-itemfg-vend.company     = ipbf-ttImportEstimate.Company
                e-itemfg-vend.i-no        = ipcFGItemID
                e-itemfg-vend.run-qty[1]  = dQty1
                e-itemfg-vend.run-qty[2]  = dQty2
                e-itemfg-vend.run-cost[1] = dPrice1
                e-itemfg-vend.run-cost[2] = dPrice2     
                e-itemfg-vend.est-no      = ipbf-eb.est-no   
                e-itemfg-vend.form-no     = ipbf-eb.form-no
                e-itemfg-vend.blank-no    = ipbf-eb.blank-no
                e-itemfg-vend.eqty        = ipbf-eb.eqty
                e-itemfg-vend.std-uom     = ipbf-ttImportEstimate.VendorCostUOM
                e-itemfg-vend.vend-item   = ipbf-ttImportEstimate.VendorItemID
                e-itemfg-vend.vend-no     = ""
                e-itemfg-vend.item-type   = NO
                .
        END.
        RELEASE e-itemfg-vend.
        /*find imported Vend on est*/
        FIND FIRST e-itemfg-vend NO-LOCK  
            WHERE e-itemfg-vend.company EQ ipbf-ttImportEstimate.Company 
            AND e-itemfg-vend.i-no EQ ipcFGItemID
            AND e-itemfg-vend.est-no EQ ipbf-eb.est-no
            AND e-itemfg-vend.form-no EQ ipbf-eb.form-no
            AND e-itemfg-vend.blank-no EQ ipbf-eb.blank-no
            AND e-itemfg-vend.vend-no EQ ipbf-ttImportEstimate.VendorID
            NO-ERROR.
        IF NOT AVAILABLE e-itemfg-vend THEN 
        DO:
            CREATE e-itemfg-vend.
            ASSIGN 
                e-itemfg-vend.company     = ipbf-ttImportEstimate.Company
                e-itemfg-vend.i-no        = ipcFGItemID
                e-itemfg-vend.run-qty[1]  = dQty1
                e-itemfg-vend.run-qty[2]  = dQty2
                e-itemfg-vend.run-cost[1] = dPrice1
                e-itemfg-vend.run-cost[2] = dPrice2     
                e-itemfg-vend.est-no      = ipbf-eb.est-no   
                e-itemfg-vend.form-no     = ipbf-eb.form-no
                e-itemfg-vend.blank-no    = ipbf-eb.blank-no
                e-itemfg-vend.eqty        = ipbf-eb.eqty
                e-itemfg-vend.std-uom     = ipbf-ttImportEstimate.VendorCostUOM
                e-itemfg-vend.vend-item   = ipbf-ttImportEstimate.VendorItemID
                e-itemfg-vend.vend-no     = ipbf-ttImportEstimate.VendorID
                e-itemfg-vend.item-type   = NO
                .
        END.
        RELEASE e-itemfg-vend.
        
        /*find imported Vend on IF1*/
        FIND FIRST e-itemfg-vend NO-LOCK  
            WHERE e-itemfg-vend.company EQ ipbf-ttImportEstimate.Company 
            AND e-itemfg-vend.i-no EQ ipcFGItemID
            AND e-itemfg-vend.est-no EQ ""
            AND e-itemfg-vend.form-no EQ ipbf-eb.form-no
            AND e-itemfg-vend.blank-no EQ ipbf-eb.blank-no
            AND e-itemfg-vend.vend-no EQ ipbf-ttImportEstimate.VendorID
            NO-ERROR.
        IF NOT AVAILABLE e-itemfg-vend THEN 
        DO:
            CREATE e-itemfg-vend.
            ASSIGN 
                e-itemfg-vend.company     = ipbf-ttImportEstimate.Company
                e-itemfg-vend.i-no        = ipcFGItemID
                e-itemfg-vend.run-qty[1]  = dQty1
                e-itemfg-vend.run-qty[2]  = dQty2
                e-itemfg-vend.run-cost[1] = dPrice1
                e-itemfg-vend.run-cost[2] = dPrice2     
                e-itemfg-vend.est-no      = ""  
                e-itemfg-vend.form-no     = ipbf-eb.form-no
                e-itemfg-vend.blank-no    = ipbf-eb.blank-no
                e-itemfg-vend.eqty        = ipbf-eb.eqty
                e-itemfg-vend.std-uom     = ipbf-ttImportEstimate.VendorCostUOM
                e-itemfg-vend.vend-item   = ipbf-ttImportEstimate.VendorItemID
                e-itemfg-vend.vend-no     = ipbf-ttImportEstimate.VendorID
                e-itemfg-vend.item-type   = NO
                .
        END.
        RELEASE e-itemfg-vend.
    END. /*ipbf-ttImportEstimate.VendorID ne ""*/

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
    DEFINE VARIABLE riNote    AS ROWID     NO-UNDO.
    
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
    ELSE 
    DO:
        cNoteType = "S".
        IF ipbf-ttImportEstimate.SpecNote1Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.SpecNote1Note, ipbf-ttImportEstimate.SpecNote1Title, ipbf-ttImportEstimate.SpecNote1Group, cNoteType, OUTPUT riNote).
        IF ipbf-ttImportEstimate.SpecNote2Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.SpecNote2Note, ipbf-ttImportEstimate.SpecNote2Title, ipbf-ttImportEstimate.SpecNote2Group, cNoteType, OUTPUT riNote).
        IF ipbf-ttImportEstimate.SpecNote3Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.SpecNote3Note, ipbf-ttImportEstimate.SpecNote3Title, ipbf-ttImportEstimate.SpecNote3Group, cNoteType, OUTPUT riNote).
        IF ipbf-ttImportEstimate.SpecNote4Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.SpecNote4Note, ipbf-ttImportEstimate.SpecNote4Title, ipbf-ttImportEstimate.SpecNote4Group, cNoteType, OUTPUT riNote).
        IF ipbf-ttImportEstimate.SpecNote5Note NE "" THEN 
            RUN util/AddNote.p (ipcRecKey, ipbf-ttImportEstimate.SpecNote5Note, ipbf-ttImportEstimate.SpecNote5Title, ipbf-ttImportEstimate.SpecNote5Group, cNoteType, OUTPUT riNote).
    END.
     

END PROCEDURE.

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportEstimate FOR ttImportEstimate.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportEstimate FOR ttImportEstimate.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
       
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportEstimate.CustomerID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank CustomerID".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportEstimate.Style EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Style".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportEstimate.Category EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Category".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportEstimate.Quantity LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Quantity".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportEstimate.LengthBox LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Box Length".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportEstimate.WidthBox LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Blank Box Width".
    END.
    IF oplValid THEN 
    DO:
        IF LOOKUP(ipbf-ttImportEstimate.Industry,"C,F") EQ 0 THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Industry not C or F"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipbf-ttImportEstimate.Company
            AND cust.cust-no EQ ipbf-ttImportEstimate.CustomerID
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
            WHERE bf-ttImportEstimate.Company EQ ipbf-ttImportEstimate.Company
            AND bf-ttImportEstimate.CustomerID EQ ipbf-ttImportEstimate.CustomerID
            AND bf-ttImportEstimate.PartID EQ ipbf-ttImportEstimate.PartID
            AND ROWID(bf-ttImportEstimate) NE ROWID(ipbf-ttImportEstimate)
            NO-ERROR.
        IF AVAILABLE bf-ttImportEstimate THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportEstimate.EstimateNo NE "" THEN 
            FIND FIRST est NO-LOCK 
                WHERE est.company EQ ipbf-ttImportEstimate.Company
                AND est.est-no EQ ipbf-ttImportEstimate.EstimateNo
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
        IF ipbf-ttImportEstimate.ShipToID NE "" THEN 
        DO:
            FIND FIRST shipto NO-LOCK 
                WHERE shipto.company EQ ipbf-ttImportEstimate.Company
                AND shipto.ship-id EQ ipbf-ttImportEstimate.ShipToID
                NO-ERROR.
            IF NOT AVAILABLE shipto THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid ShipTo ID"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.FGItemID NE "" AND ipbf-ttImportEstimate.FGItemID NE "<AUTO>" THEN 
        DO:
            FIND FIRST itemfg NO-LOCK 
                WHERE itemfg.company EQ ipbf-ttImportEstimate.Company
                AND itemfg.i-no EQ ipbf-ttImportEstimate.FGItemID
                NO-ERROR.
            IF NOT AVAILABLE itemfg THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid FG Item"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.Style NE "" THEN 
        DO:
            FIND FIRST style NO-LOCK 
                WHERE style.company EQ ipbf-ttImportEstimate.Company
                AND style.style EQ ipbf-ttImportEstimate.Style
                NO-ERROR.
            IF NOT AVAILABLE style THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Style"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.Flute NE "" THEN 
        DO:
            FIND FIRST flute NO-LOCK 
                WHERE flute.company EQ ipbf-ttImportEstimate.Company
                AND flute.code EQ ipbf-ttImportEstimate.Flute
                NO-ERROR.
            IF NOT AVAILABLE flute THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Flute"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.BoardID NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ipbf-ttImportEstimate.Company
                AND item.i-no EQ ipbf-ttImportEstimate.BoardID
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Board ID"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.GlueID NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ipbf-ttImportEstimate.Company
                AND item.i-no EQ ipbf-ttImportEstimate.GlueID
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Glue ID"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.Category NE "" THEN 
        DO:
            FIND FIRST fgcat NO-LOCK 
                WHERE fgcat.company EQ ipbf-ttImportEstimate.Company
                AND fgcat.procat EQ ipbf-ttImportEstimate.Category
                NO-ERROR.
            IF NOT AVAILABLE fgcat THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Category"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.Ink1Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ipbf-ttImportEstimate.Company
                AND item.i-no EQ ipbf-ttImportEstimate.Ink1Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 1 Code"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.Ink2Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ipbf-ttImportEstimate.Company
                AND item.i-no EQ ipbf-ttImportEstimate.Ink2Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 2 Code"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.Ink3Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ipbf-ttImportEstimate.Company
                AND item.i-no EQ ipbf-ttImportEstimate.Ink3Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 3 Code"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.Ink4Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ipbf-ttImportEstimate.Company
                AND item.i-no EQ ipbf-ttImportEstimate.Ink4Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 4 Code"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.Ink5Code NE "" THEN 
        DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ipbf-ttImportEstimate.Company
                AND item.i-no EQ ipbf-ttImportEstimate.Ink5Code
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ink 5 Code"
                    .
        END.
        IF oplValid AND ipbf-ttImportEstimate.SalesManID NE "" THEN 
        DO:
            FIND FIRST sman NO-LOCK 
                WHERE sman.company EQ ipbf-ttImportEstimate.Company
                AND sman.sman EQ ipbf-ttImportEstimate.SalesManID
                NO-ERROR.
            IF NOT AVAILABLE sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Salesman"
                    .    
        END.    
        IF oplValid AND ipbf-ttImportEstimate.VendorID NE "" THEN 
        DO:
            FIND FIRST vend NO-LOCK 
                WHERE vend.company EQ ipbf-ttImportEstimate.Company
                AND vend.vend-no EQ ipbf-ttImportEstimate.VendorID
                NO-ERROR.
            IF NOT AVAILABLE vend THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Vendor"
                    .
        END.
    END.
    
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


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportEstimate FOR ttImportEstimate.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.


    DEFINE VARIABLE riEb      AS ROWID     NO-UNDO.
    DEFINE VARIABLE cIndustry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEstType  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE riItemfg  AS ROWID     NO-UNDO.
        
    ASSIGN 
        cIndustry = ipbf-ttImportEstimate.Industry
        .
    IF cIndustry EQ "C" THEN 
        iEstType = 5. 
    ELSE 
        iEstType = 1.
    FIND FIRST est NO-LOCK 
        WHERE ROWID(est) EQ ipbf-ttImportEstimate.riParentEst NO-ERROR.
    IF NOT AVAILABLE est THEN 
    DO:
        RUN est/NewEstimate.p (cIndustry, iEstType, OUTPUT riEb).
    END.
    ELSE 
    DO:
        RUN est/NewEstimateForm.p (cIndustry, ROWID(est), OUTPUT riEb).
    END.
    iopiAdded = iopiAdded + 1.
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
        eb.eqty         = ipbf-ttImportEstimate.Quantity.
    ASSIGN 
        est-qty.eqty   = eb.eqty
        est.est-qty[1] = eb.eqty
        ef.eqty        = eb.eqty
        est-qty.qty[1] = eb.eqty
        eb.bl-qty = eb.eqty
        .

    ASSIGN 
        eb.part-no      = ipbf-ttImportEstimate.PartID
        eb.part-dscr1   = ipbf-ttImportEstimate.PartName
        eb.part-dscr2   = ipbf-ttImportEstimate.PartDescription
        eb.style        = ipbf-ttImportEstimate.Style
        eb.sman         = ipbf-ttImportEstimate.SalesManID
        eb.die-in       = ipbf-ttImportEstimate.DieInches
        eb.cad-no       = ipbf-ttImportEstimate.CadID
        eb.die-no       = ipbf-ttImportEstimate.DieID
        eb.num-up       = 1
        eb.num-wid      = 1
        eb.num-len      = 1
        eb.pur-man      = ipbf-ttImportEstimate.Purchased EQ "YES" OR ipbf-ttImportEstimate.Purchased EQ "P"
        eb.spare-char-1 = ipbf-ttImportEstimate.Designer
        eb.len          = ipbf-ttImportEstimate.LengthBox
        eb.wid          = ipbf-ttImportEstimate.WidthBox
        eb.dep          = ipbf-ttImportEstimate.DepthBox
        eb.i-coldscr    = ipbf-ttImportEstimate.InkDescription
        ef.board        = ipbf-ttImportEstimate.Board     
        ef.nc           = NOT eb.pur-man   
        ef.blank-qty    = 1
        ef.trim-w       = ipbf-ttImportEstimate.WidthDie
        ef.trim-l       = ipbf-ttImportEstimate.LengthDie
        .

    IF ipbf-ttImportEstimate.Category NE '' THEN 
        eb.procat       = ipbf-ttImportEstimate.Category.
    
    eb.quantityPerSet = 1.
    
    IF ipbf-ttImportEstimate.QuantityYield GT 0 THEN 
        eb.yld-qty      = ipbf-ttImportEstimate.QuantityYield.
    ELSE 
        eb.yld-qty      = 1.
        
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ  eb.company 
        AND cust.cust-no EQ ipbf-ttImportEstimate.CustomerID
        NO-ERROR.

    IF AVAILABLE cust THEN 
    DO: 
        eb.cust-no = cust.cust-no.
        IF ipbf-ttImportEstimate.ShipToID NE '' THEN 
            FIND FIRST shipto NO-LOCK 
                WHERE shipto.company EQ cust.company
                AND shipto.cust-no EQ cust.cust-no
                AND shipto.ship-id EQ ipbf-ttImportEstimate.ShipToID
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
    IF ipbf-ttImportEstimate.SalesManID NE '' THEN 
        eb.sman = ipbf-ttImportEstimate.SalesManID.
            
    IF ipbf-ttImportEstimate.TabInOut EQ '' THEN 
        eb.tab-in = YES.
    ELSE 
        eb.tab-in = ipbf-ttImportEstimate.TabInOut EQ "In".
        
    eb.dust = ipbf-ttImportEstimate.FPanel.
    eb.fpanel = ipbf-ttImportEstimate.BPanel.
        
    RUN est/CalcBlankSize.p (cIndustry, ROWID(eb)).
   
    IF ipbf-ttImportEstimate.LengthBlank GT 0 THEN 
        eb.t-len = ipbf-ttImportEstimate.LengthBlank.
    IF ipbf-ttImportEstimate.WidthBlank GT 0 THEN 
        eb.t-wid = ipbf-ttImportEstimate.WidthBlank.    
    
    RUN pAssignInks (BUFFER eb, BUFFER ipbf-ttImportEstimate).

    IF ef.board EQ '' THEN 
    DO:
        FIND FIRST ITEM NO-LOCK 
            WHERE item.company EQ ef.company
            AND  item.flute EQ ipbf-ttImportEstimate.Flute
            AND item.reg-no EQ ipbf-ttImportEstimate.Test
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
  
    RUN pAddNotes(BUFFER ipbf-ttImportEstimate, "Estimate", est.rec_key).

    RUN est/CalcPacking.p(ROWID(eb)).
               
    IF ipbf-ttImportEstimate.FGItemID NE "" THEN 
    DO:
        IF ipbf-ttImportEstimate.FGItemID EQ "<AUTO>" THEN 
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
                AND itemfg.i-no EQ ipbf-ttImportEstimate.FGItemID
                NO-ERROR.
        END.  
    END.
        
    IF AVAILABLE itemfg THEN 
    DO:
        eb.stock-no = itemfg.i-no.
        RUN pAddNotes(BUFFER ipbf-ttImportEstimate, "Spec", itemfg.rec_key).
        RUN pAddFarm(BUFFER ipbf-ttImportEstimate, BUFFER eb, itemfg.i-no).
            
    END. 
    
END PROCEDURE.

