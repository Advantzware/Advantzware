
/*------------------------------------------------------------------------
    File        : ImportShipTo.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Ship Tos	

    Author(s)   : BV
    Created     : Fri Nov 24 16:18:38 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportVendCost
    FIELD Company            AS CHARACTER 
    FIELD Location           AS CHARACTER 
    FIELD FGItemID          AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "FG Item#" HELP "Required - Must be Valid - Size:15"
    FIELD VendorID          AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Vendor" HELP "Optional - Field Validated - Size:8"
    FIELD CustomerID        AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Customer" HELP "Optional - Field Validated - Size:8"
    FIELD PurchaseUOM       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Cost UOM" HELP "Required - Must be Valid - Size:3"
    FIELD VendorItemID      AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Vendor Item#" HELP "Optional - validated - Size:15"
    FIELD LevelQuantity01   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 1" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM01 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 1" HELP "Optional - decimal"
    FIELD LevelSetup01      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 1" HELP "Optional - decimal" 
    FIELD LevelQuantity02   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 2" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM02 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 2" HELP "Optional - decimal"
    FIELD LevelSetup02      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 2" HELP "Optional - decimal" 
    FIELD LevelQuantity03   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 3" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM03 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 3" HELP "Optional - decimal"
    FIELD LevelSetup03      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 3" HELP "Optional - decimal" 
    FIELD LevelQuantity04   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 4" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM04 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 4" HELP "Optional - decimal"
    FIELD LevelSetup04      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 4" HELP "Optional - decimal" 
    FIELD LevelQuantity05   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 5" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM05 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 5" HELP "Optional - decimal"
    FIELD LevelSetup05      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 5" HELP "Optional - decimal" 
    FIELD LevelQuantity06   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 6" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM06 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 6" HELP "Optional - decimal"
    FIELD LevelSetup06      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 6" HELP "Optional - decimal" 
    FIELD LevelQuantity07   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 7" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM07 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 7" HELP "Optional - decimal"
    FIELD LevelSetup07      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 7" HELP "Optional - decimal" 
    FIELD LevelQuantity08   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 8" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM08 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 8" HELP "Optional - decimal"
    FIELD LevelSetup08      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 8" HELP "Optional - decimal" 
    FIELD LevelQuantity09   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 9" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM09 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 9" HELP "Optional - decimal"
    FIELD LevelSetup09      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 9" HELP "Optional - decimal" 
    FIELD LevelQuantity10   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 10" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM10 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 10" HELP "Optional - decimal"
    FIELD LevelSetup10      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 10" HELP "Optional - decimal"
    FIELD SheetWidthMin     AS DECIMAL   FORMAT ">>9.9999" INITIAL 0.0000 COLUMN-LABEL "Minimum Sheet Width" HELP "Optional - Defaults to 0.0000 -  decimal"
    FIELD SheetWidthMax     AS DECIMAL   FORMAT ">>9.9999" INITIAL 999.9999 COLUMN-LABEL "Maximum Sheet Width" HELP "Optional - Defaults to 999.9999 - decimal"
    FIELD SheetLengthMin    AS DECIMAL   FORMAT ">>9.9999" INITIAL 0.0000 COLUMN-LABEL "Minimum Sheet Length" HELP "Optional - Defaults to 0.0000 - decimal"
    FIELD SheetLengthMax    AS DECIMAL   FORMAT ">>9.9999" INITIAL 999.9999 COLUMN-LABEL "Maximum Sheet Length" HELP "Optional - Defaults to 999.9999 - decimal"
    FIELD MinimumCharge     AS DECIMAL   FORMAT "->>,>>9.99" COLUMN-LABEL "Minimum Charge" HELP "Optional - decimal"
    FIELD GSAMarkupPercent  AS DECIMAL   FORMAT ">,>>9":U INITIAL 0 COLUMN-LABEL "GS&&A O/H Markup %" HELP "Optional - Decimal"
    .
    

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

                                    
/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportVendCost"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportVendCost FOR ttImportVendCost.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportVendCost FOR ttImportVendCost.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVendCost.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company.".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVendCost.FGItemID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: FG Item#.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVendCost.PurchaseUOM EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Cost UOM.".
    END.

    IF oplValid AND iplFieldValidation THEN 
    DO: 
        IF oplValid AND ipbf-ttImportVendCost.FGItemID NE "" THEN
            RUN pIsValidFGItemID IN hdValidator (ipbf-ttImportVendCost.FGItemID, NO, ipbf-ttImportVendCost.Company, OUTPUT oplValid, OUTPUT cValidNote). 
        
        IF oplValid AND ipbf-ttImportVendCost.VendorID NE "" THEN 
            RUN pIsValidVendor IN hdValidator (ipbf-ttImportVendCost.VendorID, NO, ipbf-ttImportVendCost.Company, OUTPUT oplValid, OUTPUT cValidNote). 
        
        IF oplValid AND ipbf-ttImportVendCost.CustomerID NE "" THEN 
            RUN pIsValidCustomerID IN hdValidator (ipbf-ttImportVendCost.CustomerID, NO, ipbf-ttImportVendCost.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportVendCost.PurchaseUOM NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Cost UOM", ipbf-ttImportVendCost.PurchaseUOM, "C,CS,L,MSF,M,EA,LB,DRM,ROL,PKG,SET,DOZ,BDL", OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportVendCost FOR ttImportVendCost.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.



    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipbf-ttImportVendCost.Company
        AND itemfg.i-no EQ ipbf-ttImportVendCost.FGItemID NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST e-itemfg EXCLUSIVE-LOCK
            WHERE e-itemfg.company EQ ipbf-ttImportVendCost.Company
            AND e-itemfg.i-no EQ itemfg.i-no NO-ERROR.
        IF AVAILABLE e-itemfg THEN 
            FIND FIRST e-itemfg-vend EXCLUSIVE-LOCK 
                WHERE e-itemfg-vend.company EQ ipbf-ttImportVendCost.Company
                AND e-itemfg-vend.i-no    EQ ipbf-ttImportVendCost.FGItemID
                AND e-itemfg-vend.vend-no EQ ipbf-ttImportVendCost.VendorID
                AND e-itemfg-vend.cust-no EQ ipbf-ttImportVendCost.CustomerID 
                NO-ERROR.
      
        IF NOT AVAILABLE e-itemfg-vend THEN 
        DO:
          
            iopiAdded = iopiAdded + 1.

            CREATE e-itemfg-vend.
            ASSIGN 
                e-itemfg-vend.company   = ipbf-ttImportVendCost.Company
                e-itemfg-vend.i-no      = ipbf-ttImportVendCost.FGItemID
                e-itemfg-vend.vend-no      = ipbf-ttImportVendCost.VendorID  
                e-itemfg-vend.cust-no      = ipbf-ttImportVendCost.CustomerID  
                e-itemfg-vend.item-type = NO
                e-itemfg-vend.est-no    = ""
                e-itemfg-vend.roll-w[27]   = 0     
                e-itemfg-vend.roll-w[28]   = 999.9999     
                e-itemfg-vend.roll-w[29]   = 0    
                e-itemfg-vend.roll-w[30]   = 999.9999
                .

        END.
        RUN pAssignValueC (ipbf-ttImportVendCost.PurchaseUOM, YES, INPUT-OUTPUT e-itemfg.std-uom).  
        RUN pAssignValueC (ipbf-ttImportVendCost.PurchaseUOM, YES, INPUT-OUTPUT e-itemfg-vend.std-uom).
        RUN pAssignValueC (ipbf-ttImportVendCost.VendorItemID, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.vend-item).  
        RUN pAssignValueD (ipbf-ttImportVendCost.SheetWidthMin, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.roll-w[27]).
        RUN pAssignValueD (ipbf-ttImportVendCost.SheetWidthMax, YES, INPUT-OUTPUT e-itemfg-vend.roll-w[28]).
        RUN pAssignValueD (ipbf-ttImportVendCost.SheetLengthMin, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.roll-w[29]).
        RUN pAssignValueD (ipbf-ttImportVendCost.SheetLengthMax, YES, INPUT-OUTPUT e-itemfg-vend.roll-w[30]).
        RUN pAssignValueD (ipbf-ttImportVendCost.MinimumCharge, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.spare-dec-1).
        RUN pAssignValueD (ipbf-ttImportVendCost.GSAMarkupPercent, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.markup).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity01, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[1]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM01, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[1]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup01, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[1]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity02, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[2]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM02, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[2]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup02, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[2]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity03, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[3]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM03, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[3]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup03, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[3]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity04, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[4]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM04, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[4]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup04, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[4]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity05, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[5]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM05, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[5]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup05, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[5]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity06, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[6]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM06, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[6]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup06, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[6]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity07, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[7]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM07, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[7]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup07, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[7]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity08, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[8]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM08, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[8]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup08, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[8]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity09, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[9]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM09, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[9]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup09, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[9]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelQuantity10, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-qty[10]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelCostPerUOM10, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.run-cost[10]).
        RUN pAssignValueD (ipbf-ttImportVendCost.LevelSetup10, iplIgnoreBlanks, INPUT-OUTPUT e-itemfg-vend.setups[10]).
        

        FIND FIRST reftable EXCLUSIVE
              WHERE reftable.reftable EQ 'e-itemfg-vend.markup'
              AND reftable.company EQ e-itemfg-vend.company
              AND reftable.loc EQ e-itemfg-vend.i-no 
              AND reftable.code EQ e-itemfg-vend.vend-no NO-ERROR.
          
        IF NOT AVAILABLE reftable THEN 
        DO:
            CREATE reftable.
            ASSIGN
                reftable.reftable = 'e-itemfg-vend.markup'
                reftable.company  = e-itemfg-vend.company
                reftable.loc      = e-itemfg-vend.i-no
                reftable.code     = e-itemfg-vend.vend-no.
        END.
        ASSIGN 
            reftable.val[1] = e-itemfg-vend.markup.
          

        FIND FIRST eb NO-LOCK
            WHERE eb.company EQ itemfg.company
            AND eb.est-no EQ itemfg.est-no 
            AND eb.stock-no EQ itemfg.i-no  NO-ERROR.
        IF AVAILABLE eb THEN 
        DO:
            FIND FIRST bf-e-itemfg-vend EXCLUSIVE-LOCK
                WHERE bf-e-itemfg-vend.company EQ eb.company
                AND bf-e-itemfg-vend.est-no EQ eb.est-no
                AND bf-e-itemfg-vend.eqty EQ eb.eqty
                AND bf-e-itemfg-vend.form-no EQ eb.form-no
                AND bf-e-itemfg-vend.blank-no EQ eb.blank-no
                AND bf-e-itemfg-vend.i-no EQ eb.stock-no
                AND bf-e-itemfg-vend.vend-no EQ e-itemfg-vend.vend-no 
                NO-ERROR.
            IF NOT AVAILABLE bf-e-itemfg-vend THEN 
            DO:
                CREATE bf-e-itemfg-vend.
                ASSIGN
                    bf-e-itemfg-vend.company   = eb.company
                    bf-e-itemfg-vend.i-no      = eb.stock-no
                    bf-e-itemfg-vend.item-type = NO
                    bf-e-itemfg-vend.est-no    = eb.est-no
                    bf-e-itemfg-vend.eqty      = eb.eqty
                    bf-e-itemfg-vend.form-no   = eb.form-no
                    bf-e-itemfg-vend.blank-no  = eb.blank-no.

                FIND FIRST reftable 
                    WHERE reftable.reftable EQ "e-itemfg-vend.std-uom" 
                    AND reftable.company  EQ bf-e-itemfg-vend.company   
                    AND reftable.loc      EQ ""                      
                    AND reftable.code     EQ bf-e-itemfg-vend.est-no    
                    AND reftable.val[1]   EQ bf-e-itemfg-vend.form-no   
                    AND reftable.val[2]   EQ bf-e-itemfg-vend.blank-no NO-ERROR.

                IF NOT AVAILABLE reftable THEN 
                DO:
                    CREATE reftable.
                    ASSIGN
                        reftable.reftable = "e-itemfg-vend.std-uom"
                        reftable.company  = bf-e-itemfg-vend.company
                        reftable.loc      = ""
                        reftable.code     = bf-e-itemfg-vend.est-no
                        reftable.val[1]   = bf-e-itemfg-vend.form-no
                        reftable.val[2]   = bf-e-itemfg-vend.blank-no.
                END.
                ASSIGN 
                    reftable.code2 = e-itemfg.std-uom.
            END.
            BUFFER-COPY e-itemfg-vend EXCEPT company i-no item-type est-no eqty form-no blank-no TO bf-e-itemfg-vend.
           
        END.
    END.
       
END PROCEDURE.


/* ************************  Function Implementations ***************** */

