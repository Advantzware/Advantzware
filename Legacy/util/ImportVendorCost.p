
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
    FIELD cFGItemID          AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "FG Item#" HELP "Required - Must be Valid - Size:15"
    FIELD cVendorID          AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Vendor" HELP "Optional - Field Validated - Size:8"
    FIELD cCustomerID        AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Customer" HELP "Optional - Field Validated - Size:8"
    FIELD cPurchaseUOM       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Cost UOM" HELP "Required - Must be Valid - Size:3"
    FIELD cVendorItemID      AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Vendor Item#" HELP "Optional - validated - Size:15"
    FIELD dLevelQuantity01   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 1" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM01 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 1" HELP "Optional - decimal"
    FIELD dLevelSetup01      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 1" HELP "Optional - decimal" 
    FIELD dLevelQuantity02   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 2" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM02 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 2" HELP "Optional - decimal"
    FIELD dLevelSetup02      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 2" HELP "Optional - decimal" 
    FIELD dLevelQuantity03   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 3" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM03 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 3" HELP "Optional - decimal"
    FIELD dLevelSetup03      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 3" HELP "Optional - decimal" 
    FIELD dLevelQuantity04   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 4" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM04 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 4" HELP "Optional - decimal"
    FIELD dLevelSetup04      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 4" HELP "Optional - decimal" 
    FIELD dLevelQuantity05   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 5" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM05 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 5" HELP "Optional - decimal"
    FIELD dLevelSetup05      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 5" HELP "Optional - decimal" 
    FIELD dLevelQuantity06   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 6" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM06 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 6" HELP "Optional - decimal"
    FIELD dLevelSetup06      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 6" HELP "Optional - decimal" 
    FIELD dLevelQuantity07   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 7" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM07 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 7" HELP "Optional - decimal"
    FIELD dLevelSetup07      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 7" HELP "Optional - decimal" 
    FIELD dLevelQuantity08   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 8" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM08 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 8" HELP "Optional - decimal"
    FIELD dLevelSetup08      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 8" HELP "Optional - decimal" 
    FIELD dLevelQuantity09   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 9" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM09 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 9" HELP "Optional - decimal"
    FIELD dLevelSetup09      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 9" HELP "Optional - decimal" 
    FIELD dLevelQuantity10   AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Quantity 10" HELP "Optional - decimal" 
    FIELD dLevelCostPerUOM10 AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 10" HELP "Optional - decimal"
    FIELD dLevelSetup10      AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 10" HELP "Optional - decimal"
    FIELD dSheetWidthMin     AS DECIMAL   FORMAT ">>9.9999" INITIAL 0.0000 COLUMN-LABEL "Minimum Sheet Width" HELP "Optional - Defaults to 0.0000 -  decimal"
    FIELD dSheetWidthMax     AS DECIMAL   FORMAT ">>9.9999" INITIAL 0.0000 COLUMN-LABEL "Maximum Sheet Width" HELP "Optional - Defaults to 999.9999 - decimal"
    FIELD dSheetLengthMin    AS DECIMAL   FORMAT ">>9.9999" INITIAL 0.0000 COLUMN-LABEL "Minimum Sheet Length" HELP "Optional - Defaults to 0.0000 - decimal"
    FIELD dSheetLengthMax    AS DECIMAL   FORMAT ">>9.9999" INITIAL 0.0000 COLUMN-LABEL "Maximum Sheet Length" HELP "Optional - Defaults to 999.9999 - decimal"
    FIELD dMinimumCharge     AS DECIMAL   FORMAT "->>,>>9.99" COLUMN-LABEL "Minimum Charge" HELP "Optional - decimal"
    FIELD iGSAMarkupPercent  AS INTEGER   FORMAT ">,>>9":U INITIAL 0 COLUMN-LABEL "GS&&A O/H Markup %" HELP "Optional - integer"
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
        IF ipbf-ttImportVendCost.cFGItemID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: FG Item#.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVendCost.cPurchaseUOM EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Cost UOM.".
    END.

    IF oplValid AND iplFieldValidation THEN 
    DO: 
        IF oplValid AND ipbf-ttImportVendCost.cFGItemID NE "" THEN 
        DO:

            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ ipbf-ttImportVendCost.Company
                AND itemfg.i-no EQ ipbf-ttImportVendCost.cFGItemID NO-ERROR.

            IF NOT AVAILABLE itemfg THEN
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid FG Item#. "
                    .
        END.
        IF oplValid AND ipbf-ttImportVendCost.cVendorID NE "" THEN 
        DO:
            FIND FIRST vend NO-LOCK
                WHERE vend.company EQ ipbf-ttImportVendCost.Company
                AND vend.vend-no EQ ipbf-ttImportVendCost.cVendorID NO-ERROR.
            
            IF NOT AVAILABLE vend THEN
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Vendor. "
                    .
        END.
        IF oplValid AND ipbf-ttImportVendCost.cCustomerID NE "" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipbf-ttImportVendCost.Company
                AND  cust.cust-no EQ ipbf-ttImportVendCost.cCustomerID NO-ERROR.
            
            IF NOT AVAILABLE cust THEN
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Customer. "
                    .
        END.
        IF oplValid AND ipbf-ttImportVendCost.cPurchaseUOM NE "" THEN 
        DO:
            IF LOOKUP(ipbf-ttImportVendCost.cPurchaseUOM,"C,CS,L,MSF,M,EA,LB,DRM,ROL,PKG,SET,DOZ,BDL") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Cost UOM"
                    .
        END.
        
    END.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportVendCost FOR ttImportVendCost.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.



    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipbf-ttImportVendCost.Company
        AND itemfg.i-no EQ ipbf-ttImportVendCost.cFGItemID NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST e-itemfg EXCLUSIVE-LOCK
            WHERE e-itemfg.company EQ ipbf-ttImportVendCost.Company
            AND e-itemfg.i-no EQ itemfg.i-no NO-ERROR.
        IF AVAILABLE e-itemfg THEN 
            FIND FIRST e-itemfg-vend EXCLUSIVE-LOCK 
                WHERE e-itemfg-vend.company EQ ipbf-ttImportVendCost.Company
                AND e-itemfg-vend.i-no    EQ ipbf-ttImportVendCost.cFGItemID
                AND e-itemfg-vend.vend-no EQ ipbf-ttImportVendCost.cVendorID
                AND e-itemfg-vend.cust-no EQ ipbf-ttImportVendCost.cCustomerID NO-ERROR.
      
        IF NOT AVAILABLE e-itemfg-vend THEN 
        DO:
          
            iopiAdded = iopiAdded + 1.

            CREATE e-itemfg-vend.
            ASSIGN 
                e-itemfg-vend.company   = ipbf-ttImportVendCost.Company
                e-itemfg-vend.i-no      = ipbf-ttImportVendCost.cFGItemID
                e-itemfg-vend.item-type = NO
                e-itemfg-vend.est-no    = "".

        END.
        ASSIGN
            e-itemfg-vend.vend-no      = ipbf-ttImportVendCost.cVendorID          
            e-itemfg-vend.cust-no      = ipbf-ttImportVendCost.cCustomerID       
            e-itemfg.std-uom           = IF ipbf-ttImportVendCost.cPurchaseUOM EQ "" THEN itemfg.pur-uom ELSE ipbf-ttImportVendCost.cPurchaseUOM       
            e-itemfg-vend.vend-item    = ipbf-ttImportVendCost.cVendorItemID
            e-itemfg-vend.roll-w[27]   = IF ipbf-ttImportVendCost.dSheetWidthMin NE 0 THEN ipbf-ttImportVendCost.dSheetWidthMin ELSE 0     
            e-itemfg-vend.roll-w[28]   = IF ipbf-ttImportVendCost.dSheetWidthMax NE 0 THEN ipbf-ttImportVendCost.dSheetWidthMax ELSE  999.9999     
            e-itemfg-vend.roll-w[29]   = IF ipbf-ttImportVendCost.dSheetLengthMin NE 0 THEN ipbf-ttImportVendCost.dSheetLengthMin ELSE 0    
            e-itemfg-vend.roll-w[30]   = IF ipbf-ttImportVendCost.dSheetLengthMax NE 0 THEN  ipbf-ttImportVendCost.dSheetLengthMax ELSE  999.9999     
            e-itemfg-vend.spare-dec-1  = ipbf-ttImportVendCost.dMinimumCharge     
            e-itemfg-vend.markup       = ipbf-ttImportVendCost.iGSAMarkupPercent
            e-itemfg-vend.run-qty[1]   = ipbf-ttImportVendCost.dLevelQuantity01   
            e-itemfg-vend.run-cost[1]  = ipbf-ttImportVendCost.dLevelCostPerUOM01
            e-itemfg-vend.setups[1]    = ipbf-ttImportVendCost.dLevelSetup01
            e-itemfg-vend.run-qty[2]   = ipbf-ttImportVendCost.dLevelQuantity02   
            e-itemfg-vend.run-cost[2]  = ipbf-ttImportVendCost.dLevelCostPerUOM02
            e-itemfg-vend.setups[2]    = ipbf-ttImportVendCost.dLevelSetup02
            e-itemfg-vend.run-qty[3]   = ipbf-ttImportVendCost.dLevelQuantity03   
            e-itemfg-vend.run-cost[3]  = ipbf-ttImportVendCost.dLevelCostPerUOM03
            e-itemfg-vend.setups[3]    = ipbf-ttImportVendCost.dLevelSetup03
            e-itemfg-vend.run-qty[4]   = ipbf-ttImportVendCost.dLevelQuantity04   
            e-itemfg-vend.run-cost[4]  = ipbf-ttImportVendCost.dLevelCostPerUOM04
            e-itemfg-vend.setups[4]    = ipbf-ttImportVendCost.dLevelSetup04 
            e-itemfg-vend.run-qty[5]   = ipbf-ttImportVendCost.dLevelQuantity05  
            e-itemfg-vend.run-cost[5]  = ipbf-ttImportVendCost.dLevelCostPerUOM05
            e-itemfg-vend.setups[5]    = ipbf-ttImportVendCost.dLevelSetup05
            e-itemfg-vend.run-qty[6]   = ipbf-ttImportVendCost.dLevelQuantity06   
            e-itemfg-vend.run-cost[6]  = ipbf-ttImportVendCost.dLevelCostPerUOM06
            e-itemfg-vend.setups[6]    = ipbf-ttImportVendCost.dLevelSetup06 
            e-itemfg-vend.run-qty[7]   = ipbf-ttImportVendCost.dLevelQuantity07   
            e-itemfg-vend.run-cost[7]  = ipbf-ttImportVendCost.dLevelCostPerUOM07
            e-itemfg-vend.setups[7]    = ipbf-ttImportVendCost.dLevelSetup07
            e-itemfg-vend.run-qty[8]   = ipbf-ttImportVendCost.dLevelQuantity08   
            e-itemfg-vend.run-cost[8]  = ipbf-ttImportVendCost.dLevelCostPerUOM08
            e-itemfg-vend.setups[8]    = ipbf-ttImportVendCost.dLevelSetup08 
            e-itemfg-vend.run-qty[9]   = ipbf-ttImportVendCost.dLevelQuantity09   
            e-itemfg-vend.run-cost[9]  = ipbf-ttImportVendCost.dLevelCostPerUOM09
            e-itemfg-vend.setups[9]    = ipbf-ttImportVendCost.dLevelSetup09 
            e-itemfg-vend.run-qty[10]  = ipbf-ttImportVendCost.dLevelQuantity10   
            e-itemfg-vend.run-cost[10] = ipbf-ttImportVendCost.dLevelCostPerUOM10
            e-itemfg-vend.setups[10]   = ipbf-ttImportVendCost.dLevelSetup10.

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
            reftable.val[1] = INT(ipbf-ttImportVendCost.iGSAMarkupPercent).
          

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
                AND bf-e-itemfg-vend.vend-no EQ ipbf-ttImportVendCost.cVendorID NO-ERROR.
            IF NOT AVAILABLE bf-e-itemfg-vend THEN 
            DO:
                CREATE bf-e-itemfg-vend.
                ASSIGN
                    bf-e-itemfg-vend.company   = ipbf-ttImportVendCost.Company
                    bf-e-itemfg-vend.i-no      = ipbf-ttImportVendCost.cFGItemID
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

