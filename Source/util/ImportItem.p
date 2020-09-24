
/*------------------------------------------------------------------------
    File        : ImportItem.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Prep and Die	

    Author(s)   : Sewa Singh
    Created     : Fri Nov 15 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportItem
    FIELD Company              AS CHARACTER 
    FIELD Location             AS CHARACTER 
    FIELD ind-type             AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Industry Type" HELP "Required - Size: Corrugated or Folding" 
    FIELD i-no                 AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Item#" HELP "Required - Size:10" 
    FIELD i-name               AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Name" HELP "Optional - Size:30"
    FIELD i-dscr               AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Desc" HELP "Optional - - Size:30"
    FIELD est-dscr             AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Est.Desc" HELP "Optional - Size:30"
    FIELD i-code               AS CHARACTER FORMAT "x(14)" COLUMN-LABEL "Item Code" HELP "Optional - RM Stocked or Estimated Materials"
    FIELD tax-rcpt             AS CHARACTER FORMAT "x" COLUMN-LABEL "Taxable" HELP "Optional - Y or N (blank=N)"
    FIELD mat-type             AS CHARACTER FORMAT "x" COLUMN-LABEL "Mat'l Type" HELP "Required - Size:1"
    FIELD cost-type            AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Cost Type" HELP "Required - Size:3"
    FIELD procat               AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Category" HELP "Required - Size:20"
    FIELD q-ptd                AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<<<<" COLUMN-LABEL "QTY Usage PTD" HELP "Optional - Decimal"
    FIELD q-ytd                AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<<<<" COLUMN-LABEL "Qty Usage YTD" HELP "Optional - Decimal"
    FIELD q-lyr                AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<<<<" COLUMN-LABEL "Qty Usage Last YR" HELP "Optional - Decimal"
    FIELD ink-type             AS CHARACTER FORMAT "x" COLUMN-LABEL "Ink Type" HELP "Optional - Size:1"
    FIELD press-type           AS CHARACTER FORMAT "x" COLUMN-LABEL "Press Type" HELP "Optional - Size:1" 
    FIELD min-lbs              AS DECIMAL   FORMAT ">9.99" COLUMN-LABEL "Min Lbs/Job" HELP "Optional - Decimal"
    FIELD yield                AS INTEGER   FORMAT ">>>,>>9" COLUMN-LABEL "SI/Lb" HELP "Optional - Integer"
    FIELD weight-100           AS DECIMAL   FORMAT ">,>>9.99" COLUMN-LABEL "Wgt/100" HELP "Optional - Decimal"
    FIELD cal                  AS DECIMAL   FORMAT ">>9.99999" COLUMN-LABEL "Caliper" HELP "Optional - Decimal"  
    FIELD shrink               AS DECIMAL   FORMAT ">>9.9999" COLUMN-LABEL "Shrink%" HELP "Optional - Decimal"
    FIELD basis-w              AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Weight/MSF" HELP "Optional - Decimal"
    FIELD s-wid                AS DECIMAL   FORMAT ">>,>>9.99<<<" COLUMN-LABEL "Width" HELP "Optional - Decimal"
    FIELD s-dep                AS DECIMAL   FORMAT ">>,>>9.99<<<" COLUMN-LABEL "Depth" HELP "Optional - Decimal"
    FIELD s-len                AS DECIMAL   FORMAT ">>,>>9.99<<<" COLUMN-LABEL "Length" HELP "Optional - Decimal"
    FIELD density              AS DECIMAL   FORMAT ">>,>>9.99<<<" COLUMN-LABEL "Density" HELP "Optional - Decimal"
    FIELD r-wid                AS DECIMAL   FORMAT ">>,>>9.99<<<" COLUMN-LABEL "Roll W" HELP "Optional - Decimal"
    FIELD color-1              AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Color" HELP "Optional - Size:15"
    FIELD ect                  AS INTEGER   FORMAT ">>9" COLUMN-LABEL "ECT" HELP "Optional - Integer"
    FIELD dept-name1           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 1" HELP "Optional - Size:2"   
    FIELD dept-name2           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 2" HELP "Optional - Size:2"   
    FIELD dept-name3           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 3" HELP "Optional - Size:2"   
    FIELD dept-name4           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 4" HELP "Optional - Size:2"   
    FIELD dept-name5           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 5" HELP "Optional - Size:2"   
    FIELD dept-name6           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 6" HELP "Optional - Size:2" 
    FIELD dept-name7           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 7" HELP "Optional - Size:2" 
    FIELD dept-name8           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 8" HELP "Optional - Size:2" 
    FIELD dept-name9           AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 9" HELP "Optional - Size:2" 
    FIELD dept-name10          AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department 10" HELP "Optional - Size:2" 
    FIELD speed%1              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 1" HELP "Optional - Integer"
    FIELD speed%2              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 2" HELP "Optional - Integer"
    FIELD speed%3              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 3" HELP "Optional - Integer"
    FIELD speed%4              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 4" HELP "Optional - Integer"
    FIELD speed%5              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 5" HELP "Optional - Integer"
    FIELD speed%6              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 6" HELP "Optional - Integer"
    FIELD speed%7              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 7" HELP "Optional - Integer"
    FIELD speed%8              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 8" HELP "Optional - Integer"
    FIELD speed%9              AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 9" HELP "Optional - Integer"
    FIELD speed%10             AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Reduction% 10" HELP "Optional - Integer"
    FIELD case-l               AS DECIMAL   FORMAT ">9.9999" COLUMN-LABEL "Case Length" HELP "Optional - Decimal"             
    FIELD case-w               AS DECIMAL   FORMAT ">9.9999" COLUMN-LABEL "Case Width" HELP "Optional - Decimal"            
    FIELD case-d               AS DECIMAL   FORMAT ">9.9999" COLUMN-LABEL "Case Depth" HELP "Optional - Decimal"      
    FIELD avg-w                AS DECIMAL   FORMAT ">>>9.99" COLUMN-LABEL "Avg Wt" HELP "Optional - Decimal"  
    FIELD box-case             AS INTEGER   FORMAT ">>>>9" COLUMN-LABEL "Boxes/Bundle" HELP "Optional - Integer"          
    FIELD case-pall            AS INTEGER   FORMAT ">>>9" COLUMN-LABEL "Bundle/Pallet" HELP "Optional - Integer"          
    FIELD flute                AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Flute" HELP "Optional - Size:10"           
    FIELD reg-no               AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Test" HELP "Optional - Size:5"            
    FIELD sqin-lb              AS DECIMAL   FORMAT ">>>,>>9" COLUMN-LABEL "Sq In/Lb" HELP "Optional - Decimal"          
    FIELD linin-lb             AS DECIMAL   FORMAT ">>>,>>9" COLUMN-LABEL "Lin In/UOM" HELP "Optional - Decimal"    
    FIELD loc                  AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse" HELP "Optional - Size:5"               
    FIELD loc-bin              AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Bin" HELP "Optional - Size:12"                  
    FIELD q-onh                AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<<<<" COLUMN-LABEL "Qty On Hand" HELP "Optional - Decimal"
    FIELD pur-uom              AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Purchase UOM" HELP "Required - Size: 5"
    FIELD cons-uom             AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Consumption UOM" HELP "Required - Size: 5"     
    FIELD alloc                AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Auto Allocate" HELP "Optional - Yes or No(Blank - No)"                  
    FIELD stocked              AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Stocked" HELP "Optional - Yes or No(Blank - No)"
    FIELD pur-man              AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Purchased Or Manufactured" HELP "Required - Purchased Or Manufactured(Blank - Manufactured)"
    FIELD inv-by-cust          AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Zero On Hand" HELP "Required - Yes or No(Blank - No)" 

    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportItem"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportItem FOR ttImportItem.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-item FOR ITEM.

    FIND FIRST bf-item EXCLUSIVE-LOCK 
        WHERE bf-item.company EQ ipbf-ttImportItem.Company
        AND bf-item.i-no EQ ipbf-ttImportItem.i-no
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE bf-item.
        ASSIGN 
            bf-item.company   = ipbf-ttImportItem.Company
            bf-item.loc       = ipbf-ttImportItem.Location
            bf-item.industry  = IF ipbf-ttImportItem.ind-type EQ "Corrugated" THEN "2" ELSE "1" 
            bf-item.i-no      = ipbf-ttImportItem.i-no .
    END.
        
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportItem.i-name , YES, INPUT-OUTPUT bf-item.i-name).                                                   
    RUN pAssignValueC (ipbf-ttImportItem.i-dscr, YES, INPUT-OUTPUT bf-item.i-dscr).                                                   
    RUN pAssignValueC (ipbf-ttImportItem.est-dscr, iplIgnoreBlanks, INPUT-OUTPUT bf-item.est-dscr).                               
    RUN pAssignValueC (ipbf-ttImportItem.i-code, YES, INPUT-OUTPUT bf-item.i-code).                                                   
    RUN pAssignValueC (ipbf-ttImportItem.tax-rcpt, YES, INPUT-OUTPUT bf-item.tax-rcpt).                                                   
    RUN pAssignValueC (ipbf-ttImportItem.mat-type, iplIgnoreBlanks, INPUT-OUTPUT bf-item.mat-type).                                       
    RUN pAssignValueC (ipbf-ttImportItem.cost-type, iplIgnoreBlanks, INPUT-OUTPUT bf-item.cost-type).                                         
    RUN pAssignValueC (ipbf-ttImportItem.procat, iplIgnoreBlanks, INPUT-OUTPUT bf-item.procat).                      
    RUN pAssignValueD (ipbf-ttImportItem.q-ptd, iplIgnoreBlanks, INPUT-OUTPUT bf-item.q-ptd).                                 
    RUN pAssignValueD (ipbf-ttImportItem.q-ytd, iplIgnoreBlanks, INPUT-OUTPUT bf-item.q-ytd).                                     
    RUN pAssignValueD (ipbf-ttImportItem.q-lyr, YES, INPUT-OUTPUT bf-item.q-lyr).                                                 
    RUN pAssignValueC (ipbf-ttImportItem.ink-type, iplIgnoreBlanks, INPUT-OUTPUT bf-item.ink-type).                   
    RUN pAssignValueC (ipbf-ttImportItem.press-type, iplIgnoreBlanks, INPUT-OUTPUT bf-item.press-type).                                         
    RUN pAssignValueD (ipbf-ttImportItem.min-lbs, iplIgnoreBlanks, INPUT-OUTPUT bf-item.min-lbs).                                 
    RUN pAssignValueI (ipbf-ttImportItem.yield, iplIgnoreBlanks, INPUT-OUTPUT bf-item.yield).                                       
    RUN pAssignValueD (ipbf-ttImportItem.weight-100, iplIgnoreBlanks, INPUT-OUTPUT bf-item.weight-100).                                 
    RUN pAssignValueD (ipbf-ttImportItem.cal, iplIgnoreBlanks, INPUT-OUTPUT bf-item.cal).                                   
    RUN pAssignValueD (ipbf-ttImportItem.shrink, iplIgnoreBlanks, INPUT-OUTPUT bf-item.shrink).                             
    RUN pAssignValueD (ipbf-ttImportItem.basis-w, iplIgnoreBlanks, INPUT-OUTPUT bf-item.basis-w).                                 
    RUN pAssignValueD (ipbf-ttImportItem.s-wid, iplIgnoreBlanks, INPUT-OUTPUT bf-item.s-wid).                             
    RUN pAssignValueD (ipbf-ttImportItem.s-dep, iplIgnoreBlanks, INPUT-OUTPUT bf-item.s-dep).                                 
    RUN pAssignValueD (ipbf-ttImportItem.s-len, YES, INPUT-OUTPUT bf-item.s-len).                                          
    RUN pAssignValueD (ipbf-ttImportItem.density, YES, INPUT-OUTPUT bf-item.density).                                         
    RUN pAssignValueD (ipbf-ttImportItem.r-wid, YES, INPUT-OUTPUT bf-item.r-wid).                         
    RUN pAssignValueC (ipbf-ttImportItem.color-1, iplIgnoreBlanks, INPUT-OUTPUT bf-item.color-1).                                 
    RUN pAssignValueI (ipbf-ttImportItem.ect, YES, INPUT-OUTPUT bf-item.ect).                                          
    RUN pAssignValueC (ipbf-ttImportItem.dept-name1, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[1]).                                     
    RUN pAssignValueC (ipbf-ttImportItem.dept-name2, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[2]).                             
    RUN pAssignValueC (ipbf-ttImportItem.dept-name3, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[3]).                                           
    RUN pAssignValueC (ipbf-ttImportItem.dept-name4, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[4]).                                                 
    RUN pAssignValueC (ipbf-ttImportItem.dept-name5, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[5]).                             
    RUN pAssignValueC (ipbf-ttImportItem.dept-name6, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[6]).                             
    RUN pAssignValueC (ipbf-ttImportItem.dept-name7, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[7]).                             
    RUN pAssignValueC (ipbf-ttImportItem.dept-name8, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[8]).                                           
    RUN pAssignValueC (ipbf-ttImportItem.dept-name9, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[9]).                                                 
    RUN pAssignValueC (ipbf-ttImportItem.dept-name10, iplIgnoreBlanks, INPUT-OUTPUT bf-item.dept-name[10]).                                         
    RUN pAssignValueI (ipbf-ttImportItem.speed%1, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[1]).                     
    RUN pAssignValueI (ipbf-ttImportItem.speed%2, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[2]).                                           
    RUN pAssignValueI (ipbf-ttImportItem.speed%3, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[3]).                         
    RUN pAssignValueI (ipbf-ttImportItem.speed%4, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[4]).                 
    RUN pAssignValueI (ipbf-ttImportItem.speed%5, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[5]).                         
    RUN pAssignValueI (ipbf-ttImportItem.speed%6, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[6]).                                       
    RUN pAssignValueI (ipbf-ttImportItem.speed%7, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[7]).                         
    RUN pAssignValueI (ipbf-ttImportItem.speed%8, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[8]).                                 
    RUN pAssignValueI (ipbf-ttImportItem.speed%9, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[9]).                                   
    RUN pAssignValueI (ipbf-ttImportItem.speed%10, iplIgnoreBlanks, INPUT-OUTPUT bf-item.speed%[10]).                     
    RUN pAssignValueD (ipbf-ttImportItem.case-l, iplIgnoreBlanks, INPUT-OUTPUT bf-item.case-l).                                           
    RUN pAssignValueD (ipbf-ttImportItem.case-w, iplIgnoreBlanks, INPUT-OUTPUT bf-item.case-w).                         
    RUN pAssignValueD (ipbf-ttImportItem.case-d, iplIgnoreBlanks, INPUT-OUTPUT bf-item.case-d).                 
    RUN pAssignValueD (ipbf-ttImportItem.avg-w, iplIgnoreBlanks, INPUT-OUTPUT bf-item.avg-w).                         
    RUN pAssignValueI (ipbf-ttImportItem.box-case, YES, INPUT-OUTPUT bf-item.box-case).                                       
    RUN pAssignValueI (ipbf-ttImportItem.case-pall, iplIgnoreBlanks, INPUT-OUTPUT bf-item.case-pall).                         
    RUN pAssignValueC (ipbf-ttImportItem.flute, iplIgnoreBlanks, INPUT-OUTPUT bf-item.flute).
    RUN pAssignValueC (ipbf-ttImportItem.reg-no, iplIgnoreBlanks, INPUT-OUTPUT bf-item.reg-no).                     
    RUN pAssignValueD (ipbf-ttImportItem.sqin-lb, YES, INPUT-OUTPUT bf-item.sqin-lb).                                           
    RUN pAssignValueD (ipbf-ttImportItem.linin-lb, iplIgnoreBlanks, INPUT-OUTPUT bf-item.linin-lb).                         
    RUN pAssignValueC (ipbf-ttImportItem.loc, iplIgnoreBlanks, INPUT-OUTPUT bf-item.loc).                 
    RUN pAssignValueC (ipbf-ttImportItem.loc-bin, iplIgnoreBlanks, INPUT-OUTPU bf-item.loc-bin).
    RUN pAssignValueC (ipbf-ttImportItem.pur-uom, iplIgnoreBlanks, INPUT-OUTPUT bf-item.pur-uom).
    RUN pAssignValueC (ipbf-ttImportItem.cons-uom, iplIgnoreBlanks, INPUT-OUTPUT bf-item.cons-uom).                         
    RUN pAssignValueD (ipbf-ttImportItem.q-onh, YES, INPUT-OUTPUT bf-item.q-onh).
    
    IF ipbf-ttImportItem.alloc EQ "Yes"  THEN
    bf-item.alloc = YES .
    ELSE bf-item.alloc = NO .
    
    IF ipbf-ttImportItem.stocked EQ "Yes"  THEN
    bf-item.stocked = YES .
    ELSE bf-item.stocked = NO .
    
    IF ipbf-ttImportItem.pur-man EQ "Purchased"  THEN
    bf-item.pur-man = YES .
    ELSE bf-item.pur-man = NO .
    
    IF ipbf-ttImportItem.inv-by-cust EQ "Yes"  THEN
    bf-item.inv-by-cust = YES .
    ELSE bf-item.inv-by-cust = NO .     
      
    
    /*IF ipbf-ttImportItem.ml EQ "M" THEN
        ASSIGN bf-item.ml = YES.
    ELSE bf-item.ml = NO .*/
    
    RELEASE bf-item.
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportItem FOR ttImportItem.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE NO-UNDO.
    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportItem FOR ttImportItem.

    RUN util/Validate.p PERSISTENT SET hdValidator.

    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportItem.i-no EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Item is Blank".
    END.  

    IF oplValid THEN 
    DO:
        IF ipbf-ttImportItem.mat-type EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Mat'l Type is Blank".
    END.  
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportItem.cost-type EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Cost Type is Blank".
    END.  
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportItem.procat EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Category is Blank".
    END. 
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportItem.ind-type EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Industry Type is Blank".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportItem.i-code EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Item Code is Blank.".
    END.    
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportItem.pur-uom EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Purchase UOM is Blank".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportItem.cons-uom EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Consumption UOM is Blank".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportItem NO-LOCK 
            WHERE bf-ttImportItem.Company EQ ipbf-ttImportItem.Company
            AND bf-ttImportItem.i-no EQ ipbf-ttImportItem.i-no
            AND ROWID(bf-ttImportItem) NE ROWID(ipbf-ttImportItem)
            NO-ERROR.
        IF AVAILABLE bf-ttImportItem THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST ITEM NO-LOCK 
            WHERE ITEM.company EQ ipbf-ttImportItem.Company
            AND ITEM.i-no EQ ipbf-ttImportItem.i-no
            NO-ERROR .
        IF AVAIL ITEM THEN
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
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportItem.ind-type NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Industry Type", ipbf-ttImportItem.ind-type, "Corrugated,Folding", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportItem.i-code NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Active", ipbf-ttImportItem.i-code, "RM Stocked,Estimated Materials", OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportItem.flute NE "" THEN 
            RUN pIsValidFlute IN hdValidator (ipbf-ttImportItem.flute, NO, ipbf-ttImportItem.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportItem.procat NE "" THEN 
            RUN pIsValidProcat IN hdValidator (ipbf-ttImportItem.procat, NO, ipbf-ttImportItem.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportItem.cost-type NE "" THEN 
            RUN pIsValidCostType IN hdValidator (ipbf-ttImportItem.cost-type, NO, ipbf-ttImportItem.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportItem.mat-type NE "" THEN 
            RUN pIsValidMaterialType IN hdValidator (ipbf-ttImportItem.mat-type, NO,  OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportItem.loc NE "" THEN 
            RUN pIsValidWarehouse IN hdValidator (ipbf-ttImportItem.loc, NO, ipbf-ttImportItem.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportItem.loc-bin NE "" THEN 
            RUN pIsValidRMBinForLoc IN hdValidator (ipbf-ttImportItem.loc-bin,ipbf-ttImportItem.loc, NO, ipbf-ttImportItem.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportItem.dept-name1 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name1, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name2 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name2, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name3 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name3, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name4 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name4, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name5 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name5, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name6 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name6, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name7 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name7, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name8 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name8, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name9 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name9, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportItem.dept-name10 NE "" THEN 
           RUN pIsValidDept IN hdValidator (ipbf-ttImportItem.dept-name10, NO, OUTPUT oplValid, OUTPUT cValidNote).
           
        IF oplValid AND ipbf-ttImportItem.alloc NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Allocate", ipbf-ttImportItem.alloc, "Yes,No", OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid AND ipbf-ttImportItem.stocked NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Stocked", ipbf-ttImportItem.stocked, "Yes,No", OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid AND ipbf-ttImportItem.pur-man NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Pur Or Man", ipbf-ttImportItem.pur-man, "Purchased,Manufactured", OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid AND ipbf-ttImportItem.inv-by-cust NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Zero On Hand", ipbf-ttImportItem.inv-by-cust, "Yes,No", OUTPUT oplValid, OUTPUT cValidNote).    

        IF ipbf-ttImportItem.i-code = "RM Stocked" AND
            CAN-DO('A,B,P',ipbf-ttImportItem.mat-type) THEN DO:

            IF ipbf-ttImportItem.r-wid EQ 0 AND ipbf-ttImportItem.s-wid EQ 0 THEN DO:
                opcNote = 'Both Roll Width and Sheet Width cannot be Zero' .
                oplValid = NO .
            END.
            ELSE IF ipbf-ttImportItem.r-wid NE 0 AND ipbf-ttImportItem.s-wid NE 0 THEN DO:
                opcNote = 'Both Roll Width and Sheet Width cannot be Greater Than Zero' .
                oplValid = NO .
            END.
            ELSE IF ipbf-ttImportItem.s-wid NE 0 AND ipbf-ttImportItem.s-len EQ 0 THEN DO:
                opcNote =  'Sheet Length cannot be Zero' .
                oplValid = NO .
            END.
        END.
        IF ipbf-ttImportItem.mat-type EQ "C" AND ipbf-ttImportItem.flute NE ""
            AND ipbf-ttImportItem.reg-no EQ ""     THEN DO:
            opcNote = "Test may not be blank..." .
            oplValid = NO .
        END.

        IF ipbf-ttImportItem.ind-type EQ "Folding" THEN DO:
            if index("BAPR1234",ipbf-ttImportItem.mat-type) > 0 then do:
                if dec(ipbf-ttImportItem.cal) = 0 then do:
                    opcNote = "Caliper is mandatory" .
                    oplValid = NO .
                END.
                if dec(ipbf-ttImportItem.basis-w) = 0 then do:
                    opcNote = "Basis Weight is mandatory" .
                    oplValid = NO .
                END.
                if dec(ipbf-ttImportItem.r-wid) = 0 and ipbf-ttImportItem.i-code = "RM Stocked" and
                    (dec(ipbf-ttImportItem.s-len) = 0 or dec(ipbf-ttImportItem.s-wid) = 0 )
                    then do:
                    opcNote = "Dimensions are mandatory for Real Items!" .
                    oplValid = NO .
                END.
            END. /* "BAP" */
            else if index("W",ipbf-ttImportItem.mat-type) > 0 then do:
                if dec(ipbf-ttImportItem.sqin-lb) = 0 then do:
                    opcNote = "Sq In/Lb is mandatory!" .
                    oplValid = NO .
                END.
            END. /* "W"  */
            else if index("GTS",ipbf-ttImportItem.mat-type) > 0 then do:
                if dec(ipbf-ttImportItem.linin-lb) eq 0 AND dec(ipbf-ttImportItem.sqin-lb) eq 0 then do:
                    opcNote = "For " + (if ipbf-ttImportItem.mat-type eq "S" then "Stitch" 
                              else if ipbf-ttImportItem.mat-type eq "T" then "Tape" 
                                  else "Glue") +
                              ", " + "Sq In/Lb OR Lin In/UOM  must be entered...".
                    oplValid = NO .
                END.
            END. /* "GTS"  */ 
            else if index("DC",ipbf-ttImportItem.mat-type) > 0 then do:
                if dec(ipbf-ttImportItem.box-case) <> 0 AND dec(ipbf-ttImportItem.avg-w) <> 0 then do:
                    opcNote =  "You can enter either Case Weight or Number of blanks per Case! Only one field can be entered.".
                    oplValid = NO .
                END.
            END.
        END.    /*ipbf-ttImportItem.ind-type EQ "Folding"*/
        ELSE IF ipbf-ttImportItem.ind-type EQ "Corrugated" THEN DO:

            IF INDEX("BAP",ipbf-ttImportItem.mat-type) > 0 THEN DO:
                IF dec(ipbf-ttImportItem.cal) = 0 THEN DO:
                    opcNote = "Caliper is mandatory" .
                    oplValid = NO .
                END.
                IF dec(ipbf-ttImportItem.basis-w) = 0 THEN DO:
                    opcNote = "Basis Weight is mandatory" .
                    oplValid = NO .
                END.
                IF dec(ipbf-ttImportItem.r-wid) = 0 AND ipbf-ttImportItem.i-code = "RM Stocked" AND
                    (dec(ipbf-ttImportItem.s-len) = 0 OR dec(ipbf-ttImportItem.s-wid) = 0 )
                    THEN DO:
                    opcNote = "Dimensions are mandatory for Real Items!" .
                    oplValid = NO .
                END.
            END. /* "BAP" */
            ELSE IF INDEX("W",ipbf-ttImportItem.mat-type) > 0 THEN DO:
                IF dec(ipbf-ttImportItem.shrink) = 0 THEN DO:
                    opcNote = "Pickup% is mandatory!" .
                    oplValid = NO .
                END.
            END.  /* "W"  */ 
            ELSE IF INDEX("GTS",ipbf-ttImportItem.mat-type) > 0 THEN DO:
                IF dec(ipbf-ttImportItem.linin-lb) EQ 0 AND
                    dec(ipbf-ttImportItem.sqin-lb) EQ 0 THEN DO:
                    opcNote = "For " + (IF ipbf-ttImportItem.mat-type EQ "S" THEN "Stitch" 
                        ELSE IF ipbf-ttImportItem.mat-type EQ "T" THEN "Tape" 
                            ELSE "Glue") +
                        ", " + " Sq In/Lb OR Lin In/UOM not be blank..." .
                      oplValid = NO .
                END.
            END.  /* "GTS"  */ 
            ELSE IF INDEX("DC",ipbf-ttImportItem.mat-type) > 0 THEN DO:
                IF dec(ipbf-ttImportItem.box-case) <> 0 AND
                    dec(ipbf-ttImportItem.avg-w) <> 0 THEN DO:
                    opcNote = "You can enter either Case Weight or Number of blanks per Case! Only one field can be entered." .
                    oplValid = NO .
                END.
            END.
        END.  /* ipbf-ttImportItem.ind-type EQ "Corrugated" */
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.

    
    IF ipbf-ttImportItem.i-code EQ "RM Stocked" THEN 
        ipbf-ttImportItem.i-code = "R".
    ELSE IF ipbf-ttImportItem.i-code EQ "Estimated Materials" THEN 
        ipbf-ttImportItem.i-code = "E".

    IF ipbf-ttImportItem.ink-type EQ "Ink" THEN 
        ipbf-ttImportItem.ink-type = "I".
    ELSE IF ipbf-ttImportItem.ink-type EQ "Lacquer" THEN 
        ipbf-ttImportItem.ink-type = "L".
    ELSE IF ipbf-ttImportItem.ink-type EQ "Ultra Violet" THEN 
        ipbf-ttImportItem.ink-type = "U".
    ELSE IF ipbf-ttImportItem.ink-type EQ "Varnish" THEN 
        ipbf-ttImportItem.ink-type = "V".
    ELSE IF ipbf-ttImportItem.ink-type EQ "Aqueous" THEN 
        ipbf-ttImportItem.ink-type = "A".
    
    IF ipbf-ttImportItem.press-type EQ "Flexo" THEN 
        ipbf-ttImportItem.press-type = "F".
    ELSE IF ipbf-ttImportItem.press-type EQ "Gravure" THEN 
        ipbf-ttImportItem.press-type = "G".
    ELSE IF ipbf-ttImportItem.press-type EQ "Letterpress" THEN 
        ipbf-ttImportItem.press-type = "L".
    ELSE IF ipbf-ttImportItem.press-type EQ "Offset" THEN 
        ipbf-ttImportItem.press-type = "O".
    ELSE IF ipbf-ttImportItem.press-type EQ "Silkscreen" THEN 
        ipbf-ttImportItem.press-type = "S".

END PROCEDURE.

