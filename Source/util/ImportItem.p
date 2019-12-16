
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
    FIELD i-dscr               AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "DESC" HELP "Optional - - Size:30"
    FIELD est-dscr             AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Est.DESC" HELP "Optional - Size:30"
    FIELD i-code               AS CHARACTER FORMAT "x(14)" COLUMN-LABEL "Item Code" HELP "Optional - Size: RM Stocked or Estimated Mat'1"
    FIELD tax-rcpt             AS CHARACTER FORMAT "x" COLUMN-LABEL "Taxable" HELP "Optional - Y or N (blank=N)"
    FIELD mat-type             AS CHARACTER FORMAT "x" COLUMN-LABEL "Mat'l Type" HELP "Optional - Size:1"
    FIELD cost-type            AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Cost Type" HELP "Optional - Size:3"
    FIELD procat               AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Category" HELP "Optional - Size:20"
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

    FIND FIRST ITEM EXCLUSIVE-LOCK 
        WHERE ITEM.company EQ ipbf-ttImportItem.Company
        AND ITEM.i-no EQ ipbf-ttImportItem.i-no
        NO-ERROR.

    IF NOT AVAILABLE ITEM THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE ITEM.
        ASSIGN 
            ITEM.company   = ipbf-ttImportItem.Company
            ITEM.loc       = ipbf-ttImportItem.Location
            item.industry  = IF ipbf-ttImportItem.ind-type EQ "Corrugated" THEN "2" ELSE "1" 
            ITEM.i-no      = ipbf-ttImportItem.i-no .
    END.
        
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportItem.i-name , YES, INPUT-OUTPUT item.i-name).                                                   
    RUN pAssignValueC (ipbf-ttImportItem.i-dscr, YES, INPUT-OUTPUT item.i-dscr).                                                   
    RUN pAssignValueC (ipbf-ttImportItem.est-dscr, iplIgnoreBlanks, INPUT-OUTPUT item.est-dscr).                               
    RUN pAssignValueC (ipbf-ttImportItem.i-code, YES, INPUT-OUTPUT item.i-code).                                                   
    RUN pAssignValueC (ipbf-ttImportItem.tax-rcpt, YES, INPUT-OUTPUT item.tax-rcpt).                                                   
    RUN pAssignValueC (ipbf-ttImportItem.mat-type, iplIgnoreBlanks, INPUT-OUTPUT item.mat-type).                                       
    RUN pAssignValueC (ipbf-ttImportItem.cost-type, iplIgnoreBlanks, INPUT-OUTPUT item.cost-type).                                         
    RUN pAssignValueC (ipbf-ttImportItem.procat, iplIgnoreBlanks, INPUT-OUTPUT item.procat).                      
    RUN pAssignValueD (ipbf-ttImportItem.q-ptd, iplIgnoreBlanks, INPUT-OUTPUT item.q-ptd).                                 
    RUN pAssignValueD (ipbf-ttImportItem.q-ytd, iplIgnoreBlanks, INPUT-OUTPUT item.q-ytd).                                     
    RUN pAssignValueD (ipbf-ttImportItem.q-lyr, YES, INPUT-OUTPUT item.q-lyr).                                                 
    RUN pAssignValueC (ipbf-ttImportItem.ink-type, iplIgnoreBlanks, INPUT-OUTPUT item.ink-type).                   
    RUN pAssignValueC (ipbf-ttImportItem.press-type, iplIgnoreBlanks, INPUT-OUTPUT item.press-type).                                         
    RUN pAssignValueD (ipbf-ttImportItem.min-lbs, iplIgnoreBlanks, INPUT-OUTPUT item.min-lbs).                                 
    RUN pAssignValueI (ipbf-ttImportItem.yield, iplIgnoreBlanks, INPUT-OUTPUT item.yield).                                       
    RUN pAssignValueD (ipbf-ttImportItem.weight-100, iplIgnoreBlanks, INPUT-OUTPUT item.weight-100).                                 
    RUN pAssignValueD (ipbf-ttImportItem.cal, iplIgnoreBlanks, INPUT-OUTPUT item.cal).                                   
    RUN pAssignValueD (ipbf-ttImportItem.shrink, iplIgnoreBlanks, INPUT-OUTPUT item.shrink).                             
    RUN pAssignValueD (ipbf-ttImportItem.basis-w, iplIgnoreBlanks, INPUT-OUTPUT item.basis-w).                                 
    RUN pAssignValueD (ipbf-ttImportItem.s-wid, iplIgnoreBlanks, INPUT-OUTPUT item.s-wid).                             
    RUN pAssignValueD (ipbf-ttImportItem.s-dep, iplIgnoreBlanks, INPUT-OUTPUT item.s-dep).                                 
    RUN pAssignValueD (ipbf-ttImportItem.s-len, YES, INPUT-OUTPUT item.s-len).                                          
    RUN pAssignValueD (ipbf-ttImportItem.density, YES, INPUT-OUTPUT item.density).                                         
    RUN pAssignValueD (ipbf-ttImportItem.r-wid, YES, INPUT-OUTPUT item.r-wid).                         
    RUN pAssignValueC (ipbf-ttImportItem.color-1, iplIgnoreBlanks, INPUT-OUTPUT item.color-1).                                 
    RUN pAssignValueI (ipbf-ttImportItem.ect, YES, INPUT-OUTPUT item.ect).                                          
    RUN pAssignValueC (ipbf-ttImportItem.dept-name1, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[1]).                                     
    RUN pAssignValueC (ipbf-ttImportItem.dept-name2, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[2]).                             
    RUN pAssignValueC (ipbf-ttImportItem.dept-name3, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[3]).                                           
    RUN pAssignValueC (ipbf-ttImportItem.dept-name4, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[4]).                                                 
    RUN pAssignValueC (ipbf-ttImportItem.dept-name5, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[5]).                             
    RUN pAssignValueC (ipbf-ttImportItem.dept-name6, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[6]).                             
    RUN pAssignValueC (ipbf-ttImportItem.dept-name7, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[7]).                             
    RUN pAssignValueC (ipbf-ttImportItem.dept-name8, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[8]).                                           
    RUN pAssignValueC (ipbf-ttImportItem.dept-name9, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[9]).                                                 
    RUN pAssignValueC (ipbf-ttImportItem.dept-name10, iplIgnoreBlanks, INPUT-OUTPUT item.dept-name[10]).                                         
    RUN pAssignValueI (ipbf-ttImportItem.speed%1, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[1]).                     
    RUN pAssignValueI (ipbf-ttImportItem.speed%2, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[2]).                                           
    RUN pAssignValueI (ipbf-ttImportItem.speed%3, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[3]).                         
    RUN pAssignValueI (ipbf-ttImportItem.speed%4, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[4]).                 
    RUN pAssignValueI (ipbf-ttImportItem.speed%5, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[5]).                         
    RUN pAssignValueI (ipbf-ttImportItem.speed%6, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[6]).                                       
    RUN pAssignValueI (ipbf-ttImportItem.speed%7, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[7]).                         
    RUN pAssignValueI (ipbf-ttImportItem.speed%8, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[8]).                                 
    RUN pAssignValueI (ipbf-ttImportItem.speed%9, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[9]).                                   
    RUN pAssignValueI (ipbf-ttImportItem.speed%10, iplIgnoreBlanks, INPUT-OUTPUT item.speed%[10]).                     
    RUN pAssignValueD (ipbf-ttImportItem.case-l, iplIgnoreBlanks, INPUT-OUTPUT item.case-l).                                           
    RUN pAssignValueD (ipbf-ttImportItem.case-w, iplIgnoreBlanks, INPUT-OUTPUT item.case-w).                         
    RUN pAssignValueD (ipbf-ttImportItem.case-d, iplIgnoreBlanks, INPUT-OUTPUT item.case-d).                 
    RUN pAssignValueD (ipbf-ttImportItem.avg-w, iplIgnoreBlanks, INPUT-OUTPUT item.avg-w).                         
    RUN pAssignValueI (ipbf-ttImportItem.box-case, YES, INPUT-OUTPUT item.box-case).                                       
    RUN pAssignValueI (ipbf-ttImportItem.case-pall, iplIgnoreBlanks, INPUT-OUTPUT item.case-pall).                         
    RUN pAssignValueC (ipbf-ttImportItem.flute, iplIgnoreBlanks, INPUT-OUTPUT item.flute).
    RUN pAssignValueC (ipbf-ttImportItem.reg-no, iplIgnoreBlanks, INPUT-OUTPUT item.reg-no).                     
    RUN pAssignValueD (ipbf-ttImportItem.sqin-lb, YES, INPUT-OUTPUT item.sqin-lb).                                           
    RUN pAssignValueD (ipbf-ttImportItem.linin-lb, iplIgnoreBlanks, INPUT-OUTPUT item.linin-lb).                         
    RUN pAssignValueC (ipbf-ttImportItem.loc, iplIgnoreBlanks, INPUT-OUTPUT item.loc).                 
    RUN pAssignValueC (ipbf-ttImportItem.loc-bin, iplIgnoreBlanks, INPUT-OUTPUT item.loc-bin).                         
    RUN pAssignValueD (ipbf-ttImportItem.q-onh, YES, INPUT-OUTPUT item.q-onh).                                       
    
    
    /*IF ipbf-ttImportItem.ml EQ "M" THEN
        ASSIGN item.ml = YES.
    ELSE item.ml = NO .*/
                                                                                                                               
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
        IF ipbf-ttImportItem.ind-type EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Industry Type Type is Blank".
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
            RUN pIsValidFromList IN hdValidator ("Active", ipbf-ttImportItem.i-code, "RM Stocked,Estimated Mat'1", OUTPUT oplValid, OUTPUT cValidNote).
        
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
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.

    
    IF ipbf-ttImportItem.i-code EQ "RM Stocked" THEN 
        ipbf-ttImportItem.i-code = "R".
    ELSE IF ipbf-ttImportItem.i-code EQ "Estimated Mat'1" THEN 
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

