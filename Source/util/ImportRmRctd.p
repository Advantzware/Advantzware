
/*------------------------------------------------------------------------
    File        : ImportRmRctd.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Prep and Die	

    Author(s)   : Sewa Singh
    Created     : Tue Oct 15 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportRmRctd
    FIELD Company   AS CHARACTER 
    FIELD Location  AS CHARACTER 
    FIELD loc       AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse" HELP "Required - Size:5"
    FIELD locBin    AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Bin   " HELP "Optional - - Size:8"
    FIELD tag       AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Tag   " HELP "Optional - Size:5"
    FIELD qty       AS DECIMAL   FORMAT "->>>>>>9.9<<" COLUMN-LABEL "Qty   " HELP "Optional - Decimal"
    FIELD cost      AS DECIMAL   FORMAT "->>>,>>9.99<<<<" COLUMN-LABEL "Cost  " HELP "Optional - Decimal"
    FIELD ritaCode  AS CHARACTER FORMAT "x" COLUMN-LABEL "Rita Code" HELP "Required - Size:1"
    FIELD sNum      AS INTEGER   FORMAT ">>9" COLUMN-LABEL "S-Num" HELP "Optional - Integer"
    FIELD bNum      AS INTEGER   FORMAT ">>9" COLUMN-LABEL "B-Num" HELP "Optional - Integer"
    FIELD pass      AS INTEGER   FORMAT ">9" COLUMN-LABEL "Pass" HELP "Optional - Integer"
    FIELD jobNo     AS CHARACTER FORMAT "x(6)" COLUMN-LABEL "Job #  " HELP "Optional - Size:6"
    FIELD jobNo2    AS INTEGER   FORMAT ">9" COLUMN-LABEL "Job2" HELP "Optional - Integer"
    FIELD poNo      AS CHARACTER FORMAT "x(9)" COLUMN-LABEL "PO #   " HELP "Optional - Size:9"
    FIELD poLine    AS INTEGER   FORMAT ">9" COLUMN-LABEL "PO Line" HELP "Optional - Integer"
    FIELD RmItem    AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Item#       " HELP "Required - Size:10"
    FIELD iName     AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Item Name   " HELP "Optional - Size:30"
    FIELD purUom    AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Qty UOM" HELP "Optional - Size:3"
    FIELD costUom   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Cost UOM" HELP "Optional - Size:3"
    FIELD rctDate   AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Receipt date" HELP "Optional - date"
    FIELD diameter  AS DECIMAL   FORMAT ">>>>>9.99<<<<" COLUMN-LABEL "Diameter" HELP "Optional - Decimal"
    FIELD rollLf    AS DECIMAL   FORMAT "->>,>>9.99<<<" COLUMN-LABEL "Roll LF" HELP "Optional - Decimal" 
    FIELD rollWt    AS DECIMAL   FORMAT "->>,>>9.99" COLUMN-LABEL "Roll Weight" HELP "Optional - Decimal"
    FIELD enteredBy AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Entered By" HELP "Optional - Size:8"
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportRmRctd"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportRmRctd FOR ttImportRmRctd.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRNo   AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID   NO-UNDO.
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    DEFINE BUFFER bf-loadtag FOR loadtag.
    
    ASSIGN 
        iopiAdded = iopiAdded + 1.

    RUN sys/ref/asiseq.p (INPUT ipbf-ttImportRmRctd.Company, INPUT "rm_rcpt_seq", OUTPUT iRNo) NO-ERROR.

    CREATE bf-rm-rctd.
    ASSIGN 
        bf-rm-rctd.r-no    = iRNo
        bf-rm-rctd.Company = ipbf-ttImportRmRctd.Company .

    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportRmRctd.loc, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.loc).                                                   
    RUN pAssignValueC (ipbf-ttImportRmRctd.locBin, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.loc-bin).                                                   
    RUN pAssignValueC (ipbf-ttImportRmRctd.tag, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.tag).                                                        
    RUN pAssignValueC (ipbf-ttImportRmRctd.ritaCode, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.rita-code).                                       
    RUN pAssignValueI (ipbf-ttImportRmRctd.sNum, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.s-num).                                         
    RUN pAssignValueI (ipbf-ttImportRmRctd.bNum, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.b-num).                      
    RUN pAssignValueI (ipbf-ttImportRmRctd.pass, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.pass).                                 
    RUN pAssignValueC (ipbf-ttImportRmRctd.jobNo, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.job-no).                                     
    RUN pAssignValueI (ipbf-ttImportRmRctd.jobNo2, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.job-no2).                                                 
    RUN pAssignValueC (ipbf-ttImportRmRctd.poNo, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.po-no).                   
    RUN pAssignValueI (ipbf-ttImportRmRctd.poLine, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.po-line).                                         
    RUN pAssignValueC (ipbf-ttImportRmRctd.RmItem, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.i-no).                                 
    RUN pAssignValueC (ipbf-ttImportRmRctd.iName, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.i-name).                                       
    RUN pAssignValueCToDt (ipbf-ttImportRmRctd.rctDate, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.rct-date).                             
    RUN pAssignValueD (ipbf-ttImportRmRctd.diameter, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.diameter).                                 
    RUN pAssignValueD (ipbf-ttImportRmRctd.rollLf, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.roll-lf).                             
    RUN pAssignValueD (ipbf-ttImportRmRctd.rollWt, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.roll-wt).                                 
    RUN pAssignValueC (ipbf-ttImportRmRctd.enteredBy, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.enteredBy).
    
    FIND FIRST ITEM NO-LOCK
       WHERE ITEM.company EQ ipbf-ttImportRmRctd.Company
         AND ITEM.i-no EQ ipbf-ttImportRmRctd.RmItem NO-ERROR .
    IF avail ITEM THEN DO:
      IF ipbf-ttImportRmRctd.purUom NE item.cons-uom THEN
      DO:
         RUN sys/ref/convquom.p(ipbf-ttImportRmRctd.purUom, item.cons-uom,
                          item.basis-w, 
                          IF item.s-len EQ 0 THEN 12         ELSE item.s-len,
                          IF item.s-wid EQ 0 THEN item.r-wid ELSE item.s-wid,
                          item.s-dep,
                          ipbf-ttImportRmRctd.qty ,
                          OUTPUT ipbf-ttImportRmRctd.qty).
         ipbf-ttImportRmRctd.purUom = item.cons-uom .                  
      END. 
      IF ipbf-ttImportRmRctd.costUom NE item.cons-uom THEN
      DO:
          RUN sys/ref/convcuom.p(ipbf-ttImportRmRctd.costUom, item.cons-uom,
                          item.basis-w, 
                          IF item.s-len EQ 0 THEN 12         ELSE item.s-len,
                          IF item.s-wid EQ 0 THEN item.r-wid ELSE item.s-wid,
                          item.s-dep,
                          ipbf-ttImportRmRctd.cost ,
                          OUTPUT ipbf-ttImportRmRctd.cost). 
           ipbf-ttImportRmRctd.costUom = item.cons-uom .              
      END.        
    END.
      
    RUN pAssignValueC (ipbf-ttImportRmRctd.purUom, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.pur-uom).                                 
    RUN pAssignValueC (ipbf-ttImportRmRctd.costUom, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.cost-uom). 
    RUN pAssignValueD (ROUND(ipbf-ttImportRmRctd.qty,3), iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.qty).                                                   
    RUN pAssignValueD (ipbf-ttImportRmRctd.cost, iplIgnoreBlanks, INPUT-OUTPUT bf-rm-rctd.cost).
    

    IF bf-rm-rctd.rct-date EQ ? THEN 
        ASSIGN  bf-rm-rctd.rct-date = TODAY . 
    FIND FIRST ITEM NO-LOCK
        WHERE ITEM.company EQ bf-rm-rctd.company
        AND ITEM.i-no EQ bf-rm-rctd.i-no NO-ERROR.
    IF AVAIL ITEM THEN 
    DO:
        IF bf-rm-rctd.i-name EQ "" THEN
            ASSIGN bf-rm-rctd.i-name = item.i-name .
        IF bf-rm-rctd.cost-uom EQ "" THEN
            bf-rm-rctd.cost-uom = item.cons-uom .
        IF bf-rm-rctd.pur-uom EQ "" THEN
            bf-rm-rctd.pur-uom = item.cons-uom .
    END.
    IF bf-rm-rctd.enteredBy EQ "" THEN
        bf-rm-rctd.enteredBy = USERID(LDBNAME(1)) .

    IF ipbf-ttImportRmRctd.tag NE "" THEN
        ASSIGN bf-rm-rctd.tag = STRING(CAPS(bf-rm-rctd.i-no),"x(15)") + string(ipbf-ttImportRmRctd.tag,"99999") .
    FIND FIRST bf-loadtag NO-LOCK
        WHERE bf-loadtag.company = bf-rm-rctd.company
        AND bf-loadtag.item-type = YES
        AND bf-loadtag.tag-no = bf-rm-rctd.tag  NO-ERROR.

    IF NOT AVAILABLE bf-loadtag AND ipbf-ttImportRmRctd.tag NE "" THEN 
    DO:
        CREATE bf-loadtag.
        ASSIGN 
            bf-loadtag.company   = ipbf-ttImportRmRctd.company
            bf-loadtag.tag-no    = bf-rm-rctd.tag
            bf-loadtag.item-type = YES /*item*/
            bf-loadtag.po-no     = INTEGER(ipbf-ttImportRmRctd.poNo) 
            bf-loadtag.LINE      = INTEGER(ipbf-ttImportRmRctd.poLine )
            bf-loadtag.job-no    = ipbf-ttImportRmRctd.jobNo
            bf-loadtag.job-no2   = INTEGER(ipbf-ttImportRmRctd.jobNo2)
            bf-loadtag.i-no      = CAPS(ipbf-ttImportRmRctd.RmItem)
            bf-loadtag.i-name    = ipbf-ttImportRmRctd.iName
            bf-loadtag.qty       = bf-rm-rctd.qty
            bf-loadtag.loc       = ipbf-ttImportRmRctd.loc
            bf-loadtag.loc-bin   = ipbf-ttImportRmRctd.locBin  
            bf-loadtag.misc-char[1] = ipbf-ttImportRmRctd.tag
            .

    END.
    RELEASE bf-loadtag.
    RELEASE bf-rm-rctd.
                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportRmRctd FOR ttImportRmRctd.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportRmRctd FOR ttImportRmRctd.
    DEFINE BUFFER bf-tmp            FOR rm-rctd.  /* for tag validation */
    DEFINE BUFFER bf-rm-rdtlh       FOR rm-rdtlh. /* for tag validation */


    oplValid = YES.
    
    IF  length(ipbf-ttImportRmRctd.Company) LE 2  THEN
        ASSIGN ipbf-ttImportRmRctd.Company = FILL("0",3 - length(TRIM(ipbf-ttImportRmRctd.Company))) + trim(ipbf-ttImportRmRctd.Company).
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRmRctd.RmItem EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Item Code is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRmRctd.ritaCode NE 'R' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Please enter receipt only(Rita-code = 'R')".
    END.
    
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST bf-tmp NO-LOCK 
            WHERE bf-tmp.company EQ ipbf-ttImportRmRctd.Company
            AND bf-tmp.tag EQ (STRING(CAPS(ipbf-ttImportRmRctd.RmItem),"x(15)") + string(ipbf-ttImportRmRctd.tag,"99999"))
            AND bf-tmp.rita-code <> "P"
            NO-ERROR .
        find first bf-rm-rdtlh NO-LOCK
            where bf-rm-rdtlh.company   eq ipbf-ttImportRmRctd.Company
            and bf-rm-rdtlh.loc       eq ipbf-ttImportRmRctd.loc
            and bf-rm-rdtlh.tag       eq (STRING(CAPS(ipbf-ttImportRmRctd.RmItem),"x(15)") + string(ipbf-ttImportRmRctd.tag,"99999"))
            and bf-rm-rdtlh.qty       gt 0
            and bf-rm-rdtlh.rita-code ne "S" 
            use-index tag no-error.
              
        IF AVAIL bf-tmp OR AVAIL bf-rm-rdtlh THEN
        DO: 
            ASSIGN 
                oplValid = NO
                opcNote  = "Duplicate record exists" .
        END.
        ELSE 
            ASSIGN 
                oplValid = YES
                opcNote  = "Add record"
                .
    END.
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportRmRctd.RmItem NE "" THEN 
            RUN pIsValidRMITemID (ipbf-ttImportRmRctd.RmItem, NO, ipbf-ttImportRmRctd.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportRmRctd.loc NE "" THEN 
            RUN pIsValidWarehouse (ipbf-ttImportRmRctd.loc, NO, ipbf-ttImportRmRctd.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportRmRctd.locBin NE "" THEN 
            RUN pIsValidRMBinForLoc (ipbf-ttImportRmRctd.locBin,ipbf-ttImportRmRctd.loc, NO, ipbf-ttImportRmRctd.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportRmRctd.jobNo NE "" THEN 
            RUN pIsValidJob (ipbf-ttImportRmRctd.jobNo, NO, ipbf-ttImportRmRctd.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportRmRctd.poNo NE "0" AND ipbf-ttImportRmRctd.poNo NE "" THEN 
            RUN pIsValidPoNo (ipbf-ttImportRmRctd.poNo, NO, ipbf-ttImportRmRctd.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.

END PROCEDURE.

