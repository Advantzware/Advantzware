&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/PriceMatrixImport.p
    Purpose     : Import Price Matrix from Excel

    Syntax      :  Run oe/PriceMatrixImport.p (input cocode).

    Description : 

    Author(s)   :  BV
    Created     :  02/07/14
    Notes       : Moved from inside Browser/oe-prmtx.w
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tt-oe-prmtx LIKE oe-prmtx
    FIELD valid   AS LOGICAL   INIT TRUE
    FIELD row-no  AS INTEGER
    FIELD refcode AS CHARACTER.

&GLOBAL-DEFINE LOG-FILE price-matrix-import-log.txt

DEFINE VARIABLE gcTempDir AS CHARACTER INIT "C:\tmp\" NO-UNDO.
DEFINE VARIABLE gcLogFile AS CHARACTER INIT "C:\tmp\{&LOG-FILE}" NO-UNDO.

DEFINE STREAM log-file.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE iCountTotal       AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountAdded       AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountNotOK       AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountOverwritten AS INTEGER NO-UNDO.

 
RUN InitializeLogFile.
RUN ImportExcelData(OUTPUT iCountTotal).
RUN ProcessImportedData(OUTPUT iCountAdded,
    OUTPUT iCountOverwritten,
    OUTPUT iCountNotOK).
RUN Summarize(INPUT iCountTotal,
    INPUT iCountAdded,
    INPUT iCountOverwritten,
    INPUT iCountNotOK).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ImportExcelData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportExcelData Procedure 
PROCEDURE ImportExcelData :
    /*------------------------------------------------------------------------------
      Purpose: Reads Excel File and Imports Data into Temp-table    
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiTotalCount AS INTEGER NO-UNDO.

    DEFINE VARIABLE cExcelFile         AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lFileOK            AS LOG              NO-UNDO.
    DEFINE VARIABLE iRowCount          AS INTEGER          INIT 2 NO-UNDO.
    DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
    
    FOR EACH tt-oe-prmtx:
        DELETE tt-oe-prmtx.
    END.

    SYSTEM-DIALOG GET-FILE cExcelFile
        TITLE "Select File to Import"
        FILTERS "Excel File (*.csv,*.xls,*.xlsx) " "*.csv,*.xls,*.xlsx"
        INITIAL-DIR gcTempDir
        MUST-EXIST
        USE-FILENAME
        UPDATE lFileOK.

    IF lFileOK THEN 
    DO:
        IF LENGTH(cExcelFile) LT 4 OR
            (SUBSTR(cExcelFile,LENGTH(cExcelFile) - 3) NE ".xls" AND
            SUBSTR(cExcelFile,LENGTH(cExcelFile) - 4) NE ".xlsx" AND
            SUBSTR(cExcelFile,LENGTH(cExcelFile) - 3) NE ".csv") THEN 
        DO:
            MESSAGE "Invalid File.  Must Choose Excel or CSV File."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
        END.   
        SESSION:SET-WAIT-STATE ("general").
    
        /* Initialize Excel. */
        CREATE "Excel.Application" chExcelApplication NO-ERROR.

        /* Check if Excel got initialized. */
        IF NOT (VALID-HANDLE (chExcelApplication)) THEN 
        DO:
            MESSAGE "Unable to Start Excel." VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR. 
        END.

        /* Open our Excel File. */  
        chExcelApplication:VISIBLE = FALSE.
        chWorkbook = chExcelApplication:Workbooks:OPEN(cExcelFile) NO-ERROR.

        /* Do not display Excel error messages. */
        chExcelApplication:DisplayAlerts = FALSE NO-ERROR.

        /* Go to the Active Sheet. */
        chWorkbook:WorkSheets(1):Activate NO-ERROR.

        ASSIGN
            chWorkSheet = chExcelApplication:Sheets:ITEM(1).
        REPEAT:
            IF chWorkSheet:Range("A" + STRING(iRowCount)):VALUE = ? THEN LEAVE.
            CREATE tt-oe-prmtx.
            tt-oe-prmtx.company            = ipcCompany.
            tt-oe-prmtx.refcode            = chWorkSheet:Range("A" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.cust-no            = chWorkSheet:Range("B" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.custype            = chWorkSheet:Range("C" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.procat             = chWorkSheet:Range("D" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.i-no               = chWorkSheet:Range("E" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.meth               = chWorkSheet:Range("F" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[1]             = chWorkSheet:Range("G" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[1]           = chWorkSheet:Range("H" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[1]        = chWorkSheet:Range("I" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[1]             = chWorkSheet:Range("J" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[2]             = chWorkSheet:Range("K" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[2]           = chWorkSheet:Range("L" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[2]        = chWorkSheet:Range("M" + STRING(iRowCount)):VALUE NO-ERROR. 
            tt-oe-prmtx.uom[2]             = chWorkSheet:Range("N" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[3]             = chWorkSheet:Range("O" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[3]           = chWorkSheet:Range("P" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[3]        = chWorkSheet:Range("Q" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[3]             = chWorkSheet:Range("R" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[4]             = chWorkSheet:Range("S" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[4]           = chWorkSheet:Range("T" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[4]        = chWorkSheet:Range("U" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[4]             = chWorkSheet:Range("V" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[5]             = chWorkSheet:Range("W" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[5]           = chWorkSheet:Range("X" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[5]        = chWorkSheet:Range("Y" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[5]             = chWorkSheet:Range("Z" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[6]             = chWorkSheet:Range("AA" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[6]           = chWorkSheet:Range("AB" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[6]        = chWorkSheet:Range("AC" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[6]             = chWorkSheet:Range("AD" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[7]             = chWorkSheet:Range("AE" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[7]           = chWorkSheet:Range("AF" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[7]        = chWorkSheet:Range("AG" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[7]             = chWorkSheet:Range("AH" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[8]             = chWorkSheet:Range("AI" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[8]           = chWorkSheet:Range("AJ" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[8]        = chWorkSheet:Range("AK" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[8]             = chWorkSheet:Range("AL" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[9]             = chWorkSheet:Range("AM" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[9]           = chWorkSheet:Range("AN" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[9]        = chWorkSheet:Range("AO" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[9]             = chWorkSheet:Range("AP" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.qty[10]            = chWorkSheet:Range("AQ" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.price[10]          = chWorkSheet:Range("AR" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.discount[10]       = chWorkSheet:Range("AS" + STRING(iRowCount)):VALUE NO-ERROR.
            tt-oe-prmtx.uom[10]            = chWorkSheet:Range("AS" + STRING(iRowCount)):VALUE NO-ERROR.
            ASSIGN
                opiTotalCount      = opiTotalCount + 1
                tt-oe-prmtx.row-no = iRowCount
                iRowCount          = iRowCount + 1.
        END. /*REPEAT*/
    END. /*lFileOK*/


    /*Free memory*/
    chWorkbook = chExcelApplication:Workbooks:CLOSE() NO-ERROR.
    RELEASE OBJECT chWorkbook NO-ERROR.
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeLogFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeLogFile Procedure 
PROCEDURE InitializeLogFile :
    /*------------------------------------------------------------------------------
      Purpose: InitializeLogFile    
      Parameters:  
      Notes:       
    ------------------------------------------------------------------------------*/
    FIND FIRST users 
        WHERE users.user_id EQ USERID("nosweat")  
        NO-LOCK NO-ERROR.
 
    IF AVAILABLE users AND users.user_program[2] NE "" THEN
        ASSIGN 
            gcTempDir = users.user_program[2]
            gcLogFile = gcTempDir +  "\{&LOG-FILE}" .

    IF SEARCH(gcLogFile) <> ? THEN
        OS-DELETE VALUE(gcLogFile).

    OUTPUT STREAM log-file TO VALUE(gcLogFile).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessImportedData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessImportedData Procedure 
PROCEDURE ProcessImportedData :
    /*------------------------------------------------------------------------------
      Purpose:  Create/Update Price matrix based on imported temp-table   
      Parameters:  output parameters for summary
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiCountAdded AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCountOverwritten AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCountNotOK AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-custype  FOR custype.
    DEFINE BUFFER bf-fgcat    FOR fgcat.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.

    DEFINE VARIABLE cEffDate  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEffDate AS DATE      NO-UNDO.


    FOR EACH tt-oe-prmtx:
       
        /*VALIDATE DATA*/ 
        IF tt-oe-prmtx.custype      = "?" OR tt-oe-prmtx.custype        = ? THEN tt-oe-prmtx.custype      = "".
        IF tt-oe-prmtx.procat       = "?" OR tt-oe-prmtx.procat         = ? THEN tt-oe-prmtx.procat       = "".
        IF tt-oe-prmtx.i-no         = "?" OR tt-oe-prmtx.i-no           = ? THEN tt-oe-prmtx.i-no         = "".
        IF tt-oe-prmtx.meth         = ?   THEN tt-oe-prmtx.meth         = YES .
        IF tt-oe-prmtx.qty[1]       = ?   THEN tt-oe-prmtx.qty[1]       = 0.
        IF tt-oe-prmtx.price[1]     = ?   THEN tt-oe-prmtx.price[1]     = 0.
        IF tt-oe-prmtx.discount[1]  = ?   THEN tt-oe-prmtx.discount[1]  = 0.
        IF tt-oe-prmtx.uom[1]       = "?" OR tt-oe-prmtx.uom[1]         = ? THEN tt-oe-prmtx.uom[1]       = "".
        IF tt-oe-prmtx.qty[2]       = ?   THEN tt-oe-prmtx.qty[2]       = 0.
        IF tt-oe-prmtx.price[2]     = ?   THEN tt-oe-prmtx.price[2]     = 0.
        IF tt-oe-prmtx.discount[2]  = ?   THEN tt-oe-prmtx.discount[2]  = 0.
        IF tt-oe-prmtx.uom[2]       = "?" OR  tt-oe-prmtx.uom[2]        = ? THEN tt-oe-prmtx.uom[2]       = "".
        IF tt-oe-prmtx.qty[3]       = ?   THEN tt-oe-prmtx.qty[3]       = 0.
        IF tt-oe-prmtx.price[3]     = ?   THEN tt-oe-prmtx.price[3]     = 0.
        IF tt-oe-prmtx.discount[3]  = ?   THEN tt-oe-prmtx.discount[3]  = 0.
        IF tt-oe-prmtx.uom[3]       = "?" OR  tt-oe-prmtx.uom[3]        = ? THEN tt-oe-prmtx.uom[3]       = "".
        IF tt-oe-prmtx.qty[4]       = ?   THEN tt-oe-prmtx.qty[4]       = 0.
        IF tt-oe-prmtx.price[4]     = ?   THEN tt-oe-prmtx.price[4]     = 0.
        IF tt-oe-prmtx.discount[4]  = ?   THEN tt-oe-prmtx.discount[4]  = 0.
        IF tt-oe-prmtx.uom[4]       = "?" OR  tt-oe-prmtx.uom[4]        = ? THEN tt-oe-prmtx.uom[4]       = "".
        IF tt-oe-prmtx.qty[5]       = ?   THEN tt-oe-prmtx.qty[5]       = 0.
        IF tt-oe-prmtx.price[5]     = ?   THEN tt-oe-prmtx.price[5]     = 0.
        IF tt-oe-prmtx.discount[5]  = ?   THEN tt-oe-prmtx.discount[5]  = 0.
        IF tt-oe-prmtx.uom[5]       = "?" OR  tt-oe-prmtx.uom[5]        = ? THEN tt-oe-prmtx.uom[5]       = "".
        IF tt-oe-prmtx.qty[6]       = ?   THEN tt-oe-prmtx.qty[6]       = 0.
        IF tt-oe-prmtx.price[6]     = ?   THEN tt-oe-prmtx.price[6]     = 0.
        IF tt-oe-prmtx.discount[6]  = ?   THEN tt-oe-prmtx.discount[6]  = 0.
        IF tt-oe-prmtx.uom[6]       = "?" OR  tt-oe-prmtx.uom[6]        = ? THEN tt-oe-prmtx.uom[6]       = "".
        IF tt-oe-prmtx.qty[7]       = ?   THEN tt-oe-prmtx.qty[7]       = 0.
        IF tt-oe-prmtx.price[7]     = ?   THEN tt-oe-prmtx.price[7]     = 0.
        IF tt-oe-prmtx.discount[7]  = ?   THEN tt-oe-prmtx.discount[7]  = 0.
        IF tt-oe-prmtx.uom[7]       = "?" OR  tt-oe-prmtx.uom[7]        = ? THEN tt-oe-prmtx.uom[7]       = "".
        IF tt-oe-prmtx.qty[8]       = ?   THEN tt-oe-prmtx.qty[8]       = 0.
        IF tt-oe-prmtx.price[8]     = ?   THEN tt-oe-prmtx.price[8]     = 0.
        IF tt-oe-prmtx.discount[8]  = ?   THEN tt-oe-prmtx.discount[8]  = 0.
        IF tt-oe-prmtx.uom[8]       = "?" OR  tt-oe-prmtx.uom[8]        = ? THEN tt-oe-prmtx.uom[8]       = "". 
        IF tt-oe-prmtx.qty[9]       = ?   THEN tt-oe-prmtx.qty[9]       = 0. 
        IF tt-oe-prmtx.price[9]     = ?   THEN tt-oe-prmtx.price[9]     = 0. 
        IF tt-oe-prmtx.discount[9]  = ?   THEN tt-oe-prmtx.discount[9]  = 0. 
        IF tt-oe-prmtx.uom[9]       = "?" OR  tt-oe-prmtx.uom[9]        = ? THEN tt-oe-prmtx.uom[9]       = "". 
        IF tt-oe-prmtx.qty[10]      = ?   THEN tt-oe-prmtx.qty[10]      = 0. 
        IF tt-oe-prmtx.price[10]    = ?   THEN tt-oe-prmtx.price[10]    = 0. 
        IF tt-oe-prmtx.discount[10] = ?   THEN tt-oe-prmtx.discount[10] = 0. 
        IF tt-oe-prmtx.uom[10]      = "?" OR  tt-oe-prmtx.uom[10]       = ? THEN tt-oe-prmtx.uom[10]      = "".
        

        IF tt-oe-prmtx.cust-no EQ ? THEN
            ASSIGN
                tt-oe-prmtx.cust-no = "".
        ELSE 
        DO:
            FIND FIRST bf-cust 
                WHERE bf-cust.company = tt-oe-prmtx.company
                AND bf-cust.cust-no = tt-oe-prmtx.cust-no 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(bf-cust) THEN 
            DO:
                PUT STREAM log-file UNFORMATTED "Invalid Customer Number " + '"' + tt-oe-prmtx.cust-no + '"' + ", in row " + STRING(tt-oe-prmtx.row-no) + "." SKIP.
                tt-oe-prmtx.valid = FALSE.
            END.
        END. /*cust-no not blank*/
        FIND FIRST bf-itemfg 
            WHERE bf-itemfg.company = tt-oe-prmtx.company             
            AND bf-itemfg.cust-no = tt-oe-prmtx.cust-no
            AND bf-itemfg.i-no    = tt-oe-prmtx.i-no 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(bf-itemfg) THEN 
        DO:
            FIND FIRST bf-itemfg 
                WHERE bf-itemfg.company = tt-oe-prmtx.company
                AND bf-itemfg.i-no    = tt-oe-prmtx.i-no 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(bf-itemfg) THEN 
            DO: 
                PUT STREAM log-file UNFORMATTED "Invalid FG Item Number " + '"' + tt-oe-prmtx.i-no + '"' + ", in row " + STRING(tt-oe-prmtx.row-no) + "." SKIP.
                tt-oe-prmtx.valid = FALSE.
            END.
            ELSE
                tt-oe-prmtx.i-no    = bf-itemfg.i-no.
        END. /*not avail bf-itemfg*/
        ELSE
            tt-oe-prmtx.i-no    = bf-itemfg.i-no.
        IF tt-oe-prmtx.custype EQ ? THEN
            ASSIGN
                tt-oe-prmtx.custype = "".
        ELSE 
        DO:
            FIND FIRST bf-custype 
                WHERE bf-custype.company = tt-oe-prmtx.company
                AND bf-custype.custype = tt-oe-prmtx.custype 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(bf-custype) THEN 
            DO:
                tt-oe-prmtx.valid = FALSE.
                PUT STREAM log-file UNFORMATTED "Invalid Cust Type " + '"' + tt-oe-prmtx.custype + '"' + ", in row " + STRING(tt-oe-prmtx.row-no) + "." SKIP.
            END. /*not avail bf-custype*/
        END. /*Custtype not blank*/
        FIND FIRST bf-fgcat 
            WHERE bf-fgcat.company = tt-oe-prmtx.company
            AND bf-fgcat.procat = tt-oe-prmtx.procat 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(bf-fgcat) THEN 
        DO:
            tt-oe-prmtx.valid = FALSE.
            PUT STREAM log-file UNFORMATTED "Invalid Product Category " + '"' + tt-oe-prmtx.procat + '"' + ", in row " + STRING(tt-oe-prmtx.row-no) + "." SKIP.
        END. /*not avail fgcat*/
    
        /*PROCESS IF VALID*/
        IF tt-oe-prmtx.valid = TRUE THEN 
        DO:
            dtEffDate = IF tt-oe-prmtx.refcode NE "" THEN DATE(tt-oe-prmtx.refcode) ELSE TODAY.
            cEffDate = STRING(YEAR(dtEffDate),"9999") +
                STRING(MONTH(dtEffDate),"99")  +
                STRING(DAY(dtEffDate),"99"). 
            FIND FIRST bf-oe-prmtx 
                WHERE bf-oe-prmtx.company EQ tt-oe-prmtx.company
                AND bf-oe-prmtx.cust-no EQ  TRIM(tt-oe-prmtx.cust-no)
                AND bf-oe-prmtx.custype EQ  trim(tt-oe-prmtx.custype)
                AND bf-oe-prmtx.procat  EQ  trim(tt-oe-prmtx.procat)
                AND SUBSTRING(bf-oe-prmtx.i-no,01,15)  EQ  SUBSTRING(tt-oe-prmtx.i-no,01,15)  
                AND bf-oe-prmtx.eff-date EQ dtEffDate
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE bf-oe-prmtx THEN 
            DO:
                PUT STREAM log-file UNFORMATTED 
                    "Valid record - Adding " 
                    " EffDate: " STRING(dtEffDate)  
                    " Cust:" TRIM(tt-oe-prmtx.cust-no)
                    " CustType:" TRIM(tt-oe-prmtx.custype) 
                    " ProdCat:" TRIM(tt-oe-prmtx.procat) 
                    " Item#:" SUBSTRING(tt-oe-prmtx.i-no,01,15) ")"
                    SKIP.
                CREATE oe-prmtx.
                BUFFER-COPY tt-oe-prmtx EXCEPT rec_key refcode valid row-no TO oe-prmtx NO-ERROR.
                ASSIGN 
                    opiCountAdded     = opiCountAdded + 1
                    oe-prmtx.eff-date = dtEffDate.
            END. /* not avail bf-oe-prmtx*/
            ELSE 
            DO:
                PUT STREAM log-file UNFORMATTED 
                    "Valid record - Updating (" 
                    " EffDate: " STRING(dtEffDate)  
                    " Cust:" TRIM(tt-oe-prmtx.cust-no)
                    " CustType:" TRIM(tt-oe-prmtx.custype) 
                    " ProdCat:" TRIM(tt-oe-prmtx.procat) 
                    " Item#:" SUBSTRING(tt-oe-prmtx.i-no,01,15) ")"
                    SKIP.
                ASSIGN
                    opiCountOverwritten      = opiCountOverwritten + 1
                    bf-oe-prmtx.meth         = tt-oe-prmtx.meth
                    bf-oe-prmtx.qty[1]       = tt-oe-prmtx.qty[1]                   
                    bf-oe-prmtx.price[1]     = tt-oe-prmtx.price[1]     
                    bf-oe-prmtx.discount[1]  = tt-oe-prmtx.discount[1]  
                    bf-oe-prmtx.uom[1]       = tt-oe-prmtx.uom[1]       
                    bf-oe-prmtx.qty[2]       = tt-oe-prmtx.qty[2]       
                    bf-oe-prmtx.price[2]     = tt-oe-prmtx.price[2]     
                    bf-oe-prmtx.discount[2]  = tt-oe-prmtx.discount[2]  
                    bf-oe-prmtx.uom[2]       = tt-oe-prmtx.uom[2]       
                    bf-oe-prmtx.qty[3]       = tt-oe-prmtx.qty[3]       
                    bf-oe-prmtx.price[3]     = tt-oe-prmtx.price[3]     
                    bf-oe-prmtx.discount[3]  = tt-oe-prmtx.discount[3]  
                    bf-oe-prmtx.uom[3]       = tt-oe-prmtx.uom[3]       
                    bf-oe-prmtx.qty[4]       = tt-oe-prmtx.qty[4]       
                    bf-oe-prmtx.price[4]     = tt-oe-prmtx.price[4]     
                    bf-oe-prmtx.discount[4]  = tt-oe-prmtx.discount[4]  
                    bf-oe-prmtx.uom[4]       = tt-oe-prmtx.uom[4]       
                    bf-oe-prmtx.qty[5]       = tt-oe-prmtx.qty[5]       
                    bf-oe-prmtx.price[5]     = tt-oe-prmtx.price[5]     
                    bf-oe-prmtx.discount[5]  = tt-oe-prmtx.discount[5]  
                    bf-oe-prmtx.uom[5]       = tt-oe-prmtx.uom[5]       
                    bf-oe-prmtx.qty[6]       = tt-oe-prmtx.qty[6]       
                    bf-oe-prmtx.price[6]     = tt-oe-prmtx.price[6]     
                    bf-oe-prmtx.discount[6]  = tt-oe-prmtx.discount[6]  
                    bf-oe-prmtx.uom[6]       = tt-oe-prmtx.uom[6]       
                    bf-oe-prmtx.qty[7]       = tt-oe-prmtx.qty[7]       
                    bf-oe-prmtx.price[7]     = tt-oe-prmtx.price[7]     
                    bf-oe-prmtx.discount[7]  = tt-oe-prmtx.discount[7]  
                    bf-oe-prmtx.uom[7]       = tt-oe-prmtx.uom[7]       
                    bf-oe-prmtx.qty[8]       = tt-oe-prmtx.qty[8]       
                    bf-oe-prmtx.price[8]     = tt-oe-prmtx.price[8]     
                    bf-oe-prmtx.discount[8]  = tt-oe-prmtx.discount[8]  
                    bf-oe-prmtx.uom[8]       = tt-oe-prmtx.uom[8]       
                    bf-oe-prmtx.qty[9]       = tt-oe-prmtx.qty[9]       
                    bf-oe-prmtx.price[9]     = tt-oe-prmtx.price[9]     
                    bf-oe-prmtx.discount[9]  = tt-oe-prmtx.discount[9]  
                    bf-oe-prmtx.uom[9]       = tt-oe-prmtx.uom[9]       
                    bf-oe-prmtx.qty[10]      = tt-oe-prmtx.qty[10]      
                    bf-oe-prmtx.price[10]    = tt-oe-prmtx.price[10]    
                    bf-oe-prmtx.discount[10] = tt-oe-prmtx.discount[10] 
                    bf-oe-prmtx.uom[10]      = tt-oe-prmtx.uom[10]  
                    bf-oe-prmtx.eff-date     = dtEffDate. 
            END.  /* avail bf-oe-prmtx*/
        END.   /* tt-item-comm.valid = TRUE*/
        ELSE  opiCountNotOK = opiCountNotOK + 1.
    END.  /*  for each tt */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Summarize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Summarize Procedure 
PROCEDURE Summarize :
    /*------------------------------------------------------------------------------
      Purpose:  Summarizes data, presents message and closes log
      Parameters:  Input counts
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiCountTotal AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCountAdded AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCountOverwritten AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCountNotOK AS INTEGER NO-UNDO.

    DEFINE VARIABLE cSummaryMsg AS CHARACTER EXTENT 4 NO-UNDO.

    ASSIGN 
        cSummaryMsg[1] = "Total number of records in Excel file " + STRING(ipiCountTotal) 
        cSummaryMsg[2] = "Total number of records sucessfully created " + STRING(ipiCountAdded)
        cSummaryMsg[3] = "Total number of records over written " + STRING(ipiCountOverwritten)
        cSummaryMsg[4] = "Total number of records in Excel file with errors " + STRING(ipiCountNotOK).

    PUT STREAM log-file UNFORMATTED cSummaryMsg[1] SKIP cSummaryMsg[2] SKIP cSummaryMsg[3] SKIP cSummaryMsg[4] SKIP.
    OUTPUT STREAM log-file CLOSE.

    MESSAGE "Import completed." SKIP(1)
        cSummaryMsg[1] SKIP cSummaryMsg[2] SKIP cSummaryMsg[3] SKIP cSummaryMsg[4] SKIP(1)
        "For details please review import log file:" gcLogFile
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

