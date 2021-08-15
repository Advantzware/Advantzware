&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
"Structured Procedure File Template.

Use this template to create a new Structured Procedure file to compile and run PROGRESS 4GL code. You edit structured procedure files using the AB's Section Editor."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : purgeProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : MYT
    Created     : Wed Jun 24 09:43:52 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF STREAM sDump.
DEF STREAM sReport.

DEFINE TEMP-TABLE ttFileList
    FIELD cFileName AS CHAR 
    FIELD rRowID AS ROWID 
    FIELD cRule AS CHAR 
    FIELD cError AS CHAR  
    FIELD cMessage AS CHAR
    FIELD lPurge AS LOG
    FIELD cRec_key AS CHAR
    FIELD cKeyValues AS CHAR 
    .
    
DEF TEMP-TABLE ttPurgeList
    FIELD cTable AS CHAR 
    FIELD rRowid AS ROWID 
    FIELD lPurge AS LOG
    .     
    
DEF TEMP-TABLE ttGLHistList
    FIELD lPosted AS LOG FORMAT "POSTED/UNPOSTED"
    FIELD iYear AS INT
    FIELD iPeriod AS INT 
    FIELD cAccount AS CHAR 
    FIELD cJournal AS CHAR 
    FIELD daTxnDate AS DATE 
    FIELD deAmount AS DECIMAL 
    FIELD cCurrency AS CHAR 
    FIELD cDescription AS CHAR 
    FIELD cType AS CHAR 
    FIELD cCreatedBy AS CHAR 
    FIELD cFileName AS CHAR 
    FIELD cReckey AS CHAR 
    FIELD rRowID AS ROWID.        
    
DEF VAR cCompanyList AS CHAR NO-UNDO.
DEF VAR cFieldName AS CHAR NO-UNDO.
DEF VAR cLocList AS CHAR NO-UNDO.
DEF VAR cOutputDir AS CHAR NO-UNDO.
DEF VAR cRecKeyPrefix AS CHAR NO-UNDO.
DEF VAR cThisCompany AS CHAR NO-UNDO.
DEF VAR hActnumField AS HANDLE NO-UNDO.
DEF VAR hBNoField AS HANDLE NO-UNDO.
DEF VAR hBolNoField AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hBufferField AS HANDLE NO-UNDO.
DEF VAR hCompanyField AS HANDLE NO-UNDO.
DEF VAR hCustNoField AS HANDLE NO-UNDO.
DEF VAR hInoField AS HANDLE NO-UNDO.
DEF VAR hInvNoField AS HANDLE NO-UNDO.
DEF VAR hLocField AS HANDLE NO-UNDO.
DEF VAR hOrdNoField AS HANDLE NO-UNDO.
DEF VAR hPoNoField AS HANDLE NO-UNDO.
DEF VAR hJobField AS HANDLE NO-UNDO.
DEF VAR hJobNoField AS HANDLE NO-UNDO.
DEF VAR hJobNo2Field AS HANDLE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hRecKey AS HANDLE NO-UNDO.
DEF VAR hRnoField AS HANDLE NO-UNDO.
DEF VAR hTestField AS HANDLE NO-UNDO.
DEF VAR hXnoField AS HANDLE NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR iErrorCount AS INT NO-UNDO.
DEF VAR iProcessedCount AS INT NO-UNDO.
DEF VAR iWarningCount AS INT NO-UNDO.
DEF VAR lPurge AS LOG NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */


&IF DEFINED(EXCLUDE-dynExport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dynExport Procedure
FUNCTION dynExport RETURNS CHARACTER 
  (INPUT hRecord AS HANDLE,
   INPUT cDelim AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF




/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-outputGLaccountFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputGLaccountFile Procedure
PROCEDURE outputGLaccountFile:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcOutputDir AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
    DEFINE VARIABLE cWarning AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAmount AS DECIMAL NO-UNDO.
    
    OUTPUT STREAM sReport TO VALUE (ipcOutputDir + "\" + ipcFileName).
    
    PUT STREAM sReport UNFORMATTED 
        "Warning,Year,Period,Account,Journal,Txn Date,Amount,Currency,Description,Type,Created By,FileName,RecKey,Rowid" + CHR(10).
    FOR EACH ttGLHistList
        BREAK BY ttGLHistList.iYear
        BY ttGLHistList.iPeriod
        BY ttGLHistList.cAccount
        BY ttGLHistList.daTxnDate:
        
        IF FIRST-OF(ttGLHistList.cAccount) THEN
        DO:         
            cWarning = "".
            cAmount = 0.
            FIND FIRST account NO-LOCK
                 WHERE account.company EQ ipcCompany 
                 AND account.actnum EQ ttGLHistList.cAccount NO-ERROR .
            IF NOT AVAIL account THEN
            cWarning = " Invalid Account number".
            ELSE IF account.inactive THEN
            cWarning = " Account is inactive".              
        END. 
        
        cAmount = cAmount + ttGLHistList.deAmount.
        IF LAST-OF(ttGLHistList.cAccount) THEN
        DO:     
         PUT STREAM sReport UNFORMATTED
            cWarning + "," +
            STRING(ttGLHistList.iYear,"9999") + "," +
            STRING(ttGLHistList.iPeriod,"99") + "," +
            ttGLHistList.cAccount + "," +
            ttGLHistList.cJournal + "," +
            STRING(ttGLHistList.daTxnDate,"99/99/99") + "," +
            STRING(cAmount,"->>>>>>>>9.99") + "," +
            ttGLHistList.cCurrency + "," +
            REPLACE(ttGLHistList.cDescription,","," ") + "," +
            ttGLHistList.cType + "," +
            ttGLHistList.cCreatedBy + "," +
            ttGLHistList.cFileName + "," +   
            ttGLHistList.cReckey + ","
            STRING(ttGLHistList.rRowID) +
            CHR(10).
        END.    
    END.
    OUTPUT STREAM sReport CLOSE.
    EMPTY TEMP-TABLE ttFileList.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDeleteRecordsByRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteRecordsByRowid Procedure
PROCEDURE pDeleteRecordsByRowid:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    CREATE BUFFER hBuffer FOR TABLE ipcFileName.
    CREATE QUERY hQuery.
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " NO-LOCK").
    hQuery:QUERY-OPEN().

    OUTPUT STREAM sDump TO VALUE (cOutputDir + "\" + ipcFileName + ".d").
    FOR EACH ttPurgeList WHERE 
        ttPurgeList.cTable EQ ipcFileName TRANSACTION:
        hQuery:REPOSITION-TO-ROWID (ttPurgeList.rRowid).
        hBuffer:FIND-BY-ROWID(ttPurgeList.rRowid, EXCLUSIVE-LOCK).
        IF hBuffer:AVAILABLE THEN DO:
            PUT STREAM sDump UNFORMATTED dynExport(hBuffer, " ") SKIP.
            hBuffer:BUFFER-DELETE().
        END.
    END.
    OUTPUT STREAM sDump CLOSE.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pGetFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFieldList Procedure
PROCEDURE pGetFieldList:
/*------------------------------------------------------------------------------
 Purpose: Returns complete list of indexed fields for this table
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR.
    DEF OUTPUT PARAMETER opcFieldList AS CHAR.

    FIND FIRST asi._file NO-LOCK WHERE 
    asi._file._file-name = ipcFileName 
        NO-ERROR.

    IF AVAIL asi._file THEN FOR EACH asi._index OF asi._file, 
                                EACH asi._index-field OF asi._index, 
                                EACH asi._field OF asi._index-field NO-LOCK 
        BY asi._field._order:
        IF LOOKUP(_field._field-name,opcFieldList) EQ 0 THEN ASSIGN 
            opcFieldList = opcFieldList + asi._field._field-name + ",".
    END.

    ASSIGN 
        opcFieldList = TRIM(opcFieldList,",").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pTestGlhist) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTestGlhist Procedure
PROCEDURE pTestGlhist:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipiYear AS INT NO-UNDO.
    DEF INPUT PARAMETER ipiPeriod AS INT NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    DEF VAR daPeriodStartDate AS DATE NO-UNDO.
    DEF VAR daPeriodEndDate AS DATE NO-UNDO.
    DEF VAR cQueryString AS CHAR NO-UNDO.
            
    DEF BUFFER bglHist FOR glHist.
    DEF BUFFER bPeriod FOR period.
    DEF BUFFER cPeriod FOR period.
    DEF BUFFER bf-glhist FOR glHist.
    
    FIND company NO-LOCK WHERE 
        company.company EQ ipcCompany
        NO-ERROR.
        
    FIND period NO-LOCK WHERE
        period.company EQ ipcCompany AND 
        period.yr EQ ipiYear AND 
        period.pnum EQ ipiPeriod
        NO-ERROR.
    /* There's not a period record for this year/period, so create one based on
       the latest year where this period number exists. Make sure the period we
       create here is CLOSED, as are all subledgers.  Let the create.trg assign rec_key. */
    IF NOT AVAIL period THEN DO:
        FIND LAST bPeriod NO-LOCK WHERE 
            bPeriod.company EQ ipcCompany AND 
            bPeriod.pnum EQ ipiPeriod
            NO-ERROR.
        FIND LAST cPeriod NO-LOCK WHERE 
            cPeriod.company EQ ipcCompany AND 
            cPeriod.pnum EQ IF ipiPeriod NE company.num-per THEN ipiPeriod + 1 ELSE 1 
            NO-ERROR.
        CREATE period.
        ASSIGN 
            period.company = ipcCompany
            period.yr = ipiYear
            period.pnum = bperiod.pnum
            period.pname = ""
            period.pst = DATE(MONTH(bperiod.pst), DAY(bperiod.pst), ipiYear) 
            /* This is tricky, because it has to account for Feb 29, so it gets the
               start date for the NEXT month, then subtracts 1 */       
            period.pend = DATE(MONTH(cperiod.pst), 1, ipiYear) - 1
            period.pstat = FALSE 
            period.subledgerAP = "C"
            period.subledgerPO = "C"
            period.subledgerOP = "C"
            period.subledgerWIP = "C"
            period.subledgerRM = "C"
            period.subledgerFG = "C"
            period.subledgerBR = "C"
            period.subledgerAR = "C"
            period.subledgerGL = "C"
            period.apClosedBy = USERID(LDBNAME(1))
            period.poClosedBy = USERID(LDBNAME(1))
            period.opClosedBy = USERID(LDBNAME(1))
            period.wipClosedBy = USERID(LDBNAME(1))
            period.rmClosedBy = USERID(LDBNAME(1))
            period.fgClosedBy = USERID(LDBNAME(1))
            period.brClosedBy = USERID(LDBNAME(1))
            period.arClosedBy = USERID(LDBNAME(1))
            period.glClosedBy = USERID(LDBNAME(1))
            period.apClosed = DATETIME(TODAY, MTIME)
            period.poClosed = DATETIME(TODAY, MTIME)
            period.opClosed = DATETIME(TODAY, MTIME)
            period.wipClosed = DATETIME(TODAY, MTIME)
            period.rmClosed = DATETIME(TODAY, MTIME)
            period.fgClosed = DATETIME(TODAY, MTIME)
            period.brClosed = DATETIME(TODAY, MTIME)
            period.arClosed = DATETIME(TODAY, MTIME)
            period.glClosed = DATETIME(TODAY, MTIME)
            .
    END.
    ASSIGN 
        daPeriodStartDate = period.pst
        daPeriodEndDate = period.pend.
    RELEASE period.
        
    FOR EACH bglhist NO-LOCK WHERE 
        bglhist.company EQ ipcCompany AND 
/*        bglhist.entryType NE "B" AND*/
            /* If the year and period are included in the gl-hist use them */ 
        (bglhist.yr EQ ipiYear AND bglhist.period EQ ipiPeriod) OR 
            /* or, if not, use the trans-date to find qualifying records.
               Note that we do NOT put the (assumed) year/period in the report. */
        (bglhist.yr EQ 0 AND bglhist.tr-date GE daPeriodStartDate AND bglhist.tr-date LE daPeriodEndDate):
            
        CREATE ttGLHistList.
        ASSIGN 
            ttGLHistList.lPosted         = YES
            ttGLHistList.iYear           = IF bglhist.glyear NE 0 THEN bglhist.glyear ELSE          /* First choice = record.glyear */ 
                                           IF bglhist.yr NE 0 THEN bglhist.yr ELSE                  /* Second choice = record.year */ 
                                           IF AVAIL period AND period.pnum NE 0 THEN period.yr ELSE /* Third choice = period.pnum */   
                                           YEAR(bglhist.tr-date)                                    /* finally, year of txn date */
            ttGLHistList.iPeriod         = bglhist.period
            ttGLHistList.cAccount        = bglhist.actnum
            ttGLHistList.cJournal        = bglhist.jrnl
            ttGLHistList.daTxnDate       = bglhist.tr-date
            ttGLHistList.deAmount        = IF bglhist.tr-amt EQ ? THEN 0 ELSE bglhist.tr-amt
            ttGLHistList.cCurrency       = bglhist.curr-code[1]
            ttGLHistList.cDescription    = bglhist.tr-dscr
            ttGLHistList.cType           = bglhist.entryType
            ttGLHistList.cCreatedBy      = bglhist.createdby
            ttGLHistList.cFileName       = "glhist"
            ttGLHistList.cReckey         = bglhist.rec_key
            ttGLHistList.rRowID          = ROWID(bglhist)
            .
        IF NOT bglhist.posted THEN
        DO:
           FIND FIRST bf-glhist EXCLUSIVE-LOCK
                WHERE ROWID(bf-glhist) EQ ROWID(bglhist) NO-ERROR.
           IF AVAIL bf-glhist THEN
           bf-glhist.posted = YES.
           RELEASE bf-glhist.
        END.
    END.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pTestInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTestInvoice Procedure
PROCEDURE pTestInvoice:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    DEF VAR cFieldList AS CHAR NO-UNDO.
    DEF VAR lError AS LOG NO-UNDO.
    DEF VAR lWarning AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    DEF VAR cRule AS CHAR NO-UNDO.
    
    CREATE BUFFER hBuffer FOR TABLE ipcFileName.
    CREATE QUERY hQuery.

    ASSIGN
        hRecKey = ?
        hCompanyField = ?
        hCustNoField = ?
        hInvNoField = ?
        hXnoField = ?.
    
    /* Assign buffer-field names IF they are indexed fields */
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hTestField = hBuffer:BUFFER-FIELD(iCtr).
        /* Assign field names if exist in this table */        
        IF hTestField:NAME EQ "rec_key" THEN ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "company" THEN ASSIGN
            hCompanyField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "cust-no" THEN ASSIGN
            hCustNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "inv-no" THEN ASSIGN
            hInvNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "x-no" THEN ASSIGN
            hXnoField = hBuffer:BUFFER-FIELD(iCtr).
    END.

    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" +
        (IF hCompanyField NE ? THEN (" AND " + ipcFileName + ".company EQ '" + cThisCompany + "'") ELSE "") + 
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        /* Company tests */
        IF hCompanyField NE ? THEN 
        DO:
            IF hCompanyField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank Company"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST company WHERE 
                    company.company EQ hCompanyField:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid Company"
                        cMessage = cMessage + ",Company=" + hCompanyField:BUFFER-VALUE.  
        END.

        /* Cust-no tests */
        IF hCustNoField NE ? THEN 
        DO:
            IF hCustNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank Cust-No"
                    cMessage = cMessage + ",<blank>".
            ELSE IF hCustNoField:BUFFER-VALUE NE "" 
                    AND NOT CAN-FIND(FIRST cust WHERE 
                    cust.cust-no EQ hCustNoField:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid Cust-no"
                        cMessage = cMessage + ",Cust-no=" + hCustNoField:BUFFER-VALUE.
        END.

        /* Inv-no tests */
        IF hInvNoField NE ? THEN 
        DO:
            IF hInvNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank Inv No"
                    cMessage = cMessage + ",<blank>".
            ELSE IF hInvNoField:BUFFER-VALUE NE 0
                    AND NOT CAN-FIND(FIRST inv-head WHERE 
                    inv-head.company EQ hCompanyField:BUFFER-VALUE AND 
                    inv-head.inv-no EQ hInvNoField:BUFFER-VALUE) 
                    AND NOT CAN-FIND(FIRST ar-inv WHERE 
                    ar-inv.company EQ hCompanyField:BUFFER-VALUE AND 
                    ar-inv.inv-no EQ hInvNoField:BUFFER-VALUE)THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid Inv No"
                        cMessage = cMessage + ",Inv-no=" + hInvNoField:BUFFER-VALUE. 
                 
        END.
        
        /* X-no tests */
        IF hXNoField NE ? THEN 
        DO:
            IF hXNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank X-no"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST ar-inv WHERE 
                    ar-inv.company EQ hCompanyField:BUFFER-VALUE AND     
                    ar-inv.x-no EQ hXNoField:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid X-no"
                        cMessage = cMessage + ",X-no=" + hXNoField:BUFFER-VALUE.  
        END.
        
        IF lError 
            OR lWarning THEN 
        DO:
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName    = ipcFileName
                ttFileList.rRowID       = hBuffer:ROWID
                ttFileList.cRec_key     = hRecKey:BUFFER-VALUE
                ttFileList.cError       = IF lError THEN "Error" ELSE IF lWarning THEN "Warning" ELSE ""
                ttFileList.lPurge       = lError
                ttFileList.cRule        = SUBSTRING(cRule,2)
                ttFileList.cMessage     = SUBSTRING(cMessage,2)
                ttFileList.cKeyValues   = (IF hCompanyField NE ? THEN hCompanyField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hCustNoField NE ? THEN hCustNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hLocField NE ? THEN hLocField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hOrdNoField NE ? THEN hOrdNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hRNoField NE ? THEN hRNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hBolNoField NE ? THEN hBolNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hBNoField NE ? THEN hBNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hInvNoField NE ? THEN hInvNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hINoField NE ? THEN hINoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hXNoField NE ? THEN hXNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hActNumField NE ? THEN hActNumField:BUFFER-VALUE ELSE "")
                iErrorCount             = IF ttFileList.cError EQ "Error" THEN iErrorCount + 1 ELSE iErrorCount
                iWarningCount           = IF ttFileList.cError EQ "Warning" THEN iWarningCount + 1 ELSE iWarningCount
                .
                
        END.        
        ASSIGN 
            lError = FALSE 
            lWarning = FALSE  
            cRule = ""
            cMessage = ""
            iProcessedCount = iProcessedCount + 1.
        hQuery:GET-NEXT().
    END.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pTestBlankCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTestOneFile Procedure
PROCEDURE pTestOneFile:
/*------------------------------------------------------------------------------
 Purpose:   locates records with blank company code and adds to temp-table
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    DEF VAR cFieldList AS CHAR NO-UNDO.
    DEF VAR lError AS LOG NO-UNDO.
    DEF VAR lWarning AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    DEF VAR cRule AS CHAR NO-UNDO.
    
    /* Make a list of indexed fields in this table */
    FIND FIRST asi._file NO-LOCK WHERE 
        asi._file._file-name = ipcFileName 
        NO-ERROR.
    IF AVAIL asi._file THEN 
        FOR EACH asi._index OF asi._file, 
            EACH asi._index-field OF asi._index, 
            EACH asi._field OF asi._index-field NO-LOCK 
            BY asi._field._order:
            IF LOOKUP(_field._field-name,cFieldList) EQ 0 THEN ASSIGN 
                    cFieldList = cFieldList + asi._field._field-name + ",".
        END.
    ASSIGN 
        cFieldList = TRIM(cFieldList,",").

    CREATE BUFFER hBuffer FOR TABLE ipcFileName.
    CREATE QUERY hQuery.

    ASSIGN
        hRecKey = ?
        hActnumField = ?
        hBNoField = ?
        hBolNoField = ?
        hCompanyField = ?
        hCustNoField = ?
        hInoField = ?
        hInvNoField = ?
        hLocField = ?
        hOrdNoField = ?
        hPoNoField = ?
        hRnoField = ?
        hTestField = ?
        hXnoField = ?
        hJobField = ?
        hJobNoField = ?
        hJobNo2Field = ?.
    
    /* Assign buffer-field names IF they are indexed fields */
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hTestField = hBuffer:BUFFER-FIELD(iCtr).
        /* If this field is not in the list of indexed fields and isn't "rec_key", skip it */
        IF NOT CAN-DO(cFieldList,hTestField:NAME) 
        AND hTestField:NAME NE "rec_key" THEN NEXT.
        /* Assign field names if exist in this table */        
        IF hTestField:NAME EQ "rec_key" THEN ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "actnum" THEN ASSIGN
            hActnumField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "b-no" THEN ASSIGN
            hBNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "bol-no" THEN ASSIGN
            hBolNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "company" THEN ASSIGN
            hCompanyField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "cust-no" THEN ASSIGN
            hCustNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "i-no" THEN ASSIGN
            hInoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "inv-no" THEN ASSIGN
            hInvNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "loc" THEN ASSIGN
            hLocField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "ord-no" THEN ASSIGN 
            hOrdNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "po-no" THEN ASSIGN 
            hPoNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "r-no" THEN ASSIGN 
            hRNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "x-no" THEN ASSIGN
            hXnoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "job" THEN ASSIGN
            hJobField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "job-no" THEN ASSIGN
            hJobNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "job-no2" THEN ASSIGN
            hJobNo2Field = hBuffer:BUFFER-FIELD(iCtr).
    END.

    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
                          ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" +
                          (IF hCompanyField NE ? THEN (" AND " + ipcFileName + ".company EQ '" + cThisCompany + "'") ELSE "") + 
                          " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        /* Account number tests */
        IF hActNumField NE ? THEN DO:
            IF hActNumField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = IF CAN-DO("ar-invl",ipcFileName) THEN TRUE ELSE lError 
                lWarning = IF NOT CAN-DO("ar-invl",ipcFileName) THEN TRUE ELSE lWarning 
                cRule = cRule + ",Blank Acct No"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST account WHERE 
                                 account.company EQ cThisCompany AND 
                                 account.actnum EQ hActNumField:BUFFER-VALUE) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid Acct No"
                cMessage = cMessage + ",Acctnum=" + hActNumField:BUFFER-VALUE.  
        END.

        /* B-no tests */
        IF hBNoField NE ? THEN DO:
            IF hBNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("fg-rcpth",ipcFileName) THEN ASSIGN 
                lWarning = TRUE 
                cRule = cRule + ",Blank B-no"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST oe-bolh WHERE 
                                 oe-bolh.b-no EQ hBNoField:BUFFER-VALUE) 
            AND NOT CAN-DO("fg-rcpth,fg-rcpts,fg-rctd,fg-rdtlh,rm-rctd",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid B-no"
                cMessage = cMessage + ",B-no=" + hBNoField:BUFFER-VALUE.  
        END.

        /* BOL-no tests */
        IF hBolNoField NE ? THEN DO:
            IF hBolNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("fg-bin,fg-rdtlh",ipcFileName) THEN ASSIGN 
                lWarning = TRUE 
                cRule = cRule + ",Blank BOL-no"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST oe-bolh WHERE 
                                 oe-bolh.bol-no EQ hBolNoField:BUFFER-VALUE) 
            AND NOT CAN-DO("ar-invl,fg-bin,fg-rdtlh,inv-head",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid BOL-no"
                cMessage = cMessage + ",BOL-no=" + hBolNoField:BUFFER-VALUE.  
        END.

        /* Company tests */
        IF hCompanyField NE ? THEN DO:
            IF hCompanyField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank Company"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST company WHERE 
                                 company.company EQ hCompanyField:BUFFER-VALUE) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid Company"
                cMessage = cMessage + ",Company=" + hCompanyField:BUFFER-VALUE.  
        END.

        /* Cust-no tests */
        IF hCustNoField NE ? THEN DO:
            IF hCustNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("fg-bin,fg-rdtlh,item,itemfg,rm-bin,fg-rcpts,job-hdr",ipcFileName) THEN ASSIGN 
                lWarning = IF NOT CAN-DO("oe-rel,oe-relh,oe-rell,oe-bolh,oe-boll,oe-ord,oe-ordl,oe-ordm",ipcFileName) THEN TRUE ELSE lWarning 
                lError = IF CAN-DO("oe-rel,oe-relh,oe-rell,oe-bolh,oe-boll,oe-ord,oe-ordl,oe-ordm",ipcFileName) THEN TRUE ELSE lError 
                cRule = cRule + ",Blank Cust-No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hCustNoField:BUFFER-VALUE NE "" 
            AND NOT CAN-FIND(FIRST cust WHERE 
                                 cust.cust-no EQ hCustNoField:BUFFER-VALUE) THEN ASSIGN 
                lError = IF NOT CAN-DO("item,itemfg",ipcFileName) THEN TRUE ELSE lError 
                lWarning = IF CAN-DO("item,itemfg",ipcFileName) THEN TRUE ELSE lWarning 
                cRule = cRule + ",Invalid Cust-no"
                cMessage = cMessage + ",Cust-no=" + hCustNoField:BUFFER-VALUE.
        END.

        /* I-no tests - Test blank i-no is complicated, as other records/fields come into play */
        IF hINoField NE ? THEN DO:
            IF hINoField:BUFFER-VALUE EQ "" 
            AND ipcFileName EQ "fg-rdtlh"
            AND NOT CAN-FIND(FIRST fg-rcpth WHERE 
                             fg-rcpth.company EQ hCompanyField:BUFFER-VALUE AND 
                             fg-rcpth.r-no EQ INTEGER(hRnoField:BUFFER-VALUE)
                             USE-INDEX r-no) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank I-No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hINoField:BUFFER-VALUE EQ "" 
            AND ipcFileName EQ "fg-rcpth"
            AND NOT CAN-FIND(FIRST fg-rdtlh WHERE 
                             fg-rdtlh.company EQ hCompanyField:BUFFER-VALUE AND 
                             fg-rdtlh.r-no EQ INTEGER(hRnoField:BUFFER-VALUE)
                             USE-INDEX rm-rdtl) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank I-No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hINoField:BUFFER-VALUE EQ "" 
            AND (ipcFileName EQ "oe-boll" OR ipcFileName EQ "oe-rell")
            AND NOT CAN-FIND(FIRST oe-ord WHERE 
                             oe-ord.company EQ hCompanyField:BUFFER-VALUE AND 
                             oe-ord.ord-no EQ INTEGER(hOrdNoField:BUFFER-VALUE)
                             USE-INDEX ord-no) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank I-No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hINoField:BUFFER-VALUE NE "" THEN DO:
                IF NOT CAN-FIND(FIRST item WHERE 
                                     item.company EQ cThisCompany AND  
                                     item.i-no EQ hINoField:BUFFER-VALUE)
                    AND NOT CAN-FIND(FIRST itemfg WHERE 
                                     itemfg.company EQ cThisCompany AND 
                                     itemfg.i-no EQ hINoField:BUFFER-VALUE) THEN ASSIGN 
                    lError = IF NOT CAN-DO("item,itemfg-loc",ipcFileName) THEN TRUE ELSE lError 
                    lWarning = IF CAN-DO("item,itemfg-loc",ipcFileName) THEN TRUE ELSE lWarning 
                    cRule = cRule + ",Invalid Item No"
                    cMessage = cMessage + ",I-no=" + hINoField:BUFFER-VALUE.
/*                ELSE IF NOT CAN-DO("item,itemfg,itemfg-loc",ipcFileName) AND*/
/*                        (CAN-FIND(FIRST item WHERE                          */
/*                                 item.company EQ cThisCompany AND           */
/*                                 item.i-no EQ hINoField:BUFFER-VALUE AND    */
/*                                 item.stat EQ "I")                          */
/*                        OR CAN-FIND(FIRST itemfg WHERE                      */
/*                                itemfg.company = cThisCompany AND           */
/*                                itemfg.i-no EQ hINoField:BUFFER-VALUE AND   */
/*                                itemfg.stat EQ "I"))THEN ASSIGN             */
/*                    lWarning = TRUE                                         */
/*                    cRule = cRule + ",Inactive Item No"                     */
/*                    cMessage = cMessage + ",I-no=" + hINoField:BUFFER-VALUE.*/
                END.
        END.
        
        /* Inv-no tests */
        IF hInvNoField NE ? THEN DO:
            IF hInvNoField:BUFFER-VALUE EQ ""
            AND NOT CAN-DO("fg-bin",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank Inv No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hInvNoField:BUFFER-VALUE NE 0
                AND NOT CAN-FIND(FIRST inv-head WHERE 
                                 inv-head.company EQ hCompanyField:BUFFER-VALUE AND 
                                 inv-head.inv-no EQ hInvNoField:BUFFER-VALUE) 
                AND NOT CAN-FIND(FIRST ar-inv WHERE 
                                 ar-inv.company EQ hCompanyField:BUFFER-VALUE AND 
                                 ar-inv.inv-no EQ hInvNoField:BUFFER-VALUE)THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid Inv No"
                cMessage = cMessage + ",Inv-no=" + hInvNoField:BUFFER-VALUE. 
                 
        END.
        
        /* Loc tests */
        IF hLocField NE ? THEN DO:
            IF hLocField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank Warehouse"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST loc WHERE 
                                loc.company EQ hCompanyField:BUFFER-VALUE AND 
                                loc.loc EQ hLocField:BUFFER-VALUE) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid Warehouse"
                cMessage = cMessage + ",Loc=" + hLocField:BUFFER-VALUE.  
        END.
        
        /* Ord-no tests */
        IF hOrdNoField NE ? THEN DO:
            IF hOrdNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank Order No"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST oe-ord WHERE 
                                oe-ord.company EQ hCompanyField:BUFFER-VALUE AND 
                                oe-ord.ord-no EQ hOrdNoField:BUFFER-VALUE) 
            AND NOT CAN-DO("fg-rcpth,fg-rcpts,fg-rctd,fg-rdtlh,rm-rctd,job-hdr",ipcFileName) 
            THEN ASSIGN 
                lError = IF NOT CAN-DO("ar-inv,ar-invl,ar-invm",ipcFileName) THEN TRUE ELSE lError 
                cRule = cRule + ",Invalid Order No"
                cMessage = cMessage + ",Ord-no=" + hOrdNoField:BUFFER-VALUE.  
        END.
        
        /* Po-no tests */
        IF hPoNoField NE ? THEN 
        DO:
            IF hPoNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("job-hdr",ipcFileName) THEN ASSIGN  
                    lError = TRUE 
                    cRule = cRule + ",Blank PO No"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST po-ord WHERE 
                    po-ord.company EQ hCompanyField:BUFFER-VALUE AND 
                    po-ord.po-no EQ hPoNoField:BUFFER-VALUE) 
                    AND CAN-DO("po-ordl,po-ordl-add",ipcFileName) 
                    THEN ASSIGN 
                        lError = TRUE  
                        cRule = cRule + ",Invalid PO No"
                        cMessage = cMessage + ",Po-no=" + hPoNoField:BUFFER-VALUE.  
        END.

        /* R-no tests */
        IF hRNoField NE ? THEN DO:
            IF hRNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("fg-rcpth,fg-rctd,fg-rdtlh,fg-rdtl,fg-rcpts",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank R-no"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST oe-relh WHERE 
                                oe-relh.company EQ hCompanyField:BUFFER-VALUE AND     
                                oe-relh.r-no EQ hRNoField:BUFFER-VALUE) 
            AND NOT CAN-DO("fg-rcpth,fg-rcpts,fg-rctd,fg-rdtlh,rm-rctd,rm-rcpt,oe-rel,inv-head",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid R-no"
                cMessage = cMessage + ",R-no=" + hRNoField:BUFFER-VALUE.  
        END.
        
        /* X-no tests */
        IF hXNoField NE ? THEN DO:
            IF hXNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank X-no"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST ar-inv WHERE 
                    ar-inv.company EQ hCompanyField:BUFFER-VALUE AND     
                    ar-inv.x-no EQ hXNoField:BUFFER-VALUE) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid X-no"
                cMessage = cMessage + ",X-no=" + hXNoField:BUFFER-VALUE.  
        END.
        
        /* Job tests */
        IF hJobField NE ? THEN DO:
            IF hJobField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank job"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST job WHERE 
                    job.company EQ hCompanyField:BUFFER-VALUE AND     
                    job.job EQ hJobField:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid job"
                        cMessage = cMessage + ",job=" + hJobField:BUFFER-VALUE.  
        END.
        
        /* JobNo/JobNo2 tests */
        IF hJobNoField NE ? THEN DO:
            IF hJobField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank job-no"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST job WHERE 
                    job.company EQ hCompanyField:BUFFER-VALUE AND     
                    job.job-no EQ hJobNoField:BUFFER-VALUE AND 
                    job.job-no2 EQ hJobNo2Field:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid job-no"
                        cMessage = cMessage + ",job-no=" + hJobNoField:BUFFER-VALUE + " and job-no2=" + hJobNo2Field:BUFFER-VALUE.  
        END.
        
        /* No matter what, don't delete fg-bin or rm-bin */
        IF CAN-DO("fg-bin,rm-bin",ipcFileName)
        AND lError THEN ASSIGN 
            lError = FALSE
            lWarning = TRUE.
            

        IF lError 
        OR lWarning THEN DO:
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName    = ipcFileName
                ttFileList.rRowID       = hBuffer:ROWID
                ttFileList.cRec_key     = hRecKey:BUFFER-VALUE
                ttFileList.cError       = IF lError THEN "Error" ELSE IF lWarning THEN "Warning" ELSE ""
                ttFileList.lPurge       = lError
                ttFileList.cRule        = SUBSTRING(cRule,2)
                ttFileList.cMessage     = SUBSTRING(cMessage,2)
                ttFileList.cKeyValues   = (IF hCompanyField NE ? THEN hCompanyField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hCustNoField NE ? THEN hCustNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hLocField NE ? THEN hLocField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hOrdNoField NE ? THEN hOrdNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hPoNoField NE ? THEN hPoNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hRNoField NE ? THEN hRNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hBolNoField NE ? THEN hBolNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hBNoField NE ? THEN hBNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hInvNoField NE ? THEN hInvNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hINoField NE ? THEN hINoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hXNoField NE ? THEN hXNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hActNumField NE ? THEN hActNumField:BUFFER-VALUE ELSE "") + "," + 
                                          (IF hJobField NE ? THEN hJobField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hJobNoField NE ? THEN hJobNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hJobNo2Field NE ? THEN hJobNo2Field:BUFFER-VALUE ELSE "")
                iErrorCount             = IF ttFileList.cError EQ "Error" THEN iErrorCount + 1 ELSE iErrorCount
                iWarningCount           = IF ttFileList.cError EQ "Warning" THEN iWarningCount + 1 ELSE iWarningCount
                .
                
        END.        
        ASSIGN 
            lError = FALSE 
            lWarning = FALSE  
            cRule = ""
            cMessage = ""
            iProcessedCount = iProcessedCount + 1.
        hQuery:GET-NEXT().
    END.
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-purgeComplete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputOrphanFile Procedure
PROCEDURE outputOrphanFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    OUTPUT STREAM sReport TO VALUE (cOutputDir + "\" + "_PurgeReport.csv").
    
    PUT STREAM sReport UNFORMATTED 
        "Purge (Y/N)?,ErrLevel,Table Name,Reckey,Rowid,Company,Cust-no,Loc,Ord-no,Po-no,R-no,Bol-no,B-no,Inv-no,I-no,X-no,Acctnum,Job,Job-No,Job-No2" + CHR(10).
    FOR EACH ttFileList
        BY ttFileList.cError
        BY ttFileList.cFileName 
        BY ttFileList.cRec_key:
        PUT STREAM sReport UNFORMATTED
            (IF ttFileList.lPurge THEN "YES" ELSE "") + "," +
            ttFileList.cError + "," +
            ttFileList.cFileName + "," +   
            ttFileList.cRec_key + ","                    
            STRING(ttFileList.rRowID) + "," +                  
            ttFileList.cKeyValues + ",,"
            .
        DO iCtr = 1 TO NUM-ENTRIES(cRule):
            PUT STREAM sReport UNFORMATTED 
                ENTRY(iCtr,cRule) + "," + ENTRY(iCtr,cMessage) + ",".
        END.
        PUT STREAM sReport UNFORMATTED CHR(10).
    END.
    OUTPUT STREAM sReport CLOSE.
    EMPTY TEMP-TABLE ttFileList.
    
/*    IF lPurge THEN DO:                                                                        */
/*        FOR EACH ttFileList                                                                   */
/*            BREAK BY ttFileList.cFileName:                                                    */
/*                                                                                              */
/*            IF FIRST-OF(ttFileList.cFileName) THEN DO:                                        */
/*                OUTPUT STREAM sDump TO VALUE (cOutputDir + "\" + ttFileList.cFileName + ".d").*/
/*                CREATE BUFFER hBuffer FOR TABLE ttFileList.cFileName.                         */
/*                hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).                                         */
/*                CREATE QUERY hQuery.                                                          */
/*                hQuery:ADD-BUFFER(hBuffer).                                                   */
/*            END.                                                                              */
/*                                                                                              */
/*            DO TRANSACTION:                                                                   */
/*                hQuery:QUERY-PREPARE("FOR EACH " + ttFileList.cFileName +                     */
/*                                     " WHERE ROWID(" + ttFileList.cFileName + ")" +           */
/*                                     " EQ TO-ROWID('" + STRING(ttFileList.rRowID) + "')").    */
/*                hQuery:QUERY-OPEN().                                                          */
/*                hQuery:GET-FIRST(EXCLUSIVE-LOCK).                                             */
/*                OUTPUT STREAM sDump TO VALUE (cOutputDir + "\" + ttFileList.cFileName + ".d").*/
/*                IF hBuffer:AVAILABLE THEN DO:                                                 */
/*                    PUT STREAM sDump UNFORMATTED dynExport(hBuffer, " ") SKIP.                */
/*                    hBuffer:BUFFER-DELETE().                                                  */
/*                END.                                                                          */
/*                OUTPUT STREAM sDump CLOSE.                                                    */
/*            END.                                                                              */
/*                                                                                              */
/*            IF LAST-OF(ttFileList.cFileName) THEN DO:                                         */
/*                DELETE OBJECT hQuery.                                                         */
/*                DELETE OBJECT hBuffer.                                                        */
/*            END.                                                                              */
/*        END.                                                                                  */
/*    END.                                                                                      */
    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeEitemRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeEitemRecords Procedure
PROCEDURE purgeEitemRecords:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcCompany AS CHAR.
    DEF OUTPUT PARAMETER oplError AS LOG.
    DEF OUTPUT PARAMETER opcMessage AS CHAR.
    
    DEF VAR cTempDir AS CHAR NO-UNDO.
    DEF VAR cPurgeDir AS CHAR NO-UNDO.
    DEF VAR cPurgeFile AS CHAR NO-UNDO.
    DEF VAR lDirCreated AS LOG NO-UNDO.
    DEF VAR cDirMsg AS CHAR NO-UNDO.
        
    RUN FileSys_GetTempDirectory (OUTPUT cTempDir).
    ASSIGN 
        cPurgeDir = cTempDir + "\eItemPurge" + STRING(YEAR(TODAY),"9999") +
                                               STRING(MONTH(TODAY),"99") + 
                                               STRING(DAY(TODAY),"99") +
                                               STRING(TIME,"99999").
    RUN FileSys_CreateDirectory (INPUT cPurgeDir,
                                 OUTPUT lDirCreated,
                                 OUTPUT cDirMsg).
    IF NOT lDirCreated THEN DO:
        ASSIGN 
            oplError = TRUE
            opcMessage = cDirMsg.
        RETURN.
    END.
    
    ASSIGN 
        opcMessage = cPurgeDir.
    
&SCOPED-DEFINE cFileToPurge e-item
    OUTPUT TO VALUE(cPurgeDir + "\e-item.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
&SCOPED-DEFINE cFileToPurge e-item-vend
    OUTPUT TO VALUE(cPurgeDir + "\eitmvend.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
&SCOPED-DEFINE cFileToPurge e-itemfg
    OUTPUT TO VALUE(cPurgeDir + "\e-itemfg.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
&SCOPED-DEFINE cFileToPurge e-itemfg-vend
    OUTPUT TO VALUE(cPurgeDir + "\e-temfv.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
&SCOPEd-DEFINE cFileToPurge e-item-cust
    OUTPUT TO VALUE(cPurgeDir + "\e-item-c.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeGLhistFromFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeGLhistFromFile Procedure
PROCEDURE purgeGLhistFromFile:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR.
    DEF INPUT PARAMETER ipcFileNameExt AS CHAR.
    DEF INPUT PARAMETER ipcCompany AS CHAR.
    DEF OUTPUT PARAMETER oplError AS LOG.
    DEF OUTPUT PARAMETER opcMessage AS CHAR.
    
    DEF VAR cHeaderList AS CHAR NO-UNDO.
    DEF VAR iTableCol AS INT NO-UNDO.
    DEF VAR iRowidCol AS INT NO-UNDO.
    DEF VAR cRawRow AS CHAR NO-UNDO.
    DEF VAR cTableList AS CHAR NO-UNDO.
    DEF VAR deSummaryAmount AS DECIMAL NO-UNDO.
    
    DEF BUFFER bGlhist FOR glhist.
    DISABLE TRIGGERS FOR LOAD OF glhist.
    
    ASSIGN 
        cOutputDir = REPLACE(ipcFileName,"\" + ipcFileNameExt ,"").
    
    IF SEARCH (ipcFileName) EQ ? THEN 
    DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate specified file name".
        RETURN.
    END.
    ELSE IF SEARCH (cOutputDir + "\" + ipcFileName + ".d") NE ? THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Records already deleted for table " + ipcFileName + " in this purge.".
        RETURN.
    END.
    ELSE DO:
        OUTPUT STREAM sDump TO VALUE (cOutputDir + "\glhist.d").
                   
        FOR EACH ttGLHistList 
            BREAK BY ttGLHistList.iYear
            BY ttGLHistList.iPeriod
            BY ttGLHistList.cAccount:
            /* Only on first by break */
            IF FIRST-OF(ttGLHistList.cAccount) 
            OR FIRST-OF(ttGLHistList.iPeriod)
            OR FIRST-OF(ttGLHistList.iYear) THEN ASSIGN 
                deSummaryAmount = 0.
            /* All records in group */
            ASSIGN 
                deSummaryAmount = deSummaryAmount + ttGLHistList.deAmount.
            FIND FIRST period NO-LOCK WHERE 
                period.company EQ ipcCompany AND
                period.pst LE ttGLHistList.daTxnDate AND 
                period.pend GE ttGLHistList.daTxnDate
                NO-ERROR.
            FIND FIRST bglHist EXCLUSIVE WHERE 
                ROWID(bglHist) EQ ttGLHistList.rRowid
                NO-ERROR.
            IF AVAIL bglHist THEN DO:
                EXPORT STREAM sDump bglHist.
                DELETE bglHist.
            END.
            /* Only on last of group */
            /* When creating the balance fwd txn, it's now safe to assign year/prd 
               because we created any missing ones during the test phase */
            IF LAST-OF(ttGLHistList.cAccount) THEN DO:
                CREATE bglHist.
                ASSIGN 
                    bglhist.actnum      = ttGLHistList.cAccount     
                    bglhist.company     = ipcCompany
                    bglhist.createdBy   = USERID(LDBNAME(1))
                    bglhist.createdDate = TODAY
                    bglhist.curr-code[1]= ttGLHistList.cCurrency
                    bglhist.documentID  = ""
                    bglhist.entryType   = "B"
                    bglhist.ex-rate     = 1
                    bglhist.glYear      = IF ttGLHistList.iYear NE 0 THEN ttGLHistList.iYear ELSE 
                                          IF AVAIL period THEN period.yr ELSE YEAR(ttGLHistList.daTxnDate)
                    bglhist.jrnl        = ttGLHistList.cJournal
                    bglhist.module      = ""
                    bglhist.period      = ttGLHistList.iPeriod
                    bglhist.posted      = YES
                    bglhist.postedBy    = USERID(LDBNAME(1))
                    bglhist.rec_key     = STRING(YEAR(TODAY),"9999") + 
                                          STRING(MONTH(TODAY),"99") + 
                                          STRING(DAY(TODAY),"99") + 
                                          STRING(TIME,"99999") + 
                                          STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
                    bglhist.sourceDate  = ?
                    bglhist.tr-amt      = deSummaryAmount
                    bglhist.tr-date     = IF AVAIL period THEN period.pend ELSE ttGLHistList.daTxnDate
                    bglhist.tr-dscr     = "Balance Forward Total"
                    bglhist.tr-num      = INTEGER(STRING(bglhist.glYear,"9999") + STRING(ttGLHistList.iPeriod,"99"))
                    bglhist.yr          = IF ttGLHistList.iYear NE 0 THEN ttGLHistList.iYear ELSE 
                                          IF AVAIL period THEN period.yr ELSE YEAR(ttGLHistList.daTxnDate)
                    .
            END.
        END.
            
            
    END.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeJobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeJobs Procedure
PROCEDURE purgeJobs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcMode AS CHAR NO-UNDO.
    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeOrphansFromFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeOrphansFromFile Procedure
PROCEDURE purgeOrphansFromFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR.
    DEF OUTPUT PARAMETER oplError AS LOG.
    DEF OUTPUT PARAMETER opcMessage AS CHAR.
    
    DEF VAR cHeaderList AS CHAR NO-UNDO.
    DEF VAR iTableCol AS INT NO-UNDO.
    DEF VAR iRowidCol AS INT NO-UNDO.
    DEF VAR cRawRow AS CHAR NO-UNDO.
    DEF VAR cTableList AS CHAR NO-UNDO.
    
    ASSIGN 
        cOutputDir = REPLACE(ipcFileName,"\_PurgeReport.csv","").
    EMPTY TEMP-TABLE ttPurgeList.
    IF SEARCH (ipcFileName) EQ ? THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate specified file name".
        RETURN.
    END.
    ELSE IF SEARCH (cOutputDir + "\" + ipcFileName + ".d") NE ? THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Records already deleted for table " + ipcFileName + " in this purge.".
        RETURN.
    END.
    ELSE DO:
        INPUT FROM VALUE(ipcFileName).
        IMPORT UNFORMATTED cHeaderList.
        DO iCtr = 1 TO NUM-ENTRIES(cHeaderList):
            IF ENTRY(iCtr,cHeaderList) = "Table Name" THEN ASSIGN 
                iTableCol = iCtr.
            IF ENTRY(iCtr,cHeaderList) = "Rowid" THEN ASSIGN 
                iRowidCol = iCtr.
        END.
        INPUT CLOSE.
        IF iTableCol EQ 0 
        OR iRowidCol EQ 0 THEN DO:
            ASSIGN 
                oplError = TRUE 
                opcMessage = "Unable to locate table name or rowid column in specified file".
            RETURN.
        END.
        ELSE DO:
            INPUT FROM VALUE(ipcFileName).
            REPEAT:
                IMPORT UNFORMATTED cRawRow.
                IF ENTRY(1,cRawRow) BEGINS "Y" THEN DO:
                    CREATE ttPurgeList.
                    ASSIGN 
                        ttPurgeList.cTable = ENTRY(iTableCol,cRawRow)
                        ttPurgeList.rRowid = TO-ROWID(ENTRY(iRowidCol,cRawRow)).
                END.
            END.
            FOR EACH ttPurgeList BREAK BY ttPurgeList.cTable:
                IF FIRST-OF(ttPurgeList.cTable) THEN ASSIGN 
                    cTableList = cTableList + ttPurgeList.cTable + ",".
            END.
            ASSIGN 
                cTableList = TRIM(cTableList,",").
            DO iCtr = 1 TO NUM-ENTRIES(cTableList):
                RUN pDeleteRecordsByRowid (
                    ENTRY(iCtr,cTableList),
                    OUTPUT oplError,
                    OUTPUT opcMessage).
            END.
        END.
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeClearTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeClearTempTable Procedure
PROCEDURE purgeClearTempTable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
  EMPTY TEMP-TABLE ttGLHistList.
  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-testAccounts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testAccounts Procedure
PROCEDURE testAccounts:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipiYear AS INT NO-UNDO.
    DEF INPUT PARAMETER ipiPeriod AS INT NO-UNDO.
    DEF INPUT PARAMETER ipcOutputDir AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    OS-CREATE-DIR VALUE(ipcOutputDir).
    ASSIGN 
        cOutputDir = ipcOutputDir.
     
    RUN pTestGlhist (ipcCompany,
        ipiYear,
        ipiPeriod,
        OUTPUT oplError,
        OUTPUT opcMessage).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeOrphans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testOrphans Procedure
PROCEDURE testOrphans:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:  Calling syntax:
             RUN testOrphans IN hPurge (cFileName,
                                        cDateLimit,
                                        cOutputDir,
                                        cThisCompany,
                                        OUTPUT iProcessed,
                                        OUTPUT lError,
                                        OUTPUT cMessage).
     
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcDateLimit AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcOutputDir AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opiProcessedCount AS INT NO-UNDO.
    DEF OUTPUT PARAMETER opiErrorCount AS INT NO-UNDO.
    DEF OUTPUT PARAMETER opiWarningCount AS INT NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    DEF VAR daDateLimit AS DATE NO-UNDO.
    DEF VAR cFieldNameList AS CHAR NO-UNDO.
    
    ASSIGN
        cThisCompany = ipcCompany
        daDateLimit = IF ipcDateLimit NE "" THEN DATE(ipcDateLimit) ELSE TODAY
        cRecKeyPrefix = STRING(YEAR(daDateLimit),"9999") +
                        STRING(MONTH(daDateLimit),"99") +
                        STRING(DAY(daDateLimit),"99")
        iProcessedCount = 0
        iErrorCount = 0
        iWarningCount = 0.
   
    OS-CREATE-DIR VALUE(ipcOutputDir).
    ASSIGN 
        cOutputDir = ipcOutputDir.
         
    IF CAN-DO("ar-inv,ar-invl,ar-invm,inv-head,inv-line,inv-misc",ipcFileName) THEN 
        RUN pTestInvoice (ipcFileName,
                          cRecKeyPrefix,
                          OUTPUT oplError,
                          OUTPUT opcMessage).
    ELSE 
        RUN pTestOneFile (ipcFileName,
                          cRecKeyPrefix,
                          OUTPUT oplError,
                          OUTPUT opcMessage).
    
    
    ASSIGN 
        opiProcessedCount = iProcessedCount
        opiErrorCount = iErrorCount
        opiWarningCount = iWarningCount.                  

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF





/* ************************  Function Implementations ***************** */
&IF DEFINED(EXCLUDE-dynExport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dynExport Procedure
FUNCTION dynExport RETURNS CHARACTER 
  ( INPUT hRecord AS HANDLE , INPUT cDelim AS CHAR ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hFld     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCnt     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtnt   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTmp     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cArray   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLobname AS CHARACTER NO-UNDO.

    IF hRecord:TYPE <> "BUFFER" THEN
        RETURN ?.

    DO iCnt = 1 TO hRecord:NUM-FIELDS:

        ASSIGN 
            hFld = hRecord:BUFFER-FIELD(iCnt).

        /* Handle EXPORT for large objects by writing them out to .blb files. Omit this section in Progress 9 
         * EXPORT adds extra "" for output compatible with the INPUT statement.
         * Names for blobs are not guaranteed the same as the static EXPORT statement, IMPORT handles them correctly. 
        */
        IF hFld:DATA-TYPE = "clob" OR hFld:DATA-TYPE = "blob" THEN 
        DO:
            IF hFld:BUFFER-VALUE = ? THEN 
            DO:
                cResult = cResult + "?" + cDelim.
            END.
            ELSE 
            DO:
                cLobname = hFld:NAME +
                    (IF hFld:DATA-TYPE = "clob" THEN "!" + GET-CODEPAGE(hFld:BUFFER-VALUE) + "!" ELSE "") 
                    + hRecord:TABLE + "_" + STRING(hRecord:RECID) + ".blb".
                COPY-LOB FROM hFld:BUFFER-VALUE TO FILE cLobname NO-CONVERT.
                cResult = cResult + QUOTER(cLobname) + cDelim.
            END.
            NEXT.
        END.
      
        IF hFld:EXTENT = 0 THEN 
        DO:
            IF hFld:BUFFER-VALUE EQ ? THEN ASSIGN 
                cTmp = "?".
            ELSE 
         
                CASE hFld:DATA-TYPE:
                    WHEN "character" THEN 
                        cTmp = QUOTER(hFld:BUFFER-VALUE).
                    WHEN "raw"  THEN 
                        cTmp = '"' + STRING(hFld:BUFFER-VALUE) + '"'.
                    WHEN "datetime" OR 
                    WHEN "datetime-tz" THEN ASSIGN 
                        cTmp = STRING(YEAR(hFld:BUFFER-VALUE),"9999") 
                            + "-" + string(MONTH(hFld:BUFFER-VALUE),"99") 
                            + "-" + string(DAY(hFld:BUFFER-VALUE),"99") 
                            + "T" + substring(STRING(hFld:BUFFER-VALUE),12).
                    OTHERWISE  
                    cTmp = STRING(hFld:BUFFER-VALUE).
                END CASE.
         
            cResult = cResult + cTmp + cDelim.
        END.
        ELSE 
        DO:
            cArray = "".   
            DO iExtnt = 1 TO hFld:EXTENT:
                IF hFld:BUFFER-VALUE(iExtnt) EQ  ? THEN ASSIGN 
                    cTmp = "?".
                ELSE

                    CASE hFld:DATA-TYPE:
                        WHEN "character" THEN 
                            cTmp = QUOTER(hFld:BUFFER-VALUE(iExtnt)).
                        WHEN "raw"          THEN 
                            cTmp = '"' + STRING(hFld:BUFFER-VALUE(iExtnt)) + '"'.
                        WHEN "datetime" OR 
                        WHEN "datetime-tz" THEN 
                            cTmp = STRING(YEAR(hFld:BUFFER-VALUE(iExtnt)),"9999") 
                                + "-" + string(MONTH(hFld:BUFFER-VALUE(iExtnt)),"99") 
                                + "-" + string(DAY(hFld:BUFFER-VALUE(iExtnt)),"99") 
                                + "T" + substring(STRING(hFld:BUFFER-VALUE(iExtnt)),12).
                        OTHERWISE  
                        cTmp = STRING(hFld:BUFFER-VALUE(iExtnt)).
                    END CASE.

                cArray = cArray + cTmp + cDelim.
            END.
            cResult = cResult + RIGHT-TRIM(cArray,cDelim) + cDelim.
        END.
    END.
    RETURN RIGHT-TRIM(cResult,cDelim).
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

