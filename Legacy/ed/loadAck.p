
/*------------------------------------------------------------------------
    File        : 810exception.p
    Purpose     : Import log data from RSSBus to update database 810 stat

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Mar 24 12:53:55 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iPos                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLogFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReferenceFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEdiFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPath               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInput             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogInput       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSendStatus    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMDNStatus    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogFileName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubFolderName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cReturnChar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEdiFileLine AS CHARACTER NO-UNDO.
/* Default location if NK1 not defined */
DEFINE VARIABLE cLogfolder     AS CHARACTER NO-UNDO 
    INIT "C:\Program Files\RSSBus\RSSBus Connect\data\Amazon\Logs\Sent"
    
    .
DEFINE VARIABLE cSentFolder    AS CHARACTER NO-UNDO.
/* Default Location if NK1 not defined */
cSentFolder = "\\PPW2K-NEW\asigui\Environments\Prod\CustFiles\EDIFiles\Invoices\Amazon\Sent\".


DEFINE TEMP-TABLE ttInvoiceList
    FIELD invoiceNum   AS INTEGER
    FIELD InvoiceStatus AS CHARACTER
    INDEX i1 invoiceNum.

DEFINE STREAM sEDIFile.
DEFINE STREAM sList.
DEFINE STREAM sLog.
EMPTY TEMP-TABLE ttInvoiceList.
DISABLE TRIGGERS FOR LOAD OF eddoc.
FIND FIRST edco NO-LOCK NO-ERROR.
IF NOT AVAILABLE edco OR edco.company EQ "" THEN 
  RETURN.


RUN sys/ref/nk1look.p (INPUT edco.company, "EDILogs", "DS" /* Character*/, 
    INPUT NO /* check by cust */, 
    INPUT YES /* use cust not vendor */,
    INPUT "" /* cust */, 
    INPUT "" /* ship-to*/,
    OUTPUT cReturnChar, 
    OUTPUT lRecFound).
IF lRecFound THEN 
    cLogFolder= cReturnChar  .

RUN sys/ref/nk1look.p (INPUT edco.company, "EDILogs", "C" /* Character*/, 
    INPUT NO /* check by cust */, 
    INPUT YES /* use cust not vendor */,
    INPUT "" /* cust */, 
    INPUT "" /* ship-to*/,
    OUTPUT cReturnChar, 
    OUTPUT lRecFound).
IF lRecFound THEN
    cSentFolder = cReturnChar  .

OS-COMMAND SILENT VALUE("dir /s/b " + '"' + cLogfolder + "\" + "*.filename " + '"' + " > c:\temp\list.txt" ).

/* Read through list of file name placeholder files */
INPUT stream sList from value("c:\temp\list.txt").
REPEAT:

    IMPORT STREAM sList UNFORMATTED cInput.
    iPos = R-INDEX(cInput, "\").
    cReferenceFile = SUBSTRING(cInput, iPos + 1).
    cPath    = SUBSTRING(cInput, 1, iPos - 1).
    iPos = R-INDEX(cPath, "\").
    cSubFolderName = SUBSTRING(cPath, iPos + 1). 
 
    iPos = R-INDEX(cReferenceFile, ".").
    cEdiFileName = SUBSTRING(cReferenceFile, 1, iPos - 1).
    
    OS-COMMAND SILENT VALUE("dir /s/b " + '"' + cPath + "\" + "*.log " + '"' + " > c:\temp\loglist.txt" ).
    
    /* Read through list of log under folder cPath files */
    INPUT stream sLog from c:\temp\loglist.txt.
     
    IMPORT STREAM sLog UNFORMATTED cLogFileName.
    cLogFile = cLogFileName. 
    INPUT stream sLog close.

    /* Read the current log file to determine if transmission was successful */
    IF SEARCH(cLogFileName) NE ? THEN 
    DO:
        cSendStatus = "".
        cMDNstatus = "".
        OS-DELETE value("c:\temp\810log.tmp").
        OS-COPY value(cLogFileName) value("c:\temp\810log.tmp").
        
        /* Make sure there is an EOL at the end of the file */
        OUTPUT stream sLog TO value("c:\temp\810log.tmp") APPEND.
        PUT STREAM sLog SKIP(1).
        OUTPUT stream sLog close.
        
        INPUT stream sLog from value("c:\temp\810log.tmp") /*value(cLogFileName) */.
        REPEAT:
            IMPORT STREAM sLog UNFORMATTED cLogInput.
            
            IF cLogInput BEGINS "transmission" THEN 
            DO:
                IF INDEX(clogInput, "was successful") > 0 THEN 
                    cSendStatus = "SNT".
                ELSE
                    cSendStatus = "NST".
            END.
            
            IF /* cMDNStatus EQ "" and */ INDEX(cLogInput, "MDN") GT 0 
                AND INDEX(cLogInput, "Requested") EQ 0
                AND INDEX(cLogInput, "Signature") EQ 0 
                THEN 
            DO:
                IF INDEX(clogInput, "was successfully") > 0 
                    AND INDEX(cLogInput, "MDN") GT 0 THEN
                    
                    cMDNStatus = "SNT".
                ELSE
                    cMDNStatus = "NAK".
            END.
                     
        END.  /* Repeat */
    END.  /* if logfile found */
    
    /* Read through the EDI file sent to obtain the Advantzware Invoice number and update status  */
    RUN getInvoiceNumbersInfile.
 
END. /* repeat each log listing */
INPUT stream sList close.



FOR EACH ttInvoiceList.

    FIND FIRST inv-head NO-LOCK 
        WHERE inv-head.company EQ '001'
        AND inv-head.inv-no = ttInvoiceList.invoiceNum
        NO-ERROR.
    IF AVAILABLE inv-head THEN   
        FIND FIRST edmast NO-LOCK WHERE edmast.cust EQ inv-head.cust-no NO-ERROR.
    ELSE 
        FIND FIRST ar-inv NO-LOCK
            WHERE ar-inv.company EQ '001'
            AND ar-inv.inv-no EQ ttInvoiceList.invoiceNum
            NO-ERROR.
    IF AVAILABLE ar-inv THEN
        FIND FIRST edmast NO-LOCK WHERE edmast.cust EQ ar-inv.cust-no NO-ERROR.
    IF AVAILABLE edmast THEN    
        FIND FIRST eddoc
            WHERE eddoc.setID EQ "810"
            AND eddoc.partner EQ edmast.partner
            AND eddoc.docID EQ STRING(ttInvoiceList.invoiceNum) 
            NO-ERROR.
    IF AVAILABLE eddoc THEN 
    DO:
        
        ASSIGN 
            eddoc.c-FAdate    = TODAY
            eddoc.status-flag = ttInvoiceList.invoiceStatus
            .
    END.
END. /* Each ttInvoiceList */
OUTPUT STREAM sLog CLOSE. 

PROCEDURE getInvoiceNumbersInFile:
    DEFINE VARIABLE iInvoiceNum AS INTEGER NO-UNDO.

    IF SEARCH(cSentFolder +  cEdiFileName) EQ ? THEN
      RETURN.

    INPUT stream sEDIFile from value(cSentFolder +  cEdiFileName).
    REPEAT:
        IMPORT STREAM sEDIFile UNFORMATTED cEdiFileLine.
        cEdiFileLine = TRIM(cEdiFileLine, "~~").
        cEdiFileLine = TRIM(cEdiFileLine).
        IF cEdiFileLine BEGINS "BIG" THEN 
        DO:
            
            iInvoiceNum = INTEGER(ENTRY(3, cEdiFileLine, "*")) NO-ERROR.
            
            IF NOT ERROR-STATUS:ERROR THEN 
            DO:
                CREATE ttInvoiceList.
                ASSIGN 
                    ttInvoiceList.invoiceNum    = iInvoiceNum
                    ttInvoiceList.invoiceStatus = cMdnStatus
                    .
            END. /* Not error status */
        END. /* BIG Segment */
    END. /* Repeat */
    INPUT stream sEdifile close.
END. /* Procedure */
 
  
 