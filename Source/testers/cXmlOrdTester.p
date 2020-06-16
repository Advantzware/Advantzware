
/*------------------------------------------------------------------------
    File        : cXmlOrdTester.p
    Purpose     : 

    Syntax      :

    Description : Test cXml Order routines

    Author(s)   : 
    Created     : Mon Apr 22 17:05:05 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
//{methods/defines/globdefs.i}
//{methods/defines/hndldefs.i}
{sys/inc/var.i "new shared"}
FORM
    WITH FRAME xstat WIDTH 100 20 DOWN.
DEFINE VARIABLE labelLine    AS CHARACTER NO-UNDO.
DEFINE VARIABLE dataLine     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPgmSecurity AS HANDLE    NO-UNDO.
DEFINE VARIABLE g_company    AS CHARACTER NO-UNDO.
DEFINE VARIABLE g_loc        AS CHARACTER NO-UNDO.
ASSIGN
    g_company = "001"
    g_loc     = "MAIN".
MESSAGE "Enter company: " UPDATE g_company.
  
ASSIGN
    cocode = g_company
    locode = g_loc.
.
DEFINE VARIABLE monitorImportDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE importDirOverride AS CHARACTER NO-UNDO FORMAT "x(40)".
DEFINE VARIABLE hTempTableHandle AS HANDLE    NO-UNDO.
DEFINE VARIABLE hOutputProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lFirstOrder      AS LOGICAL   NO-UNDO INIT YES.
DEFINE VARIABLE iNumOrders       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cHeaderCSVFile   AS CHARACTER NO-UNDO INIT "C:\tmp\OrderHeader.csv".
DEFINE VARIABLE cDetailCSVFile   AS CHARACTER NO-UNDO INIT "C:\tmp\OrderLines.csv".

MESSAGE "Enter override folder or blank:" UPDATE importDirOverride.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fSuperRunning RETURNS LOGICAL
    (ipcInput AS CHARACTER  ) FORWARD.

{cxml\cxmlDefs.i}

/* ***************************  Main Block  *************************** */
RUN system/OutputProcs.p PERSISTENT SET hOutputProcs.
RUN postMonitor.

IF iNumOrders GT 0 THEN 
DO:
    OS-COMMAND NO-WAIT "start " VALUE(cHeaderCsvFile).
    OS-COMMAND NO-WAIT "start " VALUE(cDetailCsvFile).  
END.

DELETE OBJECT hOutputProcs.


PROCEDURE postMonitor:
    /*------------------------------------------------------------------------------
      Purpose:     import montiored files, process files, post files
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE monitorFile   AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE attrList      AS CHARACTER FORMAT 'X(4)' NO-UNDO.
    DEFINE VARIABLE errorStatus   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE saveMonitor   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReturn       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cXMLError     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLFile      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLProcessed AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLResponse  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE returnValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.

    FOR EACH cXMLDir:
        monitorImportDir = cXMLDir.cXMLDir.
        IF importDirOverride GT "" THEN 
          monitorImportDir = importDirOverride.

        INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
        REPEAT:
            SET monitorFile ^ attrList.
            IF attrList NE 'f' OR monitorFile BEGINS '.' OR
         INDEX(monitorFile,'.xml') EQ 0 THEN NEXT.
            /* pause to give monitor process time to finish write in case we grabbed it too soon */
            PAUSE 3 NO-MESSAGE.
            ASSIGN
                cXMLFile      = monitorImportDir + '/' + monitorFile
                cXMLProcessed = monitorImportDir + '/processed/' + monitorFile
                cXMLResponse  = monitorImportDir + '/response/' + monitorFile
                .
              
            IF SEARCH(cXMLProcessed) NE ? THEN 
            DO:
                RUN monitorActivity ('ERROR File: ' + monitorFile + ' already processed',YES,'').
                cXMLError = monitorImportDir + '/' + REPLACE(monitorFile,'.xml','.err').
                OS-RENAME VALUE(cXMLFile) VALUE(cXMLError).
                NEXT.
            END. /* if search */
            FILE-INFO:FILE-NAME = cXMLFile.
            IF FILE-INFO:FILE-SIZE EQ 0 THEN 
            DO:
                RUN monitorActivity ('ERROR File: ' + monitorFile + ' is zero size',YES,'').
                cXMLError = monitorImportDir + '/' + REPLACE(monitorFile,'.xml','.err').
                OS-RENAME VALUE(cXMLFile) VALUE(cXMLError).
                NEXT.
            END. /*if file size is 0 */
            RUN monitorActivity ('cXML',YES,monitorFile).

            IF cXMLDir.cXMLName EQ 'cXMLOrder' THEN 
            DO:
            
                RUN gencXMLOrder (cXMLFile, YES /* temptable only*/, OUTPUT returnValue). /* generate order */
                IF returnValue GT "" THEN 
                    RUN monitorActivity (returnValue,YES,'').
   
            END.
            //ELSE RUN cXML/ariba.p (cXMLFile,cXMLResponse,OUTPUT returnValue). /* transmit to Ariba */

            ASSIGN
                dataLine           = FILL(' ',1000)
                SUBSTR(dataLine,1) = RETURN-VALUE + ' ' + cXMLFile
                .
            RUN monitorActivity (dataLine,YES,'').
            FIND FIRST ttOrdHead NO-LOCK NO-ERROR.
            IF AVAILABLE ttOrdHead THEN 
            DO:
                hTempTableHandle = TEMP-TABLE ttOrdHead:HANDLE.
                RUN TempTableToCSV IN hOutputProcs (INPUT hTempTableHandle, 
                                                    INPUT cHeaderCSVFile, 
                                                    INPUT lFirstOrder,
                                                    OUTPUT lSuccess,
                                                    OUTPUT cMessage).
                hTempTableHandle = TEMP-TABLE ttOrdLines:HANDLE.
                RUN TempTableToCSV IN hOutputProcs (INPUT hTempTableHandle, 
                                                    INPUT cDetailCSVFile, 
                                                    INPUT lFirstOrder,
                                                    OUTPUT lSuccess,
                                                    OUTPUT cMessage).   
                lFirstOrder = NO.
                iNumOrders = iNumOrders + 1.             
            END.

        END. /* os-dir repeat */
        INPUT CLOSE.
    END. /* each cxmldir */
  
END PROCEDURE.

PROCEDURE monitorActivity :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipActivity AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipDateTimeStamp AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipmonitorFile AS CHARACTER NO-UNDO.

    DISPLAY ipActivity FORMAT "x(70)" WITH FRAME xstat.
    DOWN WITH FRAME xstat.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fSuperRunning RETURNS LOGICAL 
    (ipcInput AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE result AS LOGICAL NO-UNDO.

    RETURN result.


        
END FUNCTION.

{cXML/cXMLOrderProc.i}
