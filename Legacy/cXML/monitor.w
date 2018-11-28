/* monitor.w */

{custom/monitor.w "cXML" "cXML"}

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored files, process files, post files
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE monitorFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.
  DEFINE VARIABLE errorStatus AS INTEGER NO-UNDO.
  DEFINE VARIABLE saveMonitor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cXMLError AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cXMLFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cXMLProcessed AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cXMLResponse AS CHARACTER NO-UNDO.
  DEFINE VARIABLE returnValue AS CHARACTER NO-UNDO.

  FOR EACH cXMLDir:
    monitorImportDir = cXMLDir.cXMLDir.
    
    /* FTP to get Orders */
    IF cXMLDir.cXMLName EQ 'cXMLOrder' THEN
    RUN cXML/cXMLftp.p ('cXML/cXMLftp.dat',monitorImportDir).

    INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
    REPEAT:
      SET monitorFile ^ attrList.
      IF attrList NE 'f' OR monitorFile BEGINS '.' OR
         INDEX(monitorFile,'.xml') EQ 0 THEN NEXT.
      /* pause to give monitor process time to finish write in case we grabbed it too soon */
      PAUSE 3 NO-MESSAGE.
      ASSIGN
        cXMLFile = monitorImportDir + '/' + monitorFile
        cXMLProcessed = monitorImportDir + '/processed/' + monitorFile
        cXMLResponse = monitorImportDir + '/response/' + monitorFile
        .
      IF SEARCH(cXMLProcessed) NE ? THEN DO:
        RUN monitorActivity ('ERROR File: ' + monitorFile + ' already processed',YES,'').
        cXMLError = monitorImportDir + '/' + REPLACE(monitorFile,'.xml','.err').
        OS-RENAME VALUE(cXMLFile) VALUE(cXMLError).
        NEXT.
      END. /* if search */
      FILE-INFO:FILE-NAME = cXMLFile.
      IF FILE-INFO:FILE-SIZE EQ 0 THEN DO:
          RUN monitorActivity ('ERROR File: ' + monitorFile + ' is zero size',YES,'').
          cXMLError = monitorImportDir + '/' + REPLACE(monitorFile,'.xml','.err').
          OS-RENAME VALUE(cXMLFile) VALUE(cXMLError).
          NEXT.
      END. /*if file size is 0 */
      RUN monitorActivity ('cXML',YES,monitorFile).

      IF cXMLDir.cXMLName EQ 'cXMLOrder' THEN DO:
        RUN gencXMLOrder (cXMLFile, OUTPUT returnValue). /* generate order */
      END.
      ELSE RUN cXML/ariba.p (cXMLFile,cXMLResponse,OUTPUT returnValue). /* transmit to Ariba */

      ASSIGN
        dataLine = FILL(' ',1000)
        SUBSTR(dataLine,1) = RETURN-VALUE + ' ' + cXMLFile
        .
      RUN monitorActivity (dataLine,YES,'').

      /* be sure it hasn't been previously processed */
      IF returnValue BEGINS 'Success' THEN DO:
        OS-RENAME VALUE(cXMLFile) VALUE(cXMLProcessed).
        errorStatus = OS-ERROR.
        IF errorStatus NE 0 THEN
        RUN monitorActivity ('ERROR: Moving ' + monitorFile,YES,'').
      END. /* success */
      ELSE DO:
        RUN monitorActivity ('ERROR: ' + returnValue,YES,'').
        cXMLError = monitorImportDir + '/' + REPLACE(monitorFile,'.xml','.err').
        OS-RENAME VALUE(cXMLFile) VALUE(cXMLError).
        NEXT.        
      END.
    END. /* os-dir repeat */
    INPUT CLOSE.
  END. /* each cxmldir */
  
END PROCEDURE.

&SCOPED-DEFINE monitorActivity
{cXML/cXMLOrderProc.i}
