/* monitor.w */

{custom/monitor.w "fgXML" "fgXML"}

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored file, create receipt record, post
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE monitorFile AS CHARACTER NO-UNDO FORMAT 'X(50)'.
  DEFINE VARIABLE attrList    AS CHARACTER NO-UNDO FORMAT 'X(4)'.
  DEFINE VARIABLE errStatus   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE saveMonitor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lReturn     AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cErrorMsg   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iNextRNo    AS INTEGER   NO-UNDO.
  
  INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
  REPEAT:
      SET monitorFile ^ attrList.
      IF attrList NE 'f' OR monitorFile BEGINS '.' OR
         INDEX(monitorFile,'.xml') EQ 0 THEN NEXT.
      IF SEARCH(monitorImportDir + '/processed/' + monitorFile) NE ? THEN DO:
          RUN monitorActivity ('ERROR File: ' + monitorFile + ' already processed',YES,'').
          cFile = REPLACE(monitorFile,'.xml','.err').
          OS-RENAME VALUE(monitorImportDir + '/' + monitorFile)
                    VALUE(monitorImportDir + '/' + cFile).
          NEXT.
       END. /* if search */
       RUN monitorActivity ('fgXML Data Import',YES,monitorFile).
       ASSIGN
           labelLine = FILL(' ',1000)
           SUBSTR(labelLine,1)  = 'Job No'
           SUBSTR(labelLine,11) = 'Item No'
           SUBSTR(labelLine,26) = 'Quantity'
           SUBSTR(labelLine,37) = 'Cust No'
           SUBSTR(labelLine,46) = 'Error'
           .
      RUN monitorActivity (labelLine,NO,'').
      cFile = SEARCH(monitorImportDir + '/' + monitorFile).
      
      FIND LAST fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
      IF AVAILABLE fg-rctd AND fg-rctd.r-no GT iNextRNo THEN
      iNextRNo = fg-rctd.r-no.
      FIND LAST fg-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.
      IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iNextRNo THEN
      iNextRNo = fg-rcpth.r-no.

      DO WHILE TRUE:
          iNextRNo = iNextRNo + 1.
          FIND FIRST fg-rcpth NO-LOCK
               WHERE fg-rcpth.r-no EQ iNextRNo
               USE-INDEX r-no NO-ERROR.
        IF AVAILABLE fg-rcpth THEN NEXT.
        FIND FIRST fg-rctd NO-LOCK
             WHERE fg-rctd.r-no EQ iNextRNo
             USE-INDEX fg-rctd NO-ERROR.
        IF AVAILABLE fg-rctd THEN NEXT.
        LEAVE.
      END. /* while true */

      EMPTY TEMP-TABLE FGReceiptRow.  
      TEMP-TABLE FGReceiptRow:READ-XML ("File",cFile,"Empty",?,NO).

      FOR EACH FGReceiptRow:
          RUN pValidateFGImport (OUTPUT cErrorMsg).
          ASSIGN
              dataLine = FILL(' ',1000)
              SUBSTR(dataLine,1)  = FGReceiptRow.job-no + "-" + STRING(FGReceiptRow.job-no2)
              SUBSTR(dataLine,11) = FGReceiptRow.i-no
              SUBSTR(dataLine,26) = STRING(FGReceiptRow.qty,">>,>>>,>>9")
              SUBSTR(dataLine,37) = FGReceiptRow.cust-no
              SUBSTR(dataLine,46) = cErrorMsg
            .
          RUN monitorActivity (dataLine,NO,'').
          IF cErrorMsg NE "" THEN NEXT.

          CREATE fg-rctd.
          BUFFER-COPY FGReceiptRow TO fg-rctd.
          ASSIGN
              fg-rctd.r-no = iNextRNo
              fg-rctd.rita-code = "R"
              fg-rctd.trans-time   = TIME
              FGReceiptRow.TableRowid = rowid(fg-rctd)
              iNextRNo = iNextRNo + 1
              .
         RUN fg/invrecpt.p (ROWID(fg-rctd), 1).   
      END. /* reach fgreceiptrow */
      
      RUN fg/fgpost.p (INPUT TABLE FGReceiptRow).

      /* be sure it hasn't been previously processed */
      OS-RENAME VALUE(monitorImportDir + '/' + monitorFile)
                VALUE(monitorImportDir + '/processed/' + monitorFile).
      errStatus = OS-ERROR.
      IF errStatus NE 0 THEN
      RUN monitorActivity ('ERROR: Moving ' + monitorFile,YES,'').
  END. /* os-dir repeat */
  INPUT CLOSE.
END PROCEDURE.

PROCEDURE pValidateFGImport:
    DEFINE OUTPUT PARAMETER opcErrorMsg AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cUOMList AS CHARACTER NO-UNDO.
    
    /* validate rita-code */
    IF NOT CAN-DO("R,T",FGReceiptRow.rita-code) THEN
    opcErrorMsg = "Invalid RITA Code".
                
    /* validate tag */
    
    /* validate po-no */
                
    /* validate cost-uom */
    RUN sys/ref/uom-fg.p (NO, OUTPUT cUOMList).
    IF INDEX(cUOMList,FGReceiptRow.cost-uom) EQ 0 THEN
    opcErrorMsg = "Invalid Cost UOM".
END PROCEDURE.
