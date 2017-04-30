/* monitor.w */

{custom/monitor.w "RFID" "RFIDTAG"}

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored file, create receipt record, post
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE monitorFile AS CHARACTER NO-UNDO FORMAT 'X(50)'.
  DEFINE VARIABLE attrList    AS CHARACTER NO-UNDO FORMAT 'X(4)'.
  DEFINE VARIABLE errStatus   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE nextR-No    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE stdCost     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE saveRFID    AS CHARACTER NO-UNDO.

  INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
  REPEAT:
    SET monitorFile ^ attrList.
    IF attrList NE 'f' OR monitorFile BEGINS '.' OR
       INDEX(monitorFile,'.tmp') NE 0 THEN NEXT.
    /* pause to give RFID process time to finish write in case we grabbed it too soon */
    PAUSE 3 NO-MESSAGE.
    OS-RENAME VALUE(monitorImportDir + '/' + monitorFile)
              VALUE(monitorImportDir + '/' + monitorFile + '.tmp').
    IF SEARCH(monitorImportDir + '/processed/' + monitorFile) NE ? THEN DO:
      RUN monitorActivity ('ERROR File: ' + monitorFile + ' already processed',YES,'').
      NEXT.
    END.
    RUN monitorActivity ('RFID Data Import',YES,monitorFile).
    ASSIGN
      SUBSTR(labelLine,1)  = 'Tag ID'
      SUBSTR(labelLine,26) = 'Type'
      SUBSTR(labelLine,35) = 'From'
      SUBSTR(labelLine,44) = 'To Whs'
      SUBSTR(labelLine,53) = 'To Loc'
      SUBSTR(labelLine,62) = 'Load'
      SUBSTR(labelLine,71) = 'Date'
      SUBSTR(labelLine,82) = 'Time'
      SUBSTR(labelLine,91) = 'Status'
      .
    RUN monitorActivity (labelLine,NO,'').
    INPUT STREAM monitorStrm FROM VALUE(monitorImportDir + '/' + monitorFile + '.tmp') NO-ECHO.
    REPEAT:
      IMPORT STREAM monitorStrm DELIMITER '~t' rfidData.
      ASSIGN
        dataLine            = FILL(' ',1000)
        SUBSTR(dataLine,1)  = rfidData.rfidTag
        SUBSTR(dataLine,26) = rfidData.transType
        SUBSTR(dataLine,35) = rfidData.fromLocation
        SUBSTR(dataLine,44) = rfidData.toWarehouse
        SUBSTR(dataLine,53) = rfidData.toLocation
        SUBSTR(dataLine,62) = rfidData.loadNumber
        SUBSTR(dataLine,71) = STRING(rfidData.transDate,'99/99/9999')
        SUBSTR(dataLine,82) = rfidData.transTime
        .
      FIND FIRST rfidtag NO-LOCK
           WHERE rfidtag.company EQ g_company
             AND rfidtag.rfidtag EQ rfidData.rfidTag
           NO-ERROR.
      IF AVAILABLE rfidtag THEN DO:
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company EQ rfidtag.company
               AND loadtag.item-type EQ rfidtag.item-type
               AND loadtag.tag-no EQ rfidtag.tag-no
             NO-ERROR.
        IF AVAILABLE loadtag THEN DO:
          nextR-No = 1.
          FOR EACH fg-rctd NO-LOCK
              BY fg-rctd.r-no DESCENDING:
            LEAVE.
          END.
          IF AVAILABLE fg-rctd THEN nextR-No = fg-rctd.r-no.
          FIND LAST fg-rcpth NO-LOCK
               USE-INDEX r-no
               NO-ERROR.
          IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT nextR-No THEN
          nextR-No = fg-rcpth.r-no.
          FIND FIRST itemfg NO-LOCK
               WHERE itemfg.company EQ loadtag.company
                 AND itemfg.i-no EQ loadtag.i-no
               NO-ERROR.
          IF AVAILABLE itemfg THEN DO:
            IF rfidData.transType EQ 'Move' THEN DO:
              FIND FIRST fg-bin NO-LOCK
                   WHERE fg-bin.company EQ loadtag.company
                     AND fg-bin.i-no EQ loadtag.i-no
                     AND fg-bin.tag EQ loadtag.tag-no
                     AND fg-bin.qty GT 0
                   NO-ERROR.
              IF NOT AVAILABLE fg-bin THEN DO:
                SUBSTR(dataLine,91) = 'ERROR: FG Bin Record Does Not Exist'.
                RUN monitorActivity (dataLine,NO,'').
                NEXT.
              END. /* not avail fg-bin */
            END. /* transfer */
            DO TRANSACTION:
              CREATE fg-rctd.
              ASSIGN /* some fields in assign may be overwritten if transfer */
                fg-rctd.b-num        = loadtag.blank-no
                fg-rctd.company      = loadtag.company
                fg-rctd.cost-uom     = itemfg.prod-uom
                fg-rctd.i-name       = loadtag.i-name
                fg-rctd.i-no         = loadtag.i-no
                fg-rctd.job-no       = loadtag.job-no
                fg-rctd.job-no2      = loadtag.job-no2
                fg-rctd.loc          = loadtag.loc
                fg-rctd.loc-bin      = rfidData.fromLocation
                fg-rctd.partial      = loadtag.partial
                fg-rctd.po-no        = STRING(loadtag.po-no)
                fg-rctd.pur-uom      = itemfg.prod-uom
                fg-rctd.qty          = loadtag.qty
                fg-rctd.qty-case     = loadtag.qty-case
                fg-rctd.r-no         = nextR-No + 1
                fg-rctd.rct-date     = rfidData.transDate
                fg-rctd.rita-code    = 'R'
                fg-rctd.s-num        = loadtag.form-no
                fg-rctd.std-cost     = itemfg.std-tot-cost
                fg-rctd.tag          = loadtag.tag-no
                fg-rctd.trans-time   = INTEGER(SUBSTR(rfidData.transTime,1,2)) * 3600 +
                                       INTEGER(SUBSTR(rfidData.transTime,4,2)) * 60 +
                                       INTEGER(SUBSTR(rfidData.transTime,7,2))
                fg-rctd.cases        = IF loadtag.case-bundle GT 0 THEN loadtag.case-bundle ELSE 1
                fg-rctd.units-pallet = fg-rctd.cases
                fg-rctd.cases-unit   = fg-rctd.cases + (IF loadtag.partial NE 0 THEN 1 ELSE 0)
                .
              /* transfer specific */
              IF rfidData.transType EQ 'Move' THEN DO:
                ASSIGN
                  fg-rctd.cases        = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                  fg-rctd.cases-unit   = fg-bin.cases-unit
                  fg-rctd.cost-uom     = fg-bin.pur-uom
                  fg-rctd.loc-bin      = loadtag.loc-bin
                  fg-rctd.loc2         = IF rfidData.toWarehouse EQ "" THEN loadtag.loc
                                         ELSE rfidData.toWarehouse
                  fg-rctd.loc-bin2     = rfidData.toLocation
                  fg-rctd.partial      = fg-bin.partial-count
                  fg-rctd.pur-uom      = fg-bin.pur-uom
                  fg-rctd.rita-code    = 'T'
                  fg-rctd.std-cost     = fg-bin.std-tot-cost
                  fg-rctd.tag2         = fg-bin.tag
                  fg-rctd.units-pallet = fg-bin.units-pallet
                  .
              END. /* transfer */
              stdCost = fg-rctd.std-cost.
              IF fg-rctd.pur-uom NE 'EA' THEN
              RUN sys/ref/convcuom.p (fg-rctd.pur-uom,'EA',0,0,0,0,stdCost,OUTPUT stdCost).
              ASSIGN
                fg-rctd.t-qty    = fg-rctd.cases * fg-rctd.qty-case + fg-rctd.partial
                fg-rctd.ext-cost = fg-rctd.t-qty * stdCost
                .
              /* transfer specific */
              IF rfidData.transType EQ 'Move' THEN DO:
                IF fg-bin.qty GT fg-rctd.t-qty AND
                   fg-bin.tag NE ''            AND
                   fg-bin.tag EQ fg-rctd.tag2  THEN
                RUN fg/mkloadtg.p (ROWID(fg-rctd),0,INPUT-OUTPUT fg-rctd.tag2).
              END. /* transfer */
              RUN oerep/r-ltpost.p (ROWID(fg-rctd),YES).
              SUBSTR(dataLine,91) = 'Posted'.
              RELEASE fg-bin.
              RELEASE itemfg.
              RELEASE fg-rctd.
              RELEASE loadtag.
            END. /* do transaction */
          END. /* avail itemfg */
          ELSE SUBSTR(dataLine,91) = 'ERROR: FG Item Record Does Not Exist'.
        END. /* avail loadtag */
        ELSE SUBSTR(dataLine,91) = 'ERROR: LoadTag Record Does Not Exist'.
      END. /* avail rfidtag */
      ELSE SUBSTR(dataLine,91) = 'ERROR: RFIDTag Record Does Not Exist'.
      RUN monitorActivity (dataLine,NO,'').
    END. /* repeat */
    INPUT STREAM monitorStrm CLOSE.
    /* be sure it hasn't been previously processed */
    saveRFID = REPLACE(monitorFile + '.tmp','.tmp','').
    OS-RENAME VALUE(monitorImportDir + '/' + monitorFile + '.tmp')
              VALUE(monitorImportDir + '/processed/' + saveRFID).
    errStatus = OS-ERROR.
    IF errStatus NE 0 THEN
    RUN monitorActivity ('ERROR: Moving ' + monitorFile + '.tmp',YES,'').
  END. /* os-dir repeat */
  INPUT CLOSE.
  
END PROCEDURE.
