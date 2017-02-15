/* monitor.w */

{custom/monitor.w "RelXML" "RelXML"}

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored file, create receipt record, post
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE monitorFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.
  DEFINE VARIABLE errStatus AS INTEGER NO-UNDO.
  DEFINE VARIABLE saveMonitor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hPDS AS HANDLE NO-UNDO.
  DEFINE VARIABLE nextRNo AS INTEGER NO-UNDO.
  DEFINE VARIABLE nextRelease AS INTEGER NO-UNDO.
  DEFINE VARIABLE nextRelNo AS INTEGER NO-UNDO.

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
    RUN monitorActivity ('RelXML Data Import',YES,monitorFile).
    ASSIGN
      labelLine = FILL(' ',1000)
      SUBSTR(labelLine,1) = 'Company'
      SUBSTR(labelLine,9) = 'Customer'
      SUBSTR(labelLine,18) = 'Ship ID'
      SUBSTR(labelLine,27) = 'Ship Date'
      SUBSTR(labelLine,38) = 'B/S'
      .
    RUN monitorActivity (labelLine,NO,'').
    EMPTY TEMP-TABLE ttOERelh.
    EMPTY TEMP-TABLE ttOERell.
    ASSIGN
      cFile = SEARCH(monitorImportDir + '/' + monitorFile)
      hPDS = DATASET dsRelease:HANDLE
      lReturn = hPDS:READ-XML('File',cFile,'Empty',?,?,?,?).
    FOR EACH ttOERelh:
      ASSIGN
        dataLine = FILL(' ',1000)
        SUBSTR(dataLine,1) = ttOERelh.company
        SUBSTR(dataLine,9) = ttOERelh.cust-no
        SUBSTR(dataLine,18) = ttOERelh.ship-id
        SUBSTR(dataLine,27) = STRING(ttOERelh.ship-date,'99/99/9999')
        SUBSTR(dataLine,38) = ttOERelh.s-code
        .
      RUN monitorActivity (dataLine,NO,'').
      ASSIGN
        labelLine = FILL(' ',1000)
        SUBSTR(labelLine,9) = 'Order'
        SUBSTR(labelLine,16) = 'PO Number'
        SUBSTR(labelLine,32) = 'Item'
        SUBSTR(labelLine,48) = 'Tag'
        SUBSTR(labelLine,69) = 'RFID'
        SUBSTR(labelLine,94) = 'Quantity'
        SUBSTR(labelLine,106) = 'Cases'
        SUBSTR(labelLine,112) = 'Partial'
        .
      RUN monitorActivity (labelLine,NO,'').
      FOR EACH ttOERell:
        ASSIGN
          dataLine = FILL(' ',1000)
          SUBSTR(dataLine,9) = STRING(ttOERell.ord-no)
          SUBSTR(dataLine,16) = ttOERell.po-no
          SUBSTR(dataLine,32) = ttOERell.i-no
          SUBSTR(dataLine,48) = ttOERell.tag
          SUBSTR(dataLine,69) = ttOERell.rfid
          SUBSTR(dataLine,94) = STRING(ttOERell.qty)
          SUBSTR(dataLine,106) = STRING(ttOERell.cases)
          SUBSTR(dataLine,112) = STRING(ttOERell.partial)
          .
        RUN monitorActivity (dataLine,NO,'').
        IF NOT CAN-FIND(FIRST oe-ord NO-LOCK
                        WHERE oe-ord.company EQ ttOERell.company
                          AND oe-ord.ord-no EQ ttOERell.ord-no) THEN DO:
          ASSIGN
            dataLine = FILL(' ',1000)
            SUBSTR(dataLine,9) = 'ERROR: Order ' + STRING(ttOERell.ord-no) +
                                 ' does not exist.'.
          RUN monitorActivity (dataLine,NO,'').
          NEXT.
        END. /* can-find oe-ord */
        IF NOT CAN-FIND(FIRST cust NO-LOCK
                    WHERE cust.company EQ ttOERell.company
                      AND cust.cust-no EQ ttOERell.cust-no) THEN DO:
          ASSIGN
            dataLine = FILL(' ',1000)
            SUBSTR(dataLine,9) = 'ERROR: Customer ' + STRING(ttOERell.cust-no) +
                                 ' does not exist.'
            .
          RUN monitorActivity (dataLine,NO,'').
          NEXT.
        END. /* can-find cust */
        /*find the next oe-rel still scheduled*/
        FOR EACH oe-rel NO-LOCK
           WHERE oe-rel.company EQ ttOERelh.company
             AND oe-rel.ord-no EQ ttOERell.ord-no
             AND oe-rel.i-no EQ ttOERell.i-no
             AND LOOKUP(oe-rel.stat,'C,P,Z') EQ 0
          BY oe-rel.ship-date:
          LEAVE.
        END. /* each oe-rel */
        IF NOT AVAILABLE oe-rel THEN DO:
          ASSIGN
            dataLine = FILL(' ',1000)
            SUBSTR(dataLine,9) = 'ERROR: Release does not exist.'
            .
          RUN monitorActivity (dataLine,NO,'').
          NEXT.
        END. /* not avail */
       
        IF NOT AVAILABLE oe-relh THEN DO:

          FOR EACH oe-rell NO-LOCK
             WHERE oe-rell.company EQ ttOERelh.company
               AND oe-rell.ord-no EQ ttOERell.ord-no
             BY oe-rel.rel-no DESC:
           nextRelNo = oe-rell.rel-no.
           LEAVE.
          END. /* each oe-rell */
          nextRelNo = nextRelNo + 1.

          FIND LAST oe-relh USE-INDEX r-no NO-LOCK NO-ERROR.
          nextRNo = IF AVAILABLE oe-relh THEN oe-relh.r-no + 1 ELSE 1.

          RUN oe/release#.p (g_company,OUTPUT nextRelease).
          
          CREATE oe-relh.
          ASSIGN
            oe-relh.company = oe-rel.company
            oe-relh.r-no = nextRNo
            oe-relh.release# = nextRelease
            oe-relh.upd-date = TODAY
            oe-relh.upd-time = TIME
            oe-relh.user-id = USERID('NoSweat')
            oe-relh.cust-no = oe-rel.cust-no
            oe-relh.carrier = oe-rel.carrier
            oe-relh.rel-date = ttOERelh.ship-date
            oe-relh.ship-id = ttOERelh.ship-id
            oe-relh.po-no = ttOERell.po-no
            .
          
          RUN oe/custxship.p (oe-relh.company,
                              oe-relh.cust-no,
                              oe-relh.ship-id,
                              BUFFER shipto).
          
          IF AVAILABLE shipto THEN 
          ASSIGN
            oe-relh.ship-id = shipto.ship-id
            oe-relh.carrier = shipto.carrier
            oe-relh.ship-no = shipto.ship-no
            .
        END. /* not avail oe-relh */
        
        CREATE oe-rell.
        ASSIGN
          oe-rell.company = oe-relh.company
          oe-rell.r-no = oe-relh.r-no
          oe-rell.upd-date = oe-relh.upd-date
          oe-rell.upd-time = oe-relh.upd-time
          oe-rell.ord-no = oe-rel.ord-no
          oe-rell.po-no = oe-rel.po-no
          oe-rell.line = oe-rel.line
          oe-rell.link-no = oe-rel.r-no
          oe-rell.rel-no = nextRelNo
          oe-rell.i-no = oe-rel.i-no
          oe-rell.cases = ttOERell.cases
          oe-rell.qty = ttOERell.qty
          oe-rell.qty-case = ttOERell.qty-case
          oe-rell.partial = ttOERell.partial
          oe-rell.tag = ttOERell.tag
          oe-rell.s-code = ttOERelh.s-code
          oe-rell.printed = NO
          oe-rell.posted = NO
          oe-rell.deleted = NO
          .
        IF ttOERell.rfid NE '' THEN DO:
          FIND FIRST rfidtag NO-LOCK
               WHERE rfidtag.company EQ oe-rel.company
                 AND rfidtag.rfidtag EQ ttOERell.rfid
               NO-ERROR.
          IF AVAILABLE rfidtag THEN DO:
            FIND FIRST fg-bin NO-LOCK
                 WHERE fg-bin.company EQ rfidtag.company
                   AND fg-bin.tag EQ rfidtag.tag-no
                   AND fg-bin.i-no EQ oe-rel.i-no
                 NO-ERROR.
          END. /* avail rfidtag */
        END. /* rfid ne '' */
        /* rfid is blank */
        ELSE DO:
          FIND FIRST fg-bin NO-LOCK
               WHERE fg-bin.company EQ oe-rel.company
                 AND fg-bin.tag EQ ttOERell.tag
                 AND fg-bin.i-no EQ oe-rel.i-no
               NO-ERROR.
        END. /* else */
        IF AVAILABLE fg-bin THEN
        ASSIGN
          oe-rell.loc = fg-bin.loc
          oe-rell.loc-bin = fg-bin.loc-bin
          oe-rell.tag = fg-bin.tag
          oe-rell.job-no = fg-bin.job-no
          oe-rell.job-no2 = fg-bin.job-no2
          .
        ELSE DO:
          FIND FIRST itemfg NO-LOCK
               WHERE itemfg.company EQ oe-rel.company
                 AND itemfg.i-no EQ oe-rel.i-no
               NO-ERROR.
          IF AVAIL itemfg THEN
          ASSIGN
            oe-rell.loc = itemfg.def-loc
            oe-rell.loc-bin = itemfg.def-loc-bin
            .
        END. /* else */
        
        IF AVAILABLE oe-rel THEN DO:  
          FIND CURRENT oe-rel EXCLUSIVE.
          RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
          IF oe-rel.ship-id NE oe-relh.ship-id THEN
          oe-rel.ship-id = oe-relh.ship-id.
          IF oe-rel.ship-date NE oe-relh.rel-date THEN
          oe-rel.ship-date = oe-relh.rel-date.
          /* Update Actual Release QTY */
          ASSIGN
            oe-rel.qty = oe-rel.qty + oe-rell.qty
            oe-rel.rel-no = nextRelNo
            oe-rel.stat = 'A'
            .
          FIND CURRENT oe-rel NO-LOCK.
        END. /* trans */
      END. /* each ttoerell */
    END. /* each ttoerelh */
    RELEASE oe-relh.
    RELEASE oe-rell.

    DELETE OBJECT hPDS.
    /* be sure it hasn't been previously processed */
    OS-RENAME VALUE(monitorImportDir + '/' + monitorFile)
              VALUE(monitorImportDir + '/processed/' + monitorFile).
    errStatus = OS-ERROR.
    IF errStatus NE 0 THEN
    RUN monitorActivity ('ERROR: Moving ' + monitorFile,YES,'').
  END. /* os-dir repeat */
  INPUT CLOSE.
  
END PROCEDURE.
