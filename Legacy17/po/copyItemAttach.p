/* po/copyfgat.p */
/* po/copyItemAttach.p */

/* copy attachments from itemfg and create in PO */
DEF INPUT PARAMETER ipcTableModified AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcFile AS CHAR NO-UNDO.
DEF INPUT PARAMETER iprRowid AS ROWID NO-UNDO.

DEF BUFFER bf-attach FOR ATTACH.
DEF BUFFER bf-item-attach FOR ATTACH.
DEF VAR lv-from-rec_key AS CHAR NO-UNDO.
DEF VAR lv-rec_key AS CHAR NO-UNDO.
DEF VAR v-po-no AS CHAR NO-UNDO.
IF ipcTableModified EQ "PO" THEN DO:
    FIND po-ord WHERE ROWID(po-ord) EQ iprRowid 
        AND po-ord.opened = TRUE NO-LOCK NO-ERROR.
    IF NOT AVAIL po-ord THEN
        RETURN.
    lv-rec_key = po-ord.rec_key.
    RUN processPO.
END.
ELSE DO:
    FIND itemfg WHERE ROWID(itemfg) EQ iprRowid NO-LOCK NO-ERROR.
    FOR EACH po-ordl WHERE po-ordl.company EQ itemfg.company
        AND po-ordl.i-no EQ itemfg.i-no
        AND po-ordl.opened
        NO-LOCK,
        FIRST po-ord WHERE po-ord.company EQ po-ordl.company
          AND po-ord.po-no EQ po-ordl.po-no 
          AND po-ord.opened NO-LOCK:
        lv-rec_key = po-ord.rec_key.
        RUN processPO.

    END.

END.

PROCEDURE processPO:
  DEF BUFFER bf-po-ord FOR po-ord.

  FIND FIRST bf-po-ord WHERE bf-po-ord.rec_key = lv-rec_key 
      AND bf-po-ord.opened NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-po-ord THEN
      RETURN.

  /* Try via rec_key */
  FOR EACH po-ordl WHERE po-ordl.company EQ bf-po-ord.company
      AND po-ordl.po-no EQ bf-po-ord.po-no
      AND po-ordl.opened
      AND po-ordl.item-type = NO /* meaning it's an FG */
       NO-LOCK,
      FIRST itemfg WHERE itemfg.company EQ bf-po-ord.company
          AND itemfg.i-no EQ po-ordl.i-no NO-LOCK,
      EACH bf-item-attach WHERE bf-item-attach.rec_key EQ itemfg.rec_key
       AND (IF ipcFile GT "" THEN bf-item-attach.attach-file EQ ipcFile ELSE TRUE)
      NO-LOCK:

      IF itemfg.i-no GT "" THEN DO:
        v-po-no = STRING(bf-po-ord.po-no).
        lv-rec_key = bf-po-ord.rec_key.
        FIND FIRST bf-attach WHERE bf-attach.rec_key EQ lv-rec_key 
                                   AND bf-attach.attach-file EQ bf-item-attach.attach-file
                                   AND bf-attach.est-no      EQ v-po-no
                                 NO-LOCK NO-ERROR.
  
        /* Already Attached? */
        IF AVAIL bf-attach THEN
            NEXT.
  
        CREATE bf-attach.
        BUFFER-COPY bf-item-attach EXCEPT rec_key i-no TO bf-attach.
  
        ASSIGN bf-attach.rec_key = lv-rec_key
               bf-attach.company = bf-po-ord.company.
       
        ASSIGN bf-attach.creat-date = TODAY
               bf-attach.est-no = v-po-no.

      END.
  END. /* Each po-ordl */

  /* Try via item number */
  FOR EACH po-ordl WHERE po-ordl.company EQ bf-po-ord.company
      AND po-ordl.po-no EQ bf-po-ord.po-no
      AND po-ordl.opened
      AND po-ordl.item-type EQ FALSE /* item-type false is FG */
       NO-LOCK,
      FIRST itemfg WHERE itemfg.company EQ bf-po-ord.company
          AND itemfg.i-no EQ po-ordl.i-no NO-LOCK,
      EACH bf-item-attach WHERE bf-item-attach.company EQ itemfg.company
       AND bf-item-attach.i-no EQ itemfg.i-no
      NO-LOCK:

      IF itemfg.i-no GT "" THEN DO:
         v-po-no = STRING(bf-po-ord.po-no).
        lv-rec_key = bf-po-ord.rec_key.
        FIND FIRST bf-attach WHERE bf-attach.rec_key EQ lv-rec_key 
                                   AND bf-attach.attach-file = bf-item-attach.attach-file
                                   AND bf-attach.est-no      EQ v-po-no
                                 NO-LOCK NO-ERROR.
  
        /* Already Attached? */
        IF AVAIL bf-attach THEN
            NEXT.
  
        /* WFK - 07091405 - No longer linking with item */
        CREATE bf-attach.
        BUFFER-COPY bf-item-attach EXCEPT rec_key i-no TO bf-attach.
   
        ASSIGN bf-attach.rec_key = lv-rec_key
               bf-attach.company = bf-po-ord.company.
        
        ASSIGN bf-attach.creat-date = TODAY
               bf-attach.est-no = v-po-no.

      END.

  END. /* Each po-ordl */
END PROCEDURE. 
    
