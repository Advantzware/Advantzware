/* po/copyfgat.p */
/* po/copyItemAttach.p */

/* copy attachments from itemfg and create in PO */

DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiPoNo LIKE po-ordl.po-no NO-UNDO.
DEF INPUT PARAMETER ipiPoLine LIKE po-ordl.LINE NO-UNDO.
DEF INPUT PARAMETER ipcINo LIKE po-ordl.i-no NO-UNDO.

DEF BUFFER bf-attach FOR ATTACH.
DEF BUFFER bf-item-attach FOR ATTACH.
DEF BUFFER bf-po-ord FOR po-ord.
DEF VAR lv-from-rec_key AS CHAR NO-UNDO.
DEF VAR lv-rec_key AS CHAR NO-UNDO.
DEF VAR v-po-no AS CHAR NO-UNDO.
DO:
    
    RUN processPO.

END.


PROCEDURE processPO:
     FOR EACH itemfg WHERE itemfg.company EQ ipcCompany
          AND itemfg.i-no EQ ipcINo NO-LOCK,
      EACH bf-item-attach WHERE bf-item-attach.rec_key EQ itemfg.rec_key
      NO-LOCK:


      FIND FIRST bf-po-ord WHERE bf-po-ord.company EQ ipcCompany
          AND bf-po-ord.po-no EQ ipiPoNo
          NO-LOCK NO-ERROR.
      
      IF NOT AVAIL bf-po-ord THEN
          RETURN.
      lv-rec_key = bf-po-ord.rec_key.
      FIND FIRST bf-attach WHERE bf-attach.rec_key EQ lv-rec_key 
             AND bf-attach.attach-file EQ bf-item-attach.attach-file
           EXCLUSIVE-LOCK NO-ERROR.
      
      /* Deleting item off of PO, so delete the attachment */
      IF AVAIL bf-attach THEN
          DELETE bf-attach.

  END. /* Each po-ordl */
END PROCEDURE. 
    
