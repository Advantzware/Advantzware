
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR cocode AS CHAR NO-UNDO.

FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

DEF TEMP-TABLE tt-oe-rel NO-UNDO
    FIELD ROWID AS ROWID.

IF AVAIL oe-ordl THEN DO:
  cocode = oe-ordl.company.
DEF VAR lv-qty AS INT NO-UNDO.
DEF VAR v-nxt-r-no AS INT NO-UNDO.
DEF VAR v-q-back LIKE itemfg.q-back NO-UNDO.
DEF BUFFER b-oe-rell FOR oe-rell.


  {sys/ref/relpost.i}

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.link-no GT 0
        AND NOT CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rel.company
                           AND oe-boll.r-no     EQ oe-rel.link-no
                           AND oe-boll.ord-no   EQ oe-rel.ord-no
                           AND oe-boll.i-no     EQ oe-rel.i-no
                           AND oe-boll.line     EQ oe-rel.line
                           AND oe-boll.rel-no   EQ oe-rel.rel-no
                           AND oe-boll.b-ord-no EQ oe-rel.b-ord-no
                           AND oe-boll.po-no    EQ oe-rel.po-no
                         USE-INDEX ord-no)
      USE-INDEX ord-item
      /*TRANSACTION*/ :

    IF relpost-chr NE "Nothing"     OR
       NOT CAN-FIND(FIRST oe-rell  
                    WHERE oe-rell.company  EQ oe-rel.company
                      AND oe-rell.r-no     EQ oe-rel.link-no
                      AND oe-rell.ord-no   EQ oe-rel.ord-no
                      AND oe-rell.i-no     EQ oe-rel.i-no
                      AND oe-rell.line     EQ oe-rel.line
                      AND oe-rell.rel-no   EQ oe-rel.rel-no
                      AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                      AND oe-rell.po-no    EQ oe-rel.po-no
                    USE-INDEX r-no) OR
       NOT CAN-FIND(FIRST oe-rell
                    WHERE oe-rell.company  EQ oe-rel.company
                      AND oe-rell.r-no     EQ oe-rel.link-no
                      AND oe-rell.link-no  EQ oe-rel.r-no
                    USE-INDEX r-no) THEN
    DO:
       CREATE tt-oe-rel.
       tt-oe-rel.ROWID = ROWID(oe-rel).
       RELEASE tt-oe-rel.
      /* DELETE oe-rel. */
    END.

    ELSE
    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-rel.company
          AND oe-rell.r-no    EQ oe-rel.link-no
          AND CAN-FIND(FIRST oe-boll
                       WHERE oe-boll.company  EQ oe-rell.company
                         AND oe-boll.ord-no   EQ oe-rell.ord-no
                         AND oe-boll.i-no     EQ oe-rell.i-no
                         AND oe-boll.line     EQ oe-rell.line
                         AND oe-boll.r-no     EQ oe-rell.r-no
                         AND oe-boll.rel-no   EQ oe-rell.rel-no
                         AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                         AND oe-boll.po-no    EQ oe-rell.po-no
                       USE-INDEX ord-no)
        NO-LOCK:
      
       CREATE tt-oe-rel.
       tt-oe-rel.ROWID = ROWID(oe-rel).
       RELEASE tt-oe-rel.
       /* DELETE oe-rel. */
      
      LEAVE.
    END.
  END.

  FOR EACH tt-oe-rel,
      FIRST oe-rel WHERE
            ROWID(oe-rel) EQ tt-oe-rel.ROWID
      TRANSACTION:
      FIND FIRST itemfg-loc 
          WHERE itemfg-loc.company EQ oe-rel.company
            AND itemfg-loc.i-no    EQ oe-rel.i-no
            AND itemfg-loc.loc     EQ oe-rel.spare-char-1
          EXCLUSIVE-LOCK NO-ERROR.      
      IF AVAIL itemfg-loc THEN DO:
          FIND itemfg WHERE itemfg.company EQ oe-rel.company
               AND itemfg.i-no EQ oe-rel.i-no
               NO-LOCK NO-ERROR.
          RUN fg/calcqabl.p (ROWID(itemfg), 
                             itemfg-loc.loc, 
                             OUTPUT itemfg-loc.q-alloc, 
                         OUTPUT v-q-back). 

          IF itemfg-loc.q-alloc LT 0 THEN
              itemfg-loc.q-alloc = 0.
          itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
          FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
      END.

            
      DELETE oe-rel.
  END.

  EMPTY TEMP-TABLE tt-oe-rel.

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.rel-no  GT 0
      USE-INDEX ord-item
      
      BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.link-no DESC

      /*TRANSACTION*/ :
    
    IF NOT FIRST-OF(oe-rel.b-ord-no) AND oe-rel.link-no EQ 0 THEN
    DO:
       CREATE tt-oe-rel.
       tt-oe-rel.ROWID = ROWID(oe-rel).
       RELEASE tt-oe-rel.
       /* DELETE oe-rel. */
    END.
  END.

  FOR EACH tt-oe-rel,
      FIRST oe-rel WHERE
            ROWID(oe-rel) EQ tt-oe-rel.ROWID
            TRANSACTION:
      FIND FIRST itemfg-loc 
          WHERE itemfg-loc.company EQ oe-rel.company
            AND itemfg-loc.i-no    EQ oe-rel.i-no
            AND itemfg-loc.loc     EQ oe-rel.spare-char-1
          EXCLUSIVE-LOCK NO-ERROR.      
      IF AVAIL itemfg-loc THEN DO:
          FIND itemfg WHERE itemfg.company EQ oe-rel.company
               AND itemfg.i-no EQ oe-rel.i-no
               NO-LOCK NO-ERROR.
          RUN fg/calcqabl.p (ROWID(itemfg), 
                             itemfg-loc.loc, 
                             OUTPUT itemfg-loc.q-alloc, 
                         OUTPUT v-q-back). 

          IF itemfg-loc.q-alloc LT 0 THEN
              itemfg-loc.q-alloc = 0.
          itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
          FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
      END.
          
          
      DELETE oe-rel.
  END.

  EMPTY TEMP-TABLE tt-oe-rel.

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item
      
      BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.po-no

      /*TRANSACTION*/ :
  
    IF CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) OR
       (NOT LAST-OF(oe-rel.po-no) AND oe-rel.rel-no NE 0)               THEN
       DO:
          CREATE tt-oe-rel.
          tt-oe-rel.ROWID = ROWID(oe-rel).
          RELEASE tt-oe-rel.
          /* DELETE oe-rel. */
       END.
  END.
  
  FOR EACH tt-oe-rel,
      FIRST oe-rel WHERE
            ROWID(oe-rel) EQ tt-oe-rel.ROWID
            TRANSACTION:
      FIND FIRST itemfg-loc 
          WHERE itemfg-loc.company EQ oe-rel.company
            AND itemfg-loc.i-no    EQ oe-rel.i-no
            AND itemfg-loc.loc     EQ oe-rel.spare-char-1
          EXCLUSIVE-LOCK NO-ERROR.     
      IF AVAIL itemfg-loc THEN DO:
          FIND itemfg WHERE itemfg.company EQ oe-rel.company
               AND itemfg.i-no EQ oe-rel.i-no
               NO-LOCK NO-ERROR.
          RUN fg/calcqabl.p (ROWID(itemfg), 
                             itemfg-loc.loc, 
                             OUTPUT itemfg-loc.q-alloc, 
                             OUTPUT v-q-back). 

          IF itemfg-loc.q-alloc LT 0 THEN
              itemfg-loc.q-alloc = 0.
          itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
          FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
      END.

      DELETE oe-rel.
  END.

  /* Check for missing oe-rel records */
  
  FOR EACH oe-rell
      WHERE oe-rell.company  EQ cocode
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line
/*  oe-rel record should exist, even if oe-boll exists                 */
/*         AND NOT CAN-FIND(FIRST oe-boll                              */
/*                          WHERE oe-boll.company  EQ oe-rell.company  */
/*                            AND oe-boll.r-no     EQ oe-rell.r-no     */
/*                            AND oe-boll.ord-no   EQ oe-rell.ord-no   */
/*                            AND oe-boll.i-no     EQ oe-rell.i-no     */
/*                            AND oe-boll.line     EQ oe-rell.line     */
/*                            AND oe-boll.rel-no   EQ oe-rell.rel-no   */
/*                            AND oe-boll.b-ord-no EQ oe-rell.b-ord-no */
/*                            AND oe-boll.po-no    EQ oe-rell.po-no    */
/*                          USE-INDEX ord-no)                          */
      USE-INDEX ord-no EXCLUSIVE-LOCK,

      FIRST oe-relh NO-LOCK
      WHERE oe-relh.r-no    EQ oe-rell.r-no
    /*    AND (oe-relh.posted EQ NO OR relpost-chr EQ "Nothing") */
      
      BREAK BY oe-rell.r-no
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no

      TRANSACTION:

    IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-rell.qty.

    IF LAST-OF(oe-rell.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE b-oe-rell.
      IF oe-relh.posted THEN
      FOR EACH b-oe-rell
          WHERE b-oe-rell.company EQ oe-rell.company
            AND b-oe-rell.r-no    EQ oe-rell.r-no
            AND ROWID(b-oe-rell)  NE ROWID(oe-rell)
            AND CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ b-oe-rell.company
                           AND oe-boll.ord-no   EQ b-oe-rell.ord-no
                           AND oe-boll.i-no     EQ b-oe-rell.i-no
                           AND oe-boll.line     EQ b-oe-rell.line
                           AND oe-boll.r-no     EQ b-oe-rell.r-no
                           AND oe-boll.rel-no   EQ b-oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ b-oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ b-oe-rell.po-no
                         USE-INDEX ord-no)
          USE-INDEX r-no NO-LOCK:

        LEAVE.
      END.

      IF NOT AVAIL b-oe-rell THEN DO:
        RELEASE oe-rel.
        IF oe-rell.link-no NE 0 AND oe-relh.posted THEN
        FIND oe-rel NO-LOCK
            WHERE oe-rel.r-no EQ oe-rell.link-no
              AND oe-rel.company  EQ oe-rell.company
              AND oe-rel.link-no  EQ oe-rell.r-no
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX seq-no NO-ERROR.

        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.link-no  EQ oe-rell.r-no
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX link NO-ERROR.
        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX ord-item NO-ERROR.

        IF NOT AVAIL oe-rel THEN DO:
          /* FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR. */
          /* v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1. */
          RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).          
          CREATE oe-rel.
          ASSIGN
           oe-rel.company   = oe-relh.company
           oe-rel.r-no      = v-nxt-r-no
           oe-rel.link-no   = IF oe-relh.posted THEN oe-rell.r-no ELSE 0
           oe-rel.cust-no   = oe-relh.cust-no
           oe-rel.ord-no    = oe-rell.ord-no
           oe-rel.i-no      = oe-rell.i-no
           oe-rel.line      = oe-rell.line
           oe-rel.lot-no    = oe-rell.lot-no
           oe-rel.rel-no    = oe-rell.rel-no
           oe-rel.b-ord-no  = oe-rell.b-ord-no
           oe-rel.rel-date  = oe-relh.rel-date
           oe-rel.carrier   = oe-relh.carrier
           oe-rel.ship-no   = oe-relh.ship-no
           oe-rel.ship-id   = oe-relh.ship-id
           oe-rel.ship-i[1] = oe-relh.ship-i[1]
           oe-rel.ship-i[2] = oe-relh.ship-i[2]
           oe-rel.ship-i[3] = oe-relh.ship-i[3]
           oe-rel.ship-i[4] = oe-relh.ship-i[4]
           oe-rel.po-no     = oe-rell.po-no
           oe-rel.lot-no    = oe-rell.lot-no
           oe-rel.qty       = lv-qty.
          
          RUN CopyShipNote (oe-relh.rec_key, oe-rel.rec_key).
          RUN oe/custxship.p (oe-rel.company,
                              oe-rel.cust-no,
                              oe-rel.ship-id,
                              BUFFER shipto).

          if avail shipto then
            assign
             oe-rel.ship-addr[1] = shipto.ship-addr[1]
             oe-rel.ship-addr[2] = shipto.ship-addr[2]
             oe-rel.ship-city    = shipto.ship-city
             oe-rel.ship-state   = shipto.ship-state
             oe-rel.ship-zip     = shipto.ship-zip
             oe-rel.spare-char-1 = shipto.loc.
          
          oe-rell.link-no        = oe-rel.r-no.
         FIND FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rell.company
                           AND oe-boll.ord-no   EQ oe-rell.ord-no
                           AND oe-boll.i-no     EQ oe-rell.i-no
                           AND oe-boll.line     EQ oe-rell.line
                           AND oe-boll.r-no     EQ oe-rell.r-no
                           AND oe-boll.rel-no   EQ oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ oe-rell.po-no
                           
                         USE-INDEX ord-no NO-LOCK NO-ERROR.
          /* Make sure lot # is updated in reftable entry */
          IF AVAIL oe-boll THEN
          RUN set-lot-from-boll (INPUT ROWID(oe-rel), INPUT ROWID(oe-rell),
                                 INPUT ROWID(oe-boll)).
          ELSE
          RUN set-lot-from-boll (INPUT ROWID(oe-rel), INPUT ROWID(oe-rell),
                                 INPUT ?).
          
        END. /* not avail oe-rel */
      END. /* not avail b-oe-rell */
    END. /* last po */
  END. /* each oe-rell */
END.
RUN fg/fgitmloc.p (INPUT oe-ordl.i-no, INPUT ROWID(oe-ordl)).

PROCEDURE set-lot-from-boll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Special case where oe-boll exists with a lot-no
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-rel-id AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-rell-id AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-boll-id AS ROWID NO-UNDO.
DEF BUFFER bf-oe-rel FOR oe-rel.
DEF BUFFER bf-oe-rell FOR oe-rell.
DEF BUFFER bf-oe-boll FOR oe-boll.
DEF BUFFER bf-ref FOR reftable.
FIND bf-oe-rel WHERE ROWID(bf-oe-rel) = ipr-rel-id EXCLUSIVE-LOCK.
FIND bf-oe-rell WHERE ROWID(bf-oe-rell) = ipr-rell-id EXCLUSIVE-LOCK.
IF ipr-boll-id NE ? THEN
    FIND bf-oe-boll WHERE ROWID(bf-oe-boll) = ipr-boll-id EXCLUSIVE-LOCK.
IF NOT AVAIL bf-oe-boll THEN
  FIND FIRST bf-oe-boll WHERE bf-oe-boll.company  EQ oe-rell.company
                     AND bf-oe-boll.r-no     EQ oe-rell.r-no
                     AND bf-oe-boll.ord-no   EQ oe-rell.ord-no
                     AND bf-oe-boll.i-no     EQ oe-rell.i-no
                     AND bf-oe-boll.line     EQ oe-rell.line
                   NO-LOCK NO-ERROR.


 IF AVAIL bf-oe-boll THEN
     bf-oe-rel.lot-no  = bf-oe-boll.lot-no.
  ELSE
     bf-oe-rel.lot-no  = bf-oe-rell.lot-no.

  IF bf-oe-rel.lot-no <> "" AND AVAIL(bf-oe-boll) THEN DO:
    IF bf-oe-rel.lot-no EQ "" AND bf-oe-boll.lot-no NE "" THEN
       bf-oe-rel.lot-no = bf-oe-boll.lot-no.
    IF bf-oe-rel.lot-no = "" AND bf-oe-boll.lot-no NE "" THEN
      bf-oe-rel.lot-no = bf-oe-boll.lot-no.
  END.
END PROCEDURE.

PROCEDURE CopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Copies Ship Note from rec_key to rec_key
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.
    