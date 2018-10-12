/** Deduct actual release quantity from the planned release. **/
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR v-nxt-r-no LIKE oe-rel.r-no NO-UNDO.
DEF VAR lv-tot-qty LIKE oe-rel.tot-qty NO-UNDO.
DEF VAR lv-lot-no AS CHAR NO-UNDO.
DEF VAR lv-frt-pay AS CHAR NO-UNDO.
DEF VAR lv-fob-code AS CHAR NO-UNDO.
DEF VAR lv-sell-price AS DEC NO-UNDO.
DEF VAR lv-sell-price-found AS LOG NO-UNDO.
DEF VAR lv-zero-price AS LOG NO-UNDO.
DEF VAR lv-loc AS CHAR NO-UNDO.

FIND oe-rell WHERE ROWID(oe-rell) EQ ip-rowid NO-ERROR.

IF AVAIL oe-rell THEN
FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.

IF AVAIL oe-relh THEN DO:
  RELEASE oe-rel.

  IF oe-rell.link-no NE 0 THEN
  FIND FIRST oe-rel
      WHERE oe-rel.r-no EQ oe-rell.link-no
      EXCLUSIVE-LOCK 
      USE-INDEX seq-no NO-ERROR NO-WAIT.

  IF NOT AVAIL oe-rel THEN
  FIND FIRST oe-rel
      WHERE oe-rel.company  EQ oe-relh.company
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.ship-id  EQ oe-relh.ship-id
        AND oe-rel.rel-date LE oe-relh.rel-date
        AND oe-rel.rel-no   EQ 0
        AND oe-rel.b-ord-no EQ 0
        AND oe-rel.link-no  EQ 0
      EXCLUSIVE-LOCK
      USE-INDEX ord-item NO-ERROR NO-WAIT.

  IF NOT AVAIL oe-rel THEN
  FIND FIRST oe-rel
      WHERE oe-rel.company  EQ oe-relh.company
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.rel-date LE oe-relh.rel-date
        AND oe-rel.rel-no   EQ 0
        AND oe-rel.b-ord-no EQ 0
        AND oe-rel.link-no  EQ 0
      EXCLUSIVE-LOCK 
      USE-INDEX ord-item NO-ERROR NO-WAIT.

  IF AVAIL oe-rel THEN DO:

    ASSIGN
       oe-rel.qty = oe-rel.qty - oe-rell.qty
       lv-tot-qty = oe-rel.tot-qty.
    RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).

    IF oe-rel.qty LE 0 THEN
    DO:
       /* YSK no more delete oe-rel for this case - causing rec_key problem
       FIND FIRST reftable WHERE
            reftable.reftable EQ "oe-rel.lot-no" AND
            reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
            NO-LOCK NO-ERROR.

       IF AVAIL reftable THEN 
          ASSIGN lv-lot-no = reftable.CODE
                 lv-frt-pay = reftable.code2
                 lv-fob-code = reftable.dscr.

       FIND FIRST reftable WHERE
            reftable.reftable EQ "oe-rel.sell-price" AND
            reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
            NO-LOCK NO-ERROR.

       IF AVAIL reftable THEN
          ASSIGN
            lv-sell-price = reftable.val[1]
            lv-zero-price = LOGICAL(reftable.val[2])
            lv-sell-price-found = YES.
            
       FIND FIRST itemfg-loc 
            WHERE itemfg-loc.company EQ oe-rel.company
              AND itemfg-loc.i-no    EQ oe-rel.i-no
              AND itemfg-loc.loc     EQ oe-rel.spare-char-1
            EXCLUSIVE-LOCK NO-ERROR.      
       IF AVAIL itemfg-loc AND oe-rel.spare-dec-1 GT 0 THEN
         itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-rel.spare-dec-1.
       lv-loc = oe-rel.spare-char-1.
       
       DELETE oe-rel.*/

    END.
    ELSE DO:
      ASSIGN
       oe-rel.rel-no   = 0
       oe-rel.b-ord-no = 0
       oe-rel.link-no  = 0.

      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
    END.
  END.

  IF lv-tot-qty LE 0 THEN lv-tot-qty = oe-rell.qty.

  RELEASE oe-rel.

  FIND FIRST oe-rel
      WHERE oe-rel.link-no  EQ oe-rell.r-no
        AND oe-rel.company  EQ oe-rell.company
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
      USE-INDEX link-ord NO-ERROR.

  IF NOT AVAIL oe-rel THEN DO:
    /** Find last release in the oe-rel file, Indexed high to low.
        Create ACTUAL Release entry in the PLANNED Release file. **/
/*     10051225 */
/*     FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.      */
/*     v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1. */
    RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
    CREATE oe-rel.
    ASSIGN
     oe-rel.company   = oe-relh.company
     oe-rel.r-no      = v-nxt-r-no
     oe-rel.link-no   = oe-rell.r-no
     oe-rel.cust-no   = oe-relh.cust-no
     oe-rel.ord-no    = oe-rell.ord-no
     oe-rel.i-no      = oe-rell.i-no
     oe-rel.line      = oe-rell.line
     oe-rel.tot-qty   = lv-tot-qty
           
     /* oe-rel.loc       = oe-rell.loc
     oe-rel.loc-bin   = oe-rell.loc-bin
     oe-rel.tag       = oe-rell.tag
     oe-rel.cases     = oe-rell.cases
     oe-rel.qty-case  = oe-rell.qty-case
     oe-rel.partial   = oe-rell.partial */

     oe-rel.rel-no    = oe-rell.rel-no
     oe-rel.rel-date  = oe-relh.rel-date
     oe-rel.carrier   = oe-relh.carrier
     oe-rel.ship-no   = oe-relh.ship-no
     oe-rel.ship-id   = oe-relh.ship-id
     oe-rel.ship-i[1] = oe-relh.ship-i[1]
     oe-rel.ship-i[2] = oe-relh.ship-i[2]
     oe-rel.ship-i[3] = oe-relh.ship-i[3]
     oe-rel.ship-i[4] = oe-relh.ship-i[4]
     oe-rel.loc       = oe-rell.loc
     oe-rel.s-code    = oe-rell.s-code.
     
    RUN CopyShipNote (oe-relh.rec_key, oe-rel.rec_key).
    
    IF lv-loc GT "" THEN
        oe-rel.spare-char-1 = lv-loc.
     
        
    ASSIGN
       oe-rel.lot-no = lv-lot-no
       oe-rel.frt-pay = lv-frt-pay
       oe-rel.fob-code  = lv-fob-code.

    IF lv-sell-price-found THEN
    DO:                        
       ASSIGN
          oe-rel.sell-price = lv-sell-price
          oe-rel.zeroPrice = DECIMAL(lv-zero-price).       
    END.
  END.

  ASSIGN
   oe-rell.link-no = oe-rel.r-no
   oe-rel.po-no    = oe-rell.po-no
   oe-rel.qty      = oe-rel.qty + oe-rell.qty.  
  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).

  /** Use ship-no to find customer shipto because ship-no is the
      primary index. **/
  FIND FIRST shipto
      WHERE shipto.company EQ oe-relh.company
        AND shipto.cust-no EQ oe-relh.cust-no
        AND shipto.ship-no EQ oe-relh.ship-no
      NO-LOCK NO-ERROR.
  IF AVAIL shipto THEN
    ASSIGN
     oe-rel.ship-addr[1] = shipto.ship-addr[1]
     oe-rel.ship-addr[2] = shipto.ship-addr[2]
     oe-rel.ship-city    = shipto.ship-city
     oe-rel.ship-state   = shipto.ship-state
     oe-rel.ship-zip     = shipto.ship-zip.
    IF lv-loc NE "" THEN
      oe-rel.spare-char-1 = lv-loc.
  
  /* Recalc itemfg-loc.q-alloc */
  RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).
    IF lv-loc NE "" AND oe-rel.spare-char-1 EQ "" THEN
      oe-rel.spare-char-1 = lv-loc.
  RELEASE oe-rel NO-ERROR.
END.

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
