/* -------------------------------------------------- oe/oe-relbo.i 11/96 JLF */
/* Check for and Create Release Backorder                                     */
/* -------------------------------------------------------------------------- */

DEF VAR ll-same-s-code AS LOG NO-UNDO.
DEF VAR v-date AS DATE NO-UNDO.
DEF VAR v-nxt-r-no AS INT NO-UNDO.
DEF VAR iocPrompt AS CHAR NO-UNDO.
DEF VAR vrRelh AS ROWID NO-UNDO.
IF FIRST-OF({1}) THEN
  ASSIGN
   v-bol-qty  = 0
   v-complete = NO.
            
ASSIGN
   v-bol-qty = v-bol-qty + oe-boll.qty
   v-new-lot-code = ""
   v-new-frt-pay = ""
   v-new-fob-code = ""
   v-new-sell-price = 0
   v-new-zero-price = NO.

IF oe-boll.p-c AND oe-boll.s-code NE "I" AND 
    ( (BOLTransfer-log AND oe-boll.s-code <> "T") OR NOT BOLTransfer-log)
     THEN v-complete = YES.

IF LAST-OF({1}) AND NOT v-complete THEN
DO bo-try = 1 TO 2:
  v-rel-qty = 0.
  
  IF bo-try EQ 1 THEN DO:
    FOR EACH oe-rell
        WHERE oe-rell.company  EQ cocode
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.line     EQ oe-boll.line
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
          AND oe-rell.s-code   NE "I"
          AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
        NO-LOCK:
        
      FIND oe-rel NO-LOCK
          WHERE oe-rel.r-no EQ oe-rell.link-no
            AND oe-rel.ord-no EQ oe-rell.ord-no
          USE-INDEX seq-no NO-ERROR.

      IF AVAIL oe-rel AND oe-rel.tot-qty NE 0 THEN v-rel-qty = oe-rel.tot-qty.
          
      ELSE v-rel-qty = v-rel-qty + oe-rell.qty.
    
      IF oe-rell.b-ord-no NE 0 THEN DO:
               
           FIND b-itemfg WHERE
                ROWID(b-itemfg) EQ ROWID(itemfg)
                EXCLUSIVE-LOCK NO-ERROR.
          
           IF AVAIL b-itemfg THEN
           DO:
              b-itemfg.q-back = b-itemfg.q-back - oe-rell.qty.
              RUN fg/chkfgloc.p (INPUT oe-rell.i-no, INPUT oe-rell.loc).
              FIND FIRST itemfg-loc 
                  WHERE itemfg-loc.company EQ cocode
                    AND itemfg-loc.i-no    EQ oe-rell.i-no
                    AND itemfg-loc.loc     EQ oe-rell.loc
                  EXCLUSIVE-LOCK NO-ERROR.

              IF AVAIL itemfg-loc THEN
                itemfg-loc.q-back = itemfg-loc.q-back - oe-rell.qty.

              IF b-itemfg.q-back LT 0 THEN DO: 
                  b-itemfg.q-back = 0.
                  IF AVAIL(itemfg-loc) THEN
                    itemfg-loc.q-back = 0.
              END.            
              
              FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
              FIND CURRENT b-itemfg NO-LOCK NO-ERROR.
              
           END. /* If avail itemfg */        
      END. /* If b-ord ne 0 */
    END. /* Each oe-rell */
  
    v-rel-qty = v-rel-qty - v-bol-qty.
  END.
  
  ELSE
  IF oe-boll.s-code EQ "I"                                                AND
     NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN DO:
    /*IF oe-boll.b-ord-no NE 0 THEN*/
      v-rel-qty = oe-ordl.inv-qty - oe-ordl.ship-qty.

    IF v-rel-qty GT 0 THEN
    FOR EACH oe-rell FIELDS(qty)
        WHERE oe-rell.company  EQ cocode
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.line     EQ oe-boll.line
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no GE oe-boll.b-ord-no
          AND oe-rell.s-code   NE "I"
          AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no   EQ oe-rell.r-no
                                       AND oe-relh.posted EQ NO)
        USE-INDEX ord-no NO-LOCK:
      
      v-rel-qty = v-rel-qty - oe-rell.qty.
    END.

    ELSE
    FOR EACH oe-rell FIELDS(qty)
        WHERE oe-rell.company  EQ cocode
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.line     EQ oe-boll.line
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no GE oe-boll.b-ord-no
          AND oe-rell.s-code   NE "S"
          AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no   EQ oe-rell.r-no
                                       AND oe-relh.posted EQ NO)
        USE-INDEX ord-no NO-LOCK:
      
      v-rel-qty = v-rel-qty + oe-rell.qty.
    END.
  END.
  
  ELSE
  IF (oe-boll.s-code EQ "S" OR (oe-boll.s-code EQ "T" AND BOLTransfer-log) ) 
      AND NOT oe-ordl.is-a-component THEN DO:
    /*IF oe-boll.b-ord-no NE 0 THEN*/
      v-rel-qty = oe-ordl.ship-qty - oe-ordl.inv-qty.

    IF v-rel-qty GT 0 THEN
    FOR EACH oe-rell FIELDS(qty)
        WHERE oe-rell.company  EQ cocode
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.line     EQ oe-boll.line
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no GE oe-boll.b-ord-no
          AND oe-rell.s-code   NE "S"
          AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no   EQ oe-rell.r-no
                                       AND oe-relh.posted EQ NO)
        USE-INDEX ord-no NO-LOCK:
      
      v-rel-qty = v-rel-qty - oe-rell.qty.
    END.

    ELSE
    FOR EACH oe-rell FIELDS(qty)
        WHERE oe-rell.company  EQ cocode
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.line     EQ oe-boll.line
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no GE oe-boll.b-ord-no
          AND oe-rell.s-code   NE "I"
          AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no   EQ oe-rell.r-no
                                       AND oe-relh.posted EQ NO)
        USE-INDEX ord-no NO-LOCK:
     
      v-rel-qty = v-rel-qty + oe-rell.qty.
    END.

    IF v-rel-qty GT 0 THEN v-rel-qty = 0.
  END.
  
  IF (bo-try EQ 1 AND v-rel-qty GT 0) OR
     (bo-try NE 1 AND v-rel-qty NE 0) THEN DO:

    ll-same-s-code = oe-boll.s-code NE "B" AND v-rel-qty LT 0.

    IF v-rel-qty LT 0 THEN v-rel-qty = v-rel-qty * -1.

    FIND FIRST oe-rell
        WHERE oe-rell.company  EQ cocode
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.line     EQ oe-boll.line
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.r-no     EQ oe-boll.r-no
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
        NO-LOCK.

    FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK.
  
    IF oe-rell.link-no EQ 0 THEN DO:
      FIND FIRST oe-rel
          WHERE oe-rel.company  EQ cocode
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.ship-id  EQ oe-relh.ship-id
            AND oe-rel.link-no  EQ 0
          NO-ERROR.

      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel
          WHERE oe-rel.company  EQ cocode
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.link-no  EQ 0
          NO-ERROR.
    END.

    ELSE
    FIND FIRST oe-rel
        WHERE oe-rel.r-no EQ oe-rell.link-no
          AND oe-rel.ord-no EQ oe-rell.ord-no
        USE-INDEX seq-no NO-ERROR.

    IF AVAIL oe-rel THEN oe-rel.qty = v-bol-qty.
  
    RELEASE xoe-relh.

    v-date = IF v-boreldate-char EQ "Today" THEN TODAY ELSE oe-relh.rel-date.

    IF relpost-chr NE "BOL/REL" THEN
      FIND FIRST xoe-relh
        WHERE xoe-relh.company  EQ cocode
          AND xoe-relh.cust-no  EQ oe-relh.cust-no
          AND xoe-relh.rel-date EQ v-date
          AND xoe-relh.ship-id  EQ oe-relh.ship-id
          AND xoe-relh.carrier  EQ oe-relh.carrier
          AND xoe-relh.posted   EQ NO
          AND (xoe-relh.printed EQ NO OR relmerge-log)
          AND xoe-relh.deleted  EQ NO
          AND (relmerge-chr NE "SameOrderOnly" OR
               CAN-FIND(FIRST xoe-rell
                       WHERE xoe-rell.company EQ xoe-relh.company
                         AND xoe-rell.r-no    EQ xoe-relh.r-no
                         AND xoe-rell.ord-no  EQ oe-boll.ord-no))
          AND (relmerge-chr NE "SamePO#Only" OR
                ((CAN-FIND(FIRST sys-ctrl-shipto
                        WHERE sys-ctrl-shipto.company = xoe-relh.company
                          AND sys-ctrl-shipto.NAME = "RelMerge"
                          AND sys-ctrl-shipto.cust-vend = YES
                          AND sys-ctrl-shipto.cust-vend-no = xoe-relh.cust-no))
                  AND CAN-FIND(FIRST oe-rell WHERE xoe-rell.company EQ xoe-relh.company
                                          AND xoe-rell.r-no EQ xoe-relh.r-no
                                          AND xoe-rell.po-no EQ oe-boll.po-no)
                 AND xoe-relh.rel-date = oe-relh.rel-date
                 AND xoe-relh.ship-id = oe-relh.ship-id))
        USE-INDEX post NO-ERROR.
    IF AVAIL oe-rel AND oe-rel.rel-date = v-date THEN DO:
      RUN oe/actrelmerg.p (INPUT ROWID(oe-rel), INPUT "FINDRELH", INPUT-OUTPUT iocPrompt, OUTPUT vrRelh).
      FIND xoe-relh WHERE ROWID(xoe-relh) EQ vrRElh NO-LOCK NO-ERROR.
    END.

    IF NOT AVAIL xoe-relh THEN DO:
      FIND LAST yoe-relh USE-INDEX r-no NO-LOCK NO-ERROR.
      /* 10051225 */
      RUN oe/getNextRelNo.p (INPUT "oe-relh", OUTPUT v-nxt-r-no).
      CREATE xoe-relh.
      ASSIGN
       xoe-relh.r-no      = v-nxt-r-no
       xoe-relh.company   = cocode        
       xoe-relh.rel-date  = IF v-boreldate-char EQ "Today" THEN TODAY ELSE oe-relh.rel-date
       xoe-relh.ship-no   = oe-relh.ship-no
       xoe-relh.w-ord     = oe-relh.w-ord
       xoe-relh.cust-no   = oe-relh.cust-no
       xoe-relh.carrier   = oe-relh.carrier
       xoe-relh.printed   = NO
       xoe-relh.posted    = NO
       xoe-relh.deleted   = NO
       xoe-relh.o-no      = oe-relh.o-no
       xoe-relh.old-r-no  = oe-relh.r-no
       xoe-relh.ship-id   = oe-relh.ship-id
       xoe-relh.ship-i[1] = oe-bolh.ship-i[1]
       xoe-relh.ship-i[2] = oe-bolh.ship-i[2]
       xoe-relh.ship-i[3] = oe-bolh.ship-i[3]
       xoe-relh.ship-i[4] = oe-bolh.ship-i[4].
   
      RUN oe/release#.p (cocode, OUTPUT xoe-relh.release#).
    END.
    
    IF bo-try EQ 1 THEN
      ASSIGN
       v-rel-no   = oe-rell.rel-no
       v-b-ord-no = oe-rell.b-ord-no + 1.
       
    ELSE DO:
      RELEASE xoe-rell.
      FOR EACH xoe-rell
          WHERE xoe-rell.company EQ cocode
            AND xoe-rell.ord-no  EQ oe-rell.ord-no
          USE-INDEX ord-no NO-LOCK
          BY xoe-rell.rel-no DESC:
        LEAVE.
      END.
      ASSIGN
       v-rel-no   = (IF AVAIL xoe-rell THEN xoe-rell.rel-no ELSE 0) + 1
       v-b-ord-no = 0.
    END.

    FIND FIRST oe-rel
        WHERE oe-rel.company  EQ cocode
          AND oe-rel.ord-no   EQ oe-rell.ord-no
          AND oe-rel.i-no     EQ oe-rell.i-no
          AND oe-rel.line     EQ oe-rell.line
          AND oe-rel.ship-id  EQ oe-relh.ship-id
          AND oe-rel.rel-no   EQ 0
          AND oe-rel.b-ord-no EQ 0
          AND oe-rel.link-no  EQ 0
        NO-ERROR.
    IF NOT AVAIL oe-rel THEN
    FIND FIRST oe-rel
        WHERE oe-rel.company  EQ cocode
          AND oe-rel.ord-no   EQ oe-rell.ord-no
          AND oe-rel.i-no     EQ oe-rell.i-no
          AND oe-rel.line     EQ oe-rell.line
          AND oe-rel.rel-no   EQ 0
          AND oe-rel.b-ord-no EQ 0
          AND oe-rel.link-no  EQ 0
        NO-ERROR.
    IF AVAIL oe-rel THEN DO:
      oe-rel.qty = oe-rel.qty - v-rel-qty.
      RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).
      /*IF oe-rel.qty LE 0 THEN DO: 
          IF oe-rel.spare-dec-1 GT 0 THEN DO:
              IF oe-rel.spare-char-1 GT "" THEN
                RUN fg/chkfgloc.p (INPUT oe-rel.i-no, INPUT oe-rel.spare-char-1).
    
              FIND FIRST itemfg-loc 
                  WHERE itemfg-loc.company EQ oe-rel.company
                    AND itemfg-loc.i-no    EQ oe-rel.i-no
                    AND itemfg-loc.loc     EQ oe-rel.spare-char-1
                  EXCLUSIVE-LOCK NO-ERROR.
              IF AVAIL itemfg-loc THEN
                  itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-rel.spare-dec-1.
          END.
          DELETE oe-rel.
      END.
      */
    END.

    /** Find last release in the oe-rel file. **/
/*     FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR. */
/*     i = (IF AVAIL oe-rel THEN oe-rel.r-no ELSE 0) + 1.   */
    RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
    CREATE oe-rel.
    ASSIGN
     oe-rel.r-no      = v-nxt-r-no
     oe-rel.rel-no    = v-rel-no
     oe-rel.b-ord-no  = v-b-ord-no
     oe-rel.company   = cocode
     oe-rel.loc       = oe-rell.loc
     oe-rel.cust-no   = oe-relh.cust-no
     oe-rel.ord-no    = oe-rell.ord-no
     oe-rel.po-no     = oe-boll.po-no
     v-new-lot-code   = oe-boll.lot-no
     oe-rel.lot-no    = oe-boll.lot-no
     oe-rel.i-no      = oe-rell.i-no
     oe-rel.line      = oe-rell.line
     oe-rel.loc-bin   = oe-rell.loc-bin
     oe-rel.tag       = oe-rell.tag
     oe-rel.qty-case  = oe-rell.qty-case
     oe-rel.qty       = v-rel-qty
     oe-rel.partial   = oe-rell.partial
     oe-rel.cases     = trunc((oe-rel.qty - oe-rel.partial) / oe-rel.qty-case,0)
     oe-rel.partial   = oe-rel.qty - (oe-rel.cases * oe-rel.qty-case)
     oe-rel.rel-date  = oe-bolh.rel-date
     oe-rel.carrier   = oe-relh.carrier
     oe-rel.ship-no   = oe-relh.ship-no
     oe-rel.ship-id   = oe-relh.ship-id
     oe-rel.ship-i[1] = xoe-relh.ship-i[1]
     oe-rel.ship-i[2] = xoe-relh.ship-i[2]
     oe-rel.ship-i[3] = xoe-relh.ship-i[3]
     oe-rel.ship-i[4] = xoe-relh.ship-i[4] NO-ERROR.

    IF oe-rel.cases EQ ? THEN oe-rel.cases = 0.
    IF oe-boll.s-code = "T" THEN DO:
        oe-rel.s-code = oe-boll.s-code.        
    END.
    /** Use ship-no to find customer shipto because ship-no is the
        primary index. **/
    FIND FIRST shipto
        WHERE shipto.company EQ oe-relh.company
          AND shipto.cust-no EQ oe-relh.cust-no
          AND shipto.ship-no EQ oe-relh.ship-no
        NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
      assign
       oe-rel.ship-addr[1] = shipto.ship-addr[1]
       oe-rel.ship-addr[2] = shipto.ship-addr[2]
       oe-rel.ship-city    = shipto.ship-city
       oe-rel.ship-state   = shipto.ship-state
       oe-rel.ship-zip     = shipto.ship-zip.
         
       ASSIGN 
           v-new-frt-pay = oe-boll.frt-pay
           v-new-fob-code = oe-boll.fob-code
           .
       IF v-new-lot-code EQ '' AND oe-boll.lot-no NE '' THEN
           v-new-lot-code = oe-boll.lot-no.

   
    ASSIGN
           v-new-sell-price = oe-boll.sell-price
           v-new-zero-price  = LOGICAL(oe-boll.zeroPrice).
       
            
    ASSIGN
       oe-rel.lot-no = v-new-lot-code
       oe-rel.frt-pay = v-new-frt-pay
       oe-rel.fob-code  = v-new-fob-code.

    
    ASSIGN
       oe-rel.sell-price = v-new-sell-price
       oe-rel.zeroPrice = DECIMAL(v-new-zero-price).

    RELEASE b-reftable2.

    CREATE xoe-rell.
    ASSIGN
     xoe-rell.company  = cocode
     xoe-rell.ord-no   = oe-rel.ord-no
     xoe-rell.rel-no   = oe-rel.rel-no
     xoe-rell.b-ord-no = oe-rel.b-ord-no
     xoe-rell.loc      = oe-rel.loc
     xoe-rell.loc-bin  = oe-rel.loc-bin
     xoe-rell.qty      = oe-rel.qty
     xoe-rell.qty-case = oe-rel.qty-case
     xoe-rell.cases    = oe-rel.cases
     xoe-rell.partial  = oe-rel.partial
     xoe-rell.s-code   = oe-rell.s-code
     xoe-rell.i-no     = oe-rel.i-no
     xoe-rell.line     = oe-rel.line
     xoe-rell.po-no    = oe-rel.po-no
     xoe-rell.lot-no   = oe-rel.lot-no
     xoe-rell.r-no     = xoe-relh.r-no
     oe-rel.link-no    = xoe-rell.r-no
     xoe-rell.link-no  = oe-rel.r-no
     xoe-rell.job-no   = oe-rell.job-no
     xoe-rell.job-no2  = oe-rell.job-no2.

    IF NOT ll-same-s-code AND xoe-rell.b-ord-no EQ 0 THEN DO:
      xoe-rell.s-code  = IF oe-boll.s-code EQ "I" THEN "S" ELSE "I".
      IF xoe-rell.s-code EQ "S" THEN
        xoe-rell.cust-no = xoe-relh.cust-no.
    END.

    IF xoe-rell.b-ord-no NE 0 THEN
    DO:
      ASSIGN
       oe-ordl.t-rel-qty = oe-ordl.t-rel-qty - v-rel-qty
       oe-rel.tag        = "".

      REPEAT:
         FIND b-itemfg WHERE
              ROWID(b-itemfg) EQ ROWID(itemfg)
              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         
         IF AVAIL b-itemfg THEN
         DO:
            b-itemfg.q-back = b-itemfg.q-back + v-rel-qty.
            FIND CURRENT b-itemfg NO-LOCK NO-ERROR.
            LEAVE.
         END.
        END.
    END.

    xoe-rell.tag = oe-rel.tag.

    ASSIGN
       xoe-rell.lot-no     = v-new-lot-code
       xoe-rell.frt-pay    = v-new-frt-pay
       xoe-rell.fob-code     = v-new-fob-code.


    ASSIGN
       xoe-rell.sell-price = v-new-sell-price
       xoe-rell.zeroPrice = DECIMAL(v-new-zero-price).
  END.
END.

/* END-----------------------------------COPYRIGHT 1996 ADVANCED SOFTWARE INC.*/
