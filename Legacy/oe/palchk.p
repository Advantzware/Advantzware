/* ------------------------------------------------- oe/palchk.p    */
/* O/E CUSTOMER PALLET CHECK                                        */
/* ---------------------------------------------------------------- */
DEF INPUT PARAM ip-oe-ord-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-i-no AS CHAR NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR lv-cust AS CHAR NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR v-stat AS CHAR NO-UNDO.
DEF VAR v-pal-cnt AS INT NO-UNDO.
DEF VAR li-ship-qty AS INT NO-UNDO.
DEF VAR v-dummy AS INT NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-oe-ord FOR oe-ord.
DEF VAR OEJobHold-log AS LOG  NO-UNDO.
DEF VAR lcReturn      AS CHAR NO-UNDO.
DEF VAR llRecFound    AS LOG  NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "OEJobHold", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).

IF llRecFound THEN
    OEJobHold-log = LOGICAL(lcReturn) NO-ERROR.  
    
FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ ip-oe-ord-rowid NO-ERROR.
IF AVAIL oe-ord THEN
   ASSIGN
      lv-cust = oe-ord.cust-no
      lv-stat = oe-ord.stat.

IF lv-cust NE "" AND lv-stat NE "H" THEN
   FIND FIRST cust WHERE
        cust.company EQ cocode AND
        cust.cust-no EQ lv-cust
        NO-LOCK NO-ERROR.

IF AVAIL cust AND cust.manf-day NE 0 THEN DO:
  
   FIND FIRST itemfg WHERE
        itemfg.company EQ cocode AND
        itemfg.i-no EQ ip-i-no
        NO-LOCK NO-ERROR.

   IF itemfg.stocked THEN
   DO:
      FOR EACH b-itemfg FIELDS(i-no) WHERE
          b-itemfg.company EQ cocode AND
          b-itemfg.cust-no EQ cust.cust-no AND
          b-itemfg.stocked
          NO-LOCK,
          EACH fg-bin FIELDS(qty partial-count case-count cases-unit) WHERE
               fg-bin.company EQ cocode AND
               fg-bin.i-no    EQ b-itemfg.i-no
               NO-LOCK:
    
          IF fg-bin.cases-unit NE 0 AND fg-bin.case-count NE 0 THEN
             v-qty = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) / fg-bin.cases-unit.
          ELSE
             v-qty = 0.

          {sys/inc/roundup.i v-qty}
          v-pal-cnt = v-pal-cnt + v-qty.
      END.

      FOR EACH b-oe-ord FIELDS(ord-no) WHERE
          b-oe-ord.company EQ cocode AND
          b-oe-ord.cust-no EQ cust.cust-no AND
          b-oe-ord.opened EQ YES
          NO-LOCK,
          EACH oe-ordl FIELDS(i-no qty cases-unit cas-cnt) WHERE
               oe-ordl.company EQ cocode AND
               oe-ordl.ord-no EQ b-oe-ord.ord-no AND
               oe-ordl.opened EQ YES AND
               oe-ordl.stat NE "C"
               NO-LOCK,
          FIRST b-itemfg FIELDS(case-pall) WHERE
                b-itemfg.company EQ cocode AND
                b-itemfg.i-no EQ oe-ordl.i-no AND
                b-itemfg.stocked
                NO-LOCK:

          RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT v-dummy, OUTPUT li-ship-qty).

          v-qty = oe-ordl.qty - li-ship-qty.

          IF v-qty GT 0 THEN
          DO:
             IF oe-ordl.cases-unit NE 0 AND oe-ordl.cas-cnt NE 0 THEN
                v-qty = v-qty / oe-ordl.cas-cnt / oe-ordl.cases-unit.
             ELSE
                v-qty = 0.

             {sys/inc/roundup.i v-qty}
             v-pal-cnt = v-pal-cnt + v-qty.
          END.
      END.

      IF v-pal-cnt GT cust.manf-day THEN
         DO TRANSACTION:
                 
            MESSAGE 
               "Customer's Total Pallets for Stocked Items " +
               "Exceed Approved Pallet Positions." SKIP
               "Order Will Be Placed On Hold."
               VIEW-AS ALERT-BOX WARNING BUTTONS OK.
              
            IF AVAIL oe-ord THEN DO:
               FIND CURRENT oe-ord NO-ERROR.
               ASSIGN oe-ord.stat = "H"
                      oe-ord.approved-date = TODAY. /* Hold/Approved date */
                IF OEJobHold-log THEN 
                   RUN  oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Hold").
               FIND CURRENT oe-ord NO-LOCK NO-ERROR.
            END.
         END. /* DO TRANSACTION: */
   END.

END. /* IF AVAIL cust */
