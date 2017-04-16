/* -------------------------------------------------- po/closepol.p 06/04 JLF */
/* Purchase Order Close/Reopen Module - Lines                                 */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-factor AS INT NO-UNDO.
                    
{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF VAR v-cons-qty AS INT NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR lv-stat LIKE po-ordl.stat NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR ll-reopen AS LOG NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF item.
DISABLE TRIGGERS FOR LOAD OF itemfg.

IF ip-factor EQ 2 THEN
  ASSIGN
   ip-factor = 1
   ll-reopen = YES.
ELSE
  ll-reopen = NO.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

FIND FIRST po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-ERROR.
IF AVAIL po-ordl THEN DO:
  lv-stat = po-ordl.stat.

  IF po-ordl.item-type THEN DO:   /* RM Item */

    FIND FIRST item
        WHERE item.company EQ po-ordl.company
          AND item.i-no    EQ po-ordl.i-no
        USE-INDEX i-no NO-ERROR.

    IF AVAIL item THEN DO:
      IF ll-reopen THEN 
          ASSIGN po-ordl.stat = "U".
      ELSE
        IF ip-factor EQ 1 AND
           po-ordl.t-rec-qty LT
               po-ordl.cons-qty * (1 - (po-ordl.under-pct / 100)) THEN
         ASSIGN po-ordl.stat = (IF po-ordl.stat EQ "C" THEN "U" ELSE po-ordl.stat).

      /* set orderline status to "C" */
      ELSE
          ASSIGN po-ordl.stat = "C".


      IF po-ordl.stat NE lv-stat THEN DO:
        ld = po-ordl.cons-qty - po-ordl.t-rec-qty.

        IF po-ordl.cons-uom NE item.cons-uom THEN
          RUN sys/ref/convquom.p (po-ordl.cons-uom, item.cons-uom,
                                item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                ld, OUTPUT ld).

        IF ld GT 0 THEN DO:
          IF item.cons-uom EQ "EA" THEN ld = ROUND(ld,0).
          /* update item qty on order. */
          item.q-ono = item.q-ono + (ld * ip-factor).
        END.


        /* update item qty on order. */
        IF item.q-ono LT 0 THEN item.q-ono = 0.
      
        item.q-avail = item.q-onh + item.q-ono - item.q-comm.
      END.
    END. /* IF AVAIL item THEN DO: */
  END. /* IF  RM Item  */

  ELSE DO:   /* FG Item */
      
    FIND FIRST itemfg
        WHERE itemfg.company EQ po-ordl.company
          AND itemfg.i-no    EQ po-ordl.i-no
        USE-INDEX i-no NO-ERROR.
    IF AVAIL itemfg THEN DO:
      IF LOOKUP(po-ordl.cons-uom,fg-uom-list) GT 0 THEN
        v-cons-qty = po-ordl.cons-qty.
      ELSE
        RUN sys/ref/convquom.p(po-ordl.cons-uom, "EA",
                               0, po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cons-qty, OUTPUT v-cons-qty).

      IF ll-reopen THEN po-ordl.stat = "U".

      ELSE
      IF ip-factor EQ 1                                   AND
         po-ordl.t-rec-qty LT
             v-cons-qty * (1 - (po-ordl.under-pct / 100)) THEN
        po-ordl.stat = IF po-ordl.stat EQ "C" THEN "U" ELSE po-ordl.stat.

      /* Set po orderline status to "C" */
      ELSE po-ordl.stat = "C".

      IF po-ordl.stat NE lv-stat THEN DO:
        IF v-cons-qty - po-ordl.t-rec-qty GT 0 THEN DO:
          itemfg.q-ono = itemfg.q-ono + ((v-cons-qty - po-ordl.t-rec-qty) * ip-factor).
          FIND FIRST po-ord WHERE po-ord.company EQ po-ordl.company
                              AND po-ord.po-no EQ po-ordl.po-no
                            NO-LOCK NO-ERROR.
          IF AVAIL(po-ord) THEN DO:          
              RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT po-ord.loc).
              FIND FIRST itemfg-loc 
                  WHERE itemfg-loc.company EQ itemfg.company
                    AND itemfg-loc.i-no    EQ itemfg.i-no
                    AND itemfg-loc.loc     EQ po-ord.loc
                  EXCLUSIVE-LOCK NO-ERROR.
          END.
        END.
                  
        IF itemfg.q-ono LT 0 THEN DO: 
            itemfg.q-ono = 0.
            IF AVAIL(itemfg-loc) THEN
                itemfg-loc.q-ono = 0.
            FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
        END.
      END.
    END.
  END.
END.

/* end ---------------------------------- copr. 2004  Advanced Software, Inc. */
