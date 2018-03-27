/* --------------------------------------------------- po/closepo.p 12/99 JLF */
/* Purchase Order Close/Reopen Module                                         */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.                    
DEF INPUT PARAM ip-cls-rec-qty AS LOG NO-UNDO.                    

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF VAR v-factor AS INT NO-UNDO.
DEF VAR v-cons-qty AS INT NO-UNDO.
DEF VAR v-continue AS LOG INIT YES NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.

FIND FIRST po-ord WHERE ROWID(po-ord) EQ ip-rowid NO-ERROR.

IF AVAIL po-ord THEN DO:
  v-factor = IF po-ord.stat EQ "C" THEN 1 ELSE -1.

  IF v-factor EQ -1 AND ip-cls-rec-qty THEN
  DO:
     RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

     FOR EACH po-ordl WHERE 
         po-ordl.company EQ po-ord.company AND
         po-ordl.po-no EQ po-ord.po-no NO-LOCK:
         IF po-ordl.item-type THEN
         DO:
            IF po-ordl.t-rec-qty LT
               po-ordl.cons-qty * (1 - (po-ordl.under-pct / 100)) THEN
               DO:
                  v-continue = NO.
                  LEAVE.
               END.
         END.
         ELSE
         DO:
            FIND FIRST itemfg WHERE
                 itemfg.company EQ po-ordl.company AND
                 itemfg.i-no    EQ po-ordl.i-no
                 NO-LOCK NO-ERROR.

            IF AVAIL itemfg THEN DO:
               IF LOOKUP(po-ordl.cons-uom,fg-uom-list) GT 0 THEN
                  v-cons-qty = po-ordl.cons-qty.
               ELSE
                  RUN sys/ref/convquom.p(po-ordl.cons-uom, "EA",
                                         0, po-ordl.s-len, po-ordl.s-wid, 0,
                                         po-ordl.cons-qty, OUTPUT v-cons-qty).
               IF po-ordl.t-rec-qty LT
                  v-cons-qty * (1 - (po-ordl.under-pct / 100)) THEN
                  DO:
                     v-continue = NO.
                     LEAVE.
                  END.
            END.
         END.
     END.
  END.

  IF v-continue THEN
  DO:
     FOR EACH po-ordl WHERE 
         po-ordl.company EQ po-ord.company AND
         po-ordl.po-no EQ po-ord.po-no NO-LOCK:
         RUN po/closepol.p (ROWID(po-ordl), v-factor). 
     END. /* for each po-ordl */

     po-ord.stat = IF v-factor EQ 1 THEN "O" ELSE "C".
  END.
                                                 
END.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
