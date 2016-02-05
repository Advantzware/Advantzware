
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-cas-cnt LIKE eb.cas-cnt INIT 1 NO-UNDO.
DEF OUTPUT PARAM op-cases LIKE eb.cas-pal INIT 1 NO-UNDO.

{sys/inc/var.i NEW SHARED}
               .
DEF VAR v-t-win AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-li AS INT NO-UNDO.
DEF BUFFER b-item FOR ITEM.

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN DO:
  cocode = eb.company.

  FIND FIRST ef OF eb NO-LOCK NO-ERROR.

  IF AVAIL ef THEN
  FIND FIRST item
      WHERE item.company EQ ef.company
        AND item.i-no    EQ ef.board
      NO-LOCK NO-ERROR.

  {ce/msfcalc.i}

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "OECOUNT"
      NO-LOCK NO-ERROR.

  IF eb.est-type EQ 1 AND eb.cas-cnt EQ 0 AND eb.cas-wt NE 0 AND AVAIL item THEN
  DO:
     do v-li = 1 TO 4:
        find first b-item WHERE
             b-item.company EQ ef.company and
             b-item.i-no eq ef.leaf[v-li]
             no-lock no-error.
       
        if avail b-item and b-item.mat-type eq "W" and
           (ef.leaf-l[v-li] ne 0 and ef.leaf-w[v-li] ne 0) then
           DO:
              IF ef.leaf-bnum[v-li] NE 0 THEN
                 v-t-win = v-t-win + (ef.leaf-l[v-li] * ef.leaf-w[v-li]).
              ELSE
                 v-t-win = v-t-win + (ef.leaf-l[v-li] * ef.leaf-w[v-li] / eb.num-up).
           END.
     end.
  END.
  ELSE
     v-t-win = eb.t-win.

  op-cas-cnt = IF eb.cas-cnt EQ 0 AND eb.cas-wt NE 0 AND AVAIL item THEN
                 TRUNC(eb.cas-wt / (IF v-corr THEN ((eb.t-sqin - v-t-win) * .000007)
                                              ELSE ((eb.t-sqin - v-t-win) / 144000) *
                                    item.basis-w),0)
               ELSE eb.cas-cnt.

  IF op-cas-cnt EQ 0 OR op-cas-cnt EQ ? THEN op-cas-cnt = 1.
  op-cases = eb.cas-pal.

  IF AVAIL sys-ctrl AND NOT sys-ctrl.log-fld THEN
    ASSIGN op-cas-cnt = IF eb.tr-cnt NE 0 THEN eb.tr-cnt
                                   ELSE (op-cas-cnt * eb.cas-pal)
           op-cases = 1                            .

  IF op-cas-cnt EQ 0 OR op-cas-cnt EQ ? THEN op-cas-cnt = 1.
END.
