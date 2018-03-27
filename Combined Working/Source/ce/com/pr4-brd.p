/* ----------------------------------------------- ce/com/pr4-brd.p  7/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input  parameter v-vend-no like e-item-vend.vend-no NO-UNDO.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{ce/print4.i shared shared}

def var tmp-waste as de extent 99 NO-UNDO.
def var fr-mshts as de NO-UNDO.
def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var rm-wt  as de NO-UNDO.
def var v-dec as dec NO-UNDO.
def var v-num-out as int NO-UNDO.
def var v-num-up as int NO-UNDO.
def var qm as dec NO-UNDO.
def var v-cost like item.avg-cost NO-UNDO.
def var v-cqty as   dec NO-UNDO.
DEF VAR CALL_id AS RECID NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF VAR lv-bestvend-id AS ROWID NO-UNDO.
DEF VAR lv-save-vend LIKE v-vend-no NO-UNDO.
DEF VAR brd-spo LIKE spo.
DEF VAR brd-r-spo LIKE r-spo.
def var v-setup like e-item-vend.setup no-undo.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
DEF VAR v-vend-printed AS LOG NO-UNDO.
DEF SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.

{sys/inc/ceboard.i}

do transaction:
  {sys/inc/cerun.i F}

  {ce/msfcalc.i}
end.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

lv-save-vend = v-vend-no.

for each xef where xef.company = xest.company
               AND xef.est-no = xest.est-no
          with frame brd2 no-labels no-box width 80 down:
   run sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, output v-num-up).

   tmp-waste = 0. /* zero array */
   {sys/inc/roundup.i r-spo[xef.form-no]}
   find first item {sys/look/itemW.i} and
                     item.i-no = xef.board no-lock no-error.
   if avail item then
   find first e-item of item no-lock no-error.

   assign
    zzz     = 0
    call_id = ?. /* let's find blank that needs most # sheets for this form */
   for each xeb where xeb.company = xest.company
                  AND xeb.est-no = xest.est-no and xeb.form-no = xef.form-no:
      if xeb.yld-qty / (xeb.num-up * xef.n-out * xef.n-out-l) gt zzz then do:
        zzz = xeb.yld-qty / (xeb.num-up * xef.n-out * xef.n-out-l).
        {sys/inc/roundup.i zzz}
        call_id = recid(xeb).
      end.
   end.
   find xeb where recid(xeb) = call_id no-lock no-error.
   if not avail xeb then
   find first xeb
       where xeb.company = xest.company
         AND xeb.est-no   eq xest.est-no
         and xeb.form-no eq xef.form-no
       no-lock.
   assign
    qty                   = xeb.yld-qty
    t-shtfrm[xef.form-no] = zzz.

   IF xeb.pur-man THEN
     ASSIGN
      ld-rm = rm-rate-f
      ld-hp = hand-pct-f.
   ELSE
     ASSIGN
      ld-rm = ctrl[3]
      ld-hp = ctrl[2].

   /* board */
   if xef.board ne "" then do with frame aa no-labels no-box:
      assign
       qm                    = t-blkqty[xef.form-no] / 1000
       t-blkqty[xef.form-no] = 0.
       
      for each bf-eb FIELDS(yld-qty) where bf-eb.company = xest.company
                       AND bf-eb.est-no = xest.est-no 
                       and bf-eb.form-no = xef.form-no NO-LOCK:
         t-blkqty[xef.form-no] = t-blkqty[xef.form-no] + bf-eb.yld-qty.
      end.                      /* blkqty = total all blks this form */

      b-waste = 0.
      for each est-op where est-op.company = xest.company
                        aND est-op.est-no = xest.est-no
                        AND est-op.line > 500          and
                        est-op.s-num = xef.form-no and
                        est-op.b-num = 0 NO-LOCK:
         run sys/inc/numout.p (recid(est-op), output v-num-out).
         b-waste = b-waste + (est-op.op-waste / v-num-out).
      end.
      for each est-op where est-op.company = xest.company
                        aND est-op.est-no = xest.est-no
                        AND est-op.line > 500          and
                        est-op.s-num = xef.form-no and
                        est-op.b-num ne 0 no-lock:
         run sys/inc/numout.p (recid(est-op), output v-num-out).
         b-waste = b-waste + (est-op.op-waste / (v-num-up * v-num-out)).
      end.
      {sys/inc/roundup.i b-waste}
      
      /* msf for  job */
      assign
       v-setup  = 0
       b-qty = (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                          else (xef.gsh-len * xef.gsh-wid / 144)) *
               xef.gsh-qty / 1000

       r-spo[xef.form-no] = xef.gsh-qty - t-shtfrm[xef.form-no] - b-waste.

      {ce/pr4-brd.i}

      if b-uom eq "REM" then
        b-msh = b-msh * IF NOT((ceboard-log EQ NO OR v-vend-no EQ "") AND xef.cost-msh gt 0) then (xef.gsh-wid / 26) else 1.

      assign
       b-cost              = b-cost / xef.gsh-qty
       b-tot               = b-cost * t-shtfrm[xef.form-no]
       b-totw              = b-cost * b-waste
       r-spo$[xef.form-no] = b-cost * r-spo[xef.form-no].

      if xef.nc eq no then
        assign
         b-msh               = 0
         b-cost              = 0
         b-tot               = 0
         b-totw              = 0
         r-spo$[xef.form-no] = 0
         v-setup             = 0.
         
      /* cost per sht * #wasted shts */
      ASSIGN
      dm-tot[3] = dm-tot[3] + b-totw + v-setup
      dm-tot[5] = dm-tot[5] + b-tot + b-totw + v-setup
      /* add run spoil */
      dm-tot[5] = dm-tot[5] + r-spo$[xef.form-no]
      v-hopf = if cerunf eq "HOP" then xef.n-out else 1.

      IF v-vend-printed = NO AND v-vend-no NE "" THEN
      DO:
         FIND FIRST vend WHERE
              vend.company EQ xest.company AND
              vend.vend-no EQ v-vend-no
              NO-LOCK NO-ERROR.
         
         PUT SKIP(1) "Board Vendor: " + v-vend-no + " - "
             + (IF AVAIL vend THEN vend.NAME ELSE "") FORMAT "X(60)"
             SKIP(1).

         v-vend-printed = YES.
      END.
      IF NOT gEstSummaryOnly THEN
      display
            string(xef.form-no,">9") + "-00" format "x(5)"
            item.i-name format "x(20)" space(0)
            item.basis-w
            item.cal format ">9.999<"
            "$" + string(b-msh,">>>9.99") format "x(9)" to 50 b-uom space(0)
            v-setup when v-setup ne 0 format ">>>9.99" to 61
            (b-tot + v-setup) / qm format ">>>9.99" to 69
            b-tot + v-setup format ">>>>,>>9.99" to 80 skip
            space(6)
            "Board MR  Waste"
            b-waste * v-hopf format ">>>>9" to 50 space(0) " Sht" space(0)
            b-totw * 1 format ">>>9.99" to 61
            /* cost of waste amortized per 1000 of all blanks on this form */
            b-totw / qm format ">>>9.99" to 69
            b-totw format ">>>>,>>9.99" to 80 skip
            space(6)
            "Board RUN Waste"
            r-spo[xef.form-no] * v-hopf format ">>>>9" to 50 space(0) " Sht"
            space(0)
            /* cost of spoil amortized per 1000 of all blanks on this form */
            r-spo$[xef.form-no] / qm format ">>>>9.99" to 69
            r-spo$[xef.form-no] format ">>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
      lin-count = lin-count + 1.

      find first brd where brd.form-no = xef.form-no and
                           brd.i-no    = xef.board
                           no-error.
      if not avail brd then
      do:
         create brd.
         assign brd.form-no = xef.form-no
                brd.blank-no = 00
                brd.i-no    = xef.board
                brd.dscr    = xef.brd-dscr
                brd.basis-w = item.basis-w.
      end.

      ASSIGN
      brd.qty = t-shtfrm[xef.form-no] + (b-waste + r-spo[xef.form-no]) * v-hopf
      brd.qty-mr = b-waste * v-hopf
      brd.qty-wst = r-spo[xef.form-no] * v-hopf
      brd.qty-uom = "EA"
      brd.sc-uom = b-uom
      brd.cost = b-msh + (v-setup / b-qty)
      brd.cost-m = (b-tot + v-setup + b-totw + r-spo$[xef.form-no] ) / qm
/*       brd.cost-m = b-cost +                                                       */
/*                    ((v-setup +                                                    */
/*                     (b-cost * ((b-waste + r-spo[xef.form-no]) *                   */
/*                                                  v-num-up * v-out / 1000))) / qm) */
      brd.len = xef.gsh-len
      brd.wid = xef.gsh-wid
      brd.amount = b-cost /*b-totw + r-spo$[xef.form-no] + b-tot + v-setup*/
      zzz = 0.
      /*
MESSAGE "pr4-brd:"
    brd.i-no ":" b-totw ":" r-spo$[xef.form-no] ":" b-msh ":" v-setup ":" b-qty ":" brd.cost ":" brd.cost-m
    ": cost-=:" b-cost " waste:" b-waste
    SKIP
    "Form:" xef.form-no " b-cost:" brd.amount SKIP
    brd.amount * (brd.qty - brd.qty-mr - brd.qty-wst) ":"
    brd.amount * brd.qty-mr ":" brd.amount * brd.qty-wst
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
      */
      if xef.fr-msh ne 0 then do:
         if xef.fr-uom = "MSH" then
           zzz =  (t-shtfrm[xef.form-no] / 1000) * xef.fr-msh.
         else if xef.fr-uom = "MSF" then
           zzz =  b-qty * xef.fr-msh.
         else if xef.fr-uom = "TON" then
           zzz = ((b-qty * xef.weight) / 2000) * xef.fr-msh.
         else if xef.fr-uom = "CWT" then
           zzz = ((b-qty * xef.weight) / 100 ) * xef.fr-msh.
         put space(5) "Freight In Costs" "$" +
                 string(xef.fr-msh,">>>9.99") to 50 space(1) xef.fr-uom
                        zzz to 80 skip.
      end.

      ASSIGN
      fr-mshts = zzz
      dm-tot[5] = dm-tot[5] + fr-mshts
      v-dec = t-blkqty[xef.form-no] / (t-blksht[xef.form-no] * xef.n-out * xef.n-out-l).

      {sys/inc/roundup.i v-dec}
      xef.gsh-qty = v-dec + b-waste + r-spo[xef.form-no].
   end.

   brd-spo = spo.
   DO i = 1 TO EXTENT(spo):
     brd-r-spo[i] = r-spo[i].
   END.

   if xef.medium ne "" or xef.flute ne "" or xef.lam-code ne "" then
      run ce/com/pr4-mfl.p . /* medium-flute-liner laminate */

   spo = brd-spo.
   DO i = 1 TO EXTENT(spo):
     r-spo[i] = brd-r-spo[i].
   END.

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
     assign
      rm-wt    = b-qty * item.basis-w
      rm-wt$   = (rm-wt / 100) * ld-rm
      ctrl2[3] = ctrl2[3] + rm-wt$.

   /* rm handling pct. */
   if ld-hp ne 0 then
     assign
      rm-wt%   = (b-tot + b-totw + r-spo$[xef.form-no]) * ld-hp
      ctrl2[2] = ctrl2[2] + rm-wt%.

   /* distribute cost to each blank */
   for each xeb of xef:
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no and
                           blk.id   = xeb.part-no no-error.
      if avail blk then do:
         if ld-rm ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
         if ld-hp ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
         blk.cost = blk.cost + ((b-tot + v-setup + b-totw + fr-mshts + rm-wt$ + rm-wt% +
                             + r-spo$[xef.form-no]) * blk.pct).
      end.
   end.
   find xeb where recid(xeb) = call_id no-lock no-error.
end.
call_id = recid(item).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
