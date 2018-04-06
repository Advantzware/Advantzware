
DEF VAR v-line LIKE probe.line no-undo.
def var v as int.
def var v-part-no like xeb.part-no.
def var v-part-d1 like xeb.part-dscr1.
def var v-part-d2 like xeb.part-dscr2.
DEF VAR v-i-no LIKE xeb.stock-no.
DEF VAR v-2desc AS LOG NO-UNDO.
DEF VAR v-header AS CHAR INIT "- # UP - --- Qty --- --- Description ---- -- Size/Color ---   --- Style/Part # --" NO-UNDO.
def var summary-rpt as log format "SUM/DET" init no.
def var v-carrier like xeb.carrier.
def var v-dest-cd like xeb.dest-code.
def var v-skip-pct as log.
def var v-msf as dec.
def var v-vend-no   like e-item-vend.vend-no init "" NO-UNDO.
DEF var v-vend-list AS CHAR NO-UNDO.
DEF VAR v-yld AS DEC NO-UNDO.
def var lv-brd-sf as dec format ">>>>>9.9<<" no-undo.
def var lv-brd-wu as dec format ">>>>9.9<<<<" no-undo.
DEF VAR li-blk AS INT NO-UNDO.
DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>9.9<<<<" NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEFINE VARIABLE dShrink AS DECIMAL     NO-UNDO.
DEF BUFFER bf-oe-ord FOR oe-ord.
DEF BUFFER bf-oe-ordl FOR oe-ordl.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-probe FOR probe.
IF xest.metric THEN
  ASSIGN
   ld-metric = 25.4
   lv-format = "->>,>>>mm".

{cec/get-vend.i}  /* get vendor number */

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
assign
 qtty     = 0
 ctrl[1]  = ce-ctrl.whse-mrkup / 100
 ctrl[2]  = ce-ctrl.hand-pct / 100
 ctrl[3]  = ce-ctrl.rm-rate
 ctrl[4]  = ce-ctrl.spec-%[1]
 ctrl[5]  = integer(ce-ctrl.comm-add)
 ctrl[6]  = integer(ce-ctrl.shp-add)
 ctrl[7]  = integer(ce-ctrl.sho-labor)
 ctrl[8]  = integer(ce-ctrl.trunc-99)
 ctrl[11] = ce-ctrl.spec-%[2]
 ctrl[12] = ce-ctrl.spec-%[3]
 ctrl[13] = integer(ce-ctrl.spec-add[1])
 ctrl[14] = integer(ce-ctrl.spec-add[2])
 ctrl[15] = integer(ce-ctrl.spec-add[3])
 ctrl[16] = integer(ce-ctrl.spec-add[6])
 ctrl[17] = integer(ce-ctrl.spec-add[7])
 ctrl[18] = integer(ce-ctrl.spec-add[8])
 ctrl2    = 0.


   ctrl[19] = ce-ctrl.fold-pct.

fg-rate-f = ce-ctrl.fg-rate-farm.
 
rm-rate-f = ce-ctrl.rm-rate-farm.
  
hand-pct-f = ce-ctrl.hand-pct-farm / 100.

find first xef where xef.company = xest.company 
                 AND xef.est-no eq xest.est-no.
find first xeb where xeb.company = xest.company AND xeb.est-no eq xest.est-no.
find first xop where xop.company = xest.company 
                 AND xop.est-no    eq xest.est-no
                 and xop.op-speed eq 0
                 no-lock no-error.

save-lock = xef.op-lock.
pause 0.

DO TRANSACTION:
  {est/recalc-mr.i xest}
  FIND CURRENT recalc-mr NO-LOCK.

  ASSIGN
   do-speed = xest.recalc
   do-mr    = recalc-mr.val[1] EQ 1
   do-gsa   = xest.override.

  {sys/inc/cerun.i F}
  vmclean = LOOKUP(cerunf,"McLean,HOP,CERunF 2") gt 0.

  {ce/msfcalc.i}
END.

summary-rpt = vmclean.

IF vprint THEN DO:
  /*DO i = 1 TO 4:
    IF xest.est-qty[i] NE 0 THEN qtty[i] = xest.est-qty[i].
  END.*/

  FIND FIRST est-qty
      WHERE est-qty.company EQ xest.company
        AND est-qty.est-no  EQ xest.est-no
      NO-LOCK NO-ERROR.
  IF AVAIL est-qty THEN DO i = 1 TO 20:
    IF est-qty.qty[i] NE 0 THEN
      ASSIGN
       qtty[i + 4] = est-qty.qty[i]
       rels[i + 4] = est-qty.qty[i + 20].
  END.

  {sys/inc/srtqty.i &sub=i &ext=28 &qty=qtty &rel=rels}

  FIND FIRST tt-qtty NO-ERROR.
  IF AVAIL tt-qtty THEN DELETE tt-qtty.
  CREATE tt-qtty.

  DO i = 1 TO 28:
    ASSIGN
     tt-qtty.qtty[i] = qtty[i]
     tt-qtty.rel[i]  = IF qtty[i] EQ 0 THEN 0
                       ELSE
                       IF rels[i] EQ 0 THEN 1 ELSE rels[i].
  END.

  v-do-all-forms-ink = NO.

  RUN est/getqty.w (INPUT-OUTPUT do-speed, INPUT-OUTPUT do-mr, INPUT-OUTPUT do-gsa, INPUT-OUTPUT v-drop-rc, INPUT-OUTPUT v-match-up,
                    INPUT-OUTPUT v-do-all-forms-ink, INPUT-OUTPUT v-board-cost-from-blank, INPUT NO, OUTPUT lv-error). 
  IF lv-error THEN RETURN ERROR.

  IF lv-override THEN
  FOR EACH probe
      WHERE probe.company EQ xest.company
        AND probe.est-no  EQ xest.est-no:
    DELETE probe.                 
  END.
  
  DO i = 1 to 28:
        qtty[i] = tt-qtty.qtty[i].
        rels[i] = tt-qtty.rel[i].
  end.
  {sys/inc/srtqty.i &sub=i &ext=28 &qty=qtty &rel=rels}
  do i = 1 to 28:
        if qtty[i] eq 0 then rels[i] = 0.
        else if rels[i] eq 0 then rels[i] = 1.
  end.
end.
else qtty[1] = qty.

DO TRANSACTION:
  {est/op-lock.i xest}
  FIND bf-est WHERE RECID(bf-est) EQ RECID(xest).
  FIND CURRENT recalc-mr.
  ASSIGN
   bf-est.recalc    = do-speed
   recalc-mr.val[1] = INT(do-mr)
   bf-est.override  = do-gsa
   op-lock.val[1]   = INT(bf-est.recalc)
   op-lock.val[2]   = recalc-mr.val[1].
  FIND CURRENT bf-est NO-LOCK.
  FIND CURRENT recalc-mr NO-LOCK.
  FIND CURRENT op-lock NO-LOCK.   
END.

session:set-wait-state("General").

form day_str v-module tim_str to 79  skip(2)
     with frame hdr page-top width 80 no-labels no-box stream-io.

FORM "Set Est#" xest.est-no FORMAT "x(8)"
     "SlsRep:" kli.sname
     "UserID:" xest.updated-id
     "Prober:" probe.probe-user
     cJobNo
     SKIP
     "Cust:" kli.cust-no
             kli.cust-add[1] FORMAT "x(29)" TO 44
     "Ship:" kli.ship-add[1] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[2] FORMAT "x(29)" TO 44
             kli.ship-add[2] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[3] FORMAT "x(29)" TO 44
             kli.ship-add[3] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[4] FORMAT "x(29)" TO 44
             kli.ship-add[4] FORMAT "x(29)" TO 80
             SKIP(1)

   "Finished Good:"
   v-part-no
   v-part-d1  SKIP
   v-part-d2  AT 32
   SKIP(1)

  WITH NO-LABELS NO-BOX DOWN WIDTH 80 STREAM-IO FRAME kli.

if retry then output close.

find first xeb
    where xeb.company  eq xest.company
      and xeb.est-no   eq xest.est-no
      and xeb.form-no  eq 0
      and xeb.blank-no eq 0
    no-lock no-error.
assign
 v-part-no = if avail xeb then xeb.part-no else ""
 v-part-d1 = if avail xeb then xeb.part-dscr1 else ""
 v-part-d2 = if avail xeb then xeb.part-dscr2 else "". 

for each xjob:
  delete xjob.
end.

do vmcl = 1 to 28:
  if qtty[vmcl] eq 0 then next.

  assign
   t-shtfrm   = 0
   op-tot     = 0
   v-brd-cost = 0
   qty        = qtty[vmcl]
   t-blksht   = 0
   t-blkqty   = 0.

  for each kli:
    delete kli.
  end.

  for each ink:
    delete ink.
  end.

  for each flm:
    delete flm.
  end.

  for each cas:
    delete cas.
  end.

  for each car:
    delete car.
  end.

  for each blk:
    delete blk.
  end.

  vbsf = 0.

  do transaction:
    for each est-op WHERE est-op.company = xest.company 
                      AND est-op.est-no eq xest.est-no
                      and est-op.line  gt 500:
        delete est-op.
    end.
    for each est-op WHERE est-op.company = xest.company 
                      AND est-op.est-no eq xest.est-no
                      and est-op.line  lt 500:
        create xop.
        buffer-copy est-op to xop
        assign
           xop.line = est-op.line + 500.
    end.
  end.

  {est/probeset.i qtty[vmcl] v-match-up}

  IF probe.LINE LT 100 THEN
     assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".v" + string(probe.line,"99")
            outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".a" + string(probe.line,"99")
            outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".s" + string(probe.line,"99")
            ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".p" + string(probe.line,"99").
  ELSE
     assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".v" + string(probe.line,"999")
            outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".a" + string(probe.line,"999")
            outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".s" + string(probe.line,"999")
            ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".p" + string(probe.line,"999").

  output to value(outfile1).

for each xef NO-LOCK
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no:
    xxx = 0.
   
    for each xeb NO-LOCK
        where xeb.company eq xef.company
          and xeb.est-no  eq xef.est-no 
          and xeb.form-no eq xef.form-no:
   
       find first kli where
            kli.cust-no = xeb.cust-no
            no-error.
   
       if not avail kli then do:
          find first sman where
               sman.company EQ cocode AND
               sman.sman    EQ xeb.sman
               no-lock no-error.
   
          find first cust where 
               cust.company = cocode and
               cust.cust-no = xeb.cust-no
               no-lock no-error.
   
          find first shipto where
               shipto.company EQ cocode AND
               shipto.cust-no = cust.cust-no and
               shipto.ship-no = xeb.ship-no
               no-lock no-error.
   
          create kli.
          if avail sman then
             assign
                kli.sman    = sman.sman
                kli.sname   = sman.sname.
   
          if xeb.cust-no ne "Temp" then
             assign
                kli.cust-no = xeb.cust-no
                kli.cust-add[1] = cust.name
                kli.cust-add[2] = cust.addr[1]
                kli.cust-add[3] = cust.addr[2]
                kli.cust-add[4] = cust.city + ", " + cust.state + " " + cust.zip.
          else
             assign
                kli.cust-no = xeb.cust-no
                kli.cust-add[1] = xeb.ship-name
                kli.cust-add[2] = xeb.ship-addr[1]
                kli.cust-add[3] = xeb.ship-addr[2]
                kli.cust-add[4] = xeb.ship-city + ", " + xeb.ship-state + " " +
                                  xeb.ship-zip.
   
          if kli.cust-add[3] = "" then
             assign
                kli.cust-add[3] = kli.cust-add[4]
                kli.cust-add[4] = "".
   
          if kli.cust-add[2] = "" then
             assign
                kli.cust-add[2] = kli.cust-add[3]
                kli.cust-add[3] = kli.cust-add[4]
                kli.cust-add[4] = "".
   
          IF AVAIL shipto THEN
          DO:
             if shipto.ship-no = 1 then
                assign
                   kli.ship-add[1] = "SAME"
                   kli.ship-add[2] = "" kli.ship-add[2] = ""
                   kli.ship-add[4] = "".
             else
                assign
                   kli.ship-add[1] = shipto.ship-name
                   kli.ship-add[2] = shipto.ship-addr[1]
                   kli.ship-add[3] = shipto.ship-addr[2]
                   kli.ship-add[4] = shipto.ship-city + ", " + shipto.ship-state +
                                     " " + shipto.ship-zip.
          END.
   
          if kli.ship-add[3] = "" then
             assign
                kli.ship-add[3] = kli.ship-add[4]
                kli.ship-add[4] = "".
          if kli.ship-add[2] = "" then
             assign
                kli.ship-add[2] = kli.ship-add[3]
                kli.ship-add[3] = kli.ship-add[4]
                kli.ship-add[4] = "".
       end.
   
       qty = qtty[vmcl].
       
       find first blk where
            blk.snum = xeb.form-no and
            blk.bnum = xeb.blank-no
            no-error.

       if not avail blk then do:
   
          create blk.
          assign
           blk.kli      = kli.cust-no
           blk.id       = xeb.part-no
           blk.snum     = xeb.form-no
           blk.bnum     = xeb.blank-no
           blk.qreq     = qty
           blk.qyld     = qty
           blk.yr$      = xeb.yrprice
           blk.stock-no = xeb.stock-no
           blk.pur-man  = xeb.pur-man.
       end.
       xxx = xxx + (xeb.t-sqin * xeb.num-up).
       IF cerunf = "HOP" THEN DO:
            cJobNo = "Job #: " .
            FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ xeb.company
                AND bf-oe-ordl.ord-no EQ xeb.ord-no
                AND bf-oe-ordl.i-no EQ xeb.stock-no NO-LOCK NO-ERROR.
            IF AVAIL bf-oe-ordl THEN
                cJobNo = cJobNo + bf-oe-ordl.job-no + "-" + string(bf-oe-ordl.job-no2).
            ELSE DO:
                FIND FIRST bf-oe-ord WHERE bf-oe-ord.company EQ xeb.company
                    AND bf-oe-ord.ord-no EQ xeb.ord-no NO-LOCK NO-ERROR.
                IF AVAIL bf-oe-ord THEN
                    cJobNo = cJobNo + bf-oe-ord.job-no + "-" + string(bf-oe-ord.job-no2).
            END.
        END.
        ELSE cJobNo = "".
    end.
   
    for each xeb FIELDS(t-sqin num-up)
        where xeb.company eq xef.company
          and xeb.est-no  eq xef.est-no
          and xeb.form-no eq xef.form-no
        no-lock,
        first blk
        where blk.snum eq xeb.form-no
          and blk.bnum eq xeb.blank-no:
       blk.pct = (xeb.t-sqin * xeb.num-up) / xxx.
    end.
end.

/* print header */
ASSIGN
 day_str  = STRING(TODAY,"99/99/9999")
 tim_str  = STRING(TIME,"hh:mm am") 
 v-module = IF cerunf EQ "HOP" THEN "FCD-0101" ELSE ""
 v-module = FILL(" ",59 - LENGTH(TRIM(v-module))) + TRIM(v-module).


display day_str v-module tim_str with frame hdr.
for each kli with frame kli:
   display trim(xest.est-no) @ xest.est-no
           kli.sman
           xest.updated-id
           probe.probe-user
           cJobNo FORMAT "X(20)"
           kli.sname
           kli.cust-no
           kli.cust-add
           kli.ship-add
           v-part-no
           v-part-d1
           v-part-d2.
end.
for each xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
with frame brd no-labels no-box width 82 stream-io down:
   /*if xef.lam-dscr = "R"
   then*/ assign brd-l[1] = xef.trim-l
               brd-w[1] = xef.trim-w.
   /*else assign brd-l[1] = xef.trim-w
               brd-w[1] = xef.trim-l.*/
   /* calc. sheet dimensions & weight */

  tt-blk = qtty[vmcl].

  RUN ce/box/prokalk2.p.

  if cerunf eq "HOP" then
    assign
     brd-l[2] = xef.nsh-len
     brd-w[2] = xef.nsh-wid.
  else
    assign
     brd-l[2] = xef.gsh-len
     brd-w[2] = xef.gsh-wid.

   if xef.roll = true then brd-l[3] = xef.gsh-len.
   if brd-l[2] = 0 and brd-w[2] = 0 then assign brd-l[2] = lsh-len
                                                brd-w[2] = lsh-wid .

   ASSIGN
      brd-w[3] = if xef.roll eq true then xef.roll-wid else 0
      brd-sq[1] = xef.trim-l * xef.trim-w
      brd-sq[2] = brd-l[2] * brd-w[2]
      brd-sq[3] = brd-l[3] * brd-w[3]
      brd-sf[1] = if v-corr then (brd-sq[1] * .007) else (brd-sq[1] / 144)
      brd-sf[2] = if v-corr then (brd-sq[2] * .007) else (brd-sq[2] / 144)
      brd-sf[3] = if v-corr then (brd-sq[3] * .007) else (brd-sq[3] / 144).

   find first item {sys/look/itemW.i} and item.i-no = xef.board no-lock no-error.
   if avail item then
      find first e-item of item no-lock no-error.

   ASSIGN
      brd-wu[1] = brd-sf[1]  * item.basis-w
      brd-wu[2] = brd-sf[2]  * item.basis-w
      brd-wu[3] = (brd-sf[3] * item.basis-w) / 2000
      lv-brd-sf = xef.gsh-len * xef.gsh-wid * xef.gsh-qty
      lv-brd-sf = (if v-corr then (lv-brd-sf * .007) else (lv-brd-sf / 144)) / 1000 /*tot msf*/.
   
   if avail item then lv-brd-wu = (lv-brd-sf * item.basis-w) / 2000.

   ASSIGN
      zzz = 0
      t-blksht[xef.form-no] = 0.

   for each xeb NO-LOCK
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no 
         and xeb.form-no eq xef.form-no:

      ASSIGN
         v-yld = IF xeb.cust-% lt 0 then -1 / xeb.cust-% else xeb.cust-%
         /* set total # of blanks on all forms */
         tt-blk = qtty[vmcl].

      /* set total # of blanks on this form */
      if xeb.blank-no = 1 then
         t-blksht[xef.form-no] = t-blksht[xef.form-no] + xeb.num-up.

      /* set total qty of all blanks for this form */
      assign
       t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] + (qtty[vmcl] * v-yld)
       brd-l[4]  = xeb.t-len
       brd-w[4]  = xeb.t-wid
       brd-sq[4] = xeb.t-sqin  /*brd-l[4] * brd-w[4]*/ 
       brd-sf[4] = if v-corr then (brd-sq[4] * .007) else (brd-sq[4] / 144)
       brd-wu[4] = brd-sf[4] * item.basis-w

      /* find sheet qty needed for this form (without spoil)*/
      zzz = qtty[vmcl] * v-yld / (xeb.num-up * xef.n-out * xef.n-out-l).
      /*if xest.form-qty = 1 then zzz = zzz * 2.*/
      {sys/inc/roundup.i zzz}
      IF zzz GT t-shtfrm[xeb.form-no] THEN
         t-shtfrm[xeb.form-no] = zzz.

      ASSIGN
         call_id = recid(xeb)
         vbsf = vbsf + if v-corr then (xeb.t-sqin * .007) else (xeb.t-sqin / 144).
   end.

   find xeb where recid(xeb) = call_id no-lock no-error.

   ASSIGN
      qty = qtty[vmcl]
      tmpstore = if xest.form-qty eq 1 then "BOTTOM/LID"
                 else
                 if xest.form-qty eq 2 then
                    if xef.form-no = 1 then "BOTTOM" else "LID"
                    else
                       "FORM " + trim(string(xef.form-no,">9")).
  IF (cerunf = "ASI" OR cerunf = "CERunF 1") THEN 
     v-header = "- # UP - --- Qty --- --- Desc/FG Item --- -- Size/Color ---   --- Style/Part # --".
    ELSE v-header = "- # UP - --- Qty --- --- Description ---- -- Size/Color ---   --- Style/Part # --".
        
  if summary-rpt THEN DO:
   PUT skip.
      IF lookup(cerunf,"ASI,CERunF 1") EQ 0 /*cerunf <> "ASI"*/ THEN
       PUT "         --- Qty --- --- Description ---- -- Size/Color ---" skip.
      ELSE
       PUT "         --- Qty --- --- Desc/FG Item --- -- Size/Color ---" skip.
  END.
  else
   DISPLAY skip
   tmpstore format "x(12)"
   " Width   Length   Sq.Inches  Sq.Feet/Sheet    Weight per Units" skip
   "Blank  Size:" brd-w[4] to 21
                  brd-l[4] to 30
                  brd-sq[4] to 42
                  brd-sf[4] to 52 "Sf/Blk"
                  brd-wu[4] to 70 space(0) "/M Blks" skip
   "Feed  Sheet:" brd-w[1] to 21 brd-l[1] to 30 brd-sq[1] to 42
            brd-sf[1] to 52 "Sf/Sht"
            brd-wu[1] to 70 space(0) "/M Shts" skip
   "Gross Sheet:" brd-w[2] to 21 brd-l[2] to 30 brd-sq[2] to 42
            brd-sf[2] to 52 "Sf/Sht"  brd-wu[2] to 70 space(0) "/M Shts" skip
   "Roll  Size :" /*brd-l[3]  to 30  when brd-l[3] ne 0*/
                  brd-w[3]  to 21  when brd-w[3] ne 0
            /*    brd-sq[3] to 42  when brd-w[3] ne 0
                  brd-wu[3] to 70  when brd-w[3] ne 0
                  "Tons"           when brd-w[3] ne 0 */
    lv-brd-sf TO 52 "MSF"
    lv-brd-wu TO 70 "Tons" SKIP(1)
    v-header FORMAT "x(80)".
    

   for each xeb NO-LOCK
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no 
         and xeb.form-no eq xef.form-no
       with frame blk no-box no-labels width 80 stream-io down:
      find first style  where  style.company = cocode and
                                 style.style = xeb.style no-lock no-error.

      ASSIGN
       ld-len = xeb.len * ld-metric
       ld-wid = xeb.wid * ld-metric
       ld-dep = xeb.dep * ld-metric.

      IF ld-metric NE 1 THEN DO:
        {sys/inc/roundup.i ld-len}
        {sys/inc/roundup.i ld-wid}
        {sys/inc/roundup.i ld-dep}
      END.

      ASSIGN
       sizcol[1]  = TRIM(STRING(ld-len,lv-format)) + "x" +
                    TRIM(STRING(ld-wid,lv-format)) + "x" +
                    TRIM(STRING(ld-dep,lv-format))
       sizcol[2]  = xeb.i-coldscr
       stypart[1] = style.dscr
       stypart[2] = xeb.part-no
       dsc[1]     = xeb.part-dscr1
       dsc[2]     = xeb.part-dscr2.
      IF lookup(cerunf,"ASI,CERunF 1") NE 0 /*cerunf = "ASI"*/ THEN DO:
        IF dsc[2] = "" THEN 
            ASSIGN dsc[2] = xeb.stock-no 
                v-i-no = ""   
                v-2desc = NO.
        ELSE
            ASSIGN v-i-no = xeb.stock-no 
                v-2desc = YES.
      END.

      if summary-rpt then do:
        stypart = "".
        put tmpstore format "x(7)".
      end.

      else
        /*if xest.form-qty > 1
        then*/ put space(4) string(xeb.num-up,">>9")     format "x(3)".
        /*else put space(4) string(xeb.num-up / 2,">>9") format "x(3)".*/

      put space(2)
          qtty[vmcl] format ">>>,>>>,>>9" space(1)
          dsc[1] format "x(20)"
          sizcol[1] format "x(21)"
          stypart[1] format "x(19)" skip
          space(21)
          dsc[2] format "x(20)"
          sizcol[2] format "x(21)"
          stypart[2] format "x(15)" skip.
      IF lookup(cerunf,"ASI,CERunF 1") NE 0 /*cerunf = "ASI"*/ AND v-2desc THEN
          PUT SPACE(21) v-i-no FORMAT "x(20)" SKIP.
      down.
   end.
   put skip(1).
end.
tmpstore = "".

put "Materials                 Weight Caliper    QTY/Unit    MR $  Matl$/M      TOTAL" skip.

assign
 dm-tot[3] = 0
 dm-tot[4] = 0
 dm-tot[5] = 0.

/* b o a r d        */ run ce/box/pr42-brd.p (v-vend-no).
v-brd-cost = v-brd-cost + dm-tot[5].

/* i n k s          */ run ce/box/pr42-ink.p.

/* films            */ run ce/box/pr42-flm.p.

/* case/tray/pallet */ run ce/box/pr42-cas.p.

/* special          */ run ce/box/pr42-spe.p.

for each blk:
   accumulate blk.cost (total).
end.

for each blk:
   find first xjob
        where xjob.i-no     eq blk.id
          and xjob.form-no  eq blk.snum
          and xjob.blank-no eq blk.bnum
          and xjob.qty      eq blk.qreq
        no-error.
   if not avail xjob then do:
      create xjob.
      assign
       xjob.i-no = blk.id
       xjob.qty  = blk.qreq.
   end.
   assign
    xjob.mat      = blk.cost - blk.lab
    xjob.lab      = blk.lab
    xjob.qty      = blk.qreq
    xjob.form-no  = blk.snum
    xjob.blank-no = blk.bnum
    xjob.pct      = blk.pct
    xjob.stock-no = blk.stock-no
    xjob.pur-man  = xeb.pur-man.
end.

display     "TOTAL  DIRECT  MATERIALS "
            dm-tot[3] format ">>>9.99" to 61
            dm-tot[5] / (tt-blk / 1000) format ">>>>9.99" to 69
            dm-tot[5] format ">>>>,>>9.99" to 80
            skip(1)
    with frame ac5 no-labels no-box stream-io.

/* prep */ run ce/box/pr42-prp.p .

/* misc. */ run ce/box/pr42-mis.p . /* misc. */

put skip(1)
   "Machine Description    MR (Hrs) Run  Speed    Rate     MR $    Run $  Total Cost" .

/* machines */
  run ce/box/pr42-mch.p.

  if ctrl2[2] ne 0 or ctrl2[3] ne 0 then do:
    put "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) to 80 skip.
    op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
  end.
 
  fr-tot = 0.
  for each xef
       where xef.company eq xest.company
         and xef.est-no  eq xest.est-no:

   release item.
   release e-item.

   if xef.form-no eq 1 then do:
     find first xeb of xef no-lock no-error.
     if avail xeb then 
         find first item
             {sys/look/itemW.i}
               and item.i-no eq xeb.tr-no
             no-lock no-error.
     if avail item then
       find first e-item of item no-lock no-error.
   end.

   if avail e-item and item.mat-type eq "Z" then do:
     find first xeb
         where xeb.company  eq xest.company
           and xeb.est-no   eq xest.est-no 
           and xeb.form-no  eq 0
           and xeb.blank-no eq 0
         no-error.
     tr-tot = ((xeb.len * xeb.wid * xeb.dep) * qtty[vmcl]) /
               (item.case-l * item.case-w * item.case-d).

     EMPTY TEMP-TABLE tt-ei.
     CREATE tt-ei.
     DO j = 1 TO 10:
        ASSIGN
           tt-ei.run-qty[j] = e-item.run-qty[j]
           tt-ei.run-cost[j] = e-item.run-cost[j].
     END.
     
     FIND FIRST b-qty WHERE
          b-qty.reftable = "blank-vend-qty" AND
          b-qty.company = e-item.company AND
          b-qty.CODE    = e-item.i-no
          NO-LOCK NO-ERROR.
     
     IF AVAIL b-qty THEN
     DO:
        FIND FIRST b-cost WHERE
             b-cost.reftable = "blank-vend-cost" AND
             b-cost.company = e-item.company AND
             b-cost.CODE    = e-item.i-no
             NO-LOCK NO-ERROR.
     
        DO j = 1 TO 10:
           ASSIGN
              tt-ei.run-qty[j + 10] = b-qty.val[j]
              tt-ei.run-cost[j + 10] = b-cost.val[j].
        END.
     END.

     do j = 1 to 20:
        if tt-ei.run-qty[j] < tr-tot then next.
        fr-tot = round(tr-tot * tt-ei.run-cost[j],2).
        leave.
     end.
   end.

   for each xeb
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no 
         and xeb.form-no eq xef.form-no:
      assign
       v-dest-cd = xeb.dest-code
       v-carrier = xeb.carrier.
      if v-carrier eq "" then do:
        find first eb
            where eb.company eq xest.company
              and eb.est-no  eq xest.est-no
              and eb.carrier ne ""
             no-lock no-error.
        if avail eb then do:
          v-carrier = eb.carrier.
          if v-dest-cd eq "" then v-dest-cd = eb.dest-code.
        end.
      end.
      find first carrier
          where carrier.company = cocode
            and carrier.loc = locode
            and carrier.carrier = v-carrier
          no-lock no-error.
      if avail carrier then
      find first carr-mtx
          where carr-mtx.company  eq cocode
            and carr-mtx.loc      eq locode
            and carr-mtx.carrier  eq carrier.carrier
            and carr-mtx.del-zone eq v-dest-cd
          no-lock no-error.
      find first car where car.id = xeb.part-no no-error.
      if not avail car then do:
         create car.
         assign
         car.carrier = carrier.carrier car.dscr    = carr-mtx.del-zone
         car.id      = xeb.part-no     car.snum   = xeb.form-no
         car.bnum   = xeb.blank-no.
      end.
      
      find first item
          {sys/look/itemW.i}
            and item.i-no     eq xef.board
            and item.mat-type eq "B"
            and item.avg-w    gt 0
          no-lock no-error.

      assign
       v-yld   = IF xeb.cust-% lt 0 then -1 / xeb.cust-% else xeb.cust-%
       li-blk  = qtty[vmcl] * v-yld
       v-msf   = (xeb.t-sqin - xeb.t-win) * li-blk / 144000
       v-msf   = v-msf * if avail item then item.avg-w else 1
       car.qty = car.qty + (xef.weight * v-msf)
       car.msf = car.msf + v-msf.
       
      if xef.medium ne "" then do:
         find first item {sys/look/itemW.i} and
                    item.i-no = xef.medium no-lock no-error.
         if avail item
         then do:
             /*override item shrink % with shrink entered in BOM button on Layout screen*/
              IF xef.spare-dec-1 NE 0 
                  THEN dShrink = xef.spare-dec-1.
                  ELSE dShrink = ITEM.shrink.
             car.qty = car.qty +
                        (item.basis-w * (1 - (dShrink / 100)) * v-msf).
         END.
      end.
      if xef.flute ne "" then do:
         find first item {sys/look/itemW.i} and
                    item.i-no = xef.flute no-lock no-error.
         if avail item
         then car.qty = car.qty +
                        (item.basis-w * v-msf).
      end.
      if xef.lam-code ne "" then do:
         find first item {sys/look/itemW.i} and
                    item.i-no = xef.lam-code no-lock no-error.
         if avail item
         then car.qty = car.qty +
                        ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                         li-blk * xeb.t-sqin / item.sqin-lb).
      end.
      if xef.adh-code ne "" then do:
         find first item {sys/look/itemW.i} and
                    item.i-no = xef.adh-code no-lock no-error.
         if avail item
         then car.qty = car.qty +
                        ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                         li-blk * xeb.t-sqin / item.sqin-lb).
      end.

      /* add pallet & case for total weight */
      for each cas where cas.id = xeb.part-no:
         find first item where item.company = cocode and
                               item.i-no = cas.ino no-lock no-error.
         car.qty = car.qty + (cas.qty *
                   if item.mat-type eq "D" then ce-ctrl.def-pal-w else
                   if item.mat-type eq "C" then ce-ctrl.def-cas-w else
                   IF CAN-DO("5,6",item.mat-type) THEN
                     (item.weight-100 / 100) ELSE 0).
      end.
   end.
  end.

  output close.

  DO TRANSACTION:
    {est/calcpcts.i xest}
    calcpcts.val[2] = v-brd-cost.
    FIND CURRENT calcpcts NO-LOCK NO-ERROR.
  END.

  v-do-gsa = (vprint and do-gsa).
  run ce/box/pr42tots.p (ROWID(probe), li-rels).

  output close.

  run ce/box/pr42mis2.p.

  if vprint then run ce/box/probemk.p (ROWID(probe)).

  for each blk:
    find first xjob
        where xjob.i-no eq blk.id
          and xjob.qty  eq blk.qreq.
    assign xjob.mat = xjob.mat / (blk.qyld / 1000)
           xjob.lab = xjob.lab / (blk.qyld / 1000)
           xjob.foh = xjob.foh / (blk.qyld / 1000)
           xjob.voh = xjob.voh / (blk.qyld / 1000).
  end.

  if not vprint then DO TRANSACTION:

     IF probe.LINE LT 100 THEN
     DO:
        if opsys eq "unix" then do:
           unix silent rm value(tmp-dir + TRIM(xest.est-no) + "-*.*" + string(probe.line,"99")).
           unix silent rm value(tmp-dir + TRIM(xest.est-no) +   ".*" + string(probe.line,"99")).
        end.
        else do: /* if opsys eq "MSDOS" then */
           dos silent del value(tmp-dir + TRIM(xest.est-no) + "-*.*" + string(probe.line,"99")).
           dos silent del value(tmp-dir + TRIM(xest.est-no) +   ".*" + string(probe.line,"99")).
        end.
     END.
     ELSE
     DO:
        if opsys eq "unix" then do:
           unix silent rm value(tmp-dir + TRIM(xest.est-no) + "-*.*" + string(probe.line,"999")).
           unix silent rm value(tmp-dir + TRIM(xest.est-no) +   ".*" + string(probe.line,"999")).
        end.
        else do: /* if opsys eq "MSDOS" then */
           dos silent del value(tmp-dir + TRIM(xest.est-no) + "-*.*" + string(probe.line,"999")).
           dos silent del value(tmp-dir + TRIM(xest.est-no) +   ".*" + string(probe.line,"999")).
        end.
     END.

     FIND CURRENT probe.
     DELETE probe.
  end.
end.  /* do vmcl = 1 to 28: */

if vprint then do vmcl = 1 to 28:
  if qtty[vmcl] eq 0 then next.

  FOR EACH bf-probe
      WHERE bf-probe.company    EQ xest.company
        AND bf-probe.est-no     EQ xest.est-no
        AND bf-probe.probe-date EQ TODAY
        AND bf-probe.est-qty    EQ qtty[vmcl]
        AND bf-probe.freight    EQ rels[vmcl]
      NO-LOCK
      BY bf-probe.probe-time DESC:
    LEAVE.
  END.

  IF bf-probe.LINE LT 100 THEN
     assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".v" + string(bf-probe.line,"99")
           outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                      string(1,"99")     + ".a" + string(bf-probe.line,"99")
           outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                      string(1,"99")     + ".s" + string(bf-probe.line,"99")
           ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                      string(1,"99")     + ".p" + string(bf-probe.line,"99").
  ELSE
     assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".v" + string(bf-probe.line,"999")
            outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".a" + string(bf-probe.line,"999")
            outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".s" + string(bf-probe.line,"999")
            ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".p" + string(bf-probe.line,"999").

  if vmclean then do:
    output to value(outfile3) append.

    vhld = 0.
     
    {ce/mclean.i 1}
    
    put skip.

    output close.
  end.

  if opsys = "unix" then
    unix silent cat value(outfile2) >> value(outfile3).
  else /* if opsys = "msdos" then */
    dos silent type value(outfile2) >> value(outfile3).

  dos silent type value(outfile3) > value(ls-outfile).

  RUN ce/probeu3.p (ROWID(bf-probe)).
end.

DO TRANSACTION:
  FIND CURRENT op-lock NO-ERROR.
  IF AVAIL op-lock THEN DELETE op-lock.
END.

SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 1992  advanced software, inc. */
