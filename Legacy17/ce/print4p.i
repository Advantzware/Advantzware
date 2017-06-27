
def var v-layout as log NO-UNDO.
DEF VAR v-line LIKE probe.line no-undo.
def var v-msf as dec no-undo.
def var v-dec as dec NO-UNDO.
def var v-skip-pct as log NO-UNDO.
DEF VAR v-i-no LIKE xeb.stock-no NO-UNDO.
DEF VAR v-2desc AS LOG NO-UNDO.
DEF VAR v-header AS CHAR INIT "   Qty      --- Description ------ -- Size / Color ----- --- Style / Part No ---" NO-UNDO. 

def var lv-brd-l           like eb.len no-undo.
def var lv-brd-w           like lv-brd-l no-undo.
def var lv-brd-sq          as dec format ">>>>9.9<<<<" no-undo.
def var lv-brd-sf          as dec format ">>>>>9.9<<"  no-undo.
def var lv-brd-wu          like lv-brd-sq no-undo.

DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>9.9<<<<" NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEF VAR ld-fg-rate AS DEC NO-UNDO.
DEF VAR cJobNo AS CHAR NO-UNDO.
DEFINE VARIABLE dShrink AS DECIMAL     NO-UNDO.

DEF BUFFER bf-est FOR est.
DEF BUFFER bf-probe FOR probe.
DEF BUFFER reftable-fm FOR reftable.
DEF BUFFER reftable-fold-pct FOR reftable.
DEF BUFFER b-item FOR ITEM.
DEF BUFFER bf-oe-ord FOR oe-ord.
DEF BUFFER bf-oe-ordl FOR oe-ordl.

DEF VAR v-t-win AS DEC DECIMALS 4 NO-UNDO.

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
 ctrl[5]  = int(ce-ctrl.comm-add)
 ctrl[6]  = int(ce-ctrl.shp-add)
 ctrl[7]  = int(ce-ctrl.sho-labor)
 ctrl[8]  = int(ce-ctrl.trunc-99)
 ctrl[11] = ce-ctrl.spec-%[2]
 ctrl[12] = ce-ctrl.spec-%[3]
 ctrl[13] = int(ce-ctrl.spec-add[1])
 ctrl[14] = int(ce-ctrl.spec-add[2])
 ctrl[15] = int(ce-ctrl.spec-add[3])
 ctrl[16] = int(ce-ctrl.spec-add[6])
 ctrl[17] = int(ce-ctrl.spec-add[7])
 ctrl[18] = int(ce-ctrl.spec-add[8]).

FIND FIRST reftable-fold-pct
     WHERE reftable-fold-pct.reftable EQ "ce-ctrl.fold-pct"
       AND reftable-fold-pct.company  EQ ce-ctrl.company
       AND reftable-fold-pct.loc      EQ ce-ctrl.loc
     NO-LOCK NO-ERROR.

IF AVAIL reftable-fold-pct THEN
   ctrl[19] = reftable-fold-pct.val[1].

if retry then output close.

find first xef where xef.company = xest.company 
                 AND xef.est-no eq xest.est-no.
find first xeb where xeb.company = xest.company 
                 AND xeb.est-no eq xest.est-no
                 AND xeb.form-no = xef.form-no.
find first xop where xop.company = xest.company 
                 AND xop.est-no    eq xest.est-no
                 and xop.op-speed eq 0
                 no-lock no-error.

FIND FIRST reftable NO-LOCK
    WHERE reftable.reftable EQ "ce-ctrl.fg-rate-farm"
      AND reftable.company  EQ ce-ctrl.company
      AND reftable.loc      EQ ce-ctrl.loc
    NO-ERROR.  
fg-rate-f = IF AVAIL reftable THEN reftable.val[1] ELSE 0.

FIND FIRST reftable NO-LOCK
    WHERE reftable.reftable EQ "ce-ctrl.rm-rate-farm"
      AND reftable.company  EQ ce-ctrl.company
      AND reftable.loc      EQ ce-ctrl.loc
    NO-ERROR.  
rm-rate-f = IF AVAIL reftable THEN reftable.val[1] ELSE 0.

FIND FIRST reftable NO-LOCK
    WHERE reftable.reftable EQ "ce-ctrl.hand-pct-farm"
      AND reftable.company  EQ ce-ctrl.company
      AND reftable.loc      EQ ce-ctrl.loc
    NO-ERROR.    
hand-pct-f = (IF AVAIL reftable THEN reftable.val[1] ELSE 0) / 100.

ld-fg-rate = IF xeb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate.

DO TRANSACTION:
  {est/recalc-mr.i xest}
  FIND CURRENT recalc-mr NO-LOCK.

  assign
   do-speed = xest.recalc
   do-mr    = recalc-mr.val[1] EQ 1
   do-gsa   = xest.override.
END.

assign
 save-qty  = qty
 save-lock = xef.op-lock.

do transaction:
  {sys/inc/cerun.i F}
  vmclean = lookup(cerunf,"McLean,HOP") gt 0.

  {ce/msfcalc.i}

  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "CEPg2"
                        no-lock no-error.
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "CEPg2"
     sys-ctrl.descrip = "Reverse W & L labels for press, die, & # Up on Estimate?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  v-layout = sys-ctrl.log-fld.
end.

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

find first sman where sman.sman eq xeb.sman no-lock no-error.
find first cust
    where cust.company eq cocode
      and cust.cust-no eq xeb.cust-no
    no-lock no-error.
find first shipto
    where shipto.cust-no eq cust.cust-no
      /*and shipto.ship-no eq xeb.ship-no*/
      and shipto.ship-id eq xeb.ship-id
    no-lock no-error.
find first style
    where style.company eq cocode
      and style.style   eq xeb.style
    no-lock no-error.
if avail style then ctrl2[18] = style.royalty.
if cust.cust-no ne "Temp" then
  assign
   cust-ad[1] = cust.name
   cust-ad[2] = cust.addr[1]
   cust-ad[3] = cust.addr[2]
   cust-ad[4] = cust.city + ", " + cust.state + " " + cust.zip.
else
  assign
   cust-ad[1] = xeb.ship-name
   cust-ad[2] = xeb.ship-addr[1]
   cust-ad[3] = xeb.ship-addr[2]
   cust-ad[4] = xeb.ship-city + ", " + xeb.ship-state + " " + xeb.ship-zip.

if cust-ad[3] eq "" then
  assign
   cust-ad[3] = cust-ad[4]
   cust-ad[4] = "".

if cust-ad[2] eq "" then
  assign
   cust-ad[2] = cust-ad[3]
   cust-ad[3] = cust-ad[4]
   cust-ad[4] = "".

assign
 ship-ad[1] = ship.ship-name
 ship-ad[2] = ship.ship-addr[1]
 ship-ad[3] = ship.ship-addr[2]
 ship-ad[4] = ship.ship-city + ", " + ship.ship-state + " " + ship.ship-zip.

if ship-ad[3] eq "" then
  assign
   ship-ad[3] = ship-ad[4]
   ship-ad[4] = "".
if ship-ad[2] eq "" then
  assign
   ship-ad[2] = ship-ad[3]
   ship-ad[3] = ship-ad[4]
   ship-ad[4] = "".

assign
 dsc[1]   = xeb.part-dscr1
 dsc[2]   = xeb.part-dscr2
 brd-l[1] = xeb.t-len
 brd-l[2] = xef.trim-l
 brd-w[1] = xeb.t-wid
 brd-w[2] = xef.trim-w
 ld-len   = xeb.len * ld-metric
 ld-wid   = xeb.wid * ld-metric
 ld-dep   = xeb.dep * ld-metric.

IF cerunf = "ASI" THEN DO:
    IF dsc[2] = "" THEN 
        ASSIGN dsc[2] = xeb.stock-no
            v-i-no = ""   
            v-2desc = NO.
    ELSE
        ASSIGN v-i-no = xeb.stock-no 
            v-2desc = YES.
END.

IF ld-metric NE 1 THEN DO:
  {sys/inc/roundup.i ld-len}
  {sys/inc/roundup.i ld-wid}
  {sys/inc/roundup.i ld-dep}
END.

assign
 sizcol[1]  = trim(string(ld-len,lv-format)) + "x" +
              trim(string(ld-wid,lv-format)) + "x" +
              trim(string(ld-dep,lv-format))
 sizcol[2]  = xeb.i-coldscr
 stypart[1] = if avail style then style.dscr else ""               /*djk*/
 stypart[2] = xeb.part-no.

if cerunf eq "HOP" then
  assign
   brd-l[3] = xef.nsh-len
   brd-w[3] = xef.nsh-wid.
else
  assign
   lv-brd-l = xef.nsh-len
   lv-brd-w = xef.nsh-wid
   brd-l[3] = xef.gsh-len
   brd-w[3] = xef.gsh-wid.

if xef.roll eq yes then brd-l[4] = xef.gsh-len.
if brd-l[3] eq 0 and brd-w[3] eq 0 then
  assign
   brd-l[3] = xef.lsh-len
   brd-w[3] = xef.lsh-wid.
if xef.roll eq yes then brd-w[4] = xef.roll-wid.
else brd-w[4] = 0.
assign
 brd-sq[1] = xeb.t-sqin
 brd-sq[2] = brd-l[2] * brd-w[2]
 brd-sq[3] = brd-l[3] * brd-w[3]
 brd-sq[4] = brd-l[4] * brd-w[4]
 lv-brd-sq = lv-brd-l * lv-brd-w
 brd-sf[1] = if v-corr then (brd-sq[1] * .007) else (brd-sq[1] / 144)
 brd-sf[2] = if v-corr then (brd-sq[2] * .007) else (brd-sq[2] / 144)
 brd-sf[3] = if v-corr then (brd-sq[3] * .007) else (brd-sq[3] / 144)
 brd-sf[4] = if v-corr then (brd-sq[4] * .007) else (brd-sq[4] / 144)
 lv-brd-sf = if v-corr then (lv-brd-sq * .007) else (lv-brd-sq / 144)
 call_id   = recid(xeb).

do transaction:
  /* take out window if any */
  find xeb where recid(xeb) eq call_id no-error.
  ASSIGN
     v-t-win = 0
     xeb.t-win = 0.

  IF xeb.est-type EQ 1 THEN
  do i = 1 TO 4:
    find first b-item WHERE
         b-item.company EQ xef.company and
         b-item.i-no eq xef.leaf[i]
         no-lock no-error.

    if avail b-item and b-item.mat-type eq "W" and
       (xef.leaf-l[i] ne 0 and xef.leaf-w[i] ne 0) then
       DO:
          xeb.t-win = xeb.t-win + (xef.leaf-l[i] * xef.leaf-w[i]).
          
          /*sheet fed windowing*/ 
          IF xef.leaf-bnum[i] EQ 0 THEN
             v-t-win = v-t-win + (xef.leaf-l[i] * xef.leaf-w[i] / xeb.num-up).
          ELSE
             v-t-win = v-t-win + (xef.leaf-l[i] * xef.leaf-w[i]).
       END.
  end.
  ELSE
     v-t-win = xeb.t-win.

  find xeb where recid(xeb) eq call_id no-lock no-error.
end.

find first item {sys/look/itemW.i} and item.i-no eq xef.board no-lock no-error.
assign
 brd-wu[1] = brd-sf[1] * item.basis-w
 brd-wu[2] = brd-sf[2] * item.basis-w
 brd-wu[3] = brd-sf[3] * item.basis-w
 brd-wu[4] = brd-sf[4] * item.basis-w
 lv-brd-wu = lv-brd-sf * item.basis-w.

for each xjob:
  delete xjob.
end.

/******************************* l  o  o  p  **********************************/
loupe:
do k = 1 to 28:
  assign
   v-op-qty = 0    
   op-tot   = 0
   qty      = qtty[k]
   ctrl2    = 0.

  FOR EACH est-op
      WHERE est-op.company EQ xest.company 
        AND est-op.est-no  EQ xest.est-no 
        AND est-op.line    LT 500
      NO-LOCK
      BREAK BY est-op.qty:
    
    IF FIRST-OF(est-op.qty) THEN DO:
      IF FIRST(est-op.qty) OR
         CAN-FIND(FIRST est-qty
                  WHERE est-qty.company EQ est-op.company
                    AND est-qty.est-no  EQ est-op.est-no
                    AND est-qty.eqty    EQ est-op.qty)
      THEN v-op-qty = est-op.qty.
      IF est-op.qty GE qty THEN LEAVE.
    END.
  END.

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

  if qty eq 0 then leave loupe.

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

  maxpage = k.

  run ce/prokalk.p .

  assign
   k   = maxpage
   qty = qtty[k].

  find first xop where xop.company = xest.company
                   AND xop.est-no eq xest.est-no
                   and xop.line  ge 500
                   no-lock no-error.
  find first ITEM {sys/look/itemW.i}
        and item.i-no eq xef.board
      no-lock no-error.
  if avail item then find first e-item of item no-lock no-error.
  assign
   brd-sf[4] = xef.gsh-len * xef.gsh-wid * xef.gsh-qty / 1000   /*tot msf*/
   brd-sf[4] = if v-corr then (brd-sf[4] * .007) else (brd-sf[4] / 144).
  if avail item then brd-wu[4] = (brd-sf[4] * item.basis-w) / 2000.

  {est/probeset.i qtty[k] v-match-up}

  IF probe.LINE LT 100 THEN
     ASSIGN
        outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"99")
        outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"99")
        outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"99").
  ELSE
     ASSIGN
        outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"999")
        outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"999")
        outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"999").
  
  assign day_str = string(today,"99/99/9999")
         tim_str = string(time,"hh:mm am") .
  form day_str
       v-module
       tim_str to 79  skip(1)
       with frame hdr page-top width 80 no-labels no-box stream-io.

  output to value(outfile1).

  ASSIGN
   v-module = IF cerunf EQ "HOP" THEN "FCD-0101" ELSE ""
   v-module = FILL(" ",59 - LENGTH(TRIM(v-module))) + TRIM(v-module).
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

  display day_str v-module tim_str with frame hdr STREAM-IO.    
  display "Est#" TRIM(xest.est-no) FORMAT "x(8)"
          "SlsRep:" sman.sname when avail sman
          "UserID:" xest.updated-id
          "Prober:" probe.probe-user
          cJobNo FORMAT "X(20)"
          skip
          "Cust:" xeb.cust-no
                  cust-ad[1] FORMAT "x(29)" TO 44
          "Ship:" ship-ad[1] FORMAT "x(29)" TO 80 SKIP
      with no-labels no-box frame qwqw STREAM-IO.
 
  if cust-ad[2] ne "" or ship-ad[2] ne "" then
    put cust-ad[2] FORMAT "x(29)" TO 44
        ship-ad[2] FORMAT "x(29)" TO 80 SKIP.
  if cust-ad[3] ne "" OR ship-ad[3] ne "" then
    put cust-ad[3] FORMAT "x(29)" TO 44
        ship-ad[3] FORMAT "x(29)" TO 80 SKIP.
  if cust-ad[4] ne "" OR ship-ad[4] ne "" then
    put cust-ad[4] FORMAT "x(29)" TO 44
        ship-ad[4] FORMAT "x(29)" TO 80 SKIP.
  IF cerunf = "ASI" THEN v-header = "   Qty      --- Desc/FG Item ----- -- Size / Color ----- --- Style / Part No ---".
    ELSE v-header = "   Qty      --- Description ------ -- Size / Color ----- --- Style / Part No ---".
  DISPLAY skip(1)
    v-header FORMAT "x(80)"

    qty format ">>>,>>>,>>>"
    dsc[1] FORMAT "x(22)"
    sizcol[1] FORMAT "x(21)"
    stypart[1] FORMAT "x(23)" SKIP
    space(12)
    dsc[2] FORMAT "x(22)"
    sizcol[2] FORMAT "x(21)"
    stypart[2] FORMAT "x(23)" /*SKIP
    SPACE(12)
    v-i-no FORMAT "x(22)"       */
/*    SKIP(1)*/
/*    IF cerunf = "ASI" AND v-2desc THEN SPACE(12) v-i-no FORMAT "x(22)"*/
    with no-box no-labels color value("blu/brown") width 80 frame aa1 STREAM-IO.
    IF cerunf = "ASI" AND v-2desc THEN      
       PUT  SPACE(12) v-i-no FORMAT "x(22)" SKIP(1).
    ELSE
       PUT SKIP(1).
      
  display
    space(15) "Width    Length   Sq.Inches  Sq.Feet/Sheet    Weight per Units"
    skip
    " Blank Size:" brd-w[1] TO 21 brd-l[1] TO 30 brd-sq[1] TO 42
    " # up:" xeb.num-up FORMAT ">>>9" brd-wu[1] TO 70 space(0) "/M     " skip
    "   Die Size:" brd-w[2] TO 21 brd-l[2] TO 30 brd-sq[2] TO 42
                   /*xef.trim-l when v-layout @ brd-w[2]
                   xef.trim-w when v-layout @ brd-l[2]*/
    brd-sf[2] TO 52 "Sf/Sht"  brd-wu[2] TO 70 space(0) "/M Shts" skip
    with no-box no-labels width 80 frame aa4 DOWN STREAM-IO.

  if cerunf ne "HOP" then display
    "  Feed Size:" lv-brd-w TO 21 lv-brd-l TO 30 lv-brd-sq TO 42
    " #out:" xef.n-out * xef.n-out-l FORMAT ">>9" lv-brd-wu TO 70 space(0) "/M Shts" skip
    with no-box no-labels width 80 frame aa2 DOWN STREAM-IO.

  display
    " Sheet Size:" brd-w[3] TO 21 brd-l[3] TO 30 brd-sq[3] TO 42
    brd-sf[3] TO 52 "Sf/Sht"  brd-wu[3] TO 70 space(0) "/M Shts" skip
    " Roll Size :"                    WHEN brd-l[4] ne 0
    brd-w[4]  TO 21  WHEN brd-l[4] ne 0
    brd-sf[4] TO 52 "MSF"
    brd-wu[4] TO 70 "Tons"  skip(1)
    "Materials            Weight Caliper          QTY/Unit  MR $  Matl$/M    TOTAL"
    skip
    with no-box no-labels width 80 frame aa3 DOWN STREAM-IO.

  /* board */     run ce/pr4-brd.p (v-vend-no).
  v-brd-cost = dm-tot[5].

  /* i n k s */   run ce/pr4-ink.p.

  /* films */     run ce/pr4-flm.p.

  /* cas/tr/pal*/ run ce/pr4-cas.p.

  /* special */   run ce/pr4-spe.p.

  do with frame ac5 no-labels no-box:
    display "TOTAL  DIRECT  MATERIALS "
            dm-tot[3] format ">>>9.99" TO 59
            dm-tot[4] format ">>>>9.99" TO 68
            dm-tot[5] format ">,>>>,>>9.99" TO 80 skip(1) WITH STREAM-IO.
  end.

  run ce/pr4-prp.p . /* Do Prep Charges */
  run ce/pr4-mis.p .

  put skip(1)
    "Machine Description    MR (Hrs) Run  Speed    Rate   MR $    Run $    Total Cost" .

  run ce/pr4-mch.p.
  if ctrl2[2] ne 0 or ctrl2[3] ne 0 then do:
    put "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) TO 80 skip.
    op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
  end.

  find first carrier
      where carrier.company eq cocode
        and carrier.loc     eq locode
        and carrier.carrier eq xeb.carrier
      no-lock no-error.
  if avail carrier then
  find first carr-mtx
      where carr-mtx.company  eq cocode
        and carr-mtx.loc      eq locode
        and carr-mtx.carrier  eq carrier.carrier
        and carr-mtx.del-zone eq xeb.dest-code
      no-lock no-error.

  find first item
      {sys/look/itemW.i}
        and item.i-no     eq xef.board
        and item.mat-type eq "B"
        and item.avg-w    gt 0
      no-lock no-error.

  ASSIGN
  v-msf = (xeb.t-sqin - v-t-win) * qty / 144000 /* msf */
  v-msf = v-msf * if avail item then item.avg-w else 1
  xxx   = v-msf * xef.weight.

  if xef.medium ne "" then do:
    find first item
        {sys/look/itemW.i}
          and item.i-no eq xef.medium
        no-lock no-error.
    if avail item then do:
        /*override item shrink % with shrink entered in BOM button on Layout screen*/
          IF xef.spare-dec-1 NE 0 
              THEN dShrink = xef.spare-dec-1.
              ELSE dShrink = ITEM.shrink.
        xxx = xxx + (item.basis-w * (1 - (dShrink / 100)) * v-msf).
    END.
  end.

  if xef.flute ne "" then do:
    find first item
        {sys/look/itemW.i}
          and item.i-no eq xef.flute
        no-lock no-error.
    if avail item then xxx = xxx +
                             (item.basis-w * v-msf).
  end.

  if xef.lam-code ne "" then do:
    find first item
        {sys/look/itemW.i}
          and item.i-no eq xef.lam-code
        no-lock no-error.
    if avail item then xxx = xxx +
                             ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                              qty * xef.adh-sqin / xeb.num-up / item.sqin-lb).
  end.

  if xef.adh-code ne "" then do:
    find first item
        {sys/look/itemW.i}
          and item.i-no eq xef.adh-code
        no-lock no-error.
    if avail item then xxx = xxx +
                             ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                              qty * xef.adh-sqin / xeb.num-up / item.sqin-lb).
  end.

  FOR EACH brd,
      FIRST item NO-LOCK
        {sys/look/itemW.i}
          AND item.i-no EQ brd.i-no
          AND CAN-DO("5,6",item.mat-type):

    xxx = xxx + (brd.qty / 100 * item.weight-100).
  END.

  assign
   xxx = xxx + (p-qty * ce-ctrl.def-pal-w) +
               (c-qty * ce-ctrl.def-cas-w) /* add pallet & case */
   fr-tot = 0
   fg-wt  = xxx.

  if xeb.fr-out-c ne 0 then
    fr-tot = xeb.fr-out-c * xxx / 100.
    
  else
  if xeb.fr-out-m ne 0 then
    fr-tot = xeb.fr-out-m * qty / 1000.
    
  else  
  if avail carr-mtx then do:
    if carrier.chg-method eq "P" then
    do i = 1 to 10:
      fr-tot = carr-mtx.rate[i] * p-qty.
      if carr-mtx.weight[i] ge p-qty then leave.
    end.
    
    else
    if carrier.chg-method eq "W" then
    do i = 1 to 10:
      fr-tot = carr-mtx.rate[i] * xxx / 100.
      if carr-mtx.weight[i] ge xxx then leave.
    end.
    
    else do:
      find first item
          {sys/look/itemW.i}
            and item.i-no  eq xef.board
            and item.avg-w gt 0
          no-lock no-error.
      v-msf = v-msf * if avail item then item.avg-w else 1.
      
      do i = 1 to 10:
        fr-tot = carr-mtx.rate[i] * v-msf.
        if carr-mtx.weight[i] ge v-msf then leave.
      end.
    end.
       
    if fr-tot lt carr-mtx.min-rate then fr-tot = carr-mtx.min-rate.
      
     fr-tot = fr-tot + (carr-mtx.min-rate * (rels[k] - 1)).
  end.

  {sys/inc/roundup.i fg-wt}
  if (fg-wt / 100) * fg-rate-f ne 0 then
    put "Finished Goods Handling" (fg-wt / 100) * fg-rate-f TO 80 skip.

  assign
   op-tot[5] = op-tot[5] + ((fg-wt / 100) * fg-rate-f)
   ctrl2[2]  = ctrl2[2] + ((fg-wt / 100) * fg-rate-f).

  put "TOTAL  OPERATIONS        "
    op-tot[3] format ">>>>9.99"     TO 57
    op-tot[4] format ">>>>>>9.99"   TO 68
    op-tot[5] format ">,>>>,>>9.99" TO 80 skip(1).

  IF cerunf EQ "HOP" THEN DO:
    FOR EACH brd
        WHERE CAN-FIND(FIRST item
                       WHERE item.company EQ xest.company
                         AND item.i-no    EQ brd.i-no
                         AND CAN-DO("B,P,R,1,2,3,4",item.mat-type)):
      ACCUM brd.qty (TOTAL).
      ACCUM brd.qty-mr + brd.qty-wst (TOTAL).
    END.
    PUT "Total Waste Percentage"
        (ACCUM TOTAL brd.qty-mr + brd.qty-wst) / (ACCUM TOTAL brd.qty) * 100
                                    FORMAT ">>,>>9.99" TO 80
        SKIP(1).
  END.
  
  /* mat */
  do i = 1 TO 6:
    ctrl[9] = ce-ctrl.mat-pct[i] / 100.
    if ce-ctrl.mat-cost[i] gt dm-tot[5] then leave.
  end.
  /* lab */
  do i = 1 TO 6:
    ctrl[10] = ce-ctrl.lab-pct[i] / 100.
    if ce-ctrl.lab-cost[i] gt op-tot[5] then leave.
  end.
  DO TRANSACTION:
    {est/calcpcts.i xest}
    ASSIGN
     calcpcts.val[1] = ctrl[9] * 100
     calcpcts.val[2] = v-brd-cost.
    FIND CURRENT calcpcts NO-LOCK NO-ERROR.
  END.

  ASSIGN
   gsa-mat = ctrl[9]  * 100
   gsa-lab = ctrl[10] * 100
   gsa-com = ce-ctrl.comm-mrkup
   gsa-war = ce-ctrl.whse-mrkup.

  FIND FIRST reftable-fm NO-LOCK
       WHERE reftable-fm.reftable EQ "gsa-fm"
         AND reftable-fm.company  EQ xest.company
         AND reftable-fm.loc      EQ ""
         AND reftable-fm.code     EQ xest.est-no
       NO-ERROR.

  IF AVAIL reftable-fm THEN
     gsa-fm = reftable-fm.val[1].
  ELSE
     gsa-fm  = ctrl[19].

  OUTPUT CLOSE.

  RUN ce/gsa.p (ROWID(probe), qtty[k], rels[k]).

  SESSION:SET-WAIT-STATE("general").

  ASSIGN
   ctrl[9]  = gsa-mat / 100
   ctrl[10] = gsa-lab / 100
   ctrl[1]  = gsa-war / 100
   ctrl[19] = gsa-fm / 100
   vmcl = k.

  output to value(outfile1) append .
  run ce/pr4-tots.p .
  output close.

  assign
   v-prep-mat = tprep-mat
   v-prep-lab = tprep-lab.

  run ce/pr4-mis2.p.
  v-dec = qtty[k] / (xeb.num-up * xef.n-out * xef.n-out-l).
  {sys/inc/roundup.i v-dec}
  IF xef.gsh-qty EQ 0 THEN 
  do transaction:
    xef.gsh-qty = v-dec + spo + r-spo[1].
  end.
  run ce/probemk.p (ROWID(probe)).

  find first blk where blk.id eq xeb.part-no no-error.
  find first xjob
      where xjob.i-no eq blk.id
        and xjob.qty  eq qtty[k]
      no-error.
  if not avail xjob then do:
    create xjob.
    assign
     xjob.i-no     = blk.id
     xjob.qty      = qtty[k]
     xjob.cust-no  = xeb.cust-no
     xjob.form-no  = xeb.form-no
     xjob.blank-no = xeb.blank-no
     xjob.pct      = 1.00
     xjob.stock-no = xeb.stock-no.
  end.

  assign
   xjob.mat = (dm-tot[5]   + mis-tot[1] + v-prep-mat) / (qtty[k] / 1000)
   xjob.lab = (opsplit$[1] + mis-tot[3] + v-prep-lab + ctrl2[2] + ctrl2[3]) /
              (qtty[k] / 1000)
   xjob.voh = opsplit$[2]                             / (qtty[k] / 1000)
   xjob.foh = opsplit$[3]                             / (qtty[k] / 1000).

  if not do-speed and not do-mr and xest.est-qty[1] eq qtty[k] then
  for each xop where xop.company = xest.company AND xop.est-no eq xest.est-no
                 and xop.line gt 500
                 transaction:
      find first est-op where est-op.company = xop.company
                          AND est-op.est-no eq xop.est-no
                          and est-op.line  eq xop.line - 500 no-error.
      if avail est-op then est-op.num-sh = xop.num-sh.
  end.

  if not vprint then DO TRANSACTION:

    IF probe.LINE LT 100 THEN
    DO:
       if opsys = "unix" then
         unix silent rm value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"99")).
       else
         dos silent del value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"99")).
    END.
    ELSE
    DO:
       if opsys = "unix" then
         unix silent rm value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"999")).
       else
         dos silent del value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"999")).
    END.

    FIND CURRENT probe.
    DELETE probe.
  end.
end.  /* do k=1to28 */

if vprint then do k = 1 to 28:
  if qtty[k] eq 0 then leave.

  FOR EACH bf-probe
      WHERE bf-probe.company    EQ xest.company
        AND bf-probe.est-no     EQ xest.est-no
        AND bf-probe.probe-date EQ TODAY
        AND bf-probe.est-qty    EQ qtty[k]
        AND bf-probe.freight    EQ rels[k]
      NO-LOCK
      BY bf-probe.probe-time DESC:
    LEAVE.
  END.

  IF bf-probe.LINE LT 100 THEN
     assign
       outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(bf-probe.line,"99")
       outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(bf-probe.line,"99")
       outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(bf-probe.line,"99").
  ELSE
     assign
       outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(bf-probe.line,"999")
       outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(bf-probe.line,"999")
       outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(bf-probe.line,"999").
      
  if vmclean then do:
    output to value(outfile3) append.

    assign
     vmcl = if k lt 6  then 1  else
            if k lt 11 then 6  else
            if k lt 16 then 11 else
            if k lt 21 then 16 else
            if k lt 28 then 21 else 28
     vhld = vmcl.
 
    {ce/mclean.i vmcl}
    
    put skip.

    output close.
  end.

  if opsys = "unix" then
    unix silent cat value(outfile2) >> value(outfile3).
  else
    dos silent type value(outfile2) >> value(outfile3).

  IF bf-probe.LINE LT 100 THEN
     ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(bf-probe.line,"99").
  ELSE
     ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(bf-probe.line,"999").

  if search(outfile1) <> ? THEN
    dos silent  type value(outfile3) > value(ls-outfile).

  RUN ce/probeu3.p (ROWID(bf-probe)).
end.

DO TRANSACTION:
  FIND CURRENT op-lock NO-ERROR.
  IF AVAIL op-lock THEN DELETE op-lock.
END.

SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 1992  advanced software, inc. */
