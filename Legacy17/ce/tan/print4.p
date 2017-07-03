/* ------------------------------------------------- ce/tan/print4.p 10/94 gb */

{sys/inc/var.i shared}

def     shared buffer xest for est.
def     shared buffer xef  for ef.
def     shared buffer xeb  for eb.
def    buffer xeb2 for eb.
DEF BUFFER reftable-fm FOR reftable.
DEF BUFFER reftable-fold-pct FOR reftable.
def new shared buffer xop for est-op.
DEF BUFFER bf-ef FOR ef.
DEF BUFFER bf-est FOR est.
{ce/print4.i shared "new shared"}

def new shared var v-prep-mat like tprep-mat NO-UNDO.
def new shared var v-prep-lab like tprep-lab NO-UNDO.

def var v-num-on as INT NO-UNDO.
def var v-layout as LOG NO-UNDO.
def var v-msf as DEC NO-UNDO.
DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.
DEF NEW SHARED VAR DAY_str AS cha FORM "x(8)" NO-UNDO.
DEF NEW SHARED VAR tim_str AS cha FORM "x(8)" NO-UNDO.
DEF NEW SHARED VAR tmp-dir AS cha NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF VAR lv-error AS LOG NO-UNDO.
DEF VAR ls-outfile AS cha NO-UNDO.
DEF VAR v-line LIKE probe.line no-undo.
def var v-vend-no   like e-item-vend.vend-no init "" NO-UNDO.

DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>9.9<<<<" NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
def var v-module as char format "x(60)" no-undo.
def var v-brd-cost as dec no-undo.
DEF VAR lv-override AS LOG NO-UNDO.
DEF VAR ld-fg-rate AS DEC NO-UNDO.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.
DEFINE VARIABLE dShrink AS DECIMAL     NO-UNDO.

IF xest.metric THEN
  ASSIGN
   ld-metric = 25.4
   lv-format = "->>,>>>mm".

IF vprint THEN DO:
  FIND FIRST probe
      WHERE probe.company EQ xest.company
        AND probe.est-no  EQ xest.est-no
      NO-LOCK NO-ERROR.
  IF AVAIL probe THEN RUN est/d-probeu.w (OUTPUT lv-override).
END.

{cec/get-vend.i}  /* get vendor number */
{sys/inc/ceprepprice.i}

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "CEBROWSE"
    no-lock no-error.

  if not avail sys-ctrl then DO TRANSACTION:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "CEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.
        
  end.

IF sys-ctrl.char-fld NE "" THEN
   tmp-dir = sys-ctrl.char-fld.
ELSE
   tmp-dir = "users\".

IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
   tmp-dir = tmp-dir + "\".

tmp-dir = REPLACE(tmp-dir,"/","\").

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
assign
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

if retry then output close.

find first xef where xef.company = xest.company 
                 AND xef.est-no = xest.est-no.
find first xeb where xeb.company = xest.company
                 AND xeb.est-no = xest.est-no.
find first xop where xop.company = xest.company
                 AND xop.est-no = xest.est-no and
                     xop.op-speed = 0 no-lock no-error.

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
  vmclean = lookup(cerunf,"McLean,HOP") gt 0.

  {ce/msfcalc.i}

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEPg2"
      NO-LOCK NO-ERROR.
  IF NOT avail sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company = cocode
     sys-ctrl.name    = "CEPg2"
     sys-ctrl.descrip = "Reverse W & L Labels for press, die, & # Up on Estimate".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  END.
  v-layout = sys-ctrl.log-fld.
end.

if vprint THEN DO:
  RUN ce/tan/selwhif.w (INPUT-OUTPUT do-gsa, OUTPUT lv-error).

  if lv-error then return error.

  IF lv-override THEN
  for each probe where probe.company = xest.company and
                       probe.est-no = xest.est-no:
     delete probe.                 
  end.
END.

/*do with frame ask row 13 centered overlay no-labels:
  update skip(1) space(3) "     Override GS&A Percentages?" do-gsa skip(1).
end.
hide frame ask no-pause.
*/
   
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
  FIND xest WHERE RECID(xest) EQ RECID(bf-est).   
END.
/*
if program-name(2) begins "jc/jc-calc." then do:
  {sys/msg/jobstd.i jobstd1}
end.

else
if program-name(2) begins "ce/tan/probe." or
   program-name(2) begins "oe/"           then do:
  {sys/msg/calc.i kalk1}
end.
*/
SESSION:SET-WAIT-STATE("general").
find first sman   where   sman.sman    = xeb.sman no-lock no-error.
find first cust   where   cust.company = cocode and
  cust.cust-no = xeb.cust-no no-lock no-error.
find first shipto where shipto.cust-no = cust.cust-no and
  shipto.ship-no = xeb.ship-no no-lock no-error.
find first style  where  style.company = cocode and
  style.style = xeb.style no-lock no-error.
assign
  cust-ad[1] = cust.name cust-ad[2] = cust.addr[1] cust-ad[3] = cust.addr[2]
  cust-ad[4] = cust.city + ", " + cust.state + " " + cust.zip.

if cust.cust-no ne "Temp"
  then
assign
  cust-ad[1] = cust.name cust-ad[2] = cust.addr[1] cust-ad[3] = cust.addr[2]
  cust-ad[4] = cust.city + ", " + cust.state + " " + cust.zip.
else
assign
  cust-ad[1] = xeb.ship-name cust-ad[2] = xeb.ship-addr[1]
  cust-ad[3] = xeb.ship-addr[2]
  cust-ad[4] = xeb.ship-city + ", " + xeb.ship-state + " " + xeb.ship-zip.
assign
  ship-ad[1] = shipto.ship-name ship-ad[2] = shipto.ship-addr[1]
  ship-ad[3] = shipto.ship-addr[2]
  ship-ad[4] = shipto.ship-city + ", " + shipto.ship-state + " "
  + shipto.ship-zip.

if cust-ad[3] = "" then
assign cust-ad[3] = cust-ad[4] cust-ad[4] = "".
if cust-ad[2] = "" then
assign
  cust-ad[2] = cust-ad[3] cust-ad[3] = cust-ad[4] cust-ad[4] = "".
if ship-ad[3] = "" then
assign ship-ad[3] = ship-ad[4] ship-ad[4] = "".
if ship-ad[2] = "" then
  assign ship-ad[2] = ship-ad[3] ship-ad[3] = ship-ad[4] ship-ad[4] = "".
if xeb.ship-no = 1 then
  assign ship-ad[1] = "SAME" ship-ad[2] = "" ship-ad[3] = "" ship-ad[4] = "".
dsc[1] = xeb.part-dscr1.
dsc[2] = xeb.part-dscr2.

assign
 ld-len   = xeb.len * ld-metric
 ld-wid   = xeb.wid * ld-metric
 ld-dep   = xeb.dep * ld-metric.

IF ld-metric NE 1 THEN DO:
  {sys/inc/roundup.i ld-len}
  {sys/inc/roundup.i ld-wid}
  {sys/inc/roundup.i ld-dep}
END.

sizcol[1] = trim(string(ld-len,lv-format)) + "x" +
            trim(string(ld-wid,lv-format)) + "x" +
            trim(string(ld-dep,lv-format)).
sizcol[2] = xeb.i-coldscr.
stypart[1] = style.dscr.
stypart[2] = xeb.part-no.
brd-l[1] = xeb.t-len.
brd-l[2] = xef.trim-l.
if xef.roll = yes then
brd-l[4] = xef.trim-l.
brd-w[1] = xeb.t-wid.
brd-w[2] = xef.trim-w.

if cerunf eq "HOP" then
  assign
   brd-l[3] = xef.nsh-len
   brd-w[3] = xef.nsh-wid.
else
  assign
   brd-l[3] = xef.gsh-len
   brd-w[3] = xef.gsh-wid.

if brd-l[3] = 0 and brd-w[3] = 0 then
assign
 brd-l[3] = lsh-len
 brd-w[3] = lsh-wid.
if xef.roll = yes then
brd-w[4] = xef.roll-wid.
else
brd-w[4] = 0.
brd-sq[1] = xeb.t-sqin.
brd-sq[2] = xef.trim-l * xef.trim-w.
brd-sq[3] = brd-l[3] * brd-w[3].
brd-sq[4] = brd-l[4] * brd-w[4].
if v-corr then
  assign
   brd-sf[1] = brd-sq[1] * .007
   brd-sf[2] = brd-sq[2] * .007
   brd-sf[3] = brd-sq[3] * .007.
else
  assign
   brd-sf[1] = brd-sq[1] / 144
   brd-sf[2] = brd-sq[2] / 144
   brd-sf[3] = brd-sq[3] / 144.

v-num-on = xeb.num-up *
           (if xef.n-out   eq 0 then 1 else xef.n-out) *
           (if xef.n-out-l eq 0 then 1 else xef.n-out-l).


do transaction:
  /* take out window if any */
  call_id = recid(xeb).
  find xeb where recid(xeb) eq call_id no-error.
  xeb.t-win = 0.

  do i = 1 to 4:
    find first item
        {sys/look/itemW.i}
          and item.i-no eq xef.leaf[i]
        no-lock no-error.
    if avail item and item.mat-type eq "W" and
       (xef.leaf-l[i] ne 0 and xef.leaf-w[i] ne 0) then
      xeb.t-win = xeb.t-win + (xef.leaf-l[i] * xef.leaf-w[i]).
  end.
  find xeb where recid(xeb) eq call_id no-lock no-error.
end.

brd-wu[1] = xeb.t-sqin - xeb.t-win.
find first item {sys/look/itemW.i} and
  item.i-no = xef.board no-lock no-error.
brd-wu[1] = (if v-corr then (brd-wu[1] * .007) else (brd-wu[1] / 144))
                              * item.basis-w.
brd-wu[2] = brd-sf[2]         * item.basis-w.
brd-wu[3] = brd-sf[3]         * item.basis-w.
format day_str v-module tim_str to 79  skip(2)
  with frame hdr page-top width 80 no-labels no-box.

/******************************* l  o  o  p  **********************************/
k = 0.
loupe:
for each xef where xef.company = xest.company
               AND xef.est-no = xest.est-no :
  k = k + 1.
  
  do transaction:
    for each est-op where est-op.company = xest.company
                      AND est-op.est-no = xest.est-no and est-op.line gt 500:
      delete est-op.
    end.
    for each est-op where est-op.company = xest.company
                      AND est-op.est-no = xest.est-no and est-op.line lt 500:
      create xop.
      buffer-copy est-op to xop
      assign
       xop.line = est-op.line + 500.
    end.
  end.
  
  qty = xest.est-qty[1].
  
  if qty = 0 then leave loupe.
  run ce/tan/prokalk.p.  /* CTS back from ce/prokalk.p */
  
  qty = xest.est-qty[1].
  find first item {sys/look/itemW.i} and item.i-no = xef.board no-lock no-error.
  if avail item then find first e-item of item no-lock no-error.
  brd-sf[4] = (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                         else (xef.gsh-len * xef.gsh-wid / 144)) *
              xef.gsh-qty / 1000.
  brd-wu[4] = (brd-sf[4] * item.basis-w) / 2000.     /* total tons */

  {est/probeset.i qty 0}

  ASSIGN
   v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
   outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,v-probe-fmt)
   outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,v-probe-fmt)
   outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,v-probe-fmt).

  output to value(outfile1).

  ASSIGN
   day_str  = STRING(TODAY,"99/99/9999")
   tim_str  = STRING(TIME,"hh:mm am") 
   v-module = IF cerunf EQ "HOP" THEN "FCD-0101" ELSE ""
   v-module = FILL(" ",59 - LENGTH(TRIM(v-module))) + TRIM(v-module).

  display day_str v-module tim_str with frame hdr STREAM-IO.

  display
    "Tandem Estimate#" TRIM(xest.est-no) FORMAT "x(8)"
    "SalesRep:" to 49 sman.sname when avail sman
    skip "Customer:" cust-ad[1]   "Ship To:"  to 49  ship-ad[1] skip
    with no-labels no-box frame qwqw STREAM-IO.
  if cust-ad[2] ne "" OR ship-ad[2] ne "" then
  put
    space(10)   cust-ad[2]         " "   to 49  ship-ad[2] skip.
  if cust-ad[3] ne "" OR ship-ad[3] ne "" then
  put
    space(10)   cust-ad[3]         " "   to 49  ship-ad[3] skip.
  if cust-ad[4] ne "" OR ship-ad[4] ne "" then
  put
    space(10)   cust-ad[4]         " "   to 49  ship-ad[4] skip.
  display skip(1)
    "    Qty     --- Description ------ -- Size / Color ----- --- Style / Part No ---"
    with no-box no-labels  width 80 frame aa1 STREAM-IO.
  for each xeb2 where xeb.company = xest.company
                  AND xeb2.est-no = xest.est-no NO-LOCK:
    put xeb2.bl-qty format ">>>,>>>,>>9" space(1) xeb2.part-dscr1 format "x(22)"
      space(1) xeb2.i-coldscr space(2) xeb2.part-no skip.
  end.
  put xest.est-qty[1] format ">>>,>>>,>>9" " TOTAL    " space(14)
      sizcol[1] format "x(21)" space(1) stypart[1] format "x(23)" skip(1).
  display
    space(15) " Width   Length   Sq.Inches  Sq.Feet/Sheet    Weight per Units" skip
    "Blank Size :" brd-w[1] to 21 brd-l[1] to 30 brd-sq[1] to 42
    " # up:" xeb.num-up brd-wu[1] to 70 space(0) "/M Blks" skip
    " Die  Size :" brd-w[2] to 21 brd-l[2] to 30 brd-sq[2] to 42
    brd-sf[2] to 52 "Sf/Sht"  brd-wu[2] to 70 space(0) "/M Shts" skip
                   /*xef.trim-l when v-layout @ brd-w[2]
                   xef.trim-w when v-layout @ brd-l[2]*/
    "Sheet Size :" brd-w[3] to 21 brd-l[3] to 30 brd-sq[3] to 42
    brd-sf[3] to 52 "Sf/Sht"  brd-wu[3] to 70 space(0) "/M Shts" skip
    "Roll  Size :" /* brd-l[4] to 30*/  when brd-w[4] ne 0
    brd-w[4] to 21       when brd-w[4] ne 0
    /*   brd-sq[4] to 42  when brd-l[4] ne 0 */
    "Total Board =" to 44
    brd-sf[4] AT 46  "MSF"
    brd-wu[4] to 70  "Tons" skip(1)
    "Materials            Weight Caliper     QTY/Unit        MR $  Matl$/M    TOTAL"

    skip with no-box no-labels width 80 frame aa down STREAM-IO.

  /* board                   */ run ce/tan/pr4-brd.p (v-vend-no).
  v-brd-cost = v-brd-cost + dm-tot[5].

  /* medium-flute-laminate   */ run ce/tan/pr4-mfl.p.

  /* i n k s                 */ run ce/tan/pr4-ink.p.

  /* films                   */ run ce/tan/pr4-flm.p.

  /* case/tray/pallet        */ run ce/tan/pr4-cas.p.

  /* special                 */ run ce/tan/pr4-spe.p.

  do with frame ac5 no-labels no-box:
    display "TOTAL DIRECT MATERIALS "
      dm-tot[3] format ">>>>9.99"     to 59
      dm-tot[4] format ">>>>9.99"     to 68
      dm-tot[5] format ">,>>>,>>9.99" to 80 skip(1) WITH  STREAM-IO.
  end.

  /* prep */
  assign
   tprep-mat = 0
   tprep-lab = 0
   tprep-tot = 0.

  for each est-prep 
      where est-prep.company = xest.company and
            est-prep.est-no = xest.est-no
        AND index("IM",est-prep.simon) gt 0
        and est-prep.code ne ""
      no-lock
      break by est-prep.est-no
      with frame ad down no-labels no-box:
      
    if first-of(est-prep.est-no) then
      put "Prep Description"
          "Mat'l"             to 30
          "Labor"             to 40
          "Addt'l"            to 50
          "Amtz"              to 60
          "Cost/M"            to 69
          "Total Cost"        to 80
          skip.
    
    assign
     prep-add = est-prep.mkup / 100
     prep-atz = est-prep.amtz / 100.
   
    if est-prep.ml then
      assign
       prep-lab = 0
       prep-mat = est-prep.cost * est-prep.qty.
     
    else
      assign
       prep-mat = 0
       prep-lab = est-prep.cost * est-prep.qty.
    
    IF est-prep.simon = 'M' THEN DO:
        ASSIGN 
            prep-tot = prep-mat + prep-lab
            tprep-mat = tprep-mat + prep-mat
            tprep-lab = tprep-lab + prep-lab
            .
        dMCostToExcludePrep = dMCostToExcludePrep + prep-tot.
        IF ceprepprice-chr EQ 'Profit' THEN 
            dMPriceToAddPrep = dMPriceToAddPrep + prep-tot / (1 - prep-add) * prep-atz.
        ELSE 
            dMPriceToAddPrep = dMPriceToAddPrep + prep-tot * (1 + prep-add) * prep-atz.
        END.
    ELSE IF ceprepprice-chr EQ "Profit" THEN
        assign
            prep-tot  = (prep-mat + prep-lab) / (1 - prep-add) * prep-atz
            tprep-mat = tprep-mat + (prep-mat / (1 - prep-add) * prep-atz)
            tprep-lab = tprep-lab + (prep-lab / (1 - prep-add) * prep-atz).
    ELSE
       assign
          prep-tot  = (prep-mat + prep-lab) * (1 + prep-add) * prep-atz
          tprep-mat = tprep-mat + (prep-mat * (1 + prep-add) * prep-atz)
          tprep-lab = tprep-lab + (prep-lab * (1 + prep-add) * prep-atz).

    tprep-tot = tprep-tot + prep-tot.

    create xprep.
    assign
     xprep.frm      = est-prep.s-num
     xprep.blank-no = est-prep.b-num
     xprep.qty      = est-prep.qty
     xprep.std-cost = est-prep.cost
     xprep.ml       = est-prep.ml
     xprep.cost-m   = prep-tot / (qty / 1000)
     xprep.simon    = est-prep.simon
     xprep.code     = est-prep.code.

    display est-prep.dscr             format "x(20)"
            prep-mat                  format "->>>>>9.99"   to 30
            prep-lab                  format "->>>>>9.99"   to 40
            est-prep.mkup             format "  >>9.99%"   to 50
            est-prep.amtz             format "  >>9.99%"   to 60
            prep-tot / (qty / 1000)   format "->>>>9.99"    to 69
            prep-tot                  format "->>>>>>9.99" to 80
            SKIP WITH  STREAM-IO.
  end.

  /* misc. */
  run ce/tan/pr4-mis.p .

  put skip(1)
    "Machine Description    MR (Hrs) Run  Speed   Rate     MR $     Run $  Total Cost" .

  /* machines */
  run ce/tan/pr4-mch.p.

  if ctrl2[2] ne 0 OR ctrl2[3] ne 0 then
  do:
    put "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) to 80 skip.
    op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
  end.
  find first carrier where carrier.company = cocode and carrier.loc = locode
    and carrier.carrier = xeb.carrier no-lock no-error.
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
      
  assign
   v-msf = (xeb.t-sqin - xeb.t-win) * qty / 144000
   v-msf = v-msf * if avail item then item.avg-w else 1
   xxx   = xef.weight * v-msf.

  if xef.medium ne "" then do:
    find first item {sys/look/itemW.i} and
               item.i-no = xef.medium no-lock no-error.
    if avail ITEM THEN DO:
    /*override item shrink % with shrink entered in BOM button on Layout screen*/
          IF xef.spare-dec-1 NE 0 
              THEN dShrink = xef.spare-dec-1.
              ELSE dShrink = ITEM.shrink.
        xxx = xxx +
               (item.basis-w * (1 - (dShrink / 100)) * v-msf).
    END.
    
  end.
  if xef.flute ne "" then do:
    find first item {sys/look/itemW.i} and
               item.i-no = xef.flute no-lock no-error.
    if avail item
    then xxx = xxx +
               (item.basis-w * v-msf).
  end.
  if xef.lam-code ne "" then do:
    find first item {sys/look/itemW.i} and
               item.i-no = xef.lam-code no-lock no-error.
    if avail item
    then xxx = xxx +
               ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                qty * xeb.t-sqin / item.sqin-lb).
  end.
  if xef.adh-code ne "" then do:
    find first item {sys/look/itemW.i} and
               item.i-no = xef.adh-code no-lock no-error.
    if avail item
    then xxx = xxx +
               ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                qty * xeb.t-sqin / item.sqin-lb).
  end.

  xxx = xxx + (p-qty * ce-ctrl.def-pal-w) +
              (c-qty * ce-ctrl.def-cas-w). /* add pallet & case */
  
  fr-tot = 0.
  
  if xeb.fr-out-c ne 0 then
    fr-tot = xeb.fr-out-c * (xxx / 100).
    
  else
  if xeb.fr-out-m ne 0 then
    fr-tot = xeb.fr-out-m * (xest.est-qty[1] / 1000).
   
  else
  if avail carr-mtx then do:
    if carrier.chg-method eq "W" then
    do i = 1 to 10:
      fr-tot = carr-mtx.rate[i] * xxx / 100.
      if carr-mtx.weight[i] ge xxx then leave.
    end.
    
    else
    if carrier.chg-method eq "P" then
    do i = 1 to 10:
      fr-tot = carr-mtx.rate[i] * p-qty.
      if carr-mtx.weight[i] ge p-qty then leave.
    end.
      
    else do:
      find first item
          {sys/look/itemW.i}
            and item.i-no     eq xef.board
            and item.mat-type eq "B"
            and item.avg-w    gt 0
          no-lock no-error.
      v-msf = v-msf * if avail item then item.avg-w else 1.
      
      do i = 1 to 10:
        fr-tot = carr-mtx.rate[i] * v-msf.
        if carr-mtx.weight[i] ge v-msf then leave.
      end.
    end.
    
    if fr-tot lt carr-mtx.min-rate then fr-tot = carr-mtx.min-rate.
  end.

  ld-fg-rate = IF xeb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate.
  
  if (xxx / 100) ne 0 then
    put "Finished Goods Handling" (xxx / 100) * ld-fg-rate to 80 skip.
    
  op-tot[5] = op-tot[5] + ((xxx / 100) * ld-fg-rate).

  put "TOTAL OPERATIONS        " op-tot[3] format ">>>>9.99" to 59
    op-tot[4] format ">>>>>9.99"    to 68
    op-tot[5] format ">,>>>,>>9.99" to 80 skip(1).

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
  do i = 1 to 6:
    ctrl[9] = ce-ctrl.mat-pct[i] / 100.
    if ce-ctrl.mat-cost[i] gt dm-tot[5]  then
    leave.
  end.
  /* lab */
  do i = 1 to 6:
    ctrl[10] = ce-ctrl.lab-pct[i] / 100.
    if ce-ctrl.lab-cost[i] gt op-tot[5]  then
    leave.
  end.
  DO TRANSACTION:
    {est/calcpcts.i xest}
    ASSIGN
     calcpcts.val[1] = ctrl[9] * 100
     calcpcts.val[2] = v-brd-cost.
    FIND CURRENT calcpcts NO-LOCK NO-ERROR.
  END.

  assign
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
     gsa-fm = ctrl[19].
  
  output close.

  hide frame kalk1 no-pause.
  hide frame jobstd1 no-pause.
  run ce/gsa.p (ROWID(probe), qty, 1).
  /*
  if program-name(2) begins "jc/jc-calc." then do:
    {sys/msg/jobstd.i jobstd}
  end.

  else
  if program-name(2) begins "ce/tan/probe." or
     program-name(2) begins "oe/"           then do:
    {sys/msg/calc.i kalk}
  end.
  */

  ASSIGN
     ctrl[9]  = gsa-mat / 100
     ctrl[10] = gsa-lab / 100
     ctrl[1]  = gsa-war / 100
     ctrl[19] = gsa-fm / 100.

  output to value(outfile1) append .
  run ce/pr4-tots.p .    /* use ce */
  output close.

  output to value(outfile2).

  find first xeb where xeb.company = xest.company
                   AND xeb.est-no eq xest.est-no
                   and xeb.form-no ne 0 no-error.

  assign
   v-prep-mat = tprep-mat
   v-prep-lab = tprep-lab.

  find first est-prep WHERE est-prep.company = xest.company
                        AND est-prep.est-no = xest.est-no
                        AND index("SON",est-prep.simon) gt 0
                        and est-prep.code ne ""
      no-lock no-error.
      
  if (xeb.chg-method ne "P" and fr-tot ne 0) or avail est-prep then
    put skip(1)
        space(24) "B I L L A B L E    C H A R G E S"
        skip
        "Prep Description"
        "Mat'l"             to 30
        "Labor"             to 40
        "Addt'l"            to 50
        "Amtz"              to 60
        "Cost/M"            to 69
        "Total Cost"        to 80
        skip.

  assign
   tprep-mat = 0
   tprep-lab = 0
   tprep-tot = 0.

  for each est-prep where est-prep.company = xest.company
                      AND est-prep.est-no = xest.est-no
                      AND index("SON",est-prep.simon) gt 0
        and est-prep.code ne ""
      with frame ag no-box no-labels:
      
    assign
     prep-add = est-prep.mkup / 100
     prep-atz = if est-prep.amtz ne 0 then est-prep.amtz / 100 else 1.
     
    if est-prep.ml then
      assign
       prep-lab = 0
       prep-mat = est-prep.cost * est-prep.qty.
       
    else
      assign
       prep-mat = 0
       prep-lab = est-prep.cost * est-prep.qty.
    
    
    IF ceprepprice-chr EQ "Profit" THEN
       assign
          prep-tot  = (prep-mat + prep-lab) / (1 - prep-add) * prep-atz
          tprep-mat = tprep-mat + (prep-mat / (1 - prep-add) * prep-atz)
          tprep-lab = tprep-lab + (prep-lab / (1 - prep-add) * prep-atz).
    ELSE
       assign
          prep-tot  = (prep-mat + prep-lab) * (1 + prep-add) * prep-atz
          tprep-mat = tprep-mat + (prep-mat * (1 + prep-add) * prep-atz)
          tprep-lab = tprep-lab + (prep-lab * (1 + prep-add) * prep-atz).

    tprep-tot = tprep-tot + prep-tot.

    create xprep.
    assign
     xprep.frm      = est-prep.s-num
     xprep.blank-no = est-prep.b-num
     xprep.qty      = est-prep.qty
     xprep.std-cost = est-prep.cost
     xprep.ml       = est-prep.ml
     xprep.cost-m   = prep-tot / (qty / 1000)
     xprep.simon    = est-prep.simon
     xprep.code     = est-prep.code.
     
    display est-prep.dscr           format "x(20)"      to 20
            prep-mat                format "->>>>>9.99"  to 30
            prep-lab                format "->>>>>9.99"  to 40
            est-prep.mkup           format "  >>9.99%"  to 50
            est-prep.amtz           format "  >>9.99%"  to 60
            prep-tot / (qty / 1000) format "->>>>9.99"   to 69
            prep-tot                format "->>>>>9.99" to 80
              when index("SO",est-prep.simon) gt 0
            "      N/C "
              when est-prep.simon eq "N" @ prep-tot
            SKIP WITH  STREAM-IO.
  end.

  if xeb.chg-method ne "P" and fr-tot ne 0 then do:
    put "Freight"
        fr-tot                      format ">>>9.99"    to 30
        (fr-tot / (qty / 1000))     format ">>>>9.99"   to 69
        fr-tot                      format ">>>,>>9.99" to 80
        skip.
          
    assign
     tprep-mat = tprep-mat + fr-tot
     tprep-tot = tprep-tot + fr-tot.
  end.

  do i = 1 to 6:
    if index("SON",xef.mis-simon[i]) eq 0 or xef.mis-cost[i] eq "" then next.
  
    put skip(1)
        "Miscellaneous Cost"
        "Mat/F"             to 30
        "Lab/F"             to 40
        "Mat/M"             to 50
        "Lab/M"             to 60
        "Mrkup%"            to 69
        "Total Cost"        to 80
        skip.
        
    leave.
  end.
  
  do i = 1 to 6 with frame ah down no-labels no-box:
    if index("SON",xef.mis-simon[i]) eq 0 or xef.mis-cost[i] eq "" then next.
    
    IF xef.mis-simon[i] = 'M' THEN DO:
        mis-tot[5] = xef.mis-matf[i] + (xef.mis-matm[i] * qty / 1000).
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[5].
        mis-tot[6] = xef.mis-labf[i] + (xef.mis-labm[i] * qty / 1000).
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[6].
        IF ceprepprice-chr EQ 'Profit' THEN 
            ASSIGN 
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] / (1 - (xef.mis-mkup[i] / 100))
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] / (1 - (xef.mis-mkup[i] / 100))
                .
        ELSE 
            ASSIGN 
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] * (1 + (xef.mis-mkup[i] / 100))
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] * (1 + (xef.mis-mkup[i] / 100))
                .
     END.
  ELSE IF ceprepprice-chr EQ "Profit" THEN
       ASSIGN
          mis-tot[5] = (xef.mis-matf[i] + (xef.mis-matm[i] * (qty / 1000))) /
                                          (1 - (xef.mis-mkup[i] / 100))
          mis-tot[6] = (xef.mis-labf[i] + (xef.mis-labm[i] * (qty / 1000))) /
                                          (1 - (xef.mis-mkup[i] / 100)).
    ELSE
       ASSIGN
          mis-tot[5] = (xef.mis-matf[i] + (xef.mis-matm[i] * (qty / 1000))) *
                                          (1 + (xef.mis-mkup[i] / 100))
          mis-tot[6] = (xef.mis-labf[i] + (xef.mis-labm[i] * (qty / 1000))) *
                                          (1 + (xef.mis-mkup[i] / 100)).

    if mis-tot[5] ne 0 then do:
      create xprep.
      assign
       xprep.frm      = xef.mis-snum[i]
       xprep.blank-no = xef.mis-bnum[i]
       xprep.qty      = 1
       xprep.std-cost = mis-tot[5]
       xprep.ml       = yes
       xprep.cost-m   = mis-tot[5] / (qty / 1000)
       xprep.simon    = xef.mis-simon[i]
       xprep.code     = "MISM" + string(i,"9").
    end.

    if mis-tot[6] ne 0 then do:
      create xprep.
      assign
       xprep.frm      = xef.mis-snum[i]
       xprep.blank-no = xef.mis-bnum[i]
       xprep.qty      = 1
       xprep.std-cost = mis-tot[6]
       xprep.ml       = no
       xprep.cost-m   = mis-tot[6] / (qty / 1000)
       xprep.simon    = xef.mis-simon[i]
       xprep.code     = "MISL" + string(i,"9").
    end.
                  
    display xef.mis-cost[i]         format "x(20)"
            xef.mis-matf[i]         format ">>,>>9.99"  to 30
            xef.mis-labf[i]         format ">>,>>9.99"  to 40
            xef.mis-matm[i]         format ">>,>>9.99"  to 50
            xef.mis-labm[i]         format ">>,>>9.99"  to 60
            xef.mis-mkup[i]         format " >>9.99%"   to 69
            mis-tot[5] + mis-tot[6] format ">>>,>>9.99" to 80
            SKIP WITH  STREAM-IO.
  end.
  
  put skip(2).
  
  output close.

  ASSIGN
     v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
     ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(probe.line,v-probe-fmt).
  
  run ce/tan/probemk.p (ROWID(probe)).
  
  if opsys = "unix" then
    unix silent cat value(outfile2) >> value(outfile3).
  else
    dos silent type value(outfile2) >> value(outfile3).

  if search(outfile1) <> ? then dos silent  type value(outfile3) > value(ls-outfile).

  RUN ce/probeu3.p (ROWID(probe)).

  v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

  if not vprint THEN DO TRANSACTION:
    if opsys = "unix" then
      unix silent rm value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,v-probe-fmt)).
    else
      dos silent del value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,v-probe-fmt)).

    FIND CURRENT probe.
    DELETE probe.
  END.
end.

DO TRANSACTION:
  FIND CURRENT op-lock NO-ERROR.
  IF AVAIL op-lock THEN DELETE op-lock.
END.

hide frame jobstd no-pause.
hide frame kalk no-pause.
hide frame ask no-pause.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
