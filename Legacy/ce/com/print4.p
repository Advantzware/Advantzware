/* -------------------------------------------------- ce/com/print4.p 10/94 gb*/
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-ef FOR ef.
DEF BUFFER reftable-fm FOR reftable.
DEF BUFFER reftable-fold-pct FOR reftable.
def new shared buffer xop for est-op.
DEF NEW SHARED VAR DAY_str AS cha FORM "x(10)" NO-UNDO.
DEF NEW SHARED VAR tim_str AS cha FORM "x(8)" NO-UNDO.
DEF NEW SHARED VAR tmp-dir AS cha NO-UNDO.
DEF VAR CALL_id AS RECID NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.
def var v-vend-no   like e-item-vend.vend-no init "" NO-UNDO.
DEF VAR ld-fg-amt AS DEC NO-UNDO.
DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
DEF VAR lv-eqty LIKE est-op.qty NO-UNDO.

{ce/print4.i shared "new shared"}

def var lv-brd-l           like eb.len no-undo.
def var lv-brd-w           like lv-brd-l no-undo.
def var lv-brd-sq          as dec format ">>>>9.9<<<<" no-undo.
def var lv-brd-sf          as dec format ">>>>>9.9<<"  no-undo.
def var lv-brd-wu          like lv-brd-sq no-undo.

def buffer xcar for car.

def new shared var v-summ as log init NO NO-UNDO.
def new shared var fr-tot-pre as dec.

def var v-layout  as log NO-UNDO.
def var v-blk-wt  as dec NO-UNDO.
def var v-avg-com as log NO-UNDO.
def var v-avg-tan as log NO-UNDO.
def var v-mat     as dec NO-UNDO.
def var v-lab     as dec NO-UNDO.
def var v-foh     as dec NO-UNDO.
def var v-voh     as dec NO-UNDO.
def var v-msf     as dec NO-UNDO.
DEF VAR lv-error AS LOG NO-UNDO.
DEF VAR ls-outfile AS cha NO-UNDO.
DEF VAR ls-probetime AS cha NO-UNDO.
DEF VAR v-line LIKE probe.line no-undo.
DEF VAR ll-tandem AS LOG NO-UNDO.

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
DEF VAR v-header AS CHAR INIT "   Qty      --- Desc/iption ------ -- Size / Color ----- --- Style / Part No ---" NO-UNDO.
DEF VAR v-2desc AS LOG NO-UNDO.
DEF VAR v-i-no LIKE xeb.stock-no.
DEF VAR cJobNo AS CHAR NO-UNDO.
DEF BUFFER bf-oe-ord FOR oe-ord.
DEF BUFFER bf-oe-ordl FOR oe-ordl.
DEF NEW SHARED TEMP-TABLE tt-rel NO-UNDO LIKE reftable.
DEF SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.
DEF VAR vofor2 AS cha NO-UNDO.
DEF VAR dBoardTotalQty AS INT NO-UNDO.
DEF VAR dBoardTotalAmt AS DEC NO-UNDO.
DEF VAR vMrWaste LIKE brd.qty-mr NO-UNDO.
DEF VAR vRunWaste LIKE brd.qty-wst NO-UNDO.
DEF VAR vBoardAmt LIKE brd.amount NO-UNDO.
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
               sys-ctrl.char-fld = "users\"
               sys-ctrl.int-fld = 30.
        
  end.

IF sys-ctrl.char-fld NE "" THEN
   tmp-dir = sys-ctrl.char-fld.
ELSE
   tmp-dir = "users\".

IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
   tmp-dir = tmp-dir + "\".

tmp-dir = REPLACE(tmp-dir,"/","\").

find first xef where xef.company = xest.company 
                 AND xef.est-no = xest.est-no.              
find first xeb where xeb.company = xest.company 
                 AND xeb.est-no   eq xest.est-no
                 and xeb.form-no eq xef.form-no.
find first xop where xop.company = xest.company 
                 AND xop.est-no    eq xest.est-no
                 and xop.op-speed eq 0
    no-lock no-error.

RUN ce/com/istandem.p (ROWID(xest), OUTPUT ll-tandem).

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
  vmclean = LOOKUP(cerunf,"McLean,HOP,CERunF 2") GT 0.

  {ce/msfcalc.i}

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEPg2"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO:
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

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "COMBCOST"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company = cocode
     sys-ctrl.name    = "COMBCOST"
     sys-ctrl.descrip = "Average Cost for Combination Items?" 
     sys-ctrl.log-fld = NO.
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  END.
  ASSIGN
   v-avg-com = sys-ctrl.log-fld
   v-avg-tan = sys-ctrl.int-fld EQ 0.
END.

EMPTY TEMP-TABLE tt-rel.

if vprint then do:
  RUN ce/com/selwhif.w (INPUT-OUTPUT do-speed, INPUT-OUTPUT do-mr,
                        INPUT-OUTPUT do-gsa, INPUT-OUTPUT v-summ,
                        INPUT NO, OUTPUT lv-error) NO-ERROR.

  if lv-error then return error.

  FOR EACH eb NO-LOCK
      WHERE eb.company EQ xest.company
        AND eb.est-no  EQ xest.est-no,
      FIRST reftable NO-LOCK
      WHERE reftable.reftable EQ "ce/com/selwhif1.w"
        AND reftable.company  EQ eb.company
        AND reftable.loc      EQ eb.est-no
        AND reftable.code     EQ STRING(eb.form-no,"9999999999")
        AND reftable.code2    EQ STRING(eb.blank-no,"9999999999"):
    CREATE tt-rel. 
    BUFFER-COPY reftable TO tt-rel.
  END.

  IF lv-override THEN
  for each probe where probe.company = xest.company and
                       probe.est-no = xest.est-no:
     delete probe.                 
  end.

  /*
  do on endkey undo, leave with frame ask row 13 CENTERED overlay no-labels:
    update skip(1)
           space(2)
           "Recalc Machines' Speed, Spoil%, MR-Hrs, & Waste?"
           do-speed
           space(2)
           skip(1)
           space(2)
           "                      Override GS&A Percentages?"
           do-gsa
           skip(1)
           space(2)
           "                                      Summarize?"
           v-summ
           skip(1).
           
    find first xef where xef.e-num eq xest.e-num no-lock no-error.
  end.
  hide frame ask no-pause.
  if keyfunction(lastkey) eq "end-error" then leave.
  */
end.

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
if program-name(2) begins "ce/com/probe." or
   program-name(2) begins "oe/"           then do:
  {sys/msg/calc.i kalk1}
end.
*/
SESSION:SET-WAIT-STATE("general").

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

FORM day_str v-module tim_str to 79
     SKIP(1)
     "Combination Est#" xest.est-no FORMAT "x(8)"
     "UserID:" xest.updated-id
     "Prober:" probe.probe-user
     cJobNo FORMAT "X(20)"
     SKIP(1)
     with frame hdr page-top STREAM-IO width 80 no-labels no-box.

FORM "Sales Rep:" kli.sman kli.sname SKIP
     "Cust:" kli.cust-no
             kli.cust-add[1] FORMAT "x(29)" TO 44
     "Ship:" kli.ship-add[1] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[2] FORMAT "x(29)" TO 44
             kli.ship-add[2] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[3] FORMAT "x(29)" TO 44
             kli.ship-add[3] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[4] FORMAT "x(29)" TO 44
             kli.ship-add[4] FORMAT "x(29)" TO 80
             SKIP
    WITH STREAM-IO NO-LABELS NO-BOX DOWN WIDTH 80 FRAME kli.
if retry then output close.

qty = 0.
for each xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
    no-lock,
    each xeb
    where xeb.company eq xef.company
      and xeb.est-no  eq xef.est-no
      and xeb.form-no eq xef.form-no
    no-lock:
  qty = qty + if xeb.yrprice /*AND NOT ll-tandem*/ then xeb.yld-qty else xeb.bl-qty.
end.

{est/probeset.i qty 0}

ASSIGN
 v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
 outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,v-probe-fmt)
 outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,v-probe-fmt)
 outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,v-probe-fmt).

output to value(outfile1).

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


   ctrl[19] = ce-ctrl.fold-pct.


fg-rate-f = ce-ctrl.fg-rate-farm.

rm-rate-f = ce-ctrl.rm-rate-farm.

hand-pct-f = ce-ctrl.hand-pct-farm / 100.

DO TRANSACTION:
  FOR each est-op
      WHERE est-op.company EQ xest.company 
        AND est-op.est-no  EQ xest.est-no
        AND est-op.line    GT 500:
    DELETE est-op.
  END.
  FOR EACH est-op
      WHERE est-op.company EQ xest.company
        AND est-op.est-no  EQ xest.est-no
        AND est-op.line    LT 500
      BY est-op.qty:
    lv-eqty = est-op.qty.
    LEAVE.
  END.
  FOR EACH est-op
      WHERE est-op.company EQ xest.company 
        AND est-op.est-no  EQ xest.est-no
        AND est-op.qty     EQ lv-eqty
        AND est-op.line    LT 500:
    CREATE xop.
    BUFFER-COPY est-op EXCEPT rec_key TO xop.
    xop.line = est-op.line + 500.
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

for each brd:
  delete brd.
end.

for each blk:
  delete blk.
end.

for each xjob:
  delete xjob.
end.

/* run ce/com/prokalk.p.*/
for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no:

   xxx = 0.
   for each xeb where xeb.company = xest.company
               AND xeb.est-no eq xest.est-no and xeb.form-no = xef.form-no
       BY xeb.blank-no:
      find first kli where kli.cust-no = xeb.cust-no no-error.
      if not avail kli then do:
         find first sman   where sman.company = cocode AND  
                                 sman.sman    = xeb.sman no-lock no-error.
         find first cust   where   cust.company = cocode and
                                   cust.cust-no = xeb.cust-no no-lock no-error.
         find first shipto where shipto.company = cust.company and
                                 shipto.cust-no = cust.cust-no and
                                 shipto.ship-id = xeb.ship-id no-lock no-error.
         create kli.
         if avail sman then assign kli.sman    = sman.sman
                                     kli.sname   = sman.sname.
         if xeb.cust-no ne "Temp" then assign
         kli.cust-no = xeb.cust-no
         kli.cust-add[1] = cust.name
         kli.cust-add[2] = cust.addr[1]
         kli.cust-add[3] = cust.addr[2]
         kli.cust-add[4] = cust.city + ", " + cust.state + " " + cust.zip.
         else assign
         kli.cust-no = xeb.cust-no
         kli.cust-add[1] = xeb.ship-name
         kli.cust-add[2] = xeb.ship-addr[1]
         kli.cust-add[3] = xeb.ship-addr[2]
         kli.cust-add[4] = xeb.ship-city + ", " + xeb.ship-state + " " +
                           xeb.ship-zip.

         if kli.cust-add[3] = "" then assign
            kli.cust-add[3] = kli.cust-add[4] kli.cust-add[4] = "".
         if kli.cust-add[2] = "" then assign
            kli.cust-add[2] = kli.cust-add[3] kli.cust-add[3] = kli.cust-add[4]
            kli.cust-add[4] = "".
         assign
         kli.ship-add[1] = shipto.ship-name
         kli.ship-add[2] = shipto.ship-addr[1]
         kli.ship-add[3] = shipto.ship-addr[2]
         kli.ship-add[4] = shipto.ship-city + ", " + shipto.ship-state +
                                                         " " + shipto.ship-zip.
         if kli.ship-add[3] = "" then
         assign kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
         if kli.ship-add[2] = "" then
         assign kli.ship-add[2] = kli.ship-add[3]
                kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
      end.
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no no-error.
      if not avail blk then do:
         create blk.
         assign
          blk.kli      = kli.cust-no
          blk.id       = xeb.part-no
          blk.snum     = xeb.form-no
          blk.bnum     = xeb.blank-no
          blk.qreq     = xeb.bl-qty
          blk.qyld     = xeb.yld-qty
          blk.yr$      = xeb.yrprice
          blk.stock-no = xeb.stock-no.
      end.
      xxx = xxx + (xeb.t-sqin * xeb.num-up).
   end.
   for each xeb where xeb.company = xest.company
                  AND xeb.est-no eq xest.est-no
                  and xeb.form-no eq xef.form-no no-lock,
       first blk  where blk.snum eq xeb.form-no
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

display day_str v-module tim_str
        TRIM(xest.est-no) @ xest.est-no
        xest.updated-id
        probe.probe-user
        cJobNo
        with frame hdr .

for each kli with frame kli:
   display kli.sman kli.sname
           kli.cust-no
           kli.cust-add[1] kli.ship-add[1] 
           kli.cust-add[2] kli.ship-add[2]
           kli.cust-add[3] kli.ship-add[3]
           kli.cust-add[4] kli.ship-add[4].
   down.
end.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no
               BREAK BY xef.form-no
with STREAM-IO frame brd no-labels no-box width 80 down:
   IF NOT gEstSummaryOnly THEN
   ASSIGN
    brd-l  = 0
    brd-w  = 0
    brd-sq = 0
    brd-sf = 0
    brd-wu = 0

    lv-brd-l  = 0
    lv-brd-w  = 0
    lv-brd-sq = 0
    lv-brd-sf = 0
    lv-brd-wu = 0.

   /* calc. sheet dimensions & weight */
   if cerunf eq "HOP" then
     assign
      brd-l[2] = xef.nsh-len
      brd-w[2] = xef.nsh-wid.
   else
     assign
      lv-brd-l = xef.nsh-len
      lv-brd-w = xef.nsh-wid
      brd-l[2] = xef.gsh-len
      brd-w[2] = xef.gsh-wid.

   brd-l[1] = xef.trim-l.
   if xef.roll = true then brd-l[3] = xef.gsh-len.
   brd-w[1] = xef.trim-w.
   if brd-l[2] = 0 and brd-w[2] = 0 then assign brd-l[2] = xef.lsh-len
                                                brd-w[2] = xef.lsh-wid.
   if xef.roll = true then
      brd-w[3] = xef.roll-wid.

   ASSIGN
     brd-sq[1] = xef.trim-l * xef.trim-w
     brd-sq[2] = brd-l[2] * brd-w[2]
     brd-sq[3] = brd-l[3] * brd-w[3]
     lv-brd-sq = lv-brd-l * lv-brd-w
     brd-sf[1] = if v-corr then (brd-sq[1] * .007) else (brd-sq[1] / 144)
     brd-sf[2] = if v-corr then (brd-sq[2] * .007) else (brd-sq[2] / 144)
     lv-brd-sf = if v-corr then (lv-brd-sq * .007) else (lv-brd-sq / 144).

   find first xop where xop.company = xest.company
                    AND xop.est-no eq xest.est-no
                    and xop.s-num = xef.form-no and
                    xop.line ge 500 no-lock no-error.
   find first item {sys/look/itemW.i} and item.i-no = xef.board no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   ASSIGN
   brd-wu[1] = brd-sf[1]  * item.basis-w
   brd-wu[2] = brd-sf[2]  * item.basis-w
   lv-brd-wu = lv-brd-sf  * item.basis-w
   zzz = 0
   vofor2 = IF gEstSummaryOnly THEN " 1 -" + STRING(xest.form-qty,">9")
            ELSE STRING(xef.form-no,"99") + " OF " + STRING(xest.form-qty,"99").
   .
   IF NOT gEstSummaryOnly OR last(xef.form-no) THEN 
   display skip(1)
   "FORM" vOfor2 
   " Width   Length     Sq.Inches   Sq.Feet/Sheet   Weight per Units" skip
   with no-box no-labels width 80 frame aa1 DOWN STREAM-IO.

   for each xeb OF xef BY xeb.blank-no:
      /* set total # of blanks on all forms */

      ASSIGN
      tt-blk = tt-blk + if xeb.yrprice /*AND NOT ll-tandem*/ then xeb.yld-qty else xeb.bl-qty
      /* set total # of blanks on this form */
      t-blksht[xef.form-no] = t-blksht[xef.form-no] + xeb.num-up
      /* set total qty of all blanks for this form */
      t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] +
                              if xeb.yrprice then xeb.yld-qty else xeb.bl-qty.
      /* find sheet qty needed for this form (without spoil)*/
      if (xeb.yld-qty / xeb.num-up) > zzz then
      assign zzz = (xeb.yld-qty / xeb.num-up).
      {sys/inc/roundup.i zzz}
      ASSIGN
      t-shtfrm[xeb.form-no] = zzz
      call_id = recid(xeb)
      vbsf = vbsf + if v-corr then (xeb.t-sqin * .007) else (xeb.t-sqin / 144)
      brd-l[4]  = xeb.t-len
      brd-w[4]  = xeb.t-wid
      brd-sq[4] = xeb.t-sqin  /*brd-l[4] * brd-w[4]*/
      brd-sf[4] = if v-corr then (brd-sq[4] * .007) else (brd-sq[4] / 144)
      brd-wu[4] = brd-sf[4] * item.basis-w.

      IF NOT gEstSummaryOnly OR last(xef.form-no) THEN 
      display 
      /*"Blk" space(0) xeb.blank-no FORMAT "99"*/
      "Blk" + IF NOT gEstSummaryOnly THEN string(xeb.blank-no,"99") ELSE "" FORMAT "x(5)"
          "Size :" brd-w[4] to 21 brd-l[4] to 30 brd-sq[4] to 42
               brd-sf[4] to 52 "Sf/Sht"  brd-wu[4] to 70 space(0) "/M Shts" skip
      with no-box no-labels width 80 frame aa2 DOWN STREAM-IO.
   end.
   find xeb where recid(xeb) = call_id no-lock no-error. qty = xeb.yld-qty.

   run ce/com/prokalk.p.

   ASSIGN
   brd-sf[3] = (IF gEstSummaryOnly THEN brd-sf[3] ELSE 0) +
               (
               (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                          else (xef.gsh-len * xef.gsh-wid / 144) ) 
                * xef.gsh-qty / 1000 
               )

   brd-wu[3] = (IF gEstSummaryOnly THEN brd-wu[3] ELSE 0) + (
               (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                          else (xef.gsh-len * xef.gsh-wid / 144) ) * xef.gsh-qty / 1000 * item.basis-w / 2000) .

   IF NOT gEstSummaryOnly OR last(xef.form-no) THEN 
   display 
   "  Die Size :" brd-w[1] to 21 brd-l[1] to 30 brd-sq[1] to 42
            brd-sf[1] to 52 "Sf/Sht"  brd-wu[1] to 70 space(0) "/M Shts" skip
                   /*xef.trim-l when v-layout @ brd-w[1]
                   xef.trim-w when v-layout @ brd-l[1]*/
   with no-box no-labels width 80 frame aa3 DOWN STREAM-IO.
   IF LOOKUP(cerunf,"ASI,CERunF 1") NE 0 /*cerunf = "ASI"*/ THEN v-header = "   Qty      --- Desc/FG Item ----- -- Size / Color ----- --- Style / Part No ---".
   ELSE v-header = "   Qty      --- Description ------ -- Size / Color ----- --- Style / Part No ---".
   if cerunf ne "HOP" then display
   " Feed Size :" lv-brd-w to 21 lv-brd-l to 30 lv-brd-sq to 42
            " #out:" xef.n-out FORMAT ">>9" lv-brd-wu to 70 space(0) "/M Shts" skip
   with no-box no-labels width 80 frame aa4 DOWN STREAM-IO.

   IF NOT gEstSummaryOnly OR last(xef.form-no) THEN 
   display
   "Sheet Size :" brd-w[2] to 21 brd-l[2] to 30 brd-sq[2] to 42
            brd-sf[2] to 52 "Sf/Sht"  brd-wu[2] to 70 space(0) "/M Shts" skip
   "Roll  Size :"                  when brd-w[3] ne 0
                /*brd-l[3]  to 30  when brd-l[3] ne 0*/
                  brd-w[3]  to 21  when brd-w[3] ne 0
                  "Total Board ="
                  brd-sf[3] to 43  "MSF"
                  brd-wu[3] to 68  "Tons"
                  skip(1)
                  v-header FORMAT "x(80)"
/*"   Qty      --- Desc/iption ------ -- Size / Color ----- --- Style / Part No ---"*/
with no-box no-labels width 80 frame aa5 DOWN STREAM-IO.
   
   for each xeb where xeb.company = xest.company
                  AND xeb.est-no eq xest.est-no 
                  and ( (xeb.form-no = xef.form-no AND NOT gEstSummaryOnly) OR
                        (gEstSummaryOnly AND LAST(xef.form-no) )  )
       BY xeb.form-no BY xeb.blank-no
   with STREAM-IO frame blk no-box no-labels width 80 down:
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
      IF LOOKUP(cerunf,"ASI,CERunF 1") NE 0 /*cerunf = "ASI"*/ THEN DO:
        IF dsc[2] = "" THEN 
            ASSIGN dsc[2] = xeb.stock-no 
                v-i-no = ""   
                v-2desc = NO.
        ELSE
            ASSIGN v-i-no = xeb.stock-no 
                v-2desc = YES.
       END.
      /*IF NOT gEstSummaryOnly THEN */ DO:
         display /*xeb.cust-no*/
              xeb.yld-qty format ">>>,>>>,>>9"
                xeb.bl-qty when not xeb.yrprice @ xeb.yld-qty space(1)
              dsc[1] format "x(22)"  
              sizcol[1] format "x(21)"   
              stypart[1] format "x(23)" skip
              space(3) /* 10*/
              "#UP= " + string(xeb.num-up,">>9")
              dsc[2] format "x(22)"
              sizcol[2] format "x(21)"
              stypart[2] format "x(23)"WITH STREAM-IO.
         IF LOOKUP(cerunf,"ASI,CERunF 1") NE 0 /*cerunf = "ASI"*/ AND v-2desc THEN      
            PUT  SPACE(12) v-i-no FORMAT "x(22)" SKIP(1).
         ELSE
            PUT SKIP(1).
         down.
        /* dBoardTotalQty = dBoardTotalQty + IF xeb.yrprice THEN xeb.yld-qty ELSE xeb.bl-qty.
        */
      END.
      
   end.
end.

put skip(1).
IF gEstSummaryOnly THEN
    PUT
     "Materials                                     Qty/Unit         Cost/M      TOTAL".
ELSE
    PUT
    "Materials                 Weight Caliper    QTY/Unit    MR $  Matl$/M    TOTAL".
PUT    skip.
dm-tot[3] = 0. dm-tot[4] = 0. dm-tot[5] = 0.

/* b o a r d        */ run ce/com/pr4-brd.p (v-vend-no).
/* i n k s          */ run ce/com/pr4-ink.p.
/* film             */ run ce/com/pr4-flm.p.
/* case/tray/pallet */ run ce/com/pr4-cas.p.
/* special          */ run ce/com/pr4-spe.p.

IF gEstSummaryOnly THEN DO:
  /* print board summary */
  DEF TEMP-TABLE ttbrd FIELD ttRecid AS recid
                       FIELD ttType AS INT.
                       
  FOR EACH brd,
      FIRST ITEM WHERE item.company EQ xest.company
                   AND item.i-no    EQ brd.i-no
                   AND CAN-DO("B,P,R,1,2,3,4",item.mat-type)
      BREAK BY brd.i-no:
      CREATE ttBrd.
      ASSIGN ttBrd.ttRecid = RECID(brd)
             ttBrd.ttType = 1.
      IF brd.qty-mr <> 0 THEN DO:
         CREATE ttBrd.
         ASSIGN ttBrd.ttRecid = RECID(brd)
                ttBrd.ttType = 2.
      END.
      IF brd.qty-wst <> 0 THEN DO:
         CREATE ttBrd.
         ASSIGN ttBrd.ttRecid = RECID(brd)
                ttBrd.ttType = 3.
      END.
  END.

  FOR EACH ttBrd,
      EACH brd WHERE RECID(brd) = ttBrd.ttRecid,
      FIRST ITEM WHERE item.company EQ xest.company
                   AND item.i-no    EQ brd.i-no
                   AND CAN-DO("B,P,R,1,2,3,4",item.mat-type)
         BREAK BY brd.i-no BY ttBrd.ttType:
    
    ASSIGN vBoardAmt = brd.amount * (brd.qty - brd.qty-mr - brd.qty-wst)
           vMrWaste = brd.amount * brd.qty-mr
           vRunWaste = brd.amount * brd.qty-wst
           /*vCost/MMR = vMrWaste / (t-blkqty[brd.frm-no])*/
           .

    ACCUM brd.qty (TOTAL BY brd.i-no BY ttBrd.ttType).
    ACCUM vMrWaste /*brd.qty-mr*/ (TOTAL BY brd.i-no BY ttBrd.ttType).
    ACCUM vRunWaste /*brd.qty-wst*/ (TOTAL BY brd.i-no BY ttBrd.ttType).
    ACCUM vBoardAmt /*brd.amount*/ (TOTAL BY brd.i-no BY ttBrd.ttType).
    ACCUM brd.cost-m (TOTAL BY brd.i-no BY ttBrd.ttType).
    ACCUM brd.qty-mr (TOTAL BY brd.i-no BY ttBrd.ttType).
    ACCUM brd.qty-wst (TOTAL BY brd.i-no BY ttBrd.ttType).
    /*ACCUM vCost/MMR (TOTAL BY brd.i-no BY ttBrd.ttType).*/

    IF LAST-OF(ttBrd.ttType) THEN DO:
       IF ttBrd.ttType = 1 THEN DO:
       
          PUT brd.i-no brd.dscr
              /*ACCUM TOTAL BY ttBrd.ttType brd.qty TO 61 brd.sc-uom */
              (ACCUM TOTAL BY ttBrd.ttType vBoardAmt) / (ACCUM TOTAL BY ttBrd.ttTyp brd.qty) * 1000 TO 69
              ACCUM TOTAL BY ttBrd.ttType vBoardAmt /*brd.amount*/ TO 80
              SKIP. 
          dBoardTotalAmt = dBoardTotalAmt + (ACCUM TOTAL BY ttBrd.ttType vBoardAmt).
       END.
       ELSE IF ttBrd.ttType = 2 THEN DO:
       
        PUT "MR Waste" AT 11      
            ACCUM TOTAL BY ttBrd.ttType brd.qty-mr AT 45 "Sht"
            (ACCUM TOTAL BY ttBrd.ttType vMrWaste) / (ACCUM TOTAL BY ttBrd.ttType brd.qty-mr) * 1000   AT 60 
            ACCUM TOTAL BY ttBrd.ttType vMrWaste  TO 80
            SKIP.
        dBoardTotalAmt = dBoardTotalAmt + (ACCUM TOTAL BY ttbrd.ttType vMrWaste).
       END.
       ELSE IF ttBrd.ttType = 3 THEN DO:
       
        PUT "Run Waste" AT 11      
            ACCUM TOTAL BY ttBrd.ttType brd.qty-wst AT 45 "Sht"
            (ACCUM TOTAL BY ttBrd.ttType vRunWaste) / (ACCUM TOTAL BY ttBrd.ttType brd.qty-wst) * 1000 AT 60
            ACCUM TOTAL BY ttBrd.ttType /*brd.qty-wst AT 45*/ vRunWaste TO 80
            SKIP.                          
        dBoardTotalAmt = dBoardTotalAmt + (ACCUM TOTAL BY ttbrd.ttType vRunWaste).
       END.
       

    END.
   /* 
    IF /*LAST-OF(brd.i-no)*/ LAST(ttBrd.ttType)  THEN DO:
       PUT "TOTAL  BOARD        " .
       PUT /*(ACCUM TOTAL BY brd.i-no vBoardAmt) + (ACCUM TOTAL BY brd.i-no vMrWaste) +
           (ACCUM TOTAL BY brd.i-no vRunWaste)*/
           dBoardTotalAmt TO 80
           SKIP(1).
        dBoardTotalAmt = 0. 
       /*ASSIGN dBoardTotalAmt = ACCUM TOTAL brd.amount
              dBoardTotalQty = ACCUM TOTAL brd.qty*/ .
    END. 
    */   
  END.

  /* Non bord - glue... */
  FOR EACH brd,
      FIRST ITEM WHERE item.company EQ xest.company
                   AND item.i-no    EQ brd.i-no
                   AND NOT CAN-DO("B,P,R,1,2,3,4",item.mat-type)
         BREAK BY brd.i-no BY ITEM.mat-type:    
    ACCUM brd.qty (TOTAL BY brd.i-no).
    ACCUM brd.qty-mr (TOTAL BY brd.i-no).
    ACCUM brd.qty-wst (TOTAL BY brd.i-no).
    ACCUM brd.amount (TOTAL BY brd.i-no).
    ACCUM brd.cost-m (TOTAL BY brd.i-no).
    
    IF LAST-OF(brd.i-no) THEN DO:
       PUT brd.i-no brd.dscr 
           ACCUM TOTAL BY brd.i-no brd.qty AT 45
           ACCUM TOTAL BY brd.i-no brd.cost-m TO 69
           ACCUM TOTAL BY brd.i-no brd.amount TO 80
           SKIP.           
    END.
    IF LAST(brd.i-no) THEN DO:
       PUT "TOTAL OTHER MATERIALS".
       /*PUT (ACCUM TOTAL brd.qty) + dBoardTotalQty AT 45           
           ((ACCUM TOTAL brd.amount) + dBoardTotalAmt) / ((ACCUM TOTAL brd.qty) + dBoardTotalQty) * 1000 
           ((ACCUM TOTAL brd.amount) + dBoardTotalAmt) TO 80
           SKIP(1).
       */
        PUT dm-tot[3] format ">>>9.99" AT 45
            dm-tot[5] / (tt-blk / 1000) format ">>>9.99" to 69
            dm-tot[5] format ">>>>,>>9.99" to 80
            SKIP(1).
    END.
  END.
END.

v-brd-cost = v-brd-cost + dm-tot[5].

for each blk:
   find first xjob
        where xjob.i-no     eq blk.id
          and xjob.form-no  eq blk.snum
          and xjob.blank-no eq blk.bnum
        no-error.

   if not avail xjob then do:
     create xjob.
     assign
      xjob.form-no  = blk.snum
      xjob.blank-no = blk.bnum
      xjob.cust-no  = blk.kli.
   end.

   assign
    xjob.mat      = blk.cost - blk.lab
    xjob.lab      = blk.lab
    xjob.i-no     = blk.id
    xjob.pct      = blk.pct
    xjob.stock-no = blk.stock-no.
end.
IF NOT gEstSummaryOnly THEN
display     "TOTAL  DIRECT  MATERIALS "
            dm-tot[3] format ">>>9.99" to 61
            dm-tot[5] / (tt-blk / 1000) format ">>>9.99" to 69
            dm-tot[5] format ">>>>,>>9.99" to 80
            skip(1)
    with STREAM-IO frame ac5 no-labels no-box.

/* prep */ run ce/com/pr4-prp.p.
IF gEstSummaryOnly THEN DO:
  FOR EACH xprep,
      EACH est-prep NO-LOCK WHERE est-prep.company = xest.company
                              AND est-prep.est-no = xest.est-no
                              AND est-prep.s-num = xprep.frm
                              AND est-prep.b-num = xprep.blank-no
                              AND est-prep.CODE = xprep.CODE
                              AND index("IM",est-prep.simon) gt 0
                              BREAK BY xprep.CODE :
      ACCUM xprep.qty (TOTAL BY xprep.CODE).
      ACCUM xprep.std-cost (TOTAL BY xprep.CODE).
      ACCUM xprep.mat (TOTAL BY xprep.CODE).
      ACCUM xprep.lab (TOTAL BY xprep.CODE).
      ACCUM xprep.amount (TOTAL BY xprep.CODE).
      ACCUM est-prep.mkup (TOTAL BY xprep.CODE).
      ACCUM est-prep.amtz (TOTAL BY xprep.CODE).

      IF FIRST(xprep.CODE) THEN
         PUT "Prep Description " 
          "Mat'l"             to 31
          "Labor"             to 42
          "Addt'l"            to 50
          "Amtz"              to 58
          "Cost/M"            to 68
          "Total Cost"        to 79
          skip.
      IF LAST-OF(xprep.CODE) THEN DO:
         DISPLAY /*xprep.CODE*/
                 est-prep.dscr
                 ACCUM TOTAL BY xprep.CODE xprep.mat @ xprep.mat
                 ACCUM TOTAL BY xprep.CODE xprep.lab @ xprep.lab
                 ACCUM TOTAL BY xprep.CODE est-prep.mkup @ est-prep.mkup FORM ">>>9.99"
                 ACCUM TOTAL BY xprep.CODE est-prep.amtz @ est-prep.amtz FORM ">>>9.99"
                 xprep.cost-m
                 ACCUM TOTAL BY xprep.CODE xprep.amount @ xprep.amount format "->>>>>9.99"
              WITH NO-LABEL FRAME xprepsum DOWN STREAM-IO NO-BOX.
         DOWN WITH FRAME xprepsum.
      END.
      IF LAST(xprep.CODE) THEN DO:
         DISPLAY "Total Prep " @ est-prep.dscr
                 ACCUM TOTAL xprep.mat @ xprep.mat
                 ACCUM TOTAL xprep.lab @ xprep.lab
                 ACCUM TOTAL est-prep.mkup @ est-prep.mkup FORM ">>>9.99"
                 ACCUM TOTAL est-prep.amtz @ est-prep.amtz FORM ">>>9.99"
                 ACCUM TOTAL xprep.amount @ xprep.amount format "->>>>>9.99"
              WITH FRAME xprepsum DOWN STREAM-IO.
         DOWN WITH FRAME xprepsum.
      END.
  END.
END.

/* misc */ run ce/com/pr4-mis.p.

put skip(1)
   "Machine Description    MR (Hrs) Run  Speed    Rate     MR $    Run $  Total Cost" .

/* machines */
run ce/com/pr4-mch.p.

if ctrl2[2] ne 0 or ctrl2[3] ne 0 then do:
   put "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) to 80 skip.
   op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
end.

assign
 fr-tot     = 0
 fr-tot-pre = 0
 v-msf      = 0.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no,
    each xeb where xeb.company = xef.company
               AND xeb.est-no   eq xest.est-no
               and xeb.form-no eq xef.form-no,
   first carrier where carrier.company eq cocode
                    and carrier.carrier eq xeb.carrier no-lock,
   first carr-mtx where carr-mtx.company  eq cocode
      and carr-mtx.carrier  eq carrier.carrier
      and carr-mtx.del-zone eq xeb.dest-code no-lock:

  find first car where car.id eq xeb.part-no no-error.
  if not avail car then do:
    create car.
    assign
     car.carrier = carrier.carrier
     car.dscr    = carr-mtx.del-zone
     car.id      = xeb.part-no
     car.snum    = xeb.form-no
     car.bnum    = xeb.blank-no.
  end.
   
  find first item
      {sys/look/itemW.i}
        and item.i-no     eq xef.board
        and item.mat-type eq "B"
        and item.avg-w    gt 0
      no-lock no-error.
    
  assign
   v-msf    = (xeb.t-sqin - xeb.t-win) * xeb.bl-qty / 144000
   v-msf    = v-msf * if avail item then item.avg-w else 1
   v-blk-wt = xef.weight * v-msf
   car.msf  = car.msf + v-msf.

  if xef.medium ne "" then do:
    find first item {sys/look/itemW.i} and
               item.i-no = xef.medium no-lock no-error.
    if avail item
    then do:
        /*override item shrink % with shrink entered in BOM button on Layout screen*/
      IF xef.spare-dec-1 NE 0 
          THEN dShrink = xef.spare-dec-1.
          ELSE dShrink = ITEM.shrink.
        v-blk-wt = v-blk-wt +
                    (item.basis-w * (1 - (dShrink / 100)) * v-msf).
    END.
  end.
  if xef.flute ne "" then do:
    find first item {sys/look/itemW.i} and
               item.i-no = xef.flute no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    (item.basis-w * v-msf).
  end.
  if xef.lam-code ne "" then do:
    find first item {sys/look/itemW.i} and
               item.i-no = xef.lam-code no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                     xeb.bl-qty * xeb.t-sqin / item.sqin-lb).
  end.
  if xef.adh-code ne "" then do:
    find first item {sys/look/itemW.i} and
               item.i-no = xef.adh-code no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                     xeb.bl-qty * xeb.t-sqin / item.sqin-lb).
  end.
  
  car.qty = car.qty + v-blk-wt.

  find first blk
      where blk.snum eq xeb.form-no
        and blk.bnum eq xeb.blank-no
      no-lock no-error.
  if avail blk then blk.fg-wt = blk.fg-wt + v-blk-wt.

  /* add pallet & case for total weight */
  find first cas
      where cas.typ  eq 1
        and cas.snum eq xeb.form-no
        and cas.bnum eq xeb.blank-no
      no-error.
  if avail cas then do:
    find first item
        {sys/look/itemW.i}
          and item.i-no eq cas.ino
        no-lock no-error.
    if avail item then do:
      car.qty = car.qty + (cas.qty * ce-ctrl.def-cas-w /*item.basis-w*/).
      if avail blk then blk.fg-wt = blk.fg-wt + (ce-ctrl.def-cas-w /*item.basis-w*/).
    end.
    release item.
    find first cas
        where cas.typ  eq 3
          and cas.snum eq xeb.form-no
          and cas.bnum eq xeb.blank-no
        no-error.
    if avail cas then
    find first item
        {sys/look/itemW.i}
          and item.i-no eq cas.ino
        no-lock no-error.
    if avail item then do:
      car.qty = car.qty + (cas.qty * ce-ctrl.def-pal-w /*item.basis-w*/).
      if avail blk then blk.fg-wt = blk.fg-wt + (cas.qty * ce-ctrl.def-pal-w /*item.basis-w*/).
    end.
  end.

  for each cas where cas.id = xeb.part-no
                 AND CAN-DO("5,6",item.mat-type):
    find first item where item.company = cocode and
                          item.i-no = cas.ino no-lock no-error.
    car.qty = car.qty + (cas.qty * item.weight-100 / 100).
    IF AVAIL blk THEN blk.fg-wt = blk.fg-wt + (cas.qty * item.weight-100 / 100).
  end.
end.
fg-wt = 0.

for each car break by car.id:
  p-qty = 0.
  for each cas
      where cas.typ  eq 3
        and cas.snum eq car.snum
        and cas.bnum eq car.bnum:
        
    p-qty = p-qty + cas.qty.    
  end.
  
  ASSIGN
   z       = 0
   li-rels = 0.

  FOR EACH bf-eb NO-LOCK
      WHERE bf-eb.company EQ xest.company
        AND bf-eb.est-no  EQ xest.est-no
        AND bf-eb.part-no EQ car.id:
    z = z + bf-eb.bl-qty.
    FIND FIRST tt-rel
        WHERE tt-rel.reftable EQ "ce/com/selwhif1.w"
          AND tt-rel.company  EQ bf-eb.company
          AND tt-rel.loc      EQ bf-eb.est-no
          AND tt-rel.code     EQ STRING(bf-eb.form-no,"9999999999")
          AND tt-rel.code2    EQ STRING(bf-eb.blank-no,"9999999999")
        NO-ERROR.
    li-rels = li-rels + (IF AVAIL tt-rel THEN tt-rel.val[1] ELSE 1).
  END.
  
  find first xeb
      where xeb.company = xest.company
        AND xeb.est-no    eq xest.est-no
        and xeb.form-no  eq car.snum
        and xeb.blank-no eq car.bnum
      no-lock no-error.
  find first carrier
      where carrier.company eq cocode
        and carrier.loc     eq locode
        and carrier.carrier eq car.carrier
      no-lock no-error.
  release carr-mtx.
  if avail carrier then
  find first carr-mtx
      where carr-mtx.company  eq cocode
        and carr-mtx.loc      eq locode
        and carr-mtx.carrier  eq carrier.carrier
        and carr-mtx.del-zone eq car.dscr
       no-lock no-error.
  
  assign
   yyy   = 0
   zzz   = 0
   v-msf = 0.
   
  for each xcar
      where xcar.carrier eq car.carrier
        and xcar.dscr    eq car.dscr:  /* Group by zone? */
    assign
     zzz   = zzz + xcar.qty    /* zzz = total wt for price lookup */
     v-msf = v-msf + xcar.msf.  
  end.

  if xeb.fr-out-c ne 0 then
    yyy = xeb.fr-out-c * xxx / 100.
    
  else
  if xeb.fr-out-m ne 0 then
    yyy = xeb.fr-out-m * z / 1000.
    
  else  
  if avail carr-mtx then do:
    if carrier.chg-method eq "P" then
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * p-qty.
      if carr-mtx.weight[i] ge p-qty then leave.
    end.
    
    else
    if carrier.chg-method eq "W" then
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * car.qty / 100.
      if carr-mtx.weight[i] ge zzz then leave.
    end.
    
    else
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * car.msf.
      if carr-mtx.weight[i] ge v-msf then leave.
    end.
       
    if yyy lt carr-mtx.min-rate then yyy = carr-mtx.min-rate.
        
    yyy = yyy + (carr-mtx.min-rate * (li-rels - 1)).
  end.
  
  assign
   fg-wt    = fg-wt + car.qty
   car.cost = car.cost + yyy
   fr-tot   = fr-tot + yyy.
  
  if xeb.chg-method eq "P" then fr-tot-pre = fr-tot-pre + yyy.

  find first blk where blk.id eq car.id no-error.
  ASSIGN
  blk.sell = blk.sell + yyy  /* use sell for freight costs for now */
  ld-fg-rate = IF blk.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
  blk.lab  = blk.lab  + (car.qty / 100 * ld-fg-rate)
  blk.cost = blk.cost + (car.qty / 100 * ld-fg-rate)
  ld-fg-amt = ld-fg-amt + (car.qty / 100 * ld-fg-rate).
end.

if ld-fg-amt gt 1 then put "Finished Goods Handling" ld-fg-amt to 80 skip.

op-tot[5] = op-tot[5] + ld-fg-amt.

put "TOTAL  OPERATIONS        " op-tot[3] format ">>>>9.99" to 59
    op-tot[4] format ">>>>>9.99" to 69
    op-tot[5] format ">>>>,>>9.99" to 80 skip(1).

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
      if ce-ctrl.mat-cost[i] > dm-tot[5]  then leave.
   end.

/* lab */
   do i = 1 to 6:
      ctrl[10] = ce-ctrl.lab-pct[i] / 100.
      if ce-ctrl.lab-cost[i] > op-tot[5]  then leave.
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
 gsa-war = ce-ctrl.whse-mrkup
 qty     = tt-blk.

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

assign
ctrl[9]  = gsa-mat / 100
ctrl[10] = gsa-lab / 100
ctrl[1]  = gsa-war / 100
ctrl[19] = gsa-fm / 100.

output to value(outfile1) append.
run ce/com/pr4-tots.p.
output close.

run ce/com/pr4-mis2.p.

IF (v-avg-com AND NOT ll-tandem) OR
   (v-avg-tan AND ll-tandem)     THEN DO:
  assign
   v-mat = 0  
   v-lab = 0
   v-foh = 0
   v-voh = 0.

  for each xjob:
    assign
     v-mat = v-mat + xjob.mat
     v-lab = v-lab + xjob.lab
     v-foh = v-foh + xjob.foh
     v-voh = v-voh + xjob.voh.
  end.

  for each blk,    
      first xjob
      where xjob.i-no     eq blk.id
        and xjob.form-no  eq blk.snum
        and xjob.blank-no eq blk.bnum:
    assign
     blk.fact = fac-tot * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     blk.cost = tt-tot  * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.mat = v-mat   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.lab = v-lab   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.foh = v-foh   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.voh = v-voh   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk).
  end.
END.

ASSIGN
   v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
   ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(probe.line,v-probe-fmt).

if vprint then do:

  run ce/com/probemk.p (ROWID(probe)).

  if opsys = "unix" then
    unix silent cat value(outfile2) >> value(outfile3).
  else
    dos silent type value(outfile2) >> value(outfile3).

  if search(outfile1) <> ? then 
    dos silent  type value(outfile3) > value(ls-outfile).

  RUN ce/probeu3.p (ROWID(probe)).
END.

ELSE
DO TRANSACTION:
  if opsys = "unix" then
    unix silent rm value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,v-probe-fmt)).
  else
    dos silent del value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,v-probe-fmt)).

  FIND CURRENT probe.
  DELETE probe.
END.

for each blk,
    first xjob
    where xjob.i-no     eq blk.id
      and xjob.form-no  eq blk.snum
      and xjob.blank-no eq blk.bnum:
  assign
   xjob.mat = xjob.mat / ((if blk.yr$ then blk.qyld else blk.qreq) / 1000)
   xjob.lab = xjob.lab / ((if blk.yr$ then blk.qyld else blk.qreq) / 1000)
   xjob.foh = xjob.foh / ((if blk.yr$ then blk.qyld else blk.qreq) / 1000)
   xjob.voh = xjob.voh / ((if blk.yr$ then blk.qyld else blk.qreq) / 1000).
end.

DO TRANSACTION:
  FIND CURRENT op-lock NO-ERROR.
  IF AVAIL op-lock THEN DELETE op-lock.
END.

hide frame jobstd no-pause.
hide frame kalk no-pause.
hide frame ask no-pause.
hide frame kalk no-pause.

SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 1992  advanced software, inc. */
