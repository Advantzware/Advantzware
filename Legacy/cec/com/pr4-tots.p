/* --------------------------------------------------- cec/pr4-tots.p 1/94 cd  */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{cec/print4.i shared shared}

def shared var fr-tot-pre as dec.

def var op-tot-t like op-tot NO-UNDO.
def var t-cov as de NO-UNDO.
def var blk-pct like blk.pct NO-UNDO.
def var fr-blk as de NO-UNDO.
def var qm as de  NO-UNDO.
DEF VAR v-cewhspct AS LOG NO-UNDO.
DEF VAR ll-gsa-pct AS LOG NO-UNDO.


DO TRANSACTION:
  {sys/inc/cewhschg.i}
  v-cewhspct = NOT cewhschg-cha BEGINS "$".
END.

ll-gsa-pct = CAN-FIND(FIRST sys-ctrl
                      WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "CEGSA"
                        AND sys-ctrl.int-fld EQ 0).

qm = tt-blk / 1000.

find first ce-ctrl
    where ce-ctrl.company eq cocode
      and ce-ctrl.loc     eq locode
    no-lock no-error.

IF xest.gsa-mat EQ 0 THEN xest.costBoard = 0.

assign
 xxx       = dm-tot[5] - xest.costBoard + tprep-mat + mis-tot[1]
 ctrl2[9]  = xxx * ctrl[9]
 xxx       = op-tot[5] + tprep-lab + mis-tot[3]
 ctrl2[10] = xxx * ctrl[10]
 fac-tot   = dm-tot[5] + op-tot[5] +
	         tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3]
 tt-tot    = dm-tot[5] + op-tot[5] + ctrl2[1] +
	         tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3] +
	         xest.costBoard + ctrl2[9] + ctrl2[10]
 ctrl2[4]  = 0
 ctrl2[5]  = 0
 ctrl2[11] = 0
 ctrl2[12] = 0.

xest.costBoard = xest.costBoard * xest.gsa-mat / 100.

IF v-cewhspct THEN
  ctrl2[1] = (fac-tot + xest.costBoard + ctrl2[9] + ctrl2[10] ) * ctrl[1].

ctrl2[13] = (fac-tot + xest.costBoard + ctrl2[9] + ctrl2[10] ) * ctrl[19].

for each blk,
    first xeb FIELDS(t-sqin)
    where xeb.company = xest.company
      AND xeb.est-no    eq xest.est-no
      and xeb.form-no  eq blk.snum
      and xeb.blank-no eq blk.bnum
    no-lock:
  t-cov = t-cov + (xeb.t-sqin * blk.qyld).
end.

for each blk,
    first xeb
    WHERE xeb.company = xest.company
      AND xeb.est-no    eq xest.est-no
      and xeb.form-no  eq blk.snum
      and xeb.blank-no eq blk.bnum
    no-lock,
    first style
    where style.company eq cocode
      and style.style   eq xeb.style
    no-lock,
    first xjob
    where xjob.i-no     eq blk.id
      and xjob.form-no  eq blk.snum
      and xjob.blank-no eq blk.bnum:

  assign
   blk-pct  = xeb.t-sqin * blk.qyld / t-cov
   fr-blk   = blk.fact
   blk.fact = blk.fact + blk.cost  /* Freight already in fact if ctrl[6] gt 0 */
   xxx      = blk.sell              /* sell eq 0 if freight already included! */
   blk.sell = (blk.cost - blk.lab) * ctrl[9]
					/* add material gsa amount to blk.lab */
   blk.lab  = blk.lab * ctrl[10]           /* set blk.lab to labor gsa amount */
   blk.cost = blk.fact + blk.lab + blk.sell          /* add gsa's to blk.cost */
   blk.cost = blk.cost + if xeb.chg-method eq "P" then xxx else 0.
					   /* add freight if not already done */

  if vmclean then blk.fact = blk.fact - xjob.foh.

  yyy = blk.fact.    /* yyy = total cost of blk  (+ fr-tot if freight in fact */

  /* Now, let's distribute all those markups and misc. charges.... */
  /* Special Markups */

  /* spec #1 */
  if ctrl[13] gt 0 and ctrl[4] gt 0 then
    if ctrl[4] le 1 then
      blk.cost = blk.cost + (yyy * ctrl[4]).
    else
      blk.cost = blk.cost + (ctrl[4] * blk-pct).

  /* spec #2 */
  if ctrl[14] gt 0 and ctrl[11] gt 0 then
    if ctrl[11] le 1 then
      blk.cost = blk.cost + (yyy * ctrl[11]).
    else
      blk.cost = blk.cost + (ctrl[11] * blk-pct).

  /* spec #3 */
  if ctrl[15] gt 0 and ctrl[12] gt 0 then
    if ctrl[12] le 1 then
      blk.cost = blk.cost + (yyy * ctrl[12]).
    else
      blk.cost = blk.cost + (ctrl[12] * blk-pct).

  /* Warehousing */
  blk.cost = blk.cost + ((yyy + blk.lab + blk.sell - fr-blk) * ctrl[1]).

  /* Commissions */
  if ctrl[17] eq 1 and ctrl[5] gt 0 then              /* on Direct Fact. Cost */
    assign
     blk.cost = blk.cost + (yyy * (xeb.comm / 100))
     ctrl2[5] = ctrl2[5] + (yyy * (xeb.comm / 100)).
  /*
  else
  if ctrl[17] eq 0 and ctrl[5] gt 0 then                      /* on Full Cost */
    assign
     blk.cost = blk.cost + ((blk.cost - blk.sell - blk.lab) * (xeb.comm / 100))
     ctrl2[5] = ctrl2[5] + ((blk.cost - blk.sell - blk.lab) * (xeb.comm / 100)).
  */
  /* Royalties */
  if ctrl[18] eq 0 and style.royalty ne 0 then
    if style.royalty le 1 then
      blk.cost = blk.cost + (yyy * ctrl[18]).
    else
      blk.cost = blk.cost + (ctrl[18] * blk-pct).
end.

find first xeb
    where xeb.company = xest.company
      AND xeb.est-no    eq xest.est-no
      and xeb.form-no ne 0
    no-error.

if fr-tot-pre ne 0 then
  assign
   fac-tot = fac-tot + if ctrl[6] ne 0 then fr-tot-pre else 0
   tt-tot  = tt-tot  + fr-tot-pre.

/* SET PRINTED TOTALS for Misc. Charges */

/* spec #1 */
if ctrl[4] gt 0 then
  ctrl2[4]  = if ctrl[4] le 1  then (fac-tot * ctrl[4])  else ctrl[4].

/* spec #2 */
if ctrl[11] gt 0 then
  ctrl2[11] = if ctrl[11] le 1 then (fac-tot * ctrl[11]) else ctrl[11].

/* spec #3 */
if ctrl[12] gt 0 then
  ctrl2[12] = if ctrl[12] le 1 then (fac-tot * ctrl[12]) else ctrl[12].

/* Royalties */
if style.royalty ne 0 and ctrl[18] eq 1 then
  ctrl2[18] = if style.royalty le 1 then (fac-tot * ctrl[18])
				    else (ctrl[18] * qm).

put
   "*** T O T A L S                           Cost/M     MR $      Run $  Total Cost" skip
      "Direct Material" dm-tot[5] / qm                 to 48
			dm-tot[3] format ">>>>9.99"    to 57
			dm-tot[5] format ">>>>,>>9.99" to 80 skip.

if (not vmclean) then
  put "Direct Labor" op-tot[5] / qm       to 48
		     op-tot[3] format ">>>>9.99"    to 57
		     op-tot[4] format ">>>>>>9.99"  to 68
		     op-tot[5] format ">>>>,>>9.99" to 80 skip.

if tprep-mat ne 0 then
  put "Prep.  Material" tprep-mat / qm to 48 tprep-mat  to 80 skip.

if tprep-lab ne 0 then
  put "Prep.  Labor   " tprep-lab / qm to 48 tprep-lab  to 80 skip.

if mis-tot[1] ne 0 then
  put "Misc.  Material" mis-tot[1] / qm to 48 mis-tot[1] to 80 skip.

if mis-tot[3] ne 0 then
  put "Misc.  Labor   " mis-tot[3] / qm to 48 mis-tot[3] to 80 skip.

if vmclean then do:
  assign
   op-tot-t[1] = op-tot[1]
   op-tot-t[2] = op-tot[2]
   op-tot-t[3] = op-tot[3]
   op-tot-t[4] = op-tot[4]
   op-tot-t[5] = op-tot[5]

   op-tot[5]   = op-tot[5] - op-tot[6] - op-tot[7]
   op-tot[3]   = round(op-tot[5] * (op-tot-t[3] / op-tot-t[5]),2)
   op-tot[4]   = round(op-tot[5] * (op-tot-t[4] / op-tot-t[5]),2)
   fac-tot     = fac-tot   - op-tot[7].

  IF op-tot[3] EQ ? THEN op-tot[3] = 0.
  IF op-tot[4] EQ ? THEN op-tot[4] = 0.

  put "Direct Labor"
      op-tot[5] / qm                 to 48
      op-tot[3] format ">>>>9.99"    to 57
      op-tot[4] format ">>>>>>9.99"  to 68
      op-tot[5] format ">>>>,>>9.99" to 80 skip.

  assign
   op-tot[3]   = round(op-tot[6] * (op-tot-t[3] / op-tot-t[5]),2)
   op-tot[4]   = round(op-tot[6] * (op-tot-t[4] / op-tot-t[5]),2).

  IF op-tot[3] EQ ? THEN op-tot[3] = 0.
  IF op-tot[4] EQ ? THEN op-tot[4] = 0.

  put "Variable Overhead"
      op-tot[6] / qm                 to 48
      op-tot[3] format ">>>>9.99"    to 57
      op-tot[4] format ">>>>>>9.99"  to 68
      op-tot[6] format ">>>>,>>9.99" to 80 skip.
end.

if ctrl[6] ne 0 and fr-tot-pre ne 0 then
  put "Freight"  fr-tot-pre / qm to 48 fr-tot-pre to 80 skip.

put "DIRECT FACTORY COST" fac-tot / qm to 48
    fac-tot format ">>>>,>>9.99" to 80 skip.

fac-tot2 = fac-tot.

if vmclean then do:
  assign
   op-tot[3]   = round(op-tot[7] * (op-tot-t[3] / op-tot-t[5]),2)
   op-tot[4]   = round(op-tot[7] * (op-tot-t[4] / op-tot-t[5]),2).

  IF op-tot[3] EQ ? THEN op-tot[3] = 0.
  IF op-tot[4] EQ ? THEN op-tot[4] = 0.

  put "Fixed Overhead"
      op-tot[7] / qm                 to 48
      op-tot[3] format ">>>>9.99"    to 57
      op-tot[4] format ">>>>>>9.99"  to 68
      op-tot[7] format ">>>>,>>9.99" to 80 skip.

  fac-tot2 = fac-tot2 + op-tot[7].
end.

if ctrl[13] ne 0 then fac-tot2 = fac-tot2 + ctrl2[4].
if ctrl[14] ne 0 then fac-tot2 = fac-tot2 + ctrl2[11].
if ctrl[15] ne 0 then fac-tot2 = fac-tot2 + ctrl2[12].
/* if ctrl[17] ne 0 then fac-tot2 = fac-tot2 + ctrl2[5]. */
/*if ctrl[18] ne 0 then fac-tot2 = fac-tot2 + ctrl2[18]. double counted in fac-tot2 ticket 26340*/
/*
if ctrl[17] eq 1 and ctrl2[5] gt 1 then
  put "Commission" ctrl2[5] / qm to 48 ctrl2[5] to 80 skip.
*/
if ctrl[13] eq 1 and ctrl[4] ne 0 then do:
  if ctrl[4] gt 0 then put ce-ctrl.spec-l[1].
  if ctrl[4] le 1 then
    put string(ce-ctrl.spec-%[1] * 100,">>9.99") + "%" to 30.
  put ctrl2[4] / qm to 48 ctrl2[4] to 80 skip.
end.

if ctrl[14] eq 1 and ctrl[11] ne 0 then do:
  if ctrl[11] gt 0 then put ce-ctrl.spec-l[2] space(1).
  if ctrl[11] le 1 then
    put string(ce-ctrl.spec-%[2] * 100,">>9.99") + "%" to 30.
  put ctrl2[11] / qm to 48 ctrl2[11] to 80 skip.
end.

if ctrl[15] eq 1 and ctrl[12] ne 0 then do:
  if ctrl[12] gt 0 then put ce-ctrl.spec-l[3] space(1).
  if ctrl[12] le 1 then
    put string(ce-ctrl.spec-%[3] * 100,">>9.99") + "%" to 30.
  put ctrl2[12] / qm to 48 ctrl2[12] to 80 skip.
end.

IF ctrl[16] NE 0 THEN DO:
  PUT "GS&A Board".
  IF ll-gsa-pct THEN
    PUT STRING(xest.gsa-mat,">>9.99") + "%" TO 30.
  PUT xest.costBoard / qm TO 48 xest.costBoard TO 80 SKIP.

  PUT "GS&A Material".
  IF ll-gsa-pct THEN
    PUT STRING(ctrl[9] * 100,">>9.99") + "%" TO 30.
  PUT ctrl2[9] / qm TO 48 ctrl2[9] TO 80 SKIP.

  PUT "GS&A Labor".
  IF ll-gsa-pct THEN
    PUT STRING(ctrl[10] * 100,">>9.99") + "%" TO 30.
  PUT ctrl2[10] / qm TO 48 ctrl2[10] TO 80 SKIP.

  fac-tot2 = fac-tot2 + xest.costBoard + ctrl2[9] + ctrl2[10].
END.

if ctrl[18] gt 0 and ctrl2[18] ne 0 then                         /* Royalty */
  if ctrl2[18] le 1 then do:
    put "Royalty" string(ctrl2[18] * 100,">>9.99") + "%" to 30
				   (ctrl2[18] * fac-tot) to 80 skip.
    fac-tot2 = fac-tot2 + (fac-tot * ctrl2[18]).
  end.
  else do:
    put "Royalty" ctrl2[18] / qm to 48
	ctrl2[18]                to 80 skip.
    fac-tot2 = fac-tot2 + ctrl2[18].
  end.

put "TOTAL FACTORY COST" fac-tot2 / qm to 48
    fac-tot2 format ">>>>,>>9.99" to 80 skip.

ord-cost = fac-tot2.

FOR EACH blk:
  ACCUM blk.fact (TOTAL).
END.

FOR EACH blk:
  blk.fact = ord-cost * (blk.fact / (ACCUM TOTAL blk.fact)).
END.

for each blk,
    first xeb
    where xeb.company = xest.company
      AND xeb.est-no    eq xest.est-no
      and xeb.form-no  eq blk.snum
      and xeb.blank-no eq blk.bnum
    no-lock:

  blk-pct = xeb.t-sqin * blk.qyld / t-cov.

  /* Now, let's distribute all those POST FACTORY charges & markups ... */

  /* Special Markups */
  /* spec #1 */
  if ctrl[13] eq 0 and ctrl[4] ne 0 then
    if ctrl[4] le 1 then
      blk.cost = blk.cost + (fac-tot2 * blk-pct * ctrl[4]).
    else
      blk.cost = blk.cost + (ctrl2[4] * blk-pct).

  /* spec #2 */
  if ctrl[14] eq 0 and ctrl[11] ne 0 then
    if ctrl[11] le 1 then
      blk.cost = blk.cost + (fac-tot2 * blk-pct * ctrl[11]).
    else
      blk.cost = blk.cost + (ctrl2[11] * blk-pct).

  /* spec #3 */
  if ctrl[15] eq 0 and ctrl[12] ne 0 then
    if ctrl[12] le 1 then
      blk.cost = blk.cost + (fac-tot2 * blk-pct * ctrl[12]).
    else
      blk.cost = blk.cost + (ctrl2[12] * blk-pct).

  /* Royalties */
  if ctrl[18] eq 0 and style.royalty ne 0 then
    if style.royalty le 1 then
      blk.cost = blk.cost + (fac-tot * blk-pct * ctrl[18]).
    else
      blk.cost = blk.cost + (ctrl[18] * blk-pct).
end.

find first xeb
    where xeb.company = xest.company
      AND xeb.est-no    eq xest.est-no
      and xeb.form-no ne 0
    no-error.

tt-tot = fac-tot2.

if ctrl2[1] ne 0 then do:
  put "Warehousing"
      if v-cewhspct then (string(ctrl[1] * 100,">>9.99") + "%") else "" to 30
      ctrl2[1] to 80 skip.
  tt-tot = tt-tot + ctrl2[1].
end.

if ctrl2[13] ne 0 then do:
  put "Broker Comm"
      string(ctrl[19] * 100,">>9.99") + "%" to 30
      ctrl2[13] to 80 skip.
  tt-tot = tt-tot + ctrl2[13].
end.

if ctrl[6] eq 0 and fr-tot-pre ne 0 then do:
  put "Freight" fr-tot-pre / qm to 48 fr-tot-pre to 80 skip.
  tt-tot = tt-tot + fr-tot-pre.
end.
/*
if ctrl[5] gt 0 and ctrl[17] eq 0 then do:
  ctrl2[5] = fac-tot2 * (xeb.comm / 100).
  if ctrl2[5] gt 1 then
    put "Commission"  string(xeb.comm,">>9.99") + "%" to 30
	ctrl2[5] / qm to 48 ctrl2[5] to 80 skip.
  tt-tot = tt-tot + ctrl2[5].
end.
*/
if ctrl[13] eq 0 and ctrl[4] ne 0 then do:
  ctrl2[4] = if ctrl[4] le 1 then (fac-tot2 * ctrl[4]) else ctrl[4].
  put ce-ctrl.spec-l[1].
  if ctrl[4] le 1 then
    put string(ce-ctrl.spec-%[1] * 100,">>9.99") + "%" to 30.
  put ctrl2[4] / qm to 48 ctrl2[4] to 80 skip.
  tt-tot = tt-tot + ctrl2[4].
end.

if ctrl[14] eq 0 and ctrl[11] ne 0 then do:             /* set spec#2 */
  ctrl2[11] = if ctrl[11] le 1 then (fac-tot2 * ctrl[11]) else ctrl[11].
  put ce-ctrl.spec-l[2].
  if ctrl[11] le 1 then
    put string(ce-ctrl.spec-%[2] * 100,">>9.99") + "%" to 30.
  put ctrl2[11] / qm to 48 ctrl2[11] to 80 skip.
  tt-tot = tt-tot + ctrl2[11].
end.

if ctrl[15] eq 0 and ctrl[12] ne 0 then do:             /* set spec#3 */
  ctrl2[12] = if ctrl[12] le 1 then (fac-tot2 * ctrl[12]) else ctrl[12].
  put ce-ctrl.spec-l[3].
  if ctrl[12] le 1 then
    put string(ce-ctrl.spec-%[3] * 100,">>9.99") + "%" to 30.
  put ctrl2[12] / qm to 48 ctrl2[12] to 80 skip.
  tt-tot = tt-tot + ctrl2[12].
end.
      
IF ctrl[16] EQ 0 THEN DO:
  PUT "GS&A Board".
  IF ll-gsa-pct THEN
    PUT STRING(xest.gsa-mat,">>9.99") + "%" TO 30.
  PUT xest.costBoard / qm TO 48 xest.costBoard TO 80 SKIP.

  PUT "GS&A Material".
  IF ll-gsa-pct THEN
    PUT STRING(ctrl[9] * 100,">>9.99") + "%" TO 30.
  PUT ctrl2[9] / qm TO 48 ctrl2[9] TO 80 SKIP.

  PUT "GS&A Labor".
  IF ll-gsa-pct THEN
    PUT STRING(ctrl[10] * 100,">>9.99") + "%" TO 30.
  PUT ctrl2[10] / qm TO 48 ctrl2[10] TO 80 SKIP.

  tt-tot = tt-tot + xest.costBoard + ctrl2[9] + ctrl2[10].
     
END.

if ctrl[18] eq 0 and ctrl2[18] ne 0 then       /* Royalty */
  if ctrl2[18] le 1 then do:
    put "Royalty" string(ctrl2[18] * 100,">>9.99") + "%" to 30
	(ctrl2[18] * fac-tot2) / qm to 48
	(ctrl2[18] * fac-tot2)      to 80 skip.
    tt-tot = tt-tot + (fac-tot2 * ctrl2[18]).
  end.
  else do:
    put "Royalty" ctrl2[18] / qm to 48 ctrl2[18] to 80 skip.
    tt-tot = tt-tot + ctrl2[18].
  end.
      

FOR EACH blk:
  ACCUM blk.fact (TOTAL).
END.

FOR EACH blk:
  blk.cost = tt-tot * (blk.fact / (ACCUM TOTAL blk.fact)).
END.

fac-tot = fac-tot2. /*05230805*/


/* end ---------------------------------- copr. 1993  advanced software, inc. */
