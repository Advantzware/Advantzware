/* --------------------------------------------------- ce/pr4-tots.p 1/94 cd  */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{ce/print4.i shared shared}

def shared var fr-tot-pre as dec.

def var op-tot-t like op-tot NO-UNDO.
def var t-cov as de NO-UNDO.
def var blk-pct like blk.pct NO-UNDO.
def var fr-blk as de NO-UNDO.
def var qm as de  NO-UNDO.
DEF VAR ld-dm-mrkp AS DEC NO-UNDO.
DEF VAR ld-pmrkp AS DEC EXTENT 3 NO-UNDO.


{sys/inc/cerun.i F}
{sys/inc/cematl.i}

qm = tt-blk / 1000.

find first ce-ctrl
    where ce-ctrl.company eq cocode
      and ce-ctrl.loc     eq locode
    no-lock no-error.

{est/calcpcts.i xest}
IF calcpcts.val[1] EQ 0 THEN calcpcts.val[2] = 0.

IF cematl-log THEN ld-dm-mrkp = dm-tot[5] * cematl-dec / 100.

ASSIGN
 xxx       = dm-tot[5] - calcpcts.val[2] + tprep-mat + mis-tot[1]
 ctrl2[9]  = xxx * ctrl[9]
 xxx       = op-tot[5] + tprep-lab + mis-tot[3]
 ctrl2[10] = xxx * ctrl[10].
       
IF cerunf EQ "Dee" THEN DO:
  FOR EACH est-prep NO-LOCK
      WHERE est-prep.company EQ xest.company
        AND est-prep.est-no  EQ xest.est-no
        AND est-prep.code    NE "":

    IF est-prep.ml THEN
      ld-pmrkp[1] = ld-pmrkp[1] +
                    ((est-prep.cost * est-prep.qty) *
                     (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)).
    ELSE
      ld-pmrkp[2] = ld-pmrkp[2] +
                    ((est-prep.cost * est-prep.qty) *
                     (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)).
  END.

  ASSIGN
   ld-pmrkp[1] = tprep-mat - ld-pmrkp[1]
   ld-pmrkp[2] = tprep-lab - ld-pmrkp[2]
   ld-pmrkp[3] = ld-pmrkp[1] + ld-pmrkp[2]
   tprep-mat   = tprep-mat - ld-pmrkp[1]
   tprep-lab   = tprep-lab - ld-pmrkp[2].
END.

ASSIGN
 fac-tot   = dm-tot[5] + op-tot[5] +
	         tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3]
 calcpcts.val[2] = calcpcts.val[2] * calcpcts.val[1] / 100
 ctrl2[1]  = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) *
             ctrl[1]
 ctrl2[13]  = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) *
              ctrl[19]
 tt-tot    = dm-tot[5] + op-tot[5] + ctrl2[1] + ctrl2[13] +
	         tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3] +
	         calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp
 ctrl2[4]  = 0
 ctrl2[5]  = 0
 ctrl2[11] = 0
 ctrl2[12] = 0.

FIND CURRENT calcpcts NO-LOCK NO-ERROR.

for each blk,
    first xeb
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
   blk.sell = blk.sell + (calcpcts.val[2] * blk-pct)
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
  if (ctrl[13] ne 0 OR cerunf EQ "Dee") AND ctrl[4] gt 0 then
    if ctrl[4] le 1 then
      blk.cost = blk.cost + (yyy * ctrl[4]).
    else
      blk.cost = blk.cost + (ctrl[4] * blk-pct).

  /* spec #2 */
  if (ctrl[14] ne 0 OR cerunf EQ "Dee") AND ctrl[11] gt 0 then
    if ctrl[11] le 1 then
      blk.cost = blk.cost + (yyy * ctrl[11]).
    else
      blk.cost = blk.cost + (ctrl[11] * blk-pct).

  /* spec #3 */
  if (ctrl[15] ne 0 OR cerunf EQ "Dee") AND ctrl[12] gt 0 then
    if ctrl[12] le 1 then
      blk.cost = blk.cost + (yyy * ctrl[12]).
    else
      blk.cost = blk.cost + (ctrl[12] * blk-pct).

  /* Warehousing */
  blk.cost = blk.cost + ((yyy + blk.lab + blk.sell - fr-blk) * ctrl[1]).

  /* Commissions */
  /*if ctrl[17] ne 0 and ctrl[5] gt 0 then              /* on Direct Fact. Cost */
    assign
     blk.cost = blk.cost + (yyy * (xeb.comm / 100))
     ctrl2[5] = ctrl2[5] + (yyy * (xeb.comm / 100)).
  else
  if ctrl[17] eq 0 and ctrl[5] gt 0 then                      /* on Full Cost */
    assign
     blk.cost = blk.cost + ((blk.cost - blk.sell - blk.lab) * (xeb.comm / 100))
     ctrl2[5] = ctrl2[5] + ((blk.cost - blk.sell - blk.lab) * (xeb.comm / 100)).
  */
  IF ctrl[16] NE 0 OR cerunf EQ "Dee" THEN
    ASSIGN
     blk.cost = blk.cost +
                ((calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) *
                 blk-pct).

  /* Royalties */
  if cerunf NE "Dee" AND ctrl[18] gt 0 and ctrl2[18] ne 0 then 
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
if style.royalty ne 0 and ctrl[18] ne 0 then
  ctrl2[18] = if style.royalty le 1 then (fac-tot * ctrl[18])
				    else (ctrl[18] * qm).

put
   "*** T O T A L S                           Cost/M     MR $      Run $  Total Cost" skip
      "Direct Material" dm-tot[5] / qm                 to 48
			dm-tot[3] format ">>,>>9.99"    to 57
			dm-tot[5] format "->>>>,>>9.99" to 80 skip.

if not vmclean AND cerunf NE "Dee" then
  put "Direct Labor" op-tot[5] / qm       to 48
		     op-tot[3] format ">>,>>9.99"    to 57
		     op-tot[4] format ">>>>,>>9.99"  to 68
		     op-tot[5] format "->>>>,>>9.99" to 80 skip.

if tprep-mat ne 0 then
  put "Prep.  Material" tprep-mat / qm to 48 tprep-mat  to 80 skip.

if tprep-lab ne 0 then
  put "Prep.  Labor   " tprep-lab / qm to 48 tprep-lab  to 80 skip.

if mis-tot[1] ne 0 then
  put "Misc.  Material" mis-tot[1] / qm to 48 mis-tot[1] to 80 skip.

if mis-tot[3] ne 0 then
  put "Misc.  Labor   " mis-tot[3] / qm to 48 mis-tot[3] to 80 skip.

if vmclean OR cerunf EQ "Dee" then do:
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

if vmclean OR cerunf EQ "Dee" then do:
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

  PUT "Prep Markup"
      ld-pmrkp[3] / qm                 TO 48
      ld-pmrkp[3] FORMAT ">>>>,>>9.99" TO 80 SKIP.

  fac-tot2 = fac-tot2 + ld-pmrkp[3].
end.

if (ctrl[13] ne 0 OR cerunf EQ "Dee") and ctrl[4] ne 0 then do:
  if ctrl[4] gt 0 then put ce-ctrl.spec-l[1].
  if ctrl[4] le 1 then
    put string(ce-ctrl.spec-%[1] * 100,">>9.99") + "%" to 30.
  put ctrl2[4] / qm to 48 ctrl2[4] to 80 skip.
  fac-tot2 = fac-tot2 + ctrl2[4].
end.

if (ctrl[14] ne 0 OR cerunf EQ "Dee") and ctrl[11] ne 0 then do:
  if ctrl[11] gt 0 then put ce-ctrl.spec-l[2] space(1).
  if ctrl[11] le 1 then
    put string(ce-ctrl.spec-%[2] * 100,">>9.99") + "%" to 30.
  put ctrl2[11] / qm to 48 ctrl2[11] to 80 skip.
  fac-tot2 = fac-tot2 + ctrl2[11].
end.

if (ctrl[15] ne 0 OR cerunf EQ "Dee") and ctrl[12] ne 0 then do:
  if ctrl[12] gt 0 then put ce-ctrl.spec-l[3] space(1).
  if ctrl[12] le 1 then
    put string(ce-ctrl.spec-%[3] * 100,">>9.99") + "%" to 30.
  put ctrl2[12] / qm to 48 ctrl2[12] to 80 skip.
  fac-tot2 = fac-tot2 + ctrl2[12].
end.

IF ctrl[16] NE 0 OR cerunf EQ "Dee" THEN DO:
  PUT "GS&A Board"
      STRING(calcpcts.val[1],">>9.99") + "%"        TO 30
      calcpcts.val[2] / qm                          TO 48
      calcpcts.val[2]                               TO 80 SKIP

      "GS&A Material"
      STRING(ctrl[9] * 100,">>9.99")  + "%"         TO 30
      ctrl2[9] / qm                                 TO 48
      ctrl2[9]                                      TO 80 SKIP

      "GS&A Labor"
      STRING(ctrl[10] * 100,">>9.99") + "%"         TO 30
      ctrl2[10] / qm                                TO 48
      ctrl2[10]                                     TO 80 SKIP.

  IF ld-dm-mrkp NE 0 THEN
    PUT "Direct Material Markup"
        STRING(cematl-dec,">>9.99") + "%"           TO 30
        ld-dm-mrkp / qm                             TO 48
        ld-dm-mrkp                                  TO 80 SKIP.

  fac-tot2 = fac-tot2 + calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp.
END.

if cerunf NE "Dee" AND ctrl[18] gt 0 and ctrl2[18] ne 0 then                         /* Royalty */
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

IF cerunf EQ "Dee" THEN
  PUT "TOTAL CONTRIBUTION"          FORMAT "x(19)"
      (fac-tot2 - fac-tot) / qm     TO 48
      (fac-tot2 - fac-tot)          TO 80 SKIP.
ELSE
  PUT "TOTAL FACTORY COST"          FORMAT "x(19)"
      fac-tot2 / qm                 TO 48
      fac-tot2                      TO 80 SKIP.

IF cerunf NE "Dee" THEN
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

  IF ctrl[16] EQ 0 THEN
    ASSIGN
     blk.cost = blk.cost +
                ((calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) *
                 blk-pct).

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

IF cerunf NE "Dee" THEN DO:
  if ctrl2[1] ne 0 then do:
    put "Warehousing" string(ctrl[1] * 100,">>9.99") + "%" to 30
        ctrl2[1] to 80 skip.
    tt-tot = tt-tot + ctrl2[1].
  end.

  if ctrl2[13] ne 0 then do:
    put "Folding" string(ctrl[19] * 100,">>9.99") + "%" to 30
        ctrl2[13] to 80 skip.
    tt-tot = tt-tot + ctrl2[13].
  end.

  if ctrl[6] eq 0 and fr-tot-pre ne 0 then do:
    put "Freight" fr-tot-pre / qm to 48 fr-tot-pre to 80 skip.
    tt-tot = tt-tot + fr-tot-pre.
  end.

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
    PUT "GS&A Board"
        STRING(calcpcts.val[1],">>9.99") + "%"        TO 30
        calcpcts.val[2] / qm                          TO 48
        calcpcts.val[2]                               TO 80 SKIP

        "GS&A Material"
        STRING(ctrl[9] * 100,">>9.99")  + "%"         TO 30
        ctrl2[9] / qm                                 TO 48
        ctrl2[9]                                      TO 80 SKIP

        "GS&A Labor"
        STRING(ctrl[10] * 100,">>9.99") + "%"         TO 30
        ctrl2[10] / qm                                TO 48
        ctrl2[10]                                     TO 80 SKIP.

    IF ld-dm-mrkp NE 0 THEN
      PUT "Direct Material Markup"
          STRING(cematl-dec,">>9.99") + "%"           TO 30
          ld-dm-mrkp / qm                             TO 48
          ld-dm-mrkp                                  TO 80 SKIP.

    tt-tot = tt-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp.
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
END.

IF cerunf EQ "Dee" THEN
  ASSIGN
   tprep-mat = tprep-mat + ld-pmrkp[1]
   tprep-lab = tprep-lab + ld-pmrkp[2].

/* end ---------------------------------- copr. 1993  advanced software, inc. */
