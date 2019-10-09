/* ---------------------------------------------------- gl/gl-tot.i 12/00 JLF */
/* g/l report -  totaling                                                     */
/* -------------------------------------------------------------------------- */

def input param v-acct as recid.
DEF INPUT PARAM ip-int AS INT NO-UNDO.

{sys/inc/var.i shared}

def buffer b-acc for account.

{gl/gl-fs.i}

def var v-pyear like period.yr      init 0      no-undo.
def var v-fyear like period.yr      init 0      no-undo.
def var v-sperq like period.pnum    init 0      no-undo.
def var v-eperq like period.pnum    init 0      no-undo.
def var v-datep like period.pst     init today  no-undo.
def var v-dateq like period.pst     init today  no-undo.
def var v-datey like period.pst     init today  no-undo.


find first account where recid(account) eq v-acct no-lock no-error.

find first gl-ctrl where gl-ctrl.company eq account.company no-lock.

find first period
    where period.company eq account.company
      and period.pst     le udate
      and period.pend    ge udate
    no-lock no-error.
   
if avail period then
  assign
   v-pyear = period.yr
   v-datep = period.pst.
   
if "{1}" eq "trans" and index("ALCT",account.type) gt 0 then
find first period
    where period.company eq account.company
      and period.pst     le udate
      and period.pstat   eq yes
    no-lock no-error.

if avail period then v-fyear = period.yr.

assign
 v-eperq = (trunc(uperiod / 3,0) + int(uperiod modulo 3 gt 0)) * 3
 v-sperq = v-eperq - 2.
 
if v-sperq gt 9 then
  assign
   v-sperq = 10
   v-eperq = 13.

for each period
    where period.company eq account.company
      and period.yr      eq v-pyear
      and period.pnum    ge v-sperq
      and period.pnum    le v-eperq
    no-lock
    by period.pnum:
    
  v-dateq = period.pst.
  leave.
end.

for each period
    where period.company eq account.company
      and period.yr      ge v-fyear
    no-lock
    by period.yr
    by period.pnum:
    
  v-datey = period.pst.
  leave.
end.

for each gl{1}
    where gl{1}.company  eq account.company
      and gl{1}.actnum   eq account.actnum
      and (gl{1}.tr-date ge v-datey OR
           ("{1}" EQ "trans" AND index("ALCT",account.type) GT 0))
      and gl{1}.tr-date  le udate
    no-lock:

  RUN upd-totals.
end.

if account.actnum eq gl-ctrl.ret then
for each b-acc
    where b-acc.company eq account.company
      and b-acc.actnum  ne gl-ctrl.contra
      and index("RE",b-acc.type) ne 0
    no-lock,

    each gl{1}
    where gl{1}.company  eq b-acc.company
      and gl{1}.actnum   eq b-acc.actnum
      and (gl{1}.tr-date ge v-datey OR
           ("{1}" EQ "trans" AND index("ALCT",account.type) GT 0))
      and gl{1}.tr-date  le udate
    no-lock:
    
  RUN upd-totals.
end.

RETURN.

PROCEDURE upd-totals.

  IF gl{1}.tr-date GE v-datep THEN DO:
    IF ip-int EQ 0 THEN tot[1]  = tot[1]  + gl{1}.tr-amt.
    ELSE
    IF ip-int EQ 3 THEN tot3[1] = tot3[1] + gl{1}.tr-amt.
  END.

  IF gl{1}.tr-date GE v-dateq THEN DO:
    IF ip-int EQ 0 THEN tot[2]  = tot[2]  + gl{1}.tr-amt.
    ELSE
    IF ip-int EQ 3 THEN tot3[2] = tot3[2] + gl{1}.tr-amt.
  END.

  IF ip-int EQ 0 THEN tot[3]  = tot[3]  + gl{1}.tr-amt.
  ELSE
  IF ip-int EQ 3 THEN tot3[3] = tot3[3] + gl{1}.tr-amt.

END PROCEDURE.

/* end ---------------------------------- Copr. 2000  Advanced Software, Inc. */
