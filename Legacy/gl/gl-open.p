/* --------------------------------------------------- gl/gl-open.p 11/01 JLF */
/* Get Opening Balance for a GL Account #                                     */
/* -------------------------------------------------------------------------- */

def input  parameter v-recid    as   recid.
def input  parameter v-curr-yr  as   int.
def output parameter v-open-bal as   dec.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xperiod for period.
def buffer b-acc for account.

DEF SHARED VAR uperiod AS INT NO-UNDO.

def var v-fisc-yr like period.yr no-undo.
def var v-prim-bal like v-open-bal.

assign
 v-open-bal = 0
 v-prim-bal = 0.

find first account where recid(account) eq v-recid no-lock no-error.

if avail account then
find first company where company.company eq account.company no-lock.

if avail company THEN
find first gl-ctrl where gl-ctrl.company eq company.company no-lock.

IF AVAIL gl-ctrl THEN DO:
  find first period
      where period.company eq account.company
        and period.pstat   eq yes
      no-lock no-error.
  v-fisc-yr = (if avail period then period.yr else v-curr-yr) -
              int(not company.yend-per).

  if index("ALCT",account.type) gt 0 then do:
    v-open-bal = v-open-bal + (if v-curr-yr ge v-fisc-yr then account.cyr-open
                                                         else account.lyr-open).

    do i = 1 to (if v-curr-yr gt v-fisc-yr then 13 else (uperiod - 1)):
      v-open-bal = v-open-bal + (if v-curr-yr ge v-fisc-yr then account.cyr[i]
                                                           else account.lyr[i]).
    end.

    for each period
        where period.company eq account.company
          and (period.yr     lt v-curr-yr
           or  (period.yr    eq v-curr-yr and period.pnum lt uperiod))
        no-lock,

        each gltrans
        where gltrans.company eq account.company
          and gltrans.actnum  eq account.actnum
          and gltrans.tr-date ge period.pst
          and gltrans.tr-date le period.pend
        no-lock:
      v-open-bal = v-open-bal + gltrans.tr-amt.
    end.
  end.

  else do:
    if v-curr-yr le v-fisc-yr then 
    do i = 1 to uperiod - 1:
      v-open-bal = v-open-bal + (if v-curr-yr eq v-fisc-yr then account.cyr[i]
                                                           else account.lyr[i]).
    end.

    for each period
        where period.company eq account.company
          and period.yr      eq v-curr-yr 
          and period.pnum    lt uperiod
        no-lock,

        each gltrans
        where gltrans.company eq account.company
          and gltrans.actnum  eq account.actnum
          and gltrans.tr-date ge period.pst
          and gltrans.tr-date le period.pend
        no-lock:
        
      v-open-bal = v-open-bal + gltrans.tr-amt.
    end.
  end.

  if (account.actnum eq gl-ctrl.ret or account.actnum eq gl-ctrl.contra) then
  for each b-acc
      where b-acc.company eq account.company
        and index("RE",b-acc.type) ne 0
      no-lock,

      each period
      where period.company eq account.company
        and (period.yr     lt v-curr-yr
         or  (period.yr    eq v-curr-yr and period.pnum lt uperiod))
      no-lock,

      each gltrans
      where gltrans.company eq b-acc.company
        and gltrans.actnum  eq b-acc.actnum
        and gltrans.tr-date ge period.pst
        and gltrans.tr-date le period.pend
      no-lock:

    if account.actnum eq gl-ctrl.ret then
      v-open-bal = v-open-bal + gltrans.tr-amt.
    else
    if period.yr eq v-curr-yr then
      v-open-bal = v-open-bal - gltrans.tr-amt.
  end.
end.
