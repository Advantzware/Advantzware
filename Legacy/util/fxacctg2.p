
{custom/globdefs.i}

{sys/inc/var.i new shared}

def var li as int no-undo.

def temp-table tt-retain like account.
def temp-table tt-contra like account.

def var v-fisc-yr like period.yr no-undo.


SESSION:SET-WAIT-STATE("general").

assign
 cocode = g_company
 locode = g_loc.

find first company where company.company eq cocode no-lock.

find first gl-ctrl where gl-ctrl.company eq cocode no-lock.

create tt-retain.
assign
 tt-retain.cyr = 0
 tt-retain.lyr = 0.

create tt-contra.      
assign
 tt-contra.cyr = 0
 tt-contra.lyr = 0.

find first period
    where period.company eq cocode
      and period.pstat   eq yes
    no-lock no-error.
v-fisc-yr = (if avail period then period.yr else year(today)) -
            int(not company.yend-per).

for each account where account.company eq cocode:
  assign
   account.cyr = 0
   account.lyr = 0.
   
  for each period
      where period.company eq account.company
        and period.yr      ge v-fisc-yr - 1
        and period.yr      le v-fisc-yr
      no-lock,
      
      each glhist
      where glhist.company eq account.company
        and glhist.actnum  eq account.actnum
        and glhist.tr-date ge period.pst
        and glhist.tr-date le period.pend   
      no-lock
      
      by period.yr
      by period.pst:

    if period.yr eq v-fisc-yr then
      account.cyr[period.pnum] = account.cyr[period.pnum] + glhist.tr-amt.

    else
      account.lyr[period.pnum] = account.lyr[period.pnum] + glhist.tr-amt.
       
    if index("RE",account.type) gt 0 then do:
      if period.yr eq v-fisc-yr then
        assign
         tt-retain.cyr[period.pnum] = tt-retain.cyr[period.pnum] + glhist.tr-amt
         tt-contra.cyr[period.pnum] = tt-contra.cyr[period.pnum] - glhist.tr-amt.
         
      else
        assign
         tt-retain.lyr[period.pnum] = tt-retain.lyr[period.pnum] + glhist.tr-amt
         tt-contra.lyr[period.pnum] = tt-contra.lyr[period.pnum] - glhist.tr-amt.
    end.
  end.
end.

find first account
    where account.company eq cocode
      and account.actnum  eq gl-ctrl.ret
    no-error.
if avail account then do:
  do li = 1 to 13:
    assign
     account.cyr[li] = account.cyr[li] + tt-retain.cyr[li]
     account.lyr[li] = account.lyr[li] + tt-retain.lyr[li].
  end.
end.

find first account
    where account.company eq cocode
      and account.actnum  eq gl-ctrl.contra
    no-error.
if avail account then do:
  do li = 1 to 13:
    assign
     account.cyr[li] = account.cyr[li] + tt-contra.cyr[li]
     account.lyr[li] = account.lyr[li] + tt-contra.lyr[li].
  end.
end.

SESSION:SET-WAIT-STATE("").
