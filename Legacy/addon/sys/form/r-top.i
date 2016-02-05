
def var coname     as   char  format "x(35)" no-undo.
def var loname     as   char  format "x(30)" no-undo.

def var sysdate    as   date  init today     no-undo.
def var period     as   int                  no-undo.
def var udate      as   date  format 99/99/9999      init today no-undo.
def var uperiod    as   int   format ">9"    no-undo.

def var day_str    as   char  format "x(8) " no-undo.
def var tim_str    as   char  format "x(8) " no-undo.
def var str-tit    as   char                 no-undo.
def var str-tit2   as   char                 no-undo.
def var str-tit3   as   char                 no-undo.

def var tmp-dir    as   char                 no-undo.

find first company where company.company eq cocode no-lock.
find first loc     where loc.loc         eq locode no-lock.

assign
 coname  = if avail company then company.name else cocode
 loname  = if avail loc     then loc.dscr     else locode
 tim_str = string(time,"hh:mmam")
 day_str = string(today)
 str-tit = coname + " - " + loname.
 
find first period
    where period.company eq cocode
      and period.pstat   eq yes
      and period.pst     le sysdate
      and period.pend    ge sysdate
    no-lock no-error.
if not avail period then
find last period
    where period.company eq cocode
      and period.pstat   eq yes
    no-lock no-error.
if avail period then period = period.pnum.

