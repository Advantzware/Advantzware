/* ------------------------------------------------ util/reopenyr.p 03/01 JLF */
/*                                                                            */
/* g/l year end re-open                                                       */
/*                                                                            */
/* -------------------------------------------------------------------------- */
{custom/globdefs.i}
{sys/inc/var.i NEW shared}
{sys/form/s-top.f}

def var start-date as   DATE NO-UNDO.
def var end-date   like start-date NO-UNDO.
def var ret-bal    as   DEC NO-UNDO.
def var fisc-yr    as   int format "9999" NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.

def buffer b-acc for account.

ASSIGN cocode = g_company
       locode = g_loc.

form skip(18)
     with title "                  G / L    Y E A R - E N D    R E - O P E N                     "
     frame yclo row 2 width 80 no-labels overlay.
     

view frame yclo.

find first company where company.company eq cocode.

find first gl-ctrl where gl-ctrl.company eq cocode no-lock no-error.

find first period
    where period.company eq cocode
      and period.pstat   eq yes
    no-lock no-error.
assign
 fisc-yr = (if avail period then period.yr else year(today))
 fisc-yr = fisc-yr - int(not company.yend-per) - 1
 choice  = no.
 
find first period
    where period.company eq cocode
      and period.yr      gt fisc-yr
      and period.pstat   eq no
    no-lock no-error.
if avail period then do:
  message "ERROR: Cannot reopen year when a period in a " +
          "subsequent year has been closed." VIEW-AS ALERT-BOX ERROR.
  
end.

else 
do on endkey undo, leave:
  pause 0.
  display skip(1) "  Which Year?" fisc-yr space(3) skip(1)
         with frame fisc-yr row 5 centered no-labels no-box OVERLAY              .
  message "Are you sure you wish to reopen this fiscal year?" update choice VIEW-AS ALERT-BOX.
end.

if choice then do on error undo, leave:
  {util/reopenyr.i cocode}
  
  company.yend-per = no.
end.

status default "".

hide all no-pause.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
