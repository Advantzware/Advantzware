/* ------------------------------------------------ util/reopenyr.p 03/01 JLF */
/*                                                                            */
/* g/l year end re-open                                                       */
/*                                                                            */
/* -------------------------------------------------------------------------- */
USING system.SharedConfig.
{custom/globdefs.i}
{sys/inc/var.i NEW shared}
{sys/form/s-top.f}

def var start-date as   DATE NO-UNDO.
def var end-date   like start-date NO-UNDO.
def var ret-bal    as   DEC NO-UNDO.
def var fisc-yr    as   int format "9999" NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEFINE VARIABLE scInstance AS CLASS system.SharedConfig NO-UNDO.

def buffer b-acc for account.
DEFINE BUFFER bf-period FOR period.
DEFINE BUFFER bf-account FOR account.
DEFINE BUFFER bf-glhist FOR glhist.

 ASSIGN 
        scInstance           = SharedConfig:instance
        cocode =  STRING(scInstance:GetValue("ReOpenPeriodCompany")) NO-ERROR.  
    IF cocode EQ "" THEN
    ASSIGN cocode = g_company .
    locode = g_loc. 
    
/*form skip(18)
     with title "                  G / L    Y E A R - E N D    R E - O P E N                     "
     frame yclo row 2 width 80 no-labels overlay.
     

view frame yclo.  */

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
RUN spProgressBar ("Reopen Period", 1, ?). 
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
         /*display skip(1) "  Which Year?" fisc-yr space(3) skip(1)
         with frame fisc-yr row 5 centered no-labels no-box OVERLAY              . */
  message "Are you sure you wish to reopen this fiscal year?" VIEW-AS ALERT-BOX QUESTION
                                                            BUTTONS OK-CANCEL UPDATE choice .
end.
  
if choice then do on error undo, leave:
  RUN spProgressBar ("Reopen Period", 1, 4).
  {util/reopenyr.i cocode}
  
  RUN spProgressBar ("Reopen Period", 2, 4).
  FIND LAST bf-period
       where bf-period.company eq cocode
       and bf-period.yr      EQ fisc-yr
       and bf-period.pstat   eq no
       no-lock no-error.
  IF AVAIL bf-period THEN
  DO:
    FIND CURRENT bf-period EXCLUSIVE-LOCK NO-ERROR.

    for each glhist
        where glhist.company eq cocode         
        and glhist.tr-date ge bf-period.pst
        and glhist.tr-date le bf-period.pend
        and glhist.period  eq bf-period.pnum          
        :
        glhist.posted = NO.
        status default "Processing glhist: " + glhist.actnum.
    END.  
    RUN spProgressBar ("Reopen Period", 3, 4).
    FOR EACH bf-account
        where bf-account.company EQ cocode:
        bf-account.cyr[bf-period.pnum] = 0.
        status default "Processing Account: " + bf-account.actnum.        
    END.
    bf-period.pstat = YES.
    FIND CURRENT bf-period NO-LOCK NO-ERROR.
    
    FIND FIRST bf-glhist EXCLUSIVE-LOCK
         WHERE bf-glhist.company EQ cocode
         AND bf-glhist.jrnl EQ "CLOSE" 
         AND bf-glhist.glYear EQ (bf-period.yr + 1) NO-ERROR.
    IF AVAIL bf-glhist THEN DELETE bf-glhist.
    RUN spProgressBar ("Reopen Period", 4, 4).
    RELEASE glhist.
    RELEASE bf-account.
  END.      
 
  message " Process Complete"  VIEW-AS ALERT-BOX.
  
end.  

RUN spProgressBar ("Reopen Period", 1, 1).
status default "".  

/* end ---------------------------------- copr. 1992  advanced software, inc. */
