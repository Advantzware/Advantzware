/* ---------------------------------------------------------- menu.p  9/94 RM */
/* Foldware Main Menu Program                                                 */
/* -------------------------------------------------------------------------- */
/* Program Commented Out DO NOT NEED
HIDE ALL NO-PAUSE.
{sys/inc/var.i "NEW" "GLOBAL" "SHARED" } /* CTS added "global" to variables */
DEFINE NEW GLOBAL SHARED VARIABLE mess AS ch FORMAT "x(80)" EXTENT 2.
DEFINE NEW GLOBAL SHARED VARIABLE v-est-only AS LOGICAL.
if not connected("edi") and search("edi.pf") <> ? then do:
    connect -pf value(search("edi.pf")) no-error.
end.
if connected("edi") then do:
        run rc/genrcvars.p.
        run ed/login.p.
end.
/* CTS */
v-est-only = FALSE. /* Set to true for estimating only customers */
mess[1] = " Esc=Quit  F1=Go  F2=Help  F3=Insert  F4=Quit  F5=Lookup  F7=Recall F8=Clear    " .
mess[2] = "".
IF OPSYS = "msdos" THEN
DO:
  IF SEARCH("util\tst") NE ? THEN
  DO:
    IF TERMINAL = "CO80" THEN
    DOS silent util\tst.
  END.
  ASSIGN tmp-dir = "tmp~\"
    lorow   = 22.
  on f24 help.
  on f9 new-line.
END.
ELSE
IF OPSYS = "unix" THEN
DO:
  ASSIGN bypass = NO.
  INPUT THROUGH echo $HOME no-echo.
  SET tmpstore.
  /* Create tmp directory if it dose not exist. */
  INPUT THROUGH ls -F value(tmpstore) no-echo.
  REPEAT:
    SET v-filname FORMAT "x(50)".
    IF TRIM(v-filname) = "tmp/" THEN
    ASSIGN bypass = YES.
  END.
  tmp-dir = tmpstore + "/tmp/".
  IF NOT bypass THEN
  UNIX silent mkdir value(tmp-dir).
  ASSIGN bypass = NO.
  IF tmp-dir = "//tmp/" THEN
  DO:
    INPUT THROUGH echo $RCODE no-echo.
    SET tmp-dir FORMAT "x(75)".
    tmp-dir = TRIM(tmp-dir) + "/tmp/".
  END.
  ASSIGN tmpstore = ""
    lorow    = 21.
  INPUT CLOSE.
END.
/* Timeout check - comment out for non-demo systems */
def var xp_date as da initial 02/15/99.
if today > xp_date then do:
display skip(1)
"     Your FOLDWARE license agreement expired on" xp_date       skip(1)
" Please contact Advanced Software, Inc. at (215) 579-0492 to " skip(1)
"   R E N E W   Y O U R   L I C E N S E   A G R E E M E N T   " skip(1)
with title color value(col-input)
"   L I C E N S E   A G R E E M E N T   E X P I R A T I O N   "
color value(col-error) row 8 centered frame sorry no-labels.
hide frame sorry.
quit.
end. /* end of timeout check */
/* number of menu option */
DEFINE VARIABLE chosen     AS INTEGER             INITIAL 1.
/* number of choices on the menu */
DEFINE    VARIABLE cmdcount   AS INTEGER          INITIAL 15. /* DAR */
/* menu strings */
DEFINE    VARIABLE cmd        AS CHARACTER    FORMAT "x(30)" EXTENT 16. /*DAR*/
DEFINE    VARIABLE cmdlist    AS CHARACTER     INITIAL "ABCDEFGHIJKLMSX".
def new shared var parent_cmdlist as char no-undo.
def new shared var child_chosen   as int  no-undo.
parent_cmdlist = cmdlist.
child_chosen = 0.
/* DAR - R */
DEFINE VARIABLE lastchoice AS INTEGER                              INITIAL 1.
DEFINE VARIABLE proglist   AS CHARACTER                   EXTENT 15. /*DAR*/
DEFINE VARIABLE qgo        AS LOGICAL                          INITIAL FALSE.
DEFINE VARIABLE ksel       AS CHARACTER FORMAT "x".
DEFINE VARIABLE ksel2      LIKE ksel.
/* other menu strings */
DEFINE VARIABLE mstr2      AS CHARACTER FORMAT "x(34)" EXTENT 8.
DEFINE VARIABLE mstr3      AS CHARACTER FORMAT "x(37)" EXTENT 8.
ON F6 HELP. /* CTS goto menu jumping */
ON F5 choices.
ON F9 HELP.
ON TAB RETURN.
{sys/form/s-top.f}   /* 1st Screen line - date,title, time */
IF v-est-only THEN
DO:
ASSIGN
proglist[1]   =  "ce/menu.p"
proglist[2]   =  "sys/inc/noa.p"
proglist[3]   =  "rm/menu.p"
proglist[4]   =  "fg/menu.p"
proglist[5]   =  "sys/inc/noa.p"
proglist[6]   =  "sys/inc/noa.p"
proglist[7]   =  "sys/inc/nop.p"
proglist[8]   =  "sys/inc/noa.p"
proglist[9]   =  "sys/inc/noa.p"
proglist[10]  =  "ar/menu.p"
proglist[11]  =  "sys/inc/noa.p"
proglist[12]  =  "gl/menu.p"
/* proglist[13]  =  "sys/inc/nop.p" */ /* CTS */
/* proglist[14]  =  "sys/ref/sys.p"  DAR */
proglist[13]  =  "sys/admin/menu.p"
proglist[14]  =  "".
proglist[15]  =  "".
END.
ELSE
DO:
ASSIGN
proglist[1]   =  "ce/menu.p"
proglist[2]   =  "oe/menu.p"
proglist[3]   =  "rm/menu.p"
proglist[4]   =  "fg/menu.p"
proglist[5]   =  "sa/menu.p"
proglist[6]   =  "pc/menu.p"
proglist[7]   =  "sys/inc/nop.p"
proglist[8]   =  "jc/menu.p"
proglist[9]   =  "po/menu.p"
proglist[10]  =  "ar/menu.p"
proglist[11]  =  "ap/menu.p"
proglist[12]  =  "gl/menu.p"
proglist[13]  =  "ed/asi/menu.p"
proglist[14]  =  "sys/admin/menu.p"
proglist[15]  =  ""
.
END.
/* Main menu choices */
cmd[1]   = "  A. Cost Estimating".
cmd[2]   = "  B. Order Processing".
cmd[3]   = "  C. Raw Materials".
cmd[4]   = "  D. Finished Goods ".
cmd[5]   = "  E. Sales Analysis".
cmd[6]   = "  F. Production Control".
cmd[7]   = "  G. Plant Data Collection".
cmd[8]   = "  H. Job Costing".
cmd[9]   = "  I. Purchasing".
cmd[10]  = "  J. Accounts Receivable".
cmd[11]  = "  K. Accounts Payable".
cmd[12]  = "  L. General Ledger".
cmd[13]  = "  M. EDI".                          /* 9702 CAH */
cmd[14]  = "  S. System Administration".
cmd[15]  = "  X. EXIT".
DEFINE NEW SHARED FRAME f-cmd.
{menu.f}
INPUT CLEAR.
RUN login.p.
DISPLAY cmd WITH FRAME f-cmd.
COLOR DISPLAY VALUE(col-mess) cmd[chosen] WITH FRAME f-cmd.
/* main menu loop */
GETCHOICE:
REPEAT:
  NUFILE = NO.
  IF passone THEN
  DO TRANSACTION:
    FIND FIRST usr WHERE usr.uid = USERID(LDBNAME(1)) NO-ERROR.
    IF AVAILABLE usr THEN
    DO:
      ASSIGN global-uid = usr.uid
        usr-lang   = usr.usr-lang.
      FIND FIRST asi.company WHERE asi.company.company = usr.company
      NO-LOCK NO-ERROR.
    END.
    IF NOT AVAILABLE asi.company THEN
    FIND FIRST asi.company NO-LOCK NO-ERROR.
    RUN sys/ref/setcolor.p.   /* CTS - added to set application colors
                                       via sys-ctrl records set by user */
    /*
    if not available asi.company then do:
    run sys/ref/company.p .
    find first asi.company no-lock no-error.
    if not available asi.company then leave.
    cocode = asi.company.company.
    find first usr where usr.uid = userid(ldbname(1)) no-error.
    run sys/ref/loc.p .
    run sys/admin/setco.p.
    usr.company = cocode.
    usr.loc = locode.
    end.
    if available asi.company then do:
    find first usr where usr.uid = userid(ldbname(1)) no-error.
    if not available usr then find first usr where usr.uid = "root".
    usr.company = asi.company.company.
    find first loc where loc.company = asi.company.company no-lock no-error.
    usr.loc = loc.loc.
    cocode = usr.company.
    locode = usr.loc.
    find first asi.company where asi.company.company = cocode no-lock no-error.
    coname = asi.company.name.
    find first loc where loc.loc = locode no-lock no-error.
    loname = loc.dscr.
    sysdate = today.
    module = coname + " - " + locode.
    siz = (56 - length(module)) / 2.
    module = fill(" ",siz) + module .
    day_str = string(sysdate).
    passone = false.
    if keyfunction(lastkey) = "end-error" then leave.
    release asi.company.
    release loc.
    end.
    */
    IF AVAILABLE usr AND AVAILABLE asi.company THEN
    DO:
      FIND FIRST loc WHERE loc.company = asi.company.company AND
        loc.loc     = usr.loc NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loc THEN
      FIND FIRST loc WHERE loc.company = asi.company.company NO-LOCK NO-ERROR.
      IF AVAILABLE asi.company AND AVAILABLE loc AND AVAILABLE usr
        THEN
      DO:
        ASSIGN global-uid = usr.uid
          cocode            = usr.company
          locode            = usr.loc
          coname            = asi.company.name
          loname            = loc.dscr
          sysdate           = TODAY
          module            = coname + " - " + locode
          siz               = (56 - LENGTH(module)) / 2
          module            = FILL(" ",siz) + module
          day_str           = STRING(sysdate).
        IF KEYFUNCTION(LASTKEY) = "end-error" THEN
        LEAVE.
        RELEASE asi.company.
        RELEASE loc.
        RELEASE usr.
      END.
    END.
    passone = FALSE.
  END.
  RUN sys/ref/setcolor.p.   /* CTS - added to set application colors
                                     via sys-ctrl records set by user */
  FIND FIRST period WHERE period.company = cocode AND
    period.pstat   = TRUE   AND
    period.pst <= sysdate   AND
    period.pend >= sysdate NO-LOCK NO-ERROR.
  IF NOT AVAILABLE period THEN
  FIND LAST period WHERE period.company = cocode AND
    period.pstat   = TRUE NO-LOCK NO-ERROR.
  IF AVAILABLE period THEN
  period = period.pnum.
  {sys/sho/s-top.v}  /* display screen line 1 */
  /* if cursor moved to new choice , rest hilites */
  IF lastchoice NE chosen
    THEN
  DO WITH FRAME f-cmd:
    COLOR DISPLAY VALUE(col-norm)  cmd[lastchoice].
    COLOR DISPLAY VALUE(col-mess)  cmd[chosen].
    lastchoice = chosen.
  END.
  READKEY PAUSE 60.
  HIDE MESSAGE NO-PAUSE.
  if keylabel(lastkey) = "f5" then do:
    run dict.p.
    view frame f-cmd.
    next getchoice.
  end.
  /* on escape, exit is selected */
  IF KEYFUNCTION(LASTKEY) = "end-error" THEN
  DO:
    chosen = cmdcount.
    qgo = TRUE.
  END.
  ELSE
  IF LASTKEY = -1
    THEN
  DO:
    UNDO GETCHOICE.
    NEXT GETCHOICE.
  END.
  ELSE
  IF KEYFUNCTION(LASTKEY) = "help"
    THEN
  DO:
    APPLY(LASTKEY).
    NEXT GETCHOICE.
  END.
  ELSE   /* pick previous option */
  IF LASTKEY = KEYCODE("up") OR LASTKEY = KEYCODE("page-up")
    THEN
  DO:
    chosen = chosen - 1.
    IF chosen = 0 THEN
    chosen = cmdcount.
    NEXT GETCHOICE.
  END.
  ELSE  /* pick next option */
  IF ( LASTKEY = KEYCODE("down") OR LASTKEY = KEYCODE("page-down") OR
    LASTKEY = KEYCODE("tab") )
    THEN
  DO:
    chosen = chosen + 1.
    IF chosen GT cmdcount THEN
    chosen = 1.
    NEXT GETCHOICE.
  END.
  ELSE
  IF LASTKEY = KEYCODE("home")
    THEN
  DO:
    chosen = 1.
    NEXT GETCHOICE.
  END.
  ELSE
  IF LASTKEY = KEYCODE("end")
    THEN
  DO:
    chosen = 6.
    NEXT GETCHOICE.
  END.
  ELSE /* user pressed first letter of a menu choice */
  IF INDEX(cmdlist,KEYLABEL(LASTKEY)) GT 0
    THEN
  DO:
    chosen = INDEX(cmdlist,KEYLABEL(LASTKEY)).
    qgo = TRUE.
  END.
  /* go ahead */
  IF LASTKEY = KEYCODE("return") OR KEYFUNCTION(LASTKEY) = "go" OR qgo
    THEN
  DO:
    /* reset hilites */
    IF lastchoice NE chosen
      THEN
    DO WITH FRAME f-cmd:
      COLOR DISPLAY VALUE(col-norm) cmd[lastchoice].
      COLOR DISPLAY VALUE(col-mess) cmd[chosen].
      lastchoice = chosen.
    END.
    IF chosen < cmdcount
      THEN
    REPEAT WITH FRAME f-cmd:
      COLOR DISPLAY VALUE(col-inac) cmd[chosen].
      DISPLAY cmd[chosen].
      /* hide frame f-cmd no-pause. */
      /*
      if search(proglist[chosen]) = ?
      then run sys/inc/nop.p .
      else run value(proglist[chosen]).
      */
      {sys/inc/runprog.i proglist[chosen]}
      IF proglist[chosen] = "sys/inc/nop.p" OR
        proglist[chosen] = "sys/ref/sys.p" OR
        KEYFUNCTION(LASTKEY) = "end-error" OR
        LASTKEY = -1 THEN
      DO:
        COLOR DISPLAY VALUE(col-mess) cmd[chosen].
        DISPLAY cmd[chosen].
        LEAVE.
      END.
      if child_chosen > 0 then do:
        lastchoice = chosen.
        COLOR DISPLAY VALUE(col-norm) cmd[lastchoice].
        DISPLAY cmd[chosen].
        chosen = child_chosen.
      end.
      else
      IF INDEX(cmdlist,KEYLABEL(LASTKEY)) GT 0
        THEN
      DO:
        lastchoice = chosen.
        COLOR DISPLAY VALUE(col-norm) cmd[lastchoice].
        DISPLAY cmd[chosen].
        chosen = INDEX(cmdlist,KEYLABEL(LASTKEY)).
      END.
      COLOR DISPLAY VALUE(col-mess) cmd[chosen].
      DISPLAY cmd[chosen].
      VIEW FRAME f-cmd.
    END.
    ELSE /* Confirm EXIT choice */
    DO WITH FRAME ver ROW 21 WIDTH 81 CENTERED NO-BOX NO-LABELS OVERLAY
        COLOR VALUE(col-error) PROMPT-FOR VALUE(col-ask):
      choice = YES. /* choice variable is first declared before the
      assignments module (in initial global.i) */
      PAUSE 0.
      UPDATE " Do you wish to EXIT ? Y/N " choice AUTO-RETURN
        EDITING:
        READKEY.
        IF KEYFUNCTION(LASTKEY) = "end-error" THEN
        DO:
          HIDE FRAME ver NO-PAUSE.
          choice = NO.
          LEAVE.
        END.
        ELSE
        APPLY LASTKEY.
      END.
      IF choice THEN
      DO:
        HIDE FRAME f-cmd NO-PAUSE.
        HIDE FRAME ver NO-PAUSE.
        /*if opsys = "MSDOS" then dos silent del value("tmp\0*.*").*/
        IF USERID(LDBNAME(1)) = "root" THEN
        LEAVE.
        ELSE
        QUIT.
      END.
      ELSE
      HIDE FRAME ver NO-PAUSE.
      qgo = FALSE.
    END.
  END.
  /* Wrong key  */
  ELSE
  DO WITH FRAME err ROW 21 WIDTH 81 CENTERED NO-BOX NO-LABELS OVERLAY
      COLOR VALUE(col-error):
    DISPLAY
      "                              Invalid  Choice                                 "
      .
    PAUSE 2 NO-MESSAGE.
  END.
  HIDE FRAME err NO-PAUSE.
END.    /* end getchoice */
HIDE ALL NO-PAUSE.
{sys/sho/s-top.v}
IF SEARCH("util/_Dummy") = ? THEN
DO:
  OUTPUT TO util/_Dummy.
  PUT "             Do Not Erease This File !!! " SKIP
    " This file must exist, The application uses this file! ".
  OUTPUT CLOSE.
END.
/* CTS RoundTable xref */
RETURN.
RUN ce/menu.p.
RUN oe/menu.p.
RUN rm/menu.p.
RUN fg/menu.p.
RUN sa/menu.p.
RUN pc/menu.p.
RUN jc/menu.p.
RUN po/menu.p.
RUN ar/menu.p.
RUN ap/menu.p.
RUN gl/menu.p.
RUN sys/inc/nop.p.
RUN sys/inc/noa.p.
/* RUN sys/ref/sys.p.  */
RUN sys/admin/menu.p.
/* CTS end */
*/
/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
