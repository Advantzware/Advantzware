/* ------------------------------------------------------- ed/menu.p 03/97 RCI*/
/* EDI   Processing Sub-Menu Program - EDI Module                             */
/* -------------------------------------------------------------------------  */
{sys/inc/var.i SHARED}
DEF SHARED var parent_cmdlist AS char NO-UNDO.
DEF SHARED var child_chosen   AS int  NO-UNDO.
DEF var chosen     AS int                     initial 1.
DEF var cmdcount   AS int                     initial 99.
DEF var cmnd       AS char    FORMAT "x(30)"  extent 99.
DEF var cmd        AS char    FORMAT "x(30)"  extent 16.
DEF var cmdlist    AS char                    initial "123456".
DEF var cmdlist2   AS char                    initial "ABCDEFGHIJKLMRS".
DEF var lastchoice AS int                     initial 1.
DEF var proglist   AS char                    extent 65.
DEF var hyde       AS char                    initial "YYnnnn".
DEF var qgo        AS log.
IF parent_cmdlist > '' THEN
cmdlist2 = parent_cmdlist.
DEFINE SHARED FRAME f-cmd.
DEFINE NEW SHARED FRAME f-cmd2.
{sys/form/s-top.f}
{menu.f}
{ed/asi/menu.f}
ASSIGN proglist[1]   = "ed/inbound.r"
  proglist[2]   = "ed/outbound.r"
  proglist[3]   = "ed/asi/mntran.r"
  proglist[4]   = "ed/asi/mnfm.r"
  proglist[5]   = "ed/asi/mnrp.r"
  proglist[6]   = "ed/asi/mnutil.r"
  cmnd[1]       = " 1. Inbound Processing"
  cmnd[2]       = " 2. Outbound Processing"
  cmnd[3]       = " 3. Edit..."
  cmnd[4]       = " 4. Maintain..."
  cmnd[5]       = " 5. Reports..."
  cmnd[6]       = " 6. Other..."
  .
parent_cmdlist = cmdlist.
PAUSE 0.
DISPLAY cmnd[1 FOR 6]  WITH FRAME f-cmd2.
COLOR DISPLAY VALUE(col-mess) cmnd[chosen] WITH FRAME f-cmd2.
/* main menu loop */
getchoice:
REPEAT:
  {sys/sho/s-top.v}  /* display screen line 1 */
  /* if cursor moved to new choice , rest hilites */
  IF lastchoice ne chosen
    THEN
  DO WITH FRAME f-cmd2:
    COLOR DISPLAY VALUE(col-norm)  cmnd[lastchoice].
    COLOR DISPLAY VALUE(col-mess)  cmnd[chosen].
    lastchoice = chosen.
  END.
  READKEY PAUSE 60.
  HIDE MESSAGE NO-PAUSE.
  /* on escape, exit is selected */
  IF KEYFUNCTION(LASTKEY) = "end-error" THEN
  DO:
    HIDE FRAME f-cmd2 NO-PAUSE.
    LEAVE.
  END.
  ELSE
  IF LASTKEY = -1
    THEN
  DO:
    UNDO getchoice.
    NEXT getchoice.
  END.
  ELSE
  IF KEYFUNCTION(LASTKEY) = "help"
    THEN
  DO:
    APPLY(LASTKEY).
    NEXT getchoice.
  END.
  ELSE   /* pick previous option */
  IF LASTKEY = KEYCODE("up") OR LASTKEY = KEYCODE("page-up")
    THEN
  DO:
    chosen = chosen - 1.
    IF chosen = 0 THEN
    chosen = cmdcount.
    NEXT getchoice.
  END.
  ELSE  /* pick next option */
  IF ( LASTKEY = KEYCODE("down") OR LASTKEY = KEYCODE("page-down") OR
    LASTKEY = KEYCODE("tab") )
    THEN
  DO:
    chosen = chosen + 1.
    IF chosen gt cmdcount THEN
    chosen = 1.
    NEXT getchoice.
  END.
  ELSE
  IF LASTKEY = KEYCODE("home")
    THEN
  DO:
    chosen = 1.
    NEXT getchoice.
  END.
  ELSE
  IF LASTKEY = KEYCODE("end")
    THEN
  DO:
    chosen = 6.
    NEXT getchoice.
  END.
  ELSE /* user pressed first letter of a menu choice */
  IF INDEX(cmdlist,KEYLABEL(LASTKEY)) gt 0
    THEN
  DO:
    chosen = INDEX(cmdlist,KEYLABEL(LASTKEY)).
    qgo = TRUE.
  END.
  ELSE /* pressed key of previous menu... */
  IF INDEX(cmdlist2,KEYLABEL(LASTKEY)) gt 0
    THEN
  DO:
    HIDE FRAME f-cmd2 NO-PAUSE.
    LEAVE.
  END.
  /* go ahead */
  IF LASTKEY = KEYCODE("return") OR KEYFUNCTION(LASTKEY) = "go" OR qgo
    THEN
  DO:
    /* reset hilites */
    IF lastchoice ne chosen
      THEN
    DO WITH FRAME f-cmd2:
      COLOR DISPLAY VALUE(col-norm) cmnd[lastchoice].
      COLOR DISPLAY VALUE(col-mess) cmnd[chosen].
      lastchoice = chosen.
    END.
    DO WITH FRAME f-cmd2:
      COLOR DISPLAY VALUE(col-inac) cmnd[chosen].
      DISPLAY cmnd[chosen].
      IF substring(hyde,chosen,1) = "y" THEN
      DO:
        HIDE FRAME f-cmd2 NO-PAUSE.
        HIDE FRAME f-cmd  NO-PAUSE.
      END.
      {sys/inc/runprog.i proglist[chosen]}
      IF FRAME-COL(f-cmd) = 0 THEN
      VIEW FRAME f-cmd.
      PAUSE 0.
      IF FRAME-COL(f-cmd2) = 0 THEN
      VIEW FRAME f-cmd2.
      COLOR DISPLAY VALUE(col-mess) cmnd[chosen].
      DISPLAY cmnd[chosen].
      VIEW FRAME f-cmd2.
      IF child_chosen > 0 THEN
      chosen = child_chosen.
      ELSE
      IF INDEX(cmdlist2,KEYLABEL(LASTKEY)) gt 0
        THEN
      DO:
        HIDE FRAME f-cmd2 NO-PAUSE.
        LEAVE getchoice.
      END.
    END.
  END.
  /* wrong key  */
  ELSE
  DO WITH FRAME err row 21 width 81 CENTERED no-box NO-LABELS OVERLAY
      COLOR VALUE(col-error):
    DISPLAY
      "                              Invalid  Choice                                 "
      .
    PAUSE 2 NO-MESSAGE.
  END.
  HIDE FRAME err NO-PAUSE.
END.    /* end getchoice */
/* End ----------------- Copr. 1996 Report Concepts, Inc. ----------------- */
