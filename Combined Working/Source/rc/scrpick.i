/*  rc/scrpick.i - scrolling record lookup

09.06.97 by CAH on \\ricky\robj8\ Log#0000:
1.  Added &DATAGO parameter, kicks in after record is selected and
    lib_recid_ret is set, but prior to assigning &RETURN value.

08.29.93 by CH on TI4000 Log#0000:
1.  Removed displayf from from definition.  This precluded use of when phrase.
*/

DEF VAR debug AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR debug_msg AS CHAR FORMAT 'x(30)' NO-UNDO.
FORM WITH FRAME f-debug CENTER ROW 20 TITLE "Debugging Messages" NO-LABELS.

DEF VAR MSG AS CHAR EXTENT 10 NO-UNDO.
MSG[1] =
"Point to Record, Return|" + KBLABEL("GO") + "=Select, f3=Setup, "
  + KBLABEL("END-ERROR") + "=Exit, " + "{rc/keymove.i}=Jump".
MSG[2] = scr_firstkeycap + "=First, " +
scr_lastkeycap + "=Last, Pg-Up=Prior Screen, Pg-Dn=Next Screen".
MSG[3] =
KBLABEL("GO") + "=Save, " +
  KBLABEL("END-ERROR") + "=Undo, f7=Recall, f8=Clear".
MSG[4] = "Add Record; " + KBLABEL("END-ERROR") + "=Switch to Change Mode".
MSG[5] = "Enter desired Jump to value; "
+ KBLABEL("END-ERROR") + "=Return to Change Mode".
MSG[6] = "Cannot JUMP to position indicated, try another".

/*  scrpick.i - scrolling pick list

04.20.93 by CH on TI4000:
1.  Moved key definition messages inside of scr_paint loop to eliminate
blinking and constant redisplay when srolling.

04.18.93 by CH on TI4000:
1.  Moved color definitions into rc/palette.i.  Added c_norm and c_pick to this.
2.  Color on MOVE made same as c_pick.
04.17.93 by CH on TI4000:
1.  Added support for GO key to select record as well as RETURN.  This allows
for use of a mouse to simultaneously choose row and select record with
a single left-mouse-down|up sequence.
02.06.93 BY CH:
1.  Reversed color on c_pick in DOS to improve contrast against scrfm.i frames
which used the same color combination.
2.  Added help prompts for f3 setup.
3.  On entry, if no match is found, program beeps, prompts for f3 and waits
3 seconds.  If no response, clears message and leaves.

05.05.92 by CH:
1.  Added f3 hook to call file maintenance rc/fmhelp.p

04.09.92 by ch:
1.  Removed &type (frame-value) from &return assignment if string or no &type.
2.  This did not work, so made special if do case for "date" &type.
3.  This can't work either, set back more or less as it was.

09.04.91 by CH:
1.  Added parameter &DetFunct.  You can pass an expression to be processed
just prior to display of scroll line via this parameter.  Commonly would
be used to compute the value of a variable to be included in the pick line.

08.09.91 BY CH:
1.  Added statements to initialize scr_firstrec and scr_lastrec so that
find last is not done on recursize calls, e.g. from within fmx2.i

07.09.91 by CH:
1.  Split out scr_* variables, in rc/loginv.i and rc/sharedv.i (used by fmx)
2.  Reset scrollbar_down to "v" during lib_frame_rows counter to support
recursive calls.  Previously would not pick in fmx if record previously
picked was on final screen of scrolling sequence.
3.  Added initialization code for scrollbar_up/down to assume scrolling.  Other
wise bell and exit if called 2x from fmx with last call in final page of
scrolling sequence.

06.10.91 by CH:

1.  Corrected beginning and end logic and made invoking keys opsys dependent.
2.  Moved scrollbar frame down to line 4 to eliminate clearing of 1..3.
3.  Default is for 2 line headings, used column-heading "!xxx" to force
this on data which has only one heading line.  This hopefully will be
made soft in a future release.

06.05.91 by ch: added scroll bars in right-hand side of pick list.
"^" or "v" are displayed in first/last line when records are
available in that direction.

*/
{rc/scrvars.i}
DEF VAR scr_firstrec AS RECID NO-UNDO INITIAL ?.
DEF VAR scr_lastrec AS RECID  NO-UNDO INITIAL ?.
DEF VAR scrollbar_up       AS CHARACTER FORMAT "x(01)" NO-UNDO.
DEF VAR scrollbar_down     AS CHARACTER FORMAT "x(01)" NO-UNDO.
DEF VAR scr_paint AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR scr_paint_dir AS INTEGER FORMAT -9 INITIAL 1 NO-UNDO.
DEF VAR scr_paint_rec AS RECID INITIAL ? NO-UNDO.
DEF VAR scrpick_runsetup AS LOGICAL NO-UNDO.
/* DEFINE VARIABLE f-det-title     AS CHARACTER NO-UNDO INITIAL "{&TITLE}". */
/*    ( "{&title}" + " - [f3] For Setup" ) */

IF OPSYS = "MSDOS" OR OPSYS BEGINS "WIN" THEN
DO:
  c_pick = "LT-CYAN/BLACK".
  c_norm = "NORMAL".
END.

{&init}

_main:
DO WHILE KEYFUNCTION(LASTKEY) <> "END-ERROR":

  scr_firstrec = ?.
  scr_lastrec = ?.
  lib_recid_ret = ?.
  scrollbar_up    = "^".  /* assume scrolling possible upon entry */
  scrollbar_down  = "v".

  lib_frame_rows = 0.
  FOR EACH {&file} {&Index} {&Condition} NO-LOCK:
    IF lib_frame_rows = 0 THEN
    scr_firstrec = RECID({&File}).
    IF lib_frame_rows < 10 THEN
    lib_frame_rows = lib_frame_rows + 1.
    ELSE
    DO:
      scrollbar_down = "v".
      LEAVE.
    END.  /* 07.09.91 by CH */
  END.

  IF scr_firstrec = ?  /* no matches */ THEN
  DO:
    BELL.
    MESSAGE COLOR VALUE(c_wrn)
      "No matching records found; Switch to setup program?"
      UPDATE scrpick_runsetup.
    IF scrpick_runsetup THEN
    DO:
      RUN rc/fmhelp.p (INPUT "{&file}").
      HIDE ALL NO-PAUSE.
      MESSAGE "Refreshing Choices ...".
      NEXT _main.
    END.
    HIDE MESSAGE NO-PAUSE.
    LEAVE _main.
  END.
  ELSE
  DO:        /* at least one record satisfies condition */
    FIND LAST {&File} {&Index} {&Condition} NO-LOCK NO-ERROR.
    IF AVAILABLE ({&File}) THEN
    scr_lastrec = RECID({&File}).
  END.


  FORM
    {&file}.{&Choose}
    WITH SCROLL 1 lib_frame_rows DOWN CENTER OVERLAY
    FRAME {&Frame} TITLE f-det-title
    COLOR DISPLAY VALUE(c_pick) {&row}.

  FORM
    scrollbar_up SKIP(8) scrollbar_down
    WITH FRAME f-scrollbar COLOR DISPLAY VALUE(c_pick) NO-LABELS NO-BOX OVERLAY.

  scr_paint = TRUE.
  scr_paint_dir = 1.
  scr_paint_rec = scr_firstrec.

  pick:
  DO WITH FRAME {&Frame} ON ERROR UNDO, LEAVE _main:
    {rc/debug.i "top of pick block"}.
    looper:
    REPEAT WITH FRAME {&Frame}:
      {rc/debug.i "top of looper block"}.

      IF scr_paint = TRUE THEN
      DO:
        {rc/debug.i "scr_paint = true"}.
        IF scrollbar_up   = "" AND scr_paint_dir = -1
          OR scrollbar_down = "" AND scr_paint_dir = +1
          THEN
        DO:   /* can't scroll */
          BELL.
          scr_paint = FALSE.
          LEAVE.
        END.
        CLEAR FRAME {&frame} ALL NO-PAUSE.
        FIND {&file} WHERE RECID({&file}) = scr_paint_rec NO-LOCK.
        IF scr_paint_dir > 0 THEN
        DO:
          lib_i = 1.
          IF FRAME-LINE({&frame}) > 0 THEN
          UP (FRAME-LINE ({&Frame}) - 1) WITH FRAME {&Frame}.
        END.
        ELSE
        DO:
          lib_i = lib_frame_rows.
          IF FRAME-LINE({&frame}) <= lib_frame_rows THEN
          DOWN (lib_frame_rows - FRAME-LINE ({&Frame})) WITH FRAME {&Frame}.
        END.
        REPEAT:
          PAUSE 0 NO-MESSAGE.
          {&detfunct}.
          DISPLAY {&file}.{&Choose} {&DisplayF} WITH FRAME {&Frame}.
          lib_recids[lib_i] = RECID({&file}).
          lib_i = lib_i + scr_paint_dir.
          IF lib_i > lib_frame_rows OR lib_i = 0 THEN
          LEAVE.
          DOWN scr_paint_dir WITH FRAME {&frame}.
          IF scr_paint_dir > 0
            THEN
          FIND NEXT {&file} {&Index} {&Condition} NO-LOCK NO-ERROR.
          ELSE
          FIND PREV {&file} {&Index} {&Condition} NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ({&file}) THEN
          DO:
            IF scr_paint_dir < 0 /* prev-page */ THEN
            DO:
              scr_paint = TRUE.
              scr_paint_rec = scr_firstrec.
              scr_paint_dir  = 1.
              scrollbar_down = "v".
              NEXT looper.
            END.
            IF scr_paint_dir > 0 /* next-page */ THEN
            DO:
              scr_paint = TRUE.
              scr_paint_rec = scr_lastrec.
              scr_paint_dir  = -1.
              scrollbar_up = "^".
              NEXT looper.
            END.
          END.
        END.
        IF scr_paint_dir > 0   /* move cursor to first item on list */
          THEN
        UP (FRAME-LINE ({&Frame}) - 1) WITH FRAME {&Frame}.

        {rc/debug.i "about to show messages:"}.

        MESSAGE COLOR VALUE(c_norm) msg[1].
        MESSAGE COLOR VALUE(c_norm) msg[2].

      END.  /* do when scr_paint = true */
      scr_paint = FALSE.

      IF lib_recids[1] = scr_firstrec
        THEN
      scrollbar_up = "".
      ELSE
      scrollbar_up = "^".
      IF lib_recids[lib_frame_rows] = scr_lastrec
        THEN
      scrollbar_down = "".
      ELSE
      scrollbar_down = "v".

      IF lib_frame_rows >= 10 THEN
      DISPLAY scrollbar_up scrollbar_down
        WITH FRAME f-scrollbar
        COLUMN (FRAME-COL({&Frame}) - 1)
        ROW (FRAME-ROW({&Frame}) + 4)
        OVERLAY.



      DEF VAR ROWNUM AS INTEGER NO-UNDO.
      /* ON MOVE MOVE. */

      CHOOSE ROW {&file}.{&Choose} GO-ON (HOME F3 {rc/keymove.i})
        NO-ERROR WITH FRAME {&Frame}.
      {rc/debug.i "after choose"}
      COLOR DISPLAY VALUE(c_pick) {&file}.{&choose} WITH FRAME {&frame}.
      {rc/debug.i "before move"}.
      PAUSE 0 NO-MESSAGE.

      rownum = FRAME-LINE(f-det).


      IF KEYLABEL(LASTKEY) = "{rc/keymove.i}" THEN
      DO:
        HIDE MESSAGE NO-PAUSE.
        MESSAGE COLOR VALUE(C_CHG) msg[5].
        PROMPT-FOR {&FILE}.{&CHOOSE} WITH FRAME F-POSIT CENTER
          TITLE "JUMP TO" SIDE-LABELS OVERLAY
          COLOR VALUE(c_pick) NO-VALIDATE.
        HIDE FRAME F-POSIT.
        {&POSIT}
        FIND FIRST {&FILE} {&CONDITION} AND
          {&FILE}.{&CHOOSE} >= INPUT FRAME F-POSIT {&FILE}.{&CHOOSE}
          NO-LOCK NO-ERROR.
        IF AVAILABLE ({&FILE}) THEN
        DO:
          scr_paint = TRUE.
          scr_paint_rec = RECID({&FILE}).
          scr_paint_dir = +1.
          scrollbar_down = "v".
          NEXT looper.
        END.
        ELSE
        DO:
          BELL.
          MESSAGE COLOR VALUE(C_MSG) msg[6].
          PAUSE 1.
          UNDO, NEXT.
        END.
      END.


      IF KEYLABEL(LASTKEY) = "f3" THEN
      DO:
        RUN rc/fmhelp.p (INPUT "{&file}").
        HIDE ALL NO-PAUSE.
        MESSAGE "Refreshing Choices ...".
        PAUSE 0 NO-MESSAGE.
        NEXT _main.
      END.

      IF LASTKEY = scr_firstkey THEN
      DO:
        scr_paint = TRUE.
        scr_paint_rec = scr_firstrec.
        scr_paint_dir = +1.
        scrollbar_down = "v".
        NEXT looper.
      END.
      ELSE
      IF LASTKEY = scr_lastkey THEN
      DO:
        scr_paint = TRUE.
        scr_paint_rec = scr_lastrec.
        scr_paint_dir = -1.
        scrollbar_up = "^".
        NEXT looper.
      END.
      ELSE
      IF LASTKEY = KEYCODE("CURSOR-DOWN") OR LASTKEY = KEYCODE("TAB")
        OR LASTKEY = KEYCODE(" ") THEN
      DO:
        {rc/debug.i "cursor down"}.
        FIND {&file} WHERE
          RECID({&file}) = lib_recids[FRAME-LINE({&frame})] NO-LOCK.

        IF RECID({&File}) = scr_lastrec THEN
        BELL.
        ELSE
        DO:
          FIND NEXT {&file} {&Index} {&Condition} NO-LOCK NO-ERROR.
          {rc/debug.i "before frame down"}
          DOWN WITH FRAME {&Frame} COLOR DISPLAY VALUE(c_pick).
          DO lib_i = 1 TO lib_frame_rows:
            IF lib_i = 10 THEN
            lib_recids[lib_i] = RECID({&file}).
            ELSE
            lib_recids[lib_i] = lib_recids[lib_i + 1].
          END.
          {&detfunct}.
          {rc/debug.i "before display next record"}
          DISPLAY {&file}.{&Choose} {&DisplayF} WITH FRAME {&Frame}.
          {rc/debug.i "after display next record"}
          NEXT.
        END.
        {rc/debug.i "end of cursor down"}.
      END.
      ELSE
      IF LASTKEY = KEYCODE("CURSOR-UP")
        OR LASTKEY = KEYCODE("BACK-TAB") THEN
      DO:
        FIND {&file} WHERE
          RECID({&file}) = lib_recids[FRAME-LINE({&frame})] NO-LOCK.
        IF RECID({&File}) = scr_firstrec THEN
        BELL.
        ELSE
        DO:
          FIND PREV {&file} {&Index} {&Condition} NO-LOCK NO-ERROR.
          UP WITH FRAME {&Frame} COLOR DISPLAY VALUE(c_pick).
          DO lib_i = lib_frame_rows TO 1 BY -1:
            IF lib_i = 1 THEN
            lib_recids[lib_i] = RECID({&file}).
            ELSE
            lib_recids[lib_i] = lib_recids[lib_i - 1].
          END.
          {&detfunct}.
          DISPLAY {&file}.{&Choose} {&DisplayF} WITH FRAME {&Frame}.
          NEXT.
        END.
      END.
      ELSE
      IF LASTKEY = KEYCODE("PAGE-UP") THEN
      DO:
        IF lib_frame_rows < 10 THEN
        NEXT.
        scr_paint = TRUE.
        scr_paint_dir = -1.
        scr_paint_rec = lib_recids[1].
        scrollbar_up = "^".
        NEXT.
      END.
      ELSE
      IF LASTKEY = KEYCODE("PAGE-DOWN") THEN
      DO:
        IF lib_frame_rows < 10 THEN
        NEXT.
        scr_paint = TRUE.
        scr_paint_dir = +1.
        scr_paint_rec = lib_recids[lib_frame_rows].
        scrollbar_down = "v".
        NEXT.
      END.
      ELSE
      IF LASTKEY = KEYCODE("RETURN")
        OR KEYFUNCTION(LASTKEY) = "GO" THEN
      DO:
        lib_recid_ret = lib_recids[FRAME-LINE({&frame})].
        find {&file} where recid({&file}) = lib_recid_ret no-lock no-error.
        if not avail {&file} then do:
            assign lib_recid_ret = ?.
            message color value(c_err) "Fatal error in:" program-name(1).
            bell.
            pause.
            return.
        end.    
        {&datago}
        IF CAPS(SUBSTRING("{&type}",1,2)) = "ST" OR LENGTH("{&type}") = 0
          THEN
        DO:
          {&return} = {&TYPE}(FRAME-VALUE).
        END.
        ELSE
        IF FRAME-VALUE MATCHES "*~?*" THEN
        DO:
          {&return} = ?.
        END.
        ELSE
        DO:
          {&return} = {&type}(FRAME-VALUE).
        END.
        LEAVE _main.
      END.
      ELSE
      DO:
        BELL.
        NEXT.
      END.
      {rc/debug.i "Bottom of Looper"}.
    END.  /* looper */
  END. /* pick */

END. /* main */

CLEAR FRAME {&Frame} ALL.
HIDE FRAME {&Frame} NO-PAUSE.
IF lib_frame_rows >= 10 THEN
HIDE FRAME f-scrollbar NO-PAUSE.
HIDE MESSAGE NO-PAUSE.

if connected("asi") then do:
ON F5 choices.
ON F6 HELP.   /* Menu jumping - added CTS */
ON F9 HELP.
ON F10 HELP.
ON TAB RETURN.
end.


