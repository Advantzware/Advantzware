/*  rc/scrfm.i - scrolling file maintenance.
05.14.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Changed s debug to s-export.  You cannot define streams inside of
internal procedures, presence of s debug precluded using this include that way.
11.05.96 by CAH on CDS972@812<rprodemo> Log#0000:
1.  Added init function [5] = scr_Nodetails = true.
07.13.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Added addedit and addediti to allow for keystroke editing
of the update of the filename.choose field.
01.02.96 by CAH on CDS972@812<rprodemo> Log#0000:
1.  Added LEFT-END keyfunction to override choose field value.
11.14.95 by CAH on NCR3550@RD-USA [/home/dpoch] Log#0000:
1.  Added ELSE before apply lastkey following help call.
06.09.95 by CAH on CDS972@812 [\rprodemo] Log#0000:
1.  Merged scrfm2.i and scrfm.i into scrfm.i
by creating {x} and {{Xi}} arguments.  Use either one as necessary
to work around 4096 character limitation in include files.
08.25.93 by CH on TI4000 Log#0000:
1.  Removed Upflds from form definition for f-details.  This allows use of
when phrase which is disallowed in form statements.  Since Progress
defaults the form fields automatically, including them in the
form statement was unnecessarily restrictive. Same done for displayf
in form statement for frame f-det.
08.11.93 by CH on TI4000 Log#0000:
1.  Made DataEdit a double-include file field - oe/fmline.i was > 4096.
05.27.93 by CH on TI4000:
1.  Reconciled with scrfm.i.
05.18.93 by CH on TI4000:
1.  Moved dataedit prior to helprkey.i - latter was overriding any custom
help in the former.
04.18.93 by CH on TI4000:
1.  Moved color definitions into rc/palette.i.
2.  Added support for automatic field advance following applhelp calls.
helprkey.i and helptkey.i implement this functionality.
3.  Color on MOVE function made same as c_det for consistancy.
4.  Conditionalized display of scrollbar frame when scrolling is available.
04.15.93 by CH on TI4000:
1.  Added ctrl d to toggle details update.  Available in details frame and
also in scrolling frame.  Presently no indication of status.
2.  Conditionalized exit via go to when addmode is off.  Go on details frame
was causing premature exit.
3.  Shortened pauses for faster response.
4.  Copied changes into new rc/scrfm2.i.

03.02.93 by ch:
1.  Implemented scr_noadd function, required by recent work.
2.  Corrected color references, requires value(c_xxx) not just c_xxx.
02.21.93 by ch:
1.  Improved help messages.
2.  Added GO as alternative to ESC to leave.
3.  Improved color support - variables to hold standard colors.
4.  **** Keylabels are DOS specific, need softening up by OS.

02.06.93 BY CH:
1.  Added &COLOR parameter to override default black/cyan on f-det. C_det
variable sets up the color.

12.12.92 BY ch:
1.  Added optional &Setback to shift f-details up if too long to place relative
to current detail line.

07.15.92 BY CH:
1.  Corrected NEXT-PAGE shift into add mode after changing 9th line just
after exiting from add mode.
2.  Added MOVE key repositioning option.  Use &POSIT if necessary to
customize search.

06.26.92 by CH:
1.  Modified deletion routines to reduce screen repaint.
2.  Processes &detfunc when painting last line on deletion.
3.  PAGE-DOWN modified to use [rownum] instead of [lib_frame_rows]
when latter is unknown.
4.  PAGE-UP modified to use [rownum] instead of [1] when latter is unknown.

06.04.92 by CH:
1.  Added _ACD transaction block to separate code which modifies database from
code which does not.  This prevents _looper from starting transaction
which it technically should not.
2.  This version temporarily named rc/scrfm2.i until testing is complete.

06.03.92 by CH:
1.  Added END-ERROR processing on updates for upfields and detfields.  Prior
version was exiting all the way out.
2.  Added change mode message.
3.  On exit from add mode, up with with frame f-det to move back to last record.

05.04.92 by CH:
1.  Initialized scrollbar_down to "v" on entry; assume that scrolling is
possible.  Failure to do so was preventing scrfm to be re-entrant if
scrollbar_down was "" after last exit from scrfm.

05.01.92 by CH:
1.  Keyedit moved to between ADD and Change code so that it will apply to both.
Was not kicking in on change of record.

04.08.92 by CH:
1.  Added &FUNCTIONS to allow caller to pass available functionality.
Default is YYYY where ADD/CHANGE/DELETE/PICK are respective flags.

04.03.92 by CH:
1.  Problems encountered when calling within outer loop.  Added initialize
code for scr_paint_rec = ? and lib_recids[lib_i] = ?, scr_addmode = False.

*/

DEF VAR scrollbar_up       AS CHARACTER FORMAT "x(01)"  INITIAL "^" NO-UNDO.
DEF VAR scrollbar_down     AS CHARACTER FORMAT "x(01)"  INITIAL "v" NO-UNDO.
DEF VAR scr_paint AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR scr_paint_dir AS INTEGER FORMAT -9 INITIAL 1 NO-UNDO.
DEF VAR scr_paint_rec AS RECID INITIAL ? NO-UNDO.

DEF VAR scr_firstrec AS RECID NO-UNDO INITIAL ?.
DEF VAR scr_lastrec AS RECID  NO-UNDO INITIAL ?.
DEF VAR scr_addrec  AS RECID  NO-UNDO INITIAL ?. /* last record added */
DEF VAR scr_addmode AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR SCR_UPDATE_BOUNDS AS LOGICAL NO-UNDO INITIAL TRUE.
DEF VAR left_addmode AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR rownum AS INTEGER.
DEF VAR SCRFM_DELOK AS LOGICAL NO-UNDO INITIAL NO.

DEF VAR scr_functions AS CHAR INITIAL "{&FUNCTIONS}" NO-UNDO. /* ACDP */
DEF VAR scr_noAdd AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR scr_noChg AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR scr_noDel AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR scr_noPick AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR scr_noDetails AS LOGICAL INITIAL FALSE NO-UNDO.
IF SUBSTRING(scr_functions,1,1) = "N" THEN
scr_noAdd = TRUE.
IF SUBSTRING(scr_functions,2,1) = "N" THEN
scr_noChg = TRUE.
IF SUBSTRING(scr_functions,3,1) = "N" THEN
scr_noDel = TRUE.
IF SUBSTRING(scr_functions,4,1) = "N" THEN
scr_noPick = TRUE.
IF substring(scr_functions,5,1) = "N" THEN
scr_noDetails = TRUE.

DEF VAR MSG AS CHAR EXTENT 10 NO-UNDO.

/*  PROGRAM STATES:
START
ADD
KEY                 4
DATA                3
DETAILS             3
CHG
KEY                 1+2
RETURN=CHG
DATA        3
DETAILS     3
F10=DEL
*/

MSG[1] = "Point to Record, Return=Change, f9=Add, f10=Del, "
+ KBLABEL("GO") + "|" + KBLABEL("END-ERROR") + "=Exit, "
  + KBLABEL("FIND") + "=Find".
MSG[2] = scr_firstkeycap + "=First, " +
scr_lastkeycap + "=Last, Pg-Up=Prior Screen, Pg-Dn=Next Screen".
MSG[3] =
KBLABEL("GO") + "=Save, " +
  KBLABEL("END-ERROR") + "=Undo, f7=Recall, f8=Clear".
MSG[4] = "Add Record; " + KBLABEL("END-ERROR") + "=Switch to Change Mode".
MSG[5] = "Enter Find value; "
+ KBLABEL("END-ERROR") + "=Return to Change Mode".
MSG[6] = "Cannot Find that value, try another".

IF "{&COLOR}" > '' THEN
C_DET = "{&COLOR}" + ''.

DEF buffer x{&file} FOR {&file}.
DO FOR x{&file} ON error UNDO, LEAVE:
  FIND x{&file}
    WHERE RECID(x{&file}) = lib_recid_ret NO-LOCK NO-ERROR.
  IF NOT AVAIL x{&file} THEN
  FIND x{&file}
    WHERE RECID(x{&file}) = ws_recid NO-LOCK NO-ERROR.
  scr_paint_rec = IF AVAIL x{&file} THEN
  RECID(x{&file}) ELSE
  ?.  /* 9810 CAH */
  IF AVAIL x{&file} THEN
  RELEASE x{&file}.
END.

scr_paint = TRUE.
scr_paint_dir = 1.
scrollbar_down = "v".
IF {&ROWS} > 0 THEN
LIB_FRAME_ROWS = {&ROWS}.
ELSE
lib_frame_rows = 10.
SCR_UPDATE_BOUNDS = TRUE.
scr_addmode = FALSE.

DEF VAR scr-debug AS LOGICAL INITIAL FALSE NO-UNDO.

FORM
  WITH FRAME F-DETAILS CENTER OVERLAY TITLE "DETAILS"
  COLOR DISPLAY VALUE(c_pop) ROW (FRAME-ROW(F-DET) + FRAME-LINE(F-DET) + 4
  {&SETBACK}).

FORM
  scrollbar_up SKIP(8) scrollbar_down
  WITH FRAME f-scrollbar COLOR DISPLAY VALUE(C_DET) NO-LABELS NO-BOX OVERLAY.

FORM
  WITH FRAME F-CHOOSE SIDE-LABELS COLOR DISPLAY VALUE(C_POP)
  TITLE "Override".

ON f9               INSERT-MODE.
ON f10              DELETE-LINE.
ON HOME             HOME.
ON END              END.
ON f5               GET.
ON f6               PUT.
ON f7               RECALL.
ON f8               CLEAR.
ON CTRL-CURSOR-LEFT LEFT-END.
ON CTRL-F           FIND.
ON TAB              TAB.
ON SHIFT-TAB        BACK-TAB.

IF OPSYS = "MSDOS" OR OPSYS BEGINS "WIN" THEN
DO:
  scr_firstkey = KEYCODE("HOME").
  scr_lastkey  = KEYCODE("END").
  HYPERHELP_KEY = KEYCODE("F22").     /* 9611 CAH = shift-f2 */
END.
ELSE
IF OPSYS = 'UNIX' THEN
DO:
  scr_firstkey = KEYCODE("HOME").
  scr_lastkey  = KEYCODE("ESC-DOWN-ARROW").
  HYPERHELP_KEY = KEYCODE("F12").
END.
scr_firstkeycap = KEYLABEL(scr_firstkey).
scr_lastkeycap  = KEYLABEL(scr_lastkey).
HYPERHELP_KEYCAP = KEYLABEL(HYPERHELP_KEY).

lib_recid_ret = ?.
looper:
REPEAT FOR {&FILE} WITH FRAME F-DET ON ERROR UNDO, LEAVE:
  /* readkey pause 0. */
  IF KEYFUNCTION(LASTKEY) = "END-ERROR"
    AND LEFT_ADDMODE = TRUE
    AND FRAME-LINE(F-DET) = 1
    THEN
  DO:
    CLEAR FRAME f-det ALL NO-PAUSE.
    LEAVE LOOPER.
  END.
  IF scr-debug THEN
  RUN rc/debugmsg.p ("top of Looper").

  FORM
    {&FILE}.{&CHOOSE}
    WITH SCROLL 1 lib_frame_rows DOWN CENTER OVERLAY
    TITLE "{&TITLE}"
    FRAME F-DET COLOR DISPLAY VALUE(C_DET).

  IF left_addmode = TRUE THEN
  DO: /* resynch frame and row pointers */
    IF FRAME-LINE (f-det) > 1 THEN
    UP 1 WITH FRAME f-det.
    rownum = FRAME-LINE (f-det).
    lib_i = rownum.
    left_addmode = FALSE.
  END.

  IF SCR_UPDATE_BOUNDS = TRUE THEN
  DO:
    IF scr-debug THEN
    RUN rc/debugmsg.p ("update bounds").
    SCR_FIRSTREC = ?.
    SCR_LASTREC = ?.
    FIND FIRST {&FILE} {&INDEX} {&CONDITION} NO-LOCK NO-ERROR.
    IF AVAILABLE({&FILE}) THEN
    DO:
      SCR_FIRSTREC = RECID({&FILE}).
      FIND LAST {&FILE} {&INDEX} {&CONDITION} NO-LOCK NO-ERROR.
      IF AVAILABLE({&FILE}) THEN
      SCR_LASTREC = RECID({&FILE}).
    END.
    ELSE
    DO:
      IF scr_noadd THEN
      DO:
        BELL.
        HIDE MESSAGE NO-PAUSE.
        MESSAGE COLOR VALUE(C_err)
          "There are no suitable records available to edit.".
        MESSAGE COLOR VALUE(C_err)
          "Since you cannot add records with this procedure, exiting now.".
        PAUSE 1.
        LEAVE looper.
      END.
      ELSE
      DO:
        SCR_PAINT = FALSE.  /* NO POSSIBLE RECORD */
        VIEW FRAME F-DET.
        SCR_ADDMODE =TRUE.
      END.
    END.
    SCR_UPDATE_BOUNDS = FALSE.
  END.

  IF scr_paint = TRUE THEN
  DO:
    IF scr-debug THEN
    RUN rc/debugmsg.p ("scr_paint").
    IF scrollbar_up   = "" AND scr_paint_dir = -1
      OR scrollbar_down = "" AND scr_paint_dir = +1
      THEN
    DO:  /* can't scroll */
      BELL.
      scr_paint = FALSE.
      LEAVE.
    END.
    CLEAR FRAME F-DET ALL NO-PAUSE.
    DO lib_i = 1 TO 10:
      lib_recids[lib_i] = ?.
    END.
    IF scr_paint_rec = ? THEN
    scr_paint_rec =
    IF scr_paint_dir = +1 THEN
    scr_firstrec ELSE
    scr_lastrec.
    FIND {&FILE} WHERE RECID({&FILE}) = scr_paint_rec NO-LOCK NO-ERROR.
    IF NOT AVAIL {&file} THEN
    LEAVE.
    IF scr_paint_dir > 0 THEN
    DO:
      lib_i = 1.
      IF FRAME-LINE(F-DET) > 0 THEN
      UP (FRAME-LINE (F-DET) - 1) WITH FRAME F-DET.
    END.
    ELSE
    DO:
      lib_i = lib_frame_rows.
      IF FRAME-LINE(F-DET) <= lib_frame_rows THEN
      DOWN (lib_frame_rows - FRAME-LINE (F-DET)) WITH FRAME F-DET.
    END.
    _paint_io:
    REPEAT ON error UNDO, LEAVE:
      IF scr-debug THEN
      RUN rc/debugmsg.p ( "_paint_io" ).
      PAUSE 0.
      {&DETFUNCT}
      DISPLAY {&FILE}.{&CHOOSE} {&DISPLAYF} WITH FRAME F-DET.
      lib_recids[lib_i] = RECID({&FILE}).
      lib_i = lib_i + scr_paint_dir.
      IF lib_i > lib_frame_rows OR lib_i = 0 THEN
      LEAVE.
      DOWN scr_paint_dir WITH FRAME F-DET.
      IF SCR_PAINT_DIR > 0 AND RECID({&FILE}) = SCR_LASTREC
        AND LIB_I <= lib_frame_rows THEN
      DO:
        /* and keyfunction(lastkey) <> "END-ERROR" */
        IF NOT scr_noadd THEN
        DO:
          /* NOT ENOUGH RECORDS TO FILL THE SCREEN, SWITCH TO ADDMODE */
          SCR_ADDMODE = TRUE.
          scr_paint = FALSE.
          MESSAGE COLOR VALUE(C_ADD) "Reached end, Entering Add Mode".
          {rc/dbg.i "a"}
          /* 9701 */
          INPUT close.
          PAUSE 1.  /* 4.14: was 1 */
          INPUT FROM TERMINAL.
          INPUT CLEAR.
          /* 9701 */
          LEAVE _paint_io.
        END.
        ELSE
        DO:    /* SWITCH TO CHANGE MODE */
          LEAVE  _paint_io.
        END.
      END.    /* REACHED LAST REC */
      IF scr_paint_dir > 0
        THEN
      FIND NEXT {&FILE} {&Index}
        {&CONDITION} NO-LOCK NO-ERROR.
      ELSE
      FIND PREV {&FILE} {&Index}
        {&CONDITION} NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ({&FILE}) THEN
      DO:
        IF scr_paint_dir < 0 /* prev-page */ THEN
        DO:
          scr_paint = TRUE.
          scr_paint_rec = scr_firstrec.
          scr_paint_dir  = 1.
          scrollbar_down = "v".
          {rc/dbg.i "b"} NEXT looper.
        END.
        IF scr_paint_dir > 0 /* next-page */ THEN
        DO:
          scr_paint = TRUE.
          scr_paint_rec = scr_lastrec.
          scr_paint_dir  = -1.
          scrollbar_up = "v".
          {rc/dbg.i "c"} NEXT looper.
        END.
      END.
    END.    /* _paint_io */
    IF scr_paint_dir > 0 AND NOT scr_addmode
      /* move cursor to first item on list */
      THEN
    UP (FRAME-LINE (F-DET) - 1) WITH FRAME F-DET.
  END.  /* do when scr_paint = true */
  scr_paint = FALSE.

  IF lib_recids[1] = scr_firstrec
    OR lib_recids[1] = ?
    THEN
  scrollbar_up = "".
  ELSE
  scrollbar_up = "^".
  IF lib_recids[lib_frame_rows] = scr_lastrec
    OR lib_recids[lib_frame_rows] = ?
    THEN
  scrollbar_down = "".
  ELSE
  scrollbar_down = "v".

  IF (NOT scr_addmode) AND (scrollbar_up <> '' OR scrollbar_down <> '')  THEN
  DISPLAY scrollbar_up scrollbar_down
    WITH FRAME f-scrollbar
    COLUMN (FRAME-COL(F-DET) - 1) ROW (FRAME-ROW(F-DET) + 4) OVERLAY.
  ELSE
  HIDE FRAME f-scrollbar NO-PAUSE.

  {&HASHDISP}
  IF NOT SCR_ADDMODE THEN
  DO:
    HIDE MESSAGE NO-PAUSE.
    MESSAGE COLOR VALUE(C_MSG) msg[1].
    MESSAGE COLOR VALUE(C_MSG) msg[2].
    IF scr-debug THEN
    RUN rc/debugmsg.p ( "before choose row" ).
    CHOOSE ROW {&FILE}.{&CHOOSE}
  GO-ON (HOME END {rc/keymove.i})
    NO-ERROR WITH FRAME F-DET.
  COLOR DISPLAY VALUE(C_DET) {&FILE}.{&CHOOSE} WITH FRAME F-DET.
  PAUSE 0.
END.
ELSE
DO:
  HIDE MESSAGE NO-PAUSE.
  MESSAGE COLOR VALUE(C_ADD) msg[4].
  /* "Add Record or press CANCEL to return to Edit Mode". */
END.

IF KEYFUNCTION(LASTKEY) = "GO" AND NOT SCR_ADDMODE THEN
DO:
  IF scr-debug THEN
  RUN rc/debugmsg.p ( "Go Pressed - Leaving Loop" ).
  PAUSE 1.
  LEAVE LOOPER.
END.

rownum = FRAME-LINE(f-det).

/* MESSAGE LASTKEY KEYFUNCTION(LASTKEY) KEYLABEL(LASTKEY). PAUSE. */

IF KEYFUNCTION(LASTKEY) = "FIND" THEN
DO WITH FRAME F-POSIT COLUMN FRAME-COL(f-det)
    TITLE "FIND" SIDE-LABELS OVERLAY
    COLOR VALUE(c_det) NO-VALIDATE:
  HIDE MESSAGE NO-PAUSE.
  MESSAGE COLOR VALUE(C_chg) msg[5].
  FIND {&file} WHERE RECID({&file}) = lib_recids[rownum] NO-LOCK NO-ERROR.
  IF AVAIL {&file} THEN
  DISPLAY {&jumpf} {&FILE}.{&CHOOSE}.
  PROMPT-FOR {&jumpf} {&FILE}.{&CHOOSE}.
  IF "{&jumpf}" > " " THEN
  DO:
    {&jumpfunct}
  END.
  ELSE
  DO:
    IF INPUT {&file}.{&choose} < {&FILE}.{&CHOOSE}
      THEN
    FIND FIRST {&FILE} {&CONDITION} {&POSIT} AND
      {&FILE}.{&CHOOSE} >= INPUT FRAME F-POSIT {&FILE}.{&CHOOSE}
      NO-LOCK NO-ERROR.
    ELSE
    FIND NEXT {&FILE} {&CONDITION} {&POSIT} AND
      {&FILE}.{&CHOOSE} >= INPUT FRAME F-POSIT {&FILE}.{&CHOOSE}
      NO-LOCK NO-ERROR.
  END.
  IF NOT AVAIL ({&FILE}) THEN
  DO:
    BELL.
    MESSAGE COLOR VALUE (C_WRN)
      "No next record found, restarting search from the beginning".
    FIND FIRST {&FILE} {&CONDITION} {&POSIT} AND
      {&FILE}.{&CHOOSE} >= INPUT FRAME F-POSIT {&FILE}.{&CHOOSE}
      NO-LOCK NO-ERROR.
  END.
  HIDE FRAME F-POSIT.

  IF AVAILABLE ({&FILE}) THEN
  DO:
    scr_paint = TRUE.
    scr_paint_rec = RECID({&FILE}).
    scr_paint_dir = +1.
    scrollbar_down = "v".
    {rc/dbg.i "d"} NEXT looper.
  END.
  ELSE
  DO:
    BELL.
    MESSAGE COLOR VALUE(C_MSG) msg[6].
    PAUSE 0.
    UNDO, NEXT.
  END.
END.


IF LASTKEY = scr_firstkey THEN
DO:
  scr_paint = TRUE.
  scr_paint_rec = scr_firstrec.
  scr_paint_dir = +1.
  scrollbar_down = "v".
  {rc/dbg.i "e"} NEXT looper.
END.
ELSE
IF LASTKEY = scr_lastkey THEN
DO:
  scr_paint = TRUE.
  scr_paint_rec = scr_lastrec.
  scr_paint_dir = -1.
  scrollbar_up = "^".
  {rc/dbg.i "f"} NEXT looper.
END.
ELSE
IF LASTKEY = KEYCODE("CURSOR-DOWN") THEN
DO:
  FIND {&FILE} WHERE
    RECID({&FILE}) = lib_recids[FRAME-LINE(F-DET)] NO-LOCK.

  IF RECID({&FILE}) = scr_lastrec THEN
  BELL.
  ELSE
  DO:
    FIND NEXT {&FILE} {&INDEX} {&CONDITION} NO-LOCK NO-ERROR.
    DOWN WITH FRAME F-DET COLOR DISPLAY VALUE(C_DET).
    DO lib_i = 1 TO lib_frame_rows:
      IF lib_i = lib_frame_rows
        THEN
      lib_recids[lib_i] = RECID({&FILE}).
      ELSE
      lib_recids[lib_i] = lib_recids[lib_i + 1].
    END.
    {&DETFUNCT}
    DISPLAY {&FILE}.{&CHOOSE} {&DISPLAYF} WITH FRAME F-DET.
    NEXT.
  END.
END.
ELSE
IF LASTKEY = KEYCODE("CURSOR-UP")
  OR LASTKEY = KEYCODE("BACK-TAB") THEN
DO:
  FIND {&FILE} WHERE
    RECID({&FILE}) = lib_recids[FRAME-LINE(F-DET)] NO-LOCK.
  IF RECID({&FILE}) = scr_firstrec THEN
  BELL.
  ELSE
  DO:
    FIND PREV {&FILE} {&INDEX} {&CONDITION} NO-LOCK NO-ERROR.
    UP WITH FRAME F-DET COLOR DISPLAY VALUE(C_DET).
    DO lib_i = lib_frame_rows TO 1 BY -1:
      IF lib_i = 1 THEN
      lib_recids[lib_i] = RECID({&FILE}).
      ELSE
      lib_recids[lib_i] = lib_recids[lib_i - 1].
    END.
    {&DETFUNCT}
    DISPLAY {&FILE}.{&CHOOSE} {&DISPLAYF} WITH FRAME F-DET.
    NEXT.
  END.
END.
ELSE
IF LASTKEY = KEYCODE("PAGE-UP") THEN
DO:
  scr_paint = TRUE.
  scr_paint_dir = -1.
  scr_paint_rec =
  IF lib_recids[1] <> ? THEN
  lib_recids[1]
  ELSE
  lib_recids[rownum].
  scrollbar_up = "^".
  NEXT.
END.
ELSE
IF LASTKEY = KEYCODE("PAGE-DOWN") THEN
DO:
  scr_paint = TRUE.
  scr_paint_dir = +1.
  scr_paint_rec =
  IF lib_recids[lib_frame_rows] <> ?
    THEN
  lib_recids[lib_frame_rows]
  ELSE
  lib_recids[rownum].
  scrollbar_down = "v".
  NEXT.
END.
ELSE
_ACD:
DO ON ERROR UNDO _ACD, NEXT looper:
  IF KEYLABEL(LASTKEY) = "f10" THEN
  DO: /* delete record */
    IF scr_nodel THEN
    DO:
      BELL.
      MSG[7] =  "Cannot delete record with this procedure".
      MESSAGE COLOR VALUE(C_DEL) MSG[7].
      PAUSE 2.
      NEXT.
    END.

    FIND {&FILE} WHERE RECID({&FILE}) = lib_recids[rownum].
    MSG[8] = "Delete record?".
    MESSAGE COLOR VALUE(C_DEL) MSG[8] UPDATE SCRFM_DELOK.
    IF NOT SCRFM_DELOK THEN
    DO:
      HIDE MESSAGE NO-PAUSE.
      NEXT.
    END.
    {&HASHMINUS}
    IF lib_recids[rownum] = scr_lastrec  THEN
    scr_lastrec = ?.
    IF LIB_RECIDS[ROWNUM] = SCR_FIRSTREC THEN
    scr_firstrec = ?.
    {&DELCODE}
    {{&delcodei}}
    DELETE {&FILE}.
    MESSAGE COLOR VALUE(c_del) "Record deleted".
    IF scr_firstrec = ? OR scr_lastrec = ?
      THEN
    scr_update_bounds = TRUE.
    IF scr_firstrec = ? AND scr_lastrec = ? THEN
    DO:
      scr_paint = TRUE.
      SCR_PAINT_REC = ?.
      {rc/dbg.i "g"} NEXT looper.
    END.
    IF scr_lastrec = ? THEN
    DO:      /* was just deleted */
      CLEAR FRAME f-det.
      lib_recids[rownum] = ?.
      UP 1 WITH FRAME f-det.
      {rc/dbg.i "h"} NEXT looper.
    END.

    DO lib_i = rownum TO (lib_frame_rows - 1) BY 1:
      lib_recids[lib_i] = lib_recids[lib_i + 1].
    END.
    SCROLL FROM-CURRENT UP WITH FRAME F-DET.
    FIND {&FILE} WHERE RECID({&FILE})
      = lib_recids[(lib_frame_rows - 1)] NO-LOCK NO-ERROR.
    IF AVAILABLE ({&FILE}) THEN
    DO:
      FIND NEXT {&FILE} {&INDEX} {&CONDITION} NO-LOCK NO-ERROR.
      IF AVAILABLE ({&FILE}) THEN
      DO:
        DOWN (lib_frame_rows - ROWNUM).
        lib_recids[lib_frame_rows] = RECID({&FILE}).
        {&DETFUNCT}
        DISPLAY {&FILE}.{&CHOOSE} {&DISPLAYF}.
        UP (lib_frame_rows - ROWNUM).
      END.
      ELSE
      DO:
        LIB_RECIDS[lib_frame_rows] = ?.
        scrollbar_down = ''.
      END.
    END.
    ELSE
    DO:
      LIB_RECIDS[(lib_frame_rows - 1)] = ?.
      scrollbar_down = ''.
    END.
    {rc/dbg.i "i"} NEXT looper.
  END.
  ELSE
  IF KEYLABEL(LASTKEY) = "f9" OR SCR_ADDMODE = TRUE THEN
  _addrec:
  DO:
    /* add record */
    IF scr_noadd THEN
    DO:
      BELL.
      MESSAGE COLOR VALUE(c_err)
        "You cannot add records with this procedure.".
      PAUSE 1.
      scr_addmode = FALSE.
      NEXT.
    END.
    IF NOT scr_addmode THEN
    DO:
      SCROLL FROM-CURRENT DOWN.
      DO lib_i = lib_frame_rows TO (rownum + 1) BY -1:
        lib_recids[lib_i] = lib_recids[lib_i - 1].
      END.
    END.
    lib_i = rownum.
    CREATE {&FILE}.
    lib_recids[lib_i] = RECID({&FILE}).
    {&ADDCODE}
    {{&ADDCODEi}}
    UPDATE {&FILE}.{&CHOOSE}
      EDITING:
      READKEY.
      {rc/helprkey.i}
      IF KEYFUNCTION(LASTKEY) = "END-ERROR"
        AND SCR_ADDMODE = TRUE THEN
      DO:
        lib_recids[lib_i] = ?.
        scr_addmode = FALSE.
        left_addmode = TRUE.
        MESSAGE COLOR VALUE(c_wrn) "Exiting add mode".
        CLEAR FRAME f-det NO-PAUSE.
        /* PAUSE 0.  /* 04.15: was 1 */ */
        SCR_UPDATE_BOUNDS = TRUE.
        UNDO LOOPER, NEXT looper.
      END.
      {&addedit}
      {{&addediti}}
      {rc/helptkey.i}
      ELSE
      APPLY LASTKEY.
    END.
  END.
  IF NOT NEW ({&FILE}) THEN
  DO:
    FIND {&FILE} WHERE RECID({&FILE}) = lib_recids[rownum].
    LIB_RECID_RET = RECID({&FILE}).
  END.
  {&KEYEDIT}
  IF KEYFUNCTION(LASTKEY) = "RETURN" OR NEW ({&FILE}) THEN
  _chgrec:
  DO:
    /* change record if any other key pressed */

    IF NOT NEW ({&FILE}) THEN
    DO:
      {&DETFUNCT}
      {&HASHMINUS}
    END.
    UPDATE {&DISPLAYF}
      EDITING:
      READKEY.
      IF KEYFUNCTION(LASTKEY) = "LEFT-END" THEN
      DO WITH FRAME F-CHOOSE:
        UPDATE {&FILE}.{&CHOOSE}
          WITH FRAME F-CHOOSE CENTER OVERLAY COLOR VALUE(c_pop) SIDE-LABELS.
        HIDE FRAME F-CHOOSE NO-PAUSE.
        DISPLAY {&FILE}.{&CHOOSE} WITH FRAME F-DET.
        NEXT.
      END.
      {&DATAEDIT}
      {{&DATAEDITi}}
      {rc/helprkey.i}
      IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN
      DO:
        SCR_UPDATE_BOUNDS = TRUE.
        UNDO LOOPER, NEXT looper.
      END.
      IF KEYLABEL(LASTKEY) MATCHES "C...-D" THEN
      DO:
        MESSAGE "Suppress Details?" UPDATE scr_noDetails.
        NEXT.
      END.
      IF {rc/termkey.i} THEN
      DO:
        {&TERMKEY}
        {{&TERMKEYi}}
        {rc/helptkey.i}
      END.
      IF LASTKEY <> -1
        AND KEYFUNCTION(LASTKEY) <> "HELP"
        THEN
      APPLY LASTKEY.
      IF GO-PENDING THEN
      DO:
      {&DATAGO} END.
    END.

    IF NOT scr_noDetails THEN
    DO:
      UPDATE {&UPFLDS} WITH FRAME F-DETAILS
        EDITING:
        READKEY.
        IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN
        DO:
          SCR_UPDATE_BOUNDS = TRUE.
          UNDO LOOPER, NEXT looper.
        END.
        IF KEYLABEL(LASTKEY) MATCHES "C...-D" THEN
        DO:
          MESSAGE "Suppress Details?" UPDATE scr_noDetails.
          NEXT.
        END.
        IF KEYFUNCTION(LASTKEY) = "HELP" THEN
        DO:
          {&HELPKEY}
          {rc/helprkey.i}
        END.
        {&detedit}
        {{&DETEDITi}}
        {rc/helptkey.i}
        ELSE
        APPLY LASTKEY.
        IF GO-PENDING THEN
        DO:
        {&DETGO} END.
      END.
    END.    /* details frame */

    {&ADDPOST}
    {{&ADDPOSTi}}

    IF scr_addmode THEN
    DO:
      IF FRAME-LINE(F-DET) = FRAME-DOWN(F-DET)
        THEN
      DO LIB_I = 1 TO (FRAME-DOWN(F-DET) - 1):
        LIB_RECIDS[LIB_I] = LIB_RECIDS[LIB_I + 1].
      END.
      DOWN 1 WITH FRAME f-det.
    END.
    IF NEW {&FILE} THEN
    SCR_ADDREC = RECID({&FILE}).
    {&HASHPLUS}
    CLEAR FRAME F-DETAILS.
    HIDE FRAME F-DETAILS.
    IF NOT SCR_ADDMODE THEN
    SCR_UPDATE_BOUNDS = TRUE.
  END.  /* _chgrec ADD OR CHANGE */
END.  /* _ACD */
END.  /* REPEAT */

HIDE FRAME F-DETAILS NO-PAUSE.
HIDE FRAME f-choose NO-PAUSE.
HIDE FRAME f-posit NO-PAUSE.
CLEAR FRAME F-DET ALL.
HIDE FRAME F-DET NO-PAUSE.
HIDE FRAME f-scrollbar NO-PAUSE.
HIDE MESSAGE NO-PAUSE.

if connected("asi") then do:
ON F5 choices.
ON F6 HELP.   /* Menu jumping - added CTS */
ON F9 HELP.
ON F10 HELP.
ON TAB RETURN.
end.


