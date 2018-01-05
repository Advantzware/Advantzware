/*  rc/scrfm3.i - scrolling file maintenance.
10.27.98 by CAH on \\ricky\rv8 Log#0000:
1.  Added initialization of special key values.
2.  Merged any changes from rc/scrfm.i to create superset.

06.11.95 by CAH on CDS972@812 [\rprodemo] Log#0000:
1.  Added double includes ala scrfm2.i for addcode, dataedit
termkey, detedit and addpost.
09.27.94 by CAH on B28@812 Log#0000:
1.  Modified record scope to allow release of addcode via strong scoping.
05.01.94 by CAH on B28@111 Log#0000:
1.  Removed automatic centering.  FORM frames prior if so desired.
2.  Frame column of f-posit set relative to f-det.
3.  Added pause 0 on paging and first/last jumps to prevent screen flipping.
4.  Added clearing of color hilite on current row upon exit.  Since detail
frame is left on screen this touchup is required to cancel 'focus'.
04.23.94 by CAH on B28@111 Log#0000:
1.  NEW VERSION from rc/scrfm.i.
2.  Split f-scrollbar into two frames to allow for variable size f-det.
3.  Made frame title references soft.  Caller can set them in &INIT (new).
Variables are f-det-title and f-details-title.
4.  Expanded possible ROWS to 20.
5.  Added INIT parameter, executed immediately prior to start of loop.
6.  &COLOR removed: assign c_det = desired color in &INIT
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

DEF VAR scr_functions AS CHARACTER INITIAL "{&FUNCTIONS}" NO-UNDO. /* ACDP */
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
  WITH FRAME F-DETAILS OVERLAY
  TITLE f-details-title
  COLOR DISPLAY VALUE(c_det).

FORM
  scrollbar_up
  WITH FRAME f-scrollbar-up
  COLOR DISPLAY VALUE(C_DET) NO-LABELS NO-BOX OVERLAY.

FORM
  scrollbar_down
  WITH FRAME f-scrollbar-down
  COLOR DISPLAY VALUE(C_DET) NO-LABELS NO-BOX OVERLAY.

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
{&INIT}
looper:
REPEAT /* FOR {&FILE} */ WITH FRAME F-DET: /*  ON ERROR UNDO, LEAVE: */
  IF KEYFUNCTION(LASTKEY) = "END-ERROR" AND LEFT_ADDMODE = TRUE
    AND FRAME-LINE(F-DET) = 1 THEN
  LEAVE LOOPER.

  FORM
    {&FILE}.{&CHOOSE}
    WITH FRAME f-det SCROLL 1 lib_frame_rows DOWN OVERLAY
    TITLE f-det-title
    COLOR DISPLAY VALUE(C_DET).

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
    IF scrollbar_up   = "" AND scr_paint_dir = -1
      OR scrollbar_down = "" AND scr_paint_dir = +1
      THEN
    DO:  /* can't scroll */
      BELL.
      scr_paint = FALSE.
      LEAVE.
    END.
    CLEAR FRAME F-DET ALL NO-PAUSE.
    DO lib_i = 1 TO lib_frame_rows:
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
    DO WHILE TRUE:
      /* 9706 CAH pause 0 */
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
        IF NOT scr_noadd THEN
        DO:
          /* NOT ENOUGH RECORDS TO FILL THE SCREEN, SWITCH TO ADDMODE */
          SCR_ADDMODE = TRUE.
          scr_paint = FALSE.
          MESSAGE COLOR VALUE(C_ADD) "Reached end, Entering Add Mode".
          PAUSE 1.  /* 4.14: was 1 */
          LEAVE.
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
          NEXT looper.
        END.
        IF scr_paint_dir > 0 /* next-page */ THEN
        DO:
          scr_paint = TRUE.
          scr_paint_rec = scr_lastrec.
          scr_paint_dir  = -1.
          scrollbar_up = "v".
          NEXT looper.
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

  IF (NOT scr_addmode) AND (scrollbar_up <> '')
    THEN
  DISPLAY scrollbar_up
    WITH FRAME f-scrollbar-up
    COLUMNS (FRAME-COL(F-DET) - 1) ROW (FRAME-ROW(F-DET) + 4) OVERLAY.
  ELSE
  HIDE FRAME f-scrollbar-up NO-PAUSE.

  IF (NOT scr_addmode) AND (scrollbar_down <> '')
    THEN
  DISPLAY scrollbar_down
    WITH FRAME f-scrollbar-down
    COLUMNS (FRAME-COL(F-DET) - 1)
    ROW (FRAME-ROW(F-DET) + 2 + {&ROWS} ) OVERLAY.
  ELSE
  HIDE FRAME f-scrollbar-down NO-PAUSE.

  {&HASHDISP}
  IF NOT SCR_ADDMODE THEN
  DO:
    /* 9802 CAH: no-pause here caused problems */
    HIDE MESSAGE. /* NO-PAUSE. */
    MESSAGE COLOR VALUE(C_MSG) msg[1].
    MESSAGE COLOR VALUE(C_MSG) msg[2].
  CHOOSE ROW {&FILE}.{&CHOOSE} GO-ON (HOME END {rc/keymove.i})
    NO-ERROR WITH FRAME F-DET.
  COLOR DISPLAY VALUE(C_DET) {&FILE}.{&CHOOSE} WITH FRAME F-DET.
  PAUSE 0. /* 9802 */
END.
ELSE
DO:
  HIDE MESSAGE NO-PAUSE.
  MESSAGE COLOR VALUE(C_ADD) msg[4].
END.

IF KEYFUNCTION(LASTKEY) = "GO" AND NOT SCR_ADDMODE THEN
DO:
  PAUSE 1.
  LEAVE LOOPER.
END.

rownum = FRAME-LINE(f-det).
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
    NEXT looper.
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
  PAUSE 0.
  NEXT looper.
END.
ELSE
IF LASTKEY = scr_lastkey THEN
DO:
  scr_paint = TRUE.
  scr_paint_rec = scr_lastrec.
  scr_paint_dir = -1.
  scrollbar_up = "^".
  PAUSE 0.
  NEXT looper.
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
    PAUSE 0 NO-MESSAGE.    /* 9807 CAH */
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
    PAUSE 0 NO-MESSAGE.
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
  PAUSE 0.
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
  PAUSE 0.
  NEXT.
END.
ELSE
_ACD:
DO ON ERROR UNDO _ACD, NEXT LOOPER:
  IF KEYLABEL(LASTKEY) = "f10" THEN
  _delrec:
  DO /* FOR {&FILE} */
      /* TRANSACTION */ ON ERROR UNDO _delrec, NEXT looper: /* delete record */
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
    CLEAR FRAME f-det NO-PAUSE.
    MESSAGE COLOR VALUE(c_err) "Record deleted".
    PAUSE 1 NO-MESSAGE.
    IF scr_firstrec = ? OR scr_lastrec = ?
      THEN
    scr_update_bounds = TRUE.
    IF scr_firstrec = ? AND scr_lastrec = ? THEN
    DO:
      scr_paint = TRUE.
      SCR_PAINT_REC = ?.
      NEXT looper.
    END.
    IF scr_lastrec = ? THEN
    DO:      /* was just deleted */
      /* CLEAR FRAME f-det. 9810 CAH: in rc/scrfm.i, not sure which is right */
      lib_recids[rownum] = ?.
      /* 9802 CAH: UP 1 WITH FRAME f-det.
      NEXT looper. */
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
    rownum = FRAME-LINE(f-det).
    DO WHILE lib_recids[rownum] = ? AND rownum > 1:
      UP 1.
      rownum = FRAME-LINE(f-det).
    END.
    NEXT LOOPER.
  END.
  ELSE
  IF KEYLABEL(LASTKEY) = "f9" OR SCR_ADDMODE = TRUE THEN
  _addrec:
  DO {&ADDSCOPE} /* /* TRANSACTION */ /* ON ERROR UNDO _addrec, NEXT looper */
      ON ERROR UNDO _ADDREC, RETRY _ADDREC ON ENDKEY UNDO _ADDREC,
      NEXT LOOPER */:
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
        PAUSE 0.  /* 04.15: was 1 */
        SCR_UPDATE_BOUNDS = TRUE.
        UNDO LOOPER, NEXT looper.
      END.
      {&addedit}                /* 9808 CAH: Added */
      {{&addediti}}             /* 9808 CAH: Added */
      {rc/helptkey.i}
      APPLY LASTKEY.
    END.
    IF NEW {&file} THEN
    scr_addrec = RECID({&file}).
  END.
  _chgrec:
  DO /* FOR {&FILE} TRANSACTION */ ON ERROR UNDO, RETRY ON endkey UNDO, LEAVE:
    /* _chgrec, NEXT looper: */
    /* if new({&file}) then do: message "NEW REC" RECID({&FILE}). PAUSE. end. */
    IF NOT NEW ({&FILE}) THEN
    DO:
      FIND {&FILE} WHERE RECID({&FILE}) = lib_recids[rownum].
      LIB_RECID_RET = RECID({&FILE}).
    END.
    {&KEYEDIT}
    IF NOT NEW ({&FILE}) THEN
    DO:
      {&DETFUNCT}
      {&HASHMINUS}
    END.
    IF NOT scr_noDetails and "{&upflds}" > " " THEN
    DISPLAY {&UPFLDS} WITH FRAME F-DETAILS.
    PAUSE 0.
    DO WITH FRAME f-det:
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
          {{&termkeyi}}
          {rc/helptkey.i}
        END.
        {&DATAEDIT}
        {{&DATAEDITi}}
        /*
        IF KEYLABEL(LASTKEY) = "SHIFT-F5"
        THEN DO WITH FRAME f-all SIDE-LABELS color value(c_pop):
        UPDATE {&file} {&exceptf}.
        HIDE FRAME f-all NO-PAUSE.
        NEXT.
        end.
        */
        APPLY LASTKEY.
        IF GO-PENDING THEN
        DO:
          {&DATAGO}
          {{&DATAGOi}}
        END.
      END.
    END.

    IF NOT scr_noDetails and "{&upflds}" > " " THEN
    DO WITH FRAME f-details:
      UPDATE {&UPFLDS}
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
        END.
        {&DETEDIT}
        {{&DETEDITi}}
        {rc/helptkey.i}
        APPLY LASTKEY.
        IF GO-PENDING THEN
        DO:
          {&DETGO}
          {{&DETGOi}}
        END.
      END.
    END.

    {&ADDPOST}
    {{&ADDPOSTi}}

    IF scr_addmode THEN
    DO:
      IF FRAME-LINE(F-DET) = FRAME-DOWN(F-DET)
        THEN
      DO LIB_I = 1 TO (FRAME-DOWN(F-DET) - 1):
        LIB_RECIDS[LIB_I] = LIB_RECIDS[LIB_I + 1].
      END.
      PAUSE 0.
      DOWN 1 WITH FRAME f-det.
    END.
    {&HASHPLUS}
    /* CLEAR FRAME F-DETAILS. */
    IF NOT SCR_ADDMODE THEN
    SCR_UPDATE_BOUNDS = TRUE.
  END.  /* _chgrec ADD OR CHANGE */

END.  /* _ACD */
COLOR DISPLAY VALUE(C_DET) {&FILE}.{&CHOOSE} WITH FRAME F-DET.
END.  /* REPEAT */
/* end.  pick */

HIDE FRAME f-scrollbar-up   NO-PAUSE.
HIDE FRAME f-scrollbar-down NO-PAUSE.
HIDE FRAME f-choose NO-PAUSE.
HIDE FRAME f-posit NO-PAUSE.
HIDE MESSAGE NO-PAUSE.

if connected("asi") then do:
ON F5 choices.
ON F6 HELP.   /* Menu jumping - added CTS */
ON F9 HELP.
ON F10 HELP.
ON TAB RETURN.
end.


