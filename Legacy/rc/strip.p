/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\rc\strip.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
{rc/loginv.i}
{rc/stripvar.i}
DEF var qoffset          AS int NO-UNDO.        /* offset for starting column */
DEF var qdown            AS int NO-UNDO.        /* no. of entries on curr ln */
DEF var qlabel           AS char FORMAT "x(40)" extent 14 NO-UNDO.
DEF var qstart           AS int  FORMAT "999"   extent 14 NO-UNDO.
DEF var qend             AS int  FORMAT "999"   extent 14 NO-UNDO.
DEF var msub             AS int NO-UNDO.     /* current choice subscript */
DEF var mprev            AS int NO-UNDO.     /* previous choice subscript */
DEF var mtop             AS int NO-UNDO.     /* subscript# of curr 1st entry */
DEF var moffset          AS int NO-UNDO.     /* offset for initial hilite  */
DEF var mdiff            AS int NO-UNDO.     /* working value */
DEF var mpos             AS int NO-UNDO.     /* working value */
DEF var mlast            AS int NO-UNDO.     /* # of entries in strip menu */
DEF var mlook            AS int NO-UNDO.     /* subscript for alpha match */
DEF var mkeys            AS char init "" NO-UNDO. /* keys to match on */
DEF var sv-prompt        LIKE strip-prompt NO-UNDO.
DEF var do-jump          AS logi NO-UNDO.    /* WR2030 */
DEF var w-fkey           AS char NO-UNDO.    /* WR2030 */
DEF var fkey-list        AS char NO-UNDO.    /* WR2030 */
DEF var msg-color        AS char NO-UNDO.
DEF var choice-color     AS char NO-UNDO.
DEF var help-color       AS char NO-UNDO.
/* WR23512 >>>>>>>> begin */
DEFINE var c-spaces AS char NO-UNDO.
ASSIGN c-spaces = FILL(" ",78).
/* WR23512 <<<<<<<< end */
IF TERMINAL = "CO80" THEN
DO:
  ASSIGN msg-color    = "light-red/gray"
    choice-color = "light-cyan"
    help-color   = "light-cyan".
END.
ELSE
DO:
  ASSIGN msg-color    = "normal"
    choice-color = "normal"
    help-color   = "normal".
END.
ASSIGN strip-F4  = NO  /* FOR PASSING F4 BACK TO CALLING PROGRAM */
  strip-sel = "".
/* ONLY USE ONE OF THE TWO MESSAGE LINES FOR MENU */
IF strip-menu-row < SCREEN-LINES + 1 THEN
ASSIGN strip-menu-row = SCREEN-LINES + 1.
IF strip-menu-row > SCREEN-LINES + 2 THEN
ASSIGN strip-menu-row = SCREEN-LINES + 2.
/* ONLY USE LAST 3 LINES FOR HELP PROMPT (AND NORMALLY THE LAST LINE) */
IF strip-help-row < SCREEN-LINES + 1 THEN
strip-help-row = SCREEN-LINES + 3.
IF strip-help-row > SCREEN-LINES + 3 THEN
strip-help-row = SCREEN-LINES + 3.
IF strip-help-row = strip-menu-row THEN
ASSIGN strip-help-row = SCREEN-LINES + 3.
ASSIGN strip-depth = 1
  mlast = 0
  mdiff = -3  /* USE TO BUILD ACCUMULATED LINE SIZE */
  qlabel = "".
IF strip-list > "" THEN
DO:
  /* USE LIST OF LITERAL VALUES PASSED BY PROGRAM */
  IF strip-prompt = "" OR strip-prompt = ? THEN
  ASSIGN strip-prompt =
    "Select an entry using Left/Right Arrows or First Letters ".
  ELSE
  IF length(strip-prompt) > 76 THEN
  ASSIGN strip-prompt = substring(strip-prompt,1,76).
  ELSE
  ASSIGN strip-prompt = strip-prompt +
    FILL(" ",76 - length(strip-prompt) ).
  ASSIGN mlast  = 1.
  /* 9904 CAH: was returning zero so switched it to use num-entries ...
  /* NUMBER OF ENTRIES = 1 + NUMBER OF COMMAS !! */
  DO msub = 1 TO length(strip-list) ON error UNDO, LEAVE:
    IF substring(strip-list,msub,1) = "," THEN
    ASSIGN mlast = mlast + 1.
  END.
  */
  mlast = num-entries(strip-list).  /* 9904 CAH */
  DO msub = 1 TO mlast ON error UNDO, LEAVE:
    ASSIGN qlabel[msub] = ENTRY(msub,strip-list)
      qstart[msub] = mdiff + 4
      mdiff = mdiff + 3 + length(qlabel[msub])
      qend[msub] = mdiff.
  END.
END.
IF mlast = 0 THEN
DO:
  ASSIGN msg1 = " No values have been defined for this selection. ".
  RUN rc/msg.p.
  ASSIGN strip-sel[strip-depth] = "".
  RETURN.
END.
IF mlast > 14 THEN
DO:
  ASSIGN msg1 = "Too many values have been defined for this selection.".
  RUN rc/msg.p.
  ASSIGN strip-sel[strip-depth] = "".
  RETURN.
END.
ASSIGN qdown = mlast
  strip-sel[strip-depth] = ""
  mtop = 1
  moffset = 0.
/* USED WHEN DISPLAYING ENTRY NON-UNIQUELY MATCHED ON ALPHA */
READKEY PAUSE 0. /* LET EXPERT USERS KEY 1st LETTER W/OUT SEEING STRIP MENU */
IF LASTKEY > 0 THEN
EXPERT-MODE:
DO:
  IF KEYFUNCTION(LASTKEY) = "RETURN" THEN
  DO:
    /* USE THE FIRST SELECTION ON THE STRIP MENU */
    ASSIGN msub = 1
      strip-sel[strip-depth] = qlabel[msub].
    LEAVE EXPERT-MODE.
  END.
  ASSIGN mkeys = KEYLABEL(LASTKEY).
  DO mlook = 1 TO mlast:
    IF qlabel[mlook] BEGINS mkeys THEN
    DO:
      DUP-LOOK:
      DO mdiff = mlook + 1 TO mlast:
        IF qlabel[mdiff] BEGINS mkeys THEN
        LEAVE DUP-LOOK.
      END.
      IF mdiff > mlast THEN
      DO:
        /* GOT A UNIQUE MATCH ! */
        ASSIGN msub = mlook
          strip-sel[strip-depth] = qlabel[msub].
        LEAVE EXPERT-MODE.
      END.
      /* ENTRY IS AMBIGUOUS, SET TO HIGHLITE FIRST ONE FOUND */
      IF qend[mlook] > 74 THEN
      ASSIGN mtop = mlook.
      ASSIGN moffset = mlook - mtop.
      LEAVE EXPERT-MODE.
    END.
  END.  /* REPEAT */
  /* ELSE THEY KEYED AN INVALID KEY, BEEP AND CLEAR */
  BELL.
  INPUT CLEAR.
END.  /* EXPERT-MODE */
IF strip-sel[strip-depth] = "" OR strip-sel[strip-depth] = ? THEN
MAIN:
DO:
  /* pause 0 before-hide. 9709 CAH Disabled? this, bad idea. */
  ASSIGN mkeys = "".
  LOOP1:
  DO WHILE TRUE:
    /* WR27034 > */ /* WR23512 >>>>>>>> begin */
    PUT SCREEN row strip-menu-row COLUMN 1 NO-ATTR-SPACE c-spaces.
    PUT SCREEN row strip-help-row COLUMN 1 NO-ATTR-SPACE c-spaces.
    /* WR27034 < */ /* WR23512 <<<<<<<< end */
    ASSIGN qoffset = qstart[mtop] - 3
      qdown   = 0.
    LOOP2:
    DO msub = mtop TO mlast:
      /* IF LAST CHOICE WON'T FIT, AND THIS ENTRY GOES BEYOND POS 74 */
      /* THEN PUT CONTINUATION SYMBOL "===>" AT END OF LINE INSTEAD  */
      IF qend[mlast] - qoffset > 78 AND qend[msub] - qoffset > 74 THEN
      DO:
        ASSIGN mpos = qstart[msub] - (qoffset + 2) /* FIRST '-' POS */
          mdiff = 78 - mpos.                  /* # OF '-' CHARS */
        PUT SCREEN NO-ATTR-SPACE row strip-menu-row COLUMN mpos
          FILL("=", mdiff)  + ">".
        LEAVE LOOP2.
      END.
      ASSIGN qdown = qdown + 1.
      PUT SCREEN COLOR VALUE(choice-color) ATTR-SPACE
        row strip-menu-row COLUMN (qstart[msub] - qoffset)
        qlabel[msub].
    END. /* LOOP2 */
    ASSIGN msub    = mtop + moffset
      mprev   = 0
      moffset = 0.  /* WE'RE DONE WITH IT, RESET IT */
    LOOP3:
    DO WHILE TRUE:
      IF mprev >= mtop AND mprev < mtop + qdown THEN
      PUT SCREEN COLOR VALUE(choice-color) ATTR-SPACE
        row strip-menu-row COLUMN (qstart[mprev] - qoffset)
        qlabel[mprev].
      PUT SCREEN COLOR VALUE(msg-color)
        ATTR-SPACE row strip-menu-row COLUMN (qstart[msub] - qoffset)
        qlabel[msub].
      ASSIGN mprev = msub.
      PUT SCREEN COLOR VALUE(help-color)
        row strip-help-row COLUMN 3 ATTR-SPACE strip-prompt.
      READKEY.
      IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN
      LEAVE MAIN.
      IF CAN-DO( "GO,RETURN", KEYFUNCTION(LASTKEY) ) THEN
      DO:
        ASSIGN strip-sel[strip-depth] = qlabel[msub].
        LEAVE MAIN.  /* A VALID SELECTION HAS BEEN MADE */
      END.
      /*
      /* WR2030 - ENTIRE ROUTINE REWRITTEN */
      if keyfunction(lastkey) = "HELP" then
      do:
      /* CLEAR MENU AND MESSAGE AREA, SET UP FOR HOTKEY */
      /* WR27034 > */
      put screen row strip-menu-row column 1 no-attr-space fill(" ",78).
      put screen row strip-help-row column 1 no-attr-space fill(" ",78).
      /* WR27034 < */
      assign sv-prompt = strip-prompt
      prog-name = "rc/striphlp.p"
      do-jump   = no
      w-fkey    = keylabel(lastkey)
      fkey-list = "F5,F6,F9,F10,F11,F12," +
      "F21,F22,F23,F24,F25,F26," +
      "F27,F28,F29,F30,F31,F32".
      /* IS IT SHIFT-F2, OR DOES HOTKEYS FILE RUN THE JUMP PROG? */
      if w-fkey begins "PF" then
      assign w-fkey = substring(w-fkey,2).
      if can-do( fkey-list, w-fkey ) then
      do:
      if w-fkey ge "F21" and
      w-fkey le "F32" and
      length(w-fkey) = 3 then
      assign w-fkey = "ShiftF" +
      string( integer( substring(w-fkey,2,2) ) - 20 ).
      find hotkeys where
      hotkeys.user-id     = user-id and
      hotkeys.hotkey-name = w-fkey no-lock no-error.
      if not available hotkeys then
      find hotkeys where
      hotkeys.user-id     = "" and
      hotkeys.hotkey-name = w-fkey no-lock no-error.
      if (available hotkeys and
      hotkeys.hotkey-prog matches "*jump*") or
      (w-fkey = "ShiftF2" and not available hotkeys) then
      assign do-jump = yes.
      end.
      if do-jump then
      apply lastkey. /* INVOKE TRUE "HELP" IF DOING "JUMP" */
      else
      run rc/applhelp.p. /* ALLOW MULTI-LEVELS FOR HOTKEY INQ'S */
      /* RESTORE STRIP VALUES AFTER RETURNING FROM HOTKEY PROG */
      assign strip-prompt = sv-prompt
      strip-F4     = no   /* IN CASE A HOTKEY HAS SET IT */
      prog-name    = "".
      next LOOP1.
      end.
      */
      IF KEYLABEL(LASTKEY) = "HOME" OR
        KEYFUNCTION(LASTKEY) = "HOME" THEN
      DO:          /* TOGGLE BETWEEN FIRST/LAST ENTRIES IN LIST */
        IF msub > 1 AND mtop = 1 THEN
        DO:
          ASSIGN msub = 1.
          NEXT LOOP3.
        END.
        IF mlast > qdown THEN   /* MORE THAN ONE LINEFUL */
        DO:
          IF msub = 1 THEN
          DO:
            /* RIGHTMOST CHOICE WILL BE LAST ONE */
            /* LOOP LEFT UNTIL WE FIND ONE THAT WON'T FIT */
            LOOP-LEFT:
            DO mtop = mlast TO 1 BY -1:
              IF mtop = 1 THEN
              LEAVE LOOP-LEFT.
              IF qend[mlast] - qstart[mtop - 1] > 75 THEN
              LEAVE LOOP-LEFT.
            END.
            ASSIGN moffset = mlast - mtop.
          END.
          ELSE
          ASSIGN moffset = 0
            mtop    = 1.
          NEXT LOOP1.
        END.
        /* ELSE IT FITS ON ONE LINE, JUST POP TO LEFT OR RIGHT */
        IF msub = 1 THEN
        ASSIGN msub = mlast.
        ELSE
        ASSIGN msub = 1.
        NEXT LOOP3.
      END.  /* HOME logic */
      IF CAN-DO( "CURSOR-DOWN,CURSOR-RIGHT,TAB", KEYFUNCTION(LASTKEY) ) OR
        LASTKEY = 32         /* SPACE BAR */  THEN
      /* GO RIGHT 1 WORD ON LINE. IF ALREADY AT RIGHT, SCROLL RIGHT 1 */
      /* IF ALREADY ON LAST ENTRY, BEEP */
      DO:
        IF msub = mlast THEN
        DO:
          /* WRAP AROUND TO FIRST ENTRY */
          ASSIGN msub = 1.
          IF mtop = 1 THEN
          NEXT LOOP3.
          ASSIGN moffset = 0
            mtop    = 1.
          NEXT LOOP1.
        END.
        ASSIGN msub = msub + 1.
        IF msub < mtop + qdown THEN  /* NOT ON RIGHTMOST ONE */
        NEXT LOOP3.
        /* IF NEXT RIGHT IS LAST CHOICE, THEN FIND LEFTMOST ONE */
        /* THAT FITS IN 76 CHAR, ELSE LEAVE ROOM FOR '===>' SYMBOL */
        DO WHILE qend[msub] - qstart[mtop] >
            (IF msub = mlast THEN 75 ELSE 71):
          ASSIGN mtop = mtop + 1.
        END.
        ASSIGN moffset = msub - mtop.
        NEXT LOOP1.
      END.
      IF CAN-DO( "CTRL-N,NEXT-PAGE", KEYLABEL(LASTKEY) ) OR
        KEYFUNCTION(LASTKEY) = "PAGE-DOWN" THEN
      /* (N)ext line of entries */
      /* GO RIGHT 1 LINEFUL. IF ALREADY ON LAST LINE, BEEP */
      DO:
        IF mtop + qdown - 1 = mlast THEN
        DO:
          BELL.
          NEXT LOOP3.
        END.
        ASSIGN mtop    = mtop + qdown
          moffset = 0.
        NEXT LOOP1.
      END.   /* NEXT-PAGE or CTRL-N do */
      IF CAN-DO( "CURSOR-UP,CURSOR-LEFT,BACKSPACE,BACK-TAB",
        KEYFUNCTION(LASTKEY) ) THEN
      /* GO LEFT 1 WORD ON LINE. IF ALREADY AT LEFT, SCROLL LEFT 1 */
      /* IF ALREADY ON FIRST ENTRY, BEEP */
      DO:
        IF msub = 1 THEN
        DO:
          /* WRAP AROUND TO LAST ENTRY */
          ASSIGN msub = mlast.
          IF mlast = qdown THEN
          NEXT LOOP3.
          ELSE
          DO:
            /* RIGHTMOST CHOICE WILL BE LAST ONE */
            /* LOOP LEFT UNTIL WE FIND ONE THAT WON'T FIT */
            GO-LEFT:
            DO mtop = mlast TO 1 BY -1:
              IF mtop = 1 THEN
              LEAVE GO-LEFT.
              IF qend[mlast] - qstart[mtop - 1] > 75 THEN
              LEAVE GO-LEFT.
            END.
            ASSIGN moffset = mlast - mtop.
            NEXT LOOP1.
          END.
        END.
        ASSIGN msub = msub - 1.
        IF msub >= mtop THEN
        NEXT LOOP3.
        ASSIGN mtop    = mtop - 1
          moffset = 0.
        NEXT LOOP1.
      END.
      IF CAN-DO( "CTRL-P,PREV-PAGE", KEYLABEL(LASTKEY) ) OR
        KEYFUNCTION(LASTKEY) = "PAGE-UP"   THEN
      /* GO LEFT 1 LINEFUL. IF ALREADY ON FIRST LINE, BEEP */
      DO:
        IF mtop = 1 THEN
        DO:
          BELL.
          NEXT LOOP3.
        END.
        ASSIGN mdiff = qend[mtop - 1].
        LOOP-BACK:
        DO WHILE TRUE:
          ASSIGN mtop = mtop - 1.
          IF mdiff - qstart[mtop] > 71 THEN
          DO:
            ASSIGN mtop = mtop + 1.
            LEAVE LOOP-BACK.
          END.
          IF mtop = 1 THEN
          LEAVE LOOP-BACK.
        END.  /* LOOP-BACK */
        ASSIGN moffset = 0.
        NEXT LOOP1.
      END.   /* PREV-PAGE or CTRL-P do */
      /* USER ENTERED A CHARACTER TO MATCH TO AN ENTRY ON LINE */
      /* SEE IF WE CAN FIND ONE THAT MATCHES, THEN CHECK FOR MORE THAN 1 */
      ASSIGN mkeys = mkeys + KEYLABEL(LASTKEY).
      DO mlook = 1 TO mlast:
        IF qlabel[mlook] BEGINS mkeys THEN
        DO:
          DUP-CHECK:
          DO mdiff = mlook + 1 TO mlast:
            IF qlabel[mdiff] BEGINS mkeys THEN
            LEAVE DUP-CHECK.
          END.
          IF mdiff > mlast THEN
          DO:
            /* GOT A UNIQUE MATCH ! */
            ASSIGN msub                   = mlook
              strip-sel[strip-depth] = qlabel[msub].
            LEAVE MAIN.
          END.
          /* ENTRY IS AMBIGUOUS, HIGHLITE FIRST ONE FOUND */
          IF mlook >= mtop AND mlook <= mtop + qdown - 1 THEN
          DO:      /* ITS ON THE SAME FRAME, EASY CASE! */
            ASSIGN msub = mlook.
            NEXT LOOP3.
          END.
          /* IT'S ON A DIFFERENT FRAME, !@#$% !! */
          ASSIGN msub = mlook
            mtop = msub
            moffset = 0.
          NEXT LOOP1.
        END.
      END.
      BELL.  /* INVALID KEY DEPRESSED OR CAN'T MATCH KEY(S) ENTERED */
      ASSIGN mkeys = "".
      INPUT CLEAR.
    END.  /* LOOP3 */
    IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN
    LEAVE MAIN.
  END.   /* LOOP1 */
END.   /* MAIN */
IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN
ASSIGN strip-F4 = YES.
ASSIGN strip-list = ""
  strip-name = ""
  strip-prompt = "".
/* CLEAR MENU AND MESSAGE AREA */
/* WR27034 > */
/*
put screen row strip-menu-row column 1 no-attr-space fill(" ",78).
put screen row strip-help-row column 1 no-attr-space fill(" ",78).
*/
HIDE MESSAGE NO-PAUSE.
MESSAGE FILL(" ", 80).
MESSAGE FILL(" ", 80).
HIDE MESSAGE NO-PAUSE.
/* WR27034 < */
/* pause 0 before-hide.  /* DEFAULT MODE */ 9709 CAH: not a good idea */
PAUSE BEFORE-HIDE.  /* RCI Default */
RETURN.  /* TO CALLING APPLICATION */
