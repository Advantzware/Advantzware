    
  DEF VAR ll-inks AS LOG NO-UNDO.
  DEF VAR lv-i-no LIKE item.i-no NO-UNDO.
  DEF VAR lv-pass LIKE est-op.op-pass NO-UNDO.

    
  ll-inks = NO.

  IF eb.est-type GT 4 THEN
  DO li = 1 TO EXTENT(eb.i-code):
    IF eb.i-code[li] NE "" THEN DO:
      ASSIGN
       lv-i-no = eb.i-code[li]
       lv-pass = eb.i-ps[li].
      LEAVE.
    END.
  END.

  ELSE
  DO li = 1 TO EXTENT(eb.i-code2):
    IF eb.i-code2[li] NE "" THEN DO:
      ASSIGN
       lv-i-no = eb.i-code2[li]
       lv-pass = eb.i-ps2[li].
      LEAVE.
    END.
  END.

  IF lv-i-no NE "" THEN DO:
    ll-inks = NOT CAN-FIND(FIRST est-op
                           WHERE est-op.company EQ b-eb.company
                             AND est-op.est-no  EQ b-eb.est-no
                             AND est-op.s-num   EQ b-eb.form-no
                             AND (est-op.b-num  EQ b-eb.blank-no OR b-eb.est-type NE 3) 
                             AND est-op.op-pass EQ lv-pass
                             AND est-op.line    LE 500
                             AND (est-op.dept   EQ "PR" OR est-op.dept EQ "CT")).

    IF NOT ll-inks THEN
    FOR EACH item
        WHERE item.company EQ eb.company
          AND item.i-no    EQ lv-i-no
        NO-LOCK,
        EACH est-op
        WHERE est-op.company EQ b-eb.company
          AND est-op.est-no  EQ b-eb.est-no
          AND est-op.s-num   EQ b-eb.form-no
          AND (est-op.b-num  EQ b-eb.blank-no OR b-eb.est-type NE 3) 
          AND est-op.op-pass EQ lv-pass
          AND est-op.line    LE 500
          AND (est-op.dept   EQ "PR" OR est-op.dept EQ "CT")
        NO-LOCK,
        FIRST mach
        WHERE mach.company EQ est-op.company
          AND mach.m-code  EQ est-op.m-code
          AND mach.pr-type EQ item.press-type
        NO-LOCK:

      ll-inks = YES.
      LEAVE.
    END.
  END.

  IF ll-inks THEN DO:
    DO li = 1 TO EXTENT(eb.i-code):
      ASSIGN
       b-eb.i-ps[li]   = eb.i-ps[li]
       b-eb.i-code[li] = eb.i-code[li]
       b-eb.i-dscr[li] = eb.i-dscr[li]
       b-eb.i-%[li]    = eb.i-%[li].
    END.
  
    DO li = 1 TO EXTENT(eb.i-code2):
      ASSIGN
       b-eb.i-ps2[li]   = eb.i-ps2[li]
       b-eb.i-code2[li] = eb.i-code2[li]
       b-eb.i-dscr2[li] = eb.i-dscr2[li]
       b-eb.i-%2[li]    = eb.i-%2[li].
    END.

    {ce/updunit#.i b-eb}

    ASSIGN
     b-eb.i-col      = eb.i-col
     b-eb.i-pass     = eb.i-pass
     b-eb.i-coat     = eb.i-coat
     b-eb.i-coat-p   = eb.i-coat-p
     b-eb.i-coldscr  = eb.i-coldscr.

    FIND FIRST b-ef OF b-eb EXCLUSIVE NO-ERROR.
    IF AVAIL b-ef THEN DO:
    FIND FIRST b-eb1 OF b-ef WHERE ROWID(b-eb1) NE ROWID(b-eb) NO-LOCK NO-ERROR.
    IF NOT AVAIL b-eb1 THEN
      ASSIGN
       b-ef.f-col    = b-eb.i-col
       b-ef.f-pass   = b-eb.i-pass
       b-ef.f-coat   = b-eb.i-coat
       b-ef.f-coat-p = b-eb.i-coat-p.
    END.
  END.

