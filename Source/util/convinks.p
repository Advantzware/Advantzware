
DEF VAR li AS INT NO-UNDO.
DEF VAR li2 AS INT NO-UNDO.
  

SESSION:SET-WAIT-STATE ("general").

FOR EACH eb WHERE eb.est-type LE 4:
  DO li = 1 TO EXTENT(eb.i-code):
    IF eb.i-code[li] NE "" THEN
    DO li2 = 1 TO EXTENT(eb.i-code2):
      IF eb.i-code2[li2] EQ "" THEN DO:
        ASSIGN
         eb.i-ps2[li2]   = eb.i-ps[li]
         eb.i-code2[li2] = eb.i-code[li]
         eb.i-dscr2[li2] = eb.i-dscr[li]
         eb.i-%2[li2]    = eb.i-%[li].
        LEAVE.
      END.
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").
