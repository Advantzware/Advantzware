
DEF VAR li AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF eb.

PAUSE 0 BEFORE-HIDE.

FOR EACH eb BY eb.company BY eb.est-no:
  DISPLAY "Processing Company/Est#: " +
          TRIM(eb.company) + "/" + TRIM(eb.est-no) FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  ASSIGN
   eb.k-wid-array2    = 0
   eb.k-wid-scr-type2 = ""
   eb.k-len-array2    = 0
   eb.k-len-scr-type2 = "".

  DO li = 1 TO EXTENT(eb.k-wid-array):
    eb.k-wid-array2[li] = eb.k-wid-array[li].
  END.

  DO li = 1 TO EXTENT(eb.k-wid-scr-type):
    eb.k-wid-scr-type2[li] = eb.k-wid-scr-type[li].
  END.

  DO li = 1 TO EXTENT(eb.k-len-array):
    eb.k-len-array2[li] = eb.k-len-array[li].
  END.

  DO li = 1 TO EXTENT(eb.k-len-scr-type):
    eb.k-len-scr-type2[li] = eb.k-len-scr-type[li].
  END.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
