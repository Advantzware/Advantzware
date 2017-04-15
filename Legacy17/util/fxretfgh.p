
DEF BUFFER b-r for fg-rcpth.

DEF TEMP-TABLE tt-r LIKE fg-rcpth FIELD tt-rowid AS ROWID.

DEF VAR li LIKE fg-rcpth.r-no NO-UNDO.


SESSION:SET-WAIT-STATE ("general").

FOR EACH fg-rcpth NO-LOCK BREAK BY r-no:
  IF LAST-OF(r-no) AND NOT FIRST-OF(r-no) THEN
  FOR EACH b-r WHERE b-r.r-no EQ fg-rcpth.r-no NO-LOCK:
    CREATE tt-r.
    BUFFER-COPY b-r TO tt-r.
    tt-rowid = ROWID(b-r).
  END.
END.

FOR EACH tt-r,
    FIRST fg-rcpth WHERE ROWID(fg-rcpth) EQ tt-rowid NO-LOCK
    BY tt-r.r-no BY tt-r.rec_key:
    
  li = 0.
  
  FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL fg-rctd THEN li = fg-rctd.r-no.
  
  FIND LAST b-r WHERE b-r.r-no GT li USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL b-r THEN li = b-r.r-no.
  
  tt-r.r-no = li + 1.
  
  FOR EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
      BY fg-rdtlh.rec_key:
    fg-rdtlh.r-no = tt-r.r-no.
    LEAVE.
  END.
  
  FOR EACH b-r WHERE b-r.r-no EQ fg-rcpth.r-no
      BY b-r.rec_key:
    b-r.r-no = tt-r.r-no.
    LEAVE.
  END.
END.

SESSION:SET-WAIT-STATE ("").
