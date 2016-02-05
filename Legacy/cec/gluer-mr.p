DEF PARAM BUFFER io-est-op FOR est-op.


IF AVAIL io-est-op THEN DO:
  RUN est/gluer-mr.p (ROWID(io-est-op)) NO-ERROR.
  FIND CURRENT io-est-op NO-ERROR.
END.
