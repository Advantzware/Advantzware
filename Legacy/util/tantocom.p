
FOR EACH est WHERE est-type EQ 3:
  STATUS DEFAULT "Est#: " + TRIM(est-no).
  RUN util/tantocm2.p (ROWID(est)).
END.
STATUS DEFAULT "".
