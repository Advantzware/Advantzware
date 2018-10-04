  FIELD bol-no LIKE inv-line.bol-no LABEL "Usr Num"~
  FIELD i-name LIKE inv-line.i-name LABEL "Connect Name"~
  FIELD inv-no LIKE inv-line.inv-no LABEL "PID" COLUMN-LABEL "PID"~
  FIELD line LIKE inv-line.line FORMAT ">>>>>>>>>9" LABEL "Duration"~
  FIELD i-dscr LIKE inv-line.i-dscr LABEL "Trans Time"
