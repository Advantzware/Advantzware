
DISABLE TRIGGERS FOR LOAD OF {1}.

PAUSE 0 BEFORE-HIDE.

FOR EACH {1}
    WHERE {1}.company   EQ cocode
      AND {1}.{2}       GE lv-est-no
      AND TRIM({1}.{2}) GT ""
    TRANSACTION:
  DISPLAY "{1}" FORMAT "x(15)"        LABEL "Filename"
          TRIM({1}.{2}) FORMAT "x(8)" LABEL "Est#".
  RUN fix (INPUT-OUTPUT {1}.{2}).
END.
