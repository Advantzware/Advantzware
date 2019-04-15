
SESSION:SET-WAIT-STATE ("general").

FOR EACH ef-nsh BY company BY est-no:
  DISPLAY ef-nsh.company        LABEL "Company"
          TRIM(ef-nsh.est-no)   LABEL "Est#"
                                FORMAT "x(10)"
      WITH DOWN.
  IF dept EQ "" THEN dept = "RC".
END.

HIDE ALL NO-PAUSE.

SESSION:SET-WAIT-STATE ("").
