/* cc-code.i */

PROCEDURE cc-codeBuild:
  SESSION:SET-WAIT-STATE('General').
  FOR EACH {1} NO-LOCK WHERE {1}.company EQ g_company
      BREAK BY {1}.cc-code:
    IF NOT FIRST-OF({1}.cc-code) OR {1}.cc-code EQ '' THEN NEXT.
    CREATE ttblcc-code.
    ttblcc-code.cc-code = {1}.cc-code.
  END.
  SESSION:SET-WAIT-STATE('').
END PROCEDURE.
