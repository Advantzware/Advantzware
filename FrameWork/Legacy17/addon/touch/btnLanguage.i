    IF i EQ {1} THEN
    ASSIGN
      btnLanguage-{1}:HIDDEN = NO
      btnLanguage-{1}:LABEL = CAPS(ENTRY(i,languageList))
      btnLanguage-{1}:SENSITIVE = YES.
