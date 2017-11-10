    IF i EQ {1} THEN DO:
        ASSIGN
          btnLanguage-{1}:HIDDEN = NO
          btnLanguage-{1}:LABEL = ENTRY(i,languageList)
          btnLanguage-{1}:TOOLTIP = ENTRY(i,languageList)
          btnLanguage-{1}:SENSITIVE = YES
          .
        IF SEARCH(ENTRY(i,flagList)) NE ? THEN
        btnLanguage-{1}:LOAD-IMAGE-UP(ENTRY(i,flagList)).
    END.
