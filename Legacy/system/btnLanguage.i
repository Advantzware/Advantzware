IF i EQ {1} THEN DO:
    ASSIGN
        btnLanguage-{1}:HIDDEN    = NO
        btnLanguage-{1}:LABEL     = ENTRY(i,cLanguageList)
        btnLanguage-{1}:TOOLTIP   = ENTRY(i,cLanguageList)
        btnLanguage-{1}:SENSITIVE = YES
        .
    IF SEARCH(ENTRY(i,cFlagList)) NE ? THEN
        btnLanguage-{1}:LOAD-IMAGE-UP(ENTRY(i,cFlagList)).
END.
