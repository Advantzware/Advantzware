
    FOR EACH ttReleasesToPrint NO-LOCK
        WHERE ttReleasesToPrint.SessionID  EQ lv-foreachr,
        FIRST oe-relh WHERE ROWID(oe-relh) EQ ttReleasesToPrint.OeRelHRowID
