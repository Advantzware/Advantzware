DEFINE TEMP-TABLE ttFGBin NO-UNDO
    FIELD fgBinRowID AS ROWID
    FIELD qty        AS DECIMAL
    FIELD qty-case   AS INTEGER
    FIELD partial    AS INTEGER
    FIELD cases      AS INTEGER
    INDEX fgBinRowID fgBinRowID
    .