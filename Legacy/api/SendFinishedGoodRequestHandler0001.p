/*------------------------------------------------------------------------
    File        : api/SendFinishedGoodRequestHandler0001.p
    Purpose     : Returns the request data for product addition

    Syntax      :

    Description : Returns the request data for product addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    
    DEFINE INPUT        PARAMETER TABLE            FOR ttArgs.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE codeBars                  AS CHARACTER NO-UNDO INITIAL "765987".
    DEFINE VARIABLE cat05                     AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE saleRestrictionDays       AS CHARACTER NO-UNDO INITIAL "180".
    DEFINE VARIABLE boxesXbed                 AS CHARACTER NO-UNDO INITIAL "1"  .
    DEFINE VARIABLE mainUnit                  AS CHARACTER NO-UNDO INITIAL "EA".
    DEFINE VARIABLE packUnit                  AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE refUnit                   AS CHARACTER NO-UNDO INITIAL "EA".
    DEFINE VARIABLE unitsPerSale              AS CHARACTER NO-UNDO INITIAL "1".
    DEFINE VARIABLE cardCode                  AS CHARACTER NO-UNDO INITIAL "1".
    
    FIND FIRST ttArgs
         WHERE ttArgs.argType  = "ROWID"
           AND ttArgs.argKey   = "itemfg" NO-ERROR.
    IF NOT AVAILABLE ttArgs THEN DO:
        ASSIGN
            opcMessage = "No valid itemfg record passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    
    FIND FIRST itemfg NO-LOCK
         WHERE ROWID(itemfg) = TO-ROWID(ttArgs.argValue) NO-ERROR.
    IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN
            opcMessage = "Invalid itemfg ROWID passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    
    ASSIGN
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD16", cardCode)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.weight-100", STRING(itemfg.weight-100 / 100))
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD15", unitsPerSale)
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD14", refUnit)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.case-count", STRING(itemfg.case-count))
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD12", packUnit)
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD11", mainUnit)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.case-pall", STRING(itemfg.case-pall))
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD9", boxesXbed)
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD8", saleRestrictionDays)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.lead-days", STRING(itemfg.lead-days))
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD6", cat05)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.spare-char-1", itemfg.spare-char-1)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.part-dscr2", itemfg.part-dscr2)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.part-dscr1", itemfg.part-dscr1)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.cust-no", itemfg.cust-no)
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD1", codeBars)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.procat", itemfg.procat)
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.i-name", REPLACE(itemfg.i-name,'"',""))
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.i-no", REPLACE(STRING(itemfg.i-no),'"',""))
        ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.company", itemfg.company)
        .
    
    ASSIGN
        opcMessage = ""
        oplSuccess = TRUE
        .   
