/*------------------------------------------------------------------------
    File        : api/SendFinishedGood.p
    Purpose     : Returns the request data for product addition

    Syntax      :

    Description : Returns the request data for product addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.    
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

    DEFINE VARIABLE codeBars                  AS CHARACTER NO-UNDO INITIAL "765987".
    DEFINE VARIABLE cat05                     AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE saleRestrictionDays       AS CHARACTER NO-UNDO INITIAL "180".
    DEFINE VARIABLE boxesXbed                 AS CHARACTER NO-UNDO INITIAL "1"  .
    DEFINE VARIABLE mainUnit                  AS CHARACTER NO-UNDO INITIAL "EA".
    DEFINE VARIABLE packUnit                  AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE refUnit                   AS CHARACTER NO-UNDO INITIAL "EA".
    DEFINE VARIABLE unitsPerSale              AS CHARACTER NO-UNDO INITIAL "1".
    DEFINE VARIABLE cardCode                  AS CHARACTER NO-UNDO INITIAL "1".

    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE ttArgs,
            INPUT ipiAPIOutboundID,
            INPUT ipiAPIOutboundTriggerID,            
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
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
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD16", cardCode).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.weight-100", STRING(itemfg.weight-100 / 100)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD15", unitsPerSale).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD14", refUnit).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.case-count", STRING(itemfg.case-count)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD12", packUnit).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD11", mainUnit).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.case-pall", STRING(itemfg.case-pall)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD9", boxesXbed).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD8", saleRestrictionDays).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.lead-days", STRING(itemfg.lead-days)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD6", cat05).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.spare-char-1", itemfg.spare-char-1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.part-dscr2", itemfg.part-dscr2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.part-dscr1", itemfg.part-dscr1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.cust-no", itemfg.cust-no).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD1", codeBars).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.procat", itemfg.procat).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.i-name", itemfg.i-name).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.i-no", STRING(itemfg.i-no)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "itemfg.company", itemfg.company).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "stackHeight", STRING(itemfg.stackHeight)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "palletWidth", STRING(itemfg.unitWidth,">>>>9.99")).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "palletLength", STRING(itemfg.unitLength,">>>>9.99")).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "palletHeight", STRING(itemfg.unitHeight,">>>>9.99")).
        
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .
    END.
