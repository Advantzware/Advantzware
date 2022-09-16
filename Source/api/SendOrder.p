/*------------------------------------------------------------------------
    File        : api/SendOrder.p
    Purpose     : Returns the request data for an order

    Syntax      :

    Description : Returns the request data for an order

    Author(s)   : DEVA$!
    Created     : Fri Feb 18 12:02:43 EDT 2022
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
    
    DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
    
    DEFINE VARIABLE lcOrderLineData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOrderMiscData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcNoteData        AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOrderLine       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOrderMisc       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcNote            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatOrderLine AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatOrderMisc AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatNote      AS LONGCHAR NO-UNDO.
    
    DEFINE VARIABLE iDueOnMonth           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iDueOnDay             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNetDays              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dDiscPct              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iDiscDays             AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE cTermsDesc            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iOrderLineCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderMiscCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotalLineCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cNote                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTotalMiscCharge      AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-carrier  FOR carrier.
    DEFINE BUFFER bf-shipto   FOR shipto.
    DEFINE BUFFER bf-loc      FOR loc.
    DEFINE BUFFER bf-location FOR location.
      
    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
        
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE  ttArgs,
            INPUT ipiAPIOutboundID,
            INPUT ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
        FIND FIRST ttArgs
             WHERE ttArgs.argType EQ "ROWID"
               AND ttArgs.argKey  EQ "oe-ord"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid oe-ord record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE ROWID(oe-ord) EQ TO-ROWID(ttArgs.argValue) 
             NO-ERROR.
        IF NOT AVAILABLE oe-ord THEN DO:
            ASSIGN
                opcMessage = "Invalid oe-ord ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.

        oAttribute = NEW system.Attribute().
        oAttribute:RequestDataType = gcRequestDataType.
        
        RUN pGetRequestData ("SendOrder", "OrderLine", OUTPUT lcOrderLineData).
        RUN pGetRequestData ("SendOrder", "OrderMisc", OUTPUT lcOrderMiscData).
        RUN pGetRequestData ("SendOrder", "Note", OUTPUT lcNoteData).
        
        FOR EACH oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ oe-ord.company
              AND oe-ordl.ord-no  EQ oe-ord.ord-no:
            ASSIGN
                lcOrderLine     = lcOrderLineData
                iOrderLineCount = iOrderLineCount
                .
            
            lcOrderLine = oAttribute:ReplaceAttributes(lcOrderLine, BUFFER oe-ordl:HANDLE).
            
            lcConcatOrderLine = lcConcatOrderLine + lcOrderLine.    
        END.

        FOR EACH oe-ordm NO-LOCK OF oe-ord
            WHERE oe-ordm.deleted EQ FALSE:
            ASSIGN
                lcOrderMisc      = lcOrderMiscData
                iOrderMiscCount  = iOrderMiscCount + 1
                dTotalMiscCharge = dTotalMiscCharge + oe-ordm.amt.
                .
            
            lcOrderMisc = oAttribute:ReplaceAttributes(lcOrderMisc, BUFFER oe-ordm:HANDLE).
            
            lcConcatOrderMisc = lcConcatOrderMisc + lcOrderMisc.    
        END.
        
        FOR EACH notes NO-LOCK
            WHERE notes.rec_key EQ oe-ord.rec_key:
            ASSIGN
                lcNote = lcNoteData
                cNote  = notes.note_text
                cNote  = REPLACE(cNote, CHR(10) + CHR(13), " ")
                cNote  = REPLACE(cNote, CHR(10), " ")
                cNote  = REPLACE(cNote, CHR(13), " ")
                cNote  = REPLACE(cNote, "  ", " ")
                .
            
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcNote, "Note_Text", cNote).
            
            lcNote = oAttribute:ReplaceAttributes(lcNote, BUFFER notes:HANDLE).
            
            lcConcatNote = lcConcatNote + lcNote.
        END.
                  
        iTotalLineCount = iOrderLineCount + iOrderMiscCount.
        
        ioplcRequestData = REPLACE(ioplcRequestData, "$OrderLine$", lcConcatOrderLine).
        ioplcRequestData = REPLACE(ioplcRequestData, "$OrderMisc$", lcConcatOrderMisc).
        ioplcRequestData = REPLACE(ioplcRequestData, "$Notes$", lcConcatNote).

        RUN Credit_GetTerms (
            INPUT  oe-ord.company,
            INPUT  oe-ord.terms,
            OUTPUT iDueOnMonth,
            OUTPUT iDueOnDay,
            OUTPUT iNetDays, 
            OUTPUT dDiscPct,  
            OUTPUT iDiscDays,
            OUTPUT cTermsDesc,
            OUTPUT lError         
            ).
        
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "DueOnMonth", STRING(iDueOnMonth)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "DueOnDay", STRING(iDueOnDay)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "NetDays", STRING(iNetDays)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "DiscountPercent", STRING(dDiscPct)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "DiscountDays", STRING(iDiscDays)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "TermsDescription", STRING(cTermsDesc)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "OrderLineCount", STRING(iOrderLineCount)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "OrderMiscCount", STRING(iOrderMiscCount)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalLineCount", STRING(iTotalLineCount)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalMiscCharge", STRING(dTotalMiscCharge)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "APIField1", "").
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "APIField2", "").
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "APIField3", "").
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "APIField4", "").
        
        FIND FIRST bf-shipto NO-LOCK 
             WHERE bf-shipto.company EQ oe-ord.company
               AND bf-shipto.cust-no EQ oe-ord.cust-no
               AND bf-shipto.ship-id EQ oe-ord.ship-id
             NO-ERROR.

        FIND FIRST bf-carrier NO-LOCK
             WHERE bf-carrier.company EQ oe-ord.company
               AND bf-carrier.carrier EQ oe-ord.carrier
               AND bf-carrier.loc     EQ (IF AVAILABLE bf-shipto THEN bf-shipto.loc ELSE "")
             NO-ERROR.
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-carrier:HANDLE).
                                                
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER oe-ord:HANDLE).

        FIND FIRST cust NO-LOCK 
             WHERE cust.company EQ oe-ord.company
               AND cust.cust-no EQ oe-ord.cust-no
             NO-ERROR.
                  
        FIND FIRST shipto NO-LOCK 
             WHERE shipto.company EQ oe-ord.company
               AND shipto.cust-no EQ oe-ord.cust-no
               AND shipto.ship-id EQ oe-ord.ship-id
             NO-ERROR.

        FIND FIRST bf-loc NO-LOCK 
             WHERE bf-loc.company EQ oe-ord.company 
               AND bf-loc.loc     EQ oe-ord.loc
            NO-ERROR. 
        IF AVAILABLE bf-loc THEN 
            FIND FIRST bf-location NO-LOCK 
                 WHERE bf-location.company      EQ bf-loc.company 
                   AND bf-location.locationCode EQ bf-loc.loc
                 NO-ERROR.
        
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER cust:HANDLE).
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER shipto:HANDLE).
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-location:HANDLE).
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.        
        


/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetRequestData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcParentID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDetailID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE lcHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFooter AS LONGCHAR NO-UNDO.
        
    DEFINE BUFFER bf-APIOutboundDetail FOR apiOutboundDetail.
        
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN
        oplcRequestData = bf-APIOutboundDetail.data.

END PROCEDURE.
