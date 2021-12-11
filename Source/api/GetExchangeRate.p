/*------------------------------------------------------------------------
    File        : api/GetExchangeRate.p
    Purpose     : API to fetch exchange rates

    Syntax      :

    Description : API to fetch exchange rates

    Author(s)   : DEVA$!
    Created     : Wed Dec 01 01:11:33 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/
USING system.SharedConfig.

{api/ttArgs.i}
{api/CommonAPIProcs.i}

DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.    
DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

DEFINE VARIABLE cExchangeRateAsOfDate  AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtExchangeRateAsOfDate AS DATE      NO-UNDO.
DEFINE VARIABLE cBaseCurrency          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExchangeRateSymbols   AS CHARACTER NO-UNDO.

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
    FIND FIRST APIOutbound NO-LOCK
         WHERE APIOutbound.apiOutboundID EQ ipiAPIOutboundID
         NO-ERROR.
    IF NOT AVAILABLE APIOutbound THEN DO:
        ASSIGN
            opcMessage = "No APIOutbound record found"
            oplSuccess = FALSE
            .
        RETURN.        
    END.             
    
    FIND FIRST ttArgs
         WHERE ttArgs.argType  = "ROWID"
           AND ttArgs.argKey   = "ExchangeRateAsOfDate"
         NO-ERROR.
    IF AVAILABLE ttArgs THEN
        cExchangeRateAsOfDate = ttArgs.argValue.
    
    IF cExchangeRateAsOfDate EQ ? OR cExchangeRateAsOfDate EQ "" THEN
        cExchangeRateAsOfDate = "latest".
        
    IF cExchangeRateAsOfDate NE "latest" THEN DO:
        dtExchangeRateAsOfDate = DATE(cExchangeRateAsOfDate) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN   
                opcMessage = "Date '" + cExchangeRateAsOfDate + "' is invalid"
                oplSuccess = FALSE
                .
            RETURN.            
        END.
        
        cExchangeRateAsOfDate = STRING(YEAR(dtExchangeRateAsOfDate), "9999") + "-" 
                              + STRING(MONTH(dtExchangeRateAsOfDate), "99") + "-"
                              + STRING(DAY(dtExchangeRateAsOfDate), "99").
    END.
            
    FIND FIRST ttArgs
         WHERE ttArgs.argType  = "ROWID"
           AND ttArgs.argKey   = "BaseCurrency"
         NO-ERROR.
    IF AVAILABLE ttArgs THEN
        cBaseCurrency = ttArgs.argValue.
    
    FIND FIRST ttArgs
         WHERE ttArgs.argType  = "ROWID"
           AND ttArgs.argKey   = "ExchangeRateSymbols"
         NO-ERROR.
    IF AVAILABLE ttArgs THEN
        cExchangeRateSymbols = ttArgs.argValue.
    
    cExchangeRateSymbols = REPLACE(cExchangeRateSymbols, "|", ",").

    system.SharedConfig:Instance:SetValue("EndPoint_" + APIOutbound.apiID + "_AsOfDate", cExchangeRateAsOfDate).
    system.SharedConfig:Instance:SetValue("EndPoint_" + APIOutbound.apiID + "_BaseCurrency", cBaseCurrency).
    system.SharedConfig:Instance:SetValue("EndPoint_" + APIOutbound.apiID + "_RateCurrencyCodes", cExchangeRateSymbols).
                
    ASSIGN   
        opcMessage = "Success"
        oplSuccess = TRUE
        .
END.        
