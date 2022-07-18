/*------------------------------------------------------------------------
    File        : api/ResponseHandlerZohoDesk.p
    Purpose     : Processes the response and returns the status

    Syntax      :

    Description : Processes the response and returns the status

    Author(s)   : DEVA$!
    Created     : Wed Apr 27 07:17:43 EDT 2022
    Notes       :
  ----------------------------------------------------------------------*/
    USING Progress.Json.ObjectModel.*.
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject          AS JsonObject        NO-UNDO.
    DEFINE VARIABLE cErrorMessage    AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE lcResponseData   AS LONGCHAR          NO-UNDO.
    
    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        cErrorMessage  = oObject:GetJsonText("error")
        NO-ERROR.
    
    IF cErrorMessage EQ "" OR cErrorMessage EQ ? THEN
        cErrorMessage  = oObject:GetJsonText("errorCode") NO-ERROR.
    
    IF cErrorMessage NE "" THEN DO:
        ASSIGN
            opcMessage = cErrorMessage
            oplSuccess = FALSE
            .
        
        RETURN.
    END.
     
    ASSIGN
        opcMessage = "Success"
        oplSuccess = TRUE
        .
