
/*------------------------------------------------------------------------
    File        : api/ResponseHandlerExchangeRate.p
    Purpose     : 

    Syntax      :

    Description : Response Handler for exchangeratesapi.io

    Author(s)   : DEVA$!
    Created     : Wed Dec 01 01:43:44 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

    USING Progress.Json.ObjectModel.*.
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject          AS JsonObject        NO-UNDO.
    DEFINE VARIABLE joError          AS JsonObject        NO-UNDO.
    
    DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.    
    DEFINE VARIABLE cSuccess       AS CHARACTER NO-UNDO.
        
    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
    
    IF iplcResponseData EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Could not get any response"
            .
        
        RETURN.
    END.
            
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        NO-ERROR.
    
    cSuccess = oObject:GetJsonText("success") NO-ERROR.
    IF cSuccess NE "" THEN
        oplSuccess = LOGICAL(cSuccess).
    
    IF oplSuccess THEN DO:
        opcMessage = "Success".
        RETURN.
    END.
    
    joError = oObject:GetJsonObject("error") NO-ERROR.

    IF NOT ERROR-STATUS:ERROR THEN DO:
        opcMessage = joError:GetJsonText ("message") NO-ERROR.
        oplSuccess = FALSE.
        RETURN.        
    END. 

    FINALLY:
    	IF VALID-OBJECT (oModelParser) THEN
    	    DELETE OBJECT oModelParser.
    	   
    	IF VALID-OBJECT (oObject) THEN
    	   	DELETE OBJECT oObject.

        IF VALID-OBJECT (joError) THEN
            DELETE OBJECT joError.
    END FINALLY.        