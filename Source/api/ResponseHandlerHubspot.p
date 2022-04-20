/*------------------------------------------------------------------------
    File        : api/ResponseHandlerHubspot.p
    Purpose     : Processes the response and returns the status

    Syntax      :

    Description : Processes the response and returns the status

    Author(s)   : DEVA$!
    Created     : Thu Aug 12 07:17:43 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/
    USING Progress.Json.ObjectModel.*.
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser   AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject        AS JsonObject        NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR          NO-UNDO.
    DEFINE VARIABLE cStatus        AS CHARACTER         NO-UNDO.
        
    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        .
    
    opcMessage = "Failure".
    
    IF VALID-OBJECT(oObject) THEN DO:
        oplSuccess = FALSE.

        opcMessage = oObject:GetJsonText("message") NO-ERROR.
        cStatus    = oObject:GetJsonText("status") NO-ERROR.

        oplSuccess = cStatus NE "error".

        IF oplSuccess THEN
            opcMessage = "Success".
    END.

    CATCH e AS Progress.Lang.Error :
		ASSIGN
		    oplSuccess = FALSE
		    opcMessage = e:GetMessage(1)
		    .
    END CATCH.