/*------------------------------------------------------------------------
    File        : api/premier/ResponseHandler.p
    Purpose     : Returns the response

    Syntax      :

    Description : Returns the response

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 13 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    USING Progress.Json.ObjectModel.*.
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser   AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject        AS JsonObject        NO-UNDO.
    DEFINE VARIABLE cResultCode    AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR          NO-UNDO.

    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        cResultCode    = oObject:GetJsonText("result_code")
        opcMessage     = oObject:GetJsonText("result_text")
        .
    
    IF opcMessage = "" THEN
        opcMessage = oObject:GetJsonText("Message").

    IF ERROR-STATUS:ERROR OR cResultCode EQ "0" THEN DO:
        oplSuccess = FALSE.
        RETURN.
    END.

    oplSuccess = TRUE.
