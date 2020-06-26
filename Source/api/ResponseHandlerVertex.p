
/*------------------------------------------------------------------------
    File        : ResponseHandlerVertex.p
    Purpose     : 

    Syntax      :

    Description : Response Handler for Vertex API		

    Author(s)   : Mithun Porandla
    Created     : Fri Jun 26 07:04:20 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

    USING Progress.Json.ObjectModel.*.
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject          AS JsonObject        NO-UNDO.
    DEFINE VARIABLE jaErrors         AS JsonArray         NO-UNDO.
    DEFINE VARIABLE joErrors         AS JsonObject        NO-UNDO.
    DEFINE VARIABLE iLengthProperty  AS INTEGER           NO-UNDO.
    DEFINE VARIABLE iCount           AS INTEGER           NO-UNDO.    
    DEFINE VARIABLE lcResponseData   AS LONGCHAR          NO-UNDO.
    
    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        oObject        = CAST(oModelParser:PARSE(INPUT oObject:GetJsonText('data')),JsonObject)                
        NO-ERROR.

    ASSIGN
        jaErrors        = oObject:GetJsonArray("errors")
        iLengthProperty = jaErrors:LENGTH        
        NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND iLengthProperty GT 0 THEN DO:
        DO iCount = 1 TO iLengthProperty:
            ASSIGN
                joErrors   = jaErrors:GetJsonObject(iCount)
                opcMessage = joErrors:GetJsonText("detail") 
                NO-ERROR.            
        END.
        oplSuccess = FALSE.
        RETURN.        
    END. 

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .