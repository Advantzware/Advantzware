
/*------------------------------------------------------------------------
    File        : ResponseHandlerVertexToken.p
    Purpose     : 

    Syntax      :

    Description : Response Handler for Vertex API		

    Author(s)   : DEVA$!
    Created     : Mon Jun 28 09:34:45 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

    USING Progress.Json.ObjectModel.*.
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject          AS JsonObject        NO-UNDO.
    DEFINE VARIABLE cAccessToken     AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE lcResponseData   AS LONGCHAR          NO-UNDO.
    
    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        cAccessToken   = oObject:GetJsonText('access_token')              
        NO-ERROR.

    IF cAccessToken EQ "" THEN DO:
        ASSIGN
            opcMessage = "Error while generating access token"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .