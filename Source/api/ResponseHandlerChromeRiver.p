/*------------------------------------------------------------------------
    File        : api/ResponseHandlerChromeRiver.p
    Purpose     : Processes the response and returns the status

    Syntax      :

    Description : Processes the response and returns the status

    Author(s)   : DEVA$!
    Created     : Tue Jun 2 07:17:43 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/
    USING Progress.Json.ObjectModel.*.
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser   AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject        AS JsonObject        NO-UNDO.
    DEFINE VARIABLE jaErrors       AS JsonArray         NO-UNDO.
    DEFINE VARIABLE joErrors       AS JsonObject        NO-UNDO.
    DEFINE VARIABLE iArrayLength   AS INTEGER           NO-UNDO.
    DEFINE VARIABLE cResultCode    AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR          NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER           NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cDetail        AS CHARACTER         NO-UNDO.
    
    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        NO-ERROR.
    
    /* If casting to JSONObject fails then it means there are errors (as error response is sent in JSONArray) */
    IF NOT VALID-OBJECT(oObject) THEN DO:
        oplSuccess = FALSE.
        
        jaErrors = CAST(oModelParser:Parse(INPUT lcResponseData),JsonArray) NO-ERROR.
        IF VALID-OBJECT(jaErrors) THEN DO:
            iArrayLength = jaErrors:LENGTH. 
            
            DO iCount = 1 TO iArrayLength:
                ASSIGN
                    joErrors = jaErrors:GetJsonObject(iCount)
                    cMessage = joErrors:GetJsonText("detail")
                    cDetail  = joErrors:GetJsonText("message")
                    NO-ERROR.  
                
                IF cMessage NE "" AND cMessage NE ? AND cMessage NE "null" THEN
                    opcMessage = opcMessage + "~n" + cMessage.

                IF cDetail NE "" AND cDetail NE ? AND cDetail NE "null" THEN
                    opcMessage = opcMessage + "~n" + cDetail.
            END.            
            
            opcMessage = TRIM(opcMessage, "~n").
        END.
    END.
    ELSE DO:
        ASSIGN
            opcMessage = "Success"
            oplSuccess = TRUE
            .
    END.
