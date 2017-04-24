/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *   Thomas Hansen, appSolutions a|s                                  *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : HttpClient
    Purpose     : Provides HTML GET and POST functionality using Socket
                  communication
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 02 23:23:18 CET 2013
    Notes       : The HttpClient write debugging messages to the AppServer
                  or Client Logfile using the "HttpClient" custom entry type
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

&SCOPED-DEFINE HTTP-NEWLINE CHR(13) + CHR(10)

CLASS Consultingwerk.HttpClient
    IMPLEMENTS ISocketReadHandler:

    DEFINE VARIABLE hReadResponseProcedure AS HANDLE   NO-UNDO.
    DEFINE VARIABLE lcResponse             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE hSocket                AS HANDLE   NO-UNDO.
    DEFINE VARIABLE lActive                AS LOGICAL  NO-UNDO INITIAL FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Returns the HttpResult code of the last request
        Notes:   e.g. HTTP/1.1 200 OK
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HttpResult AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the HttpResult code of the last request
        Notes:   e.g. 200
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HttpResultCode AS INTEGER NO-UNDO INITIAL ?
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the Dictionary of Response headers
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResponseHeaders AS ListNameValuePair NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the Dictionary of Request headers
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RequestHeaders AS CharacterDictionary NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the HttpClient class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC HttpClient ():
        SUPER ().

        RUN Consultingwerk/read-response-procedure.p PERSISTENT SET hReadResponseProcedure (THIS-OBJECT) .

        THIS-OBJECT:RequestHeaders = NEW CharacterDictionary ("":U, CHR(1), "":U, CHR(1)) .
        THIS-OBJECT:ResponseHeaders = NEW ListNameValuePair () .

        FIX-CODEPAGE (lcResponse) = "utf-8":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs a GET http request
        Notes:
        @param pcHostName The hostname to request the resource form
        @param pcUrl The URL to request
        @return The LONGCHAR response
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GetLongchar (pcHostName AS CHARACTER,
                                        pcUrl AS CHARACTER):

        RETURN THIS-OBJECT:GetLongChar (pcHostName,
                                        ?,
                                        pcUrl,
                                        ?) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a GET http request
        Notes:
        @param pcHostName The hostname to request the resource form
        @param pcUrl The URL to request
        @param pcHttpVersion The http version to support (HTTP/1.0, HTTP/1.1)
        @return The LONGCHAR response
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GetLongchar (pcHostName AS CHARACTER,
                                        pcUrl AS CHARACTER,
                                        pcHttpVersion AS CHARACTER):

        RETURN THIS-OBJECT:GetLongChar (pcHostName,
                                        ?,
                                        pcUrl,
                                        pcHttpVersion) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a GET http request
        Notes:
        @param pcHostName The hostname to request the resource form
        @param piPort The portnumber to request from
        @param pcUrl The URL to request
        @return The LONGCHAR response
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GetLongchar (pcHostName AS CHARACTER,
                                        piPort     AS INTEGER,
                                        pcUrl      AS CHARACTER):

        RETURN THIS-OBJECT:GetLongChar (pcHostName,
                                        piPort,
                                        pcUrl,
                                        "":U) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a GET http request
        Notes:
        @param pcHostName The hostname to request the resource form
        @param piPort The portnumber to request from
        @param pcUrl The URL to request
        @param pcHttpVersion The http version to support (HTTP/1.0, HTTP/1.1)
        @return The LONGCHAR response
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GetLongchar (pcHostName AS CHARACTER,
                                        piPort AS INTEGER,
                                        pcUrl AS CHARACTER,
                                        pcHttpVersion AS CHARACTER):

        DEFINE VARIABLE lcRequest AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE i              AS INTEGER       NO-UNDO .
        DEFINE VARIABLE cKey           AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE mRequest       AS MEMPTR        NO-UNDO .
        DEFINE VARIABLE cOptions       AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE oPair          AS NameValuePair NO-UNDO .
        DEFINE VARIABLE cResource      AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cConnectString AS CHARACTER     NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 04.01.2013
           Default Ports */
        IF IntegerType:IsNullOrEmpty (piPort) THEN DO:
            IF pcUrl BEGINS "https:":U THEN
                piPort = 443 .
            ELSE
                piPort = 80 .
        END.

        IF pcUrl BEGINS "https:":U THEN
            ASSIGN cOptions = "-ssl -nohostverify":U .

        FIX-CODEPAGE (lcRequest) = "utf-8":U .

        IF CharacterType:IsNullOrEmpty (pcHttpVersion) THEN
            pcHttpVersion = "HTTP/1.0":U .

        ASSIGN lcResponse = "":U
               cResource  = NetworkHelper:ResourceFromUrl (pcUrl).

        ASSIGN lcRequest = SUBSTITUTE ("GET &1 &2":U,
                                       cResource,
                                       pcHttpVersion) + {&HTTP-NEWLINE} .

        DO i = 1 TO THIS-OBJECT:RequestHeaders:Count:
            ASSIGN cKey      = ENTRY (i, THIS-OBJECT:RequestHeaders:Keys, THIS-OBJECT:RequestHeaders:KeyDelimiter)
                   lcRequest = lcRequest + SUBSTITUTE ("&1: &2":U,
                                                       cKey,
                                                       THIS-OBJECT:RequestHeaders:GetValue (cKey)) + {&HTTP-NEWLINE} .
        END.

        ASSIGN lcRequest = lcRequest + {&HTTP-NEWLINE}
               cConnectString = SUBSTITUTE ("&3 -H &1 -S &2":U,
                                            pcHostName,
                                            piPort,
                                            cOptions).

        SET-SIZE (mRequest) = LENGTH (lcRequest, "RAW":U) + 1.

        PUT-STRING (mRequest, 1) = lcRequest .

        CREATE SOCKET hSocket .

        hSocket:SET-READ-RESPONSE-PROCEDURE ("ReadResponse":U, hReadResponseProcedure) .

        LogManager:WriteMessage ("Performing Socket Connection using: ":U + cConnectString, "HttpClient":U) .

        hSocket:CONNECT (cConnectString) .

        LogManager:WriteMessage ("Sending request: ":U + TRIM (STRING (SUBSTRING (lcRequest, 1, 200, "CHARACTER":U)), {&HTTP-NEWLINE} ), "HttpClient":U) .

        hSocket:WRITE (mRequest, 1, LENGTH (lcRequest, "RAW":U)) .

        ASSIGN lActive = TRUE .

        DO WHILE lActive:
            THIS-OBJECT:WaitForReadResponse (hSocket) .

            LogManager:WriteMessage ("Post WAIT-FOR READ-RESPONSE, Active: ":U + STRING (lActive), "HttpClient":U) .
        END.

        THIS-OBJECT:HttpResult = THIS-OBJECT:ProcessResponseHeaders (INPUT-OUTPUT lcResponse) .

        THIS-OBJECT:SetNumericHttpResultCode () .

        oPair = THIS-OBJECT:ResponseHeaders:FindByName ("Transfer-Encoding":U) .

        IF VALID-OBJECT (oPair) AND oPair:Value = "chunked":U THEN
            THIS-OBJECT:ProcessResponseChunks (INPUT-OUTPUT lcResponse) .

        RETURN lcResponse .

        FINALLY:
            hSocket:DISCONNECT () NO-ERROR .

            SET-SIZE (mRequest) = 0 .

            GarbageCollectorHelper:DeleteObject (hSocket) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a POST http request using default port settings
        Notes:
        @param pcHostName The hostname to request the resource form
        @param pcUrl The URL to request
        @param plcPostData The LONGCHAR data to be posted
        @param pcContentType The content-type, defaults to application/json
        @return The LONGCHAR response
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR PostLongchar (pcHostName    AS CHARACTER,
                                         pcUrl         AS CHARACTER,
                                         pcContentType AS CHARACTER,
                                         plcPostData   AS LONGCHAR):

        RETURN THIS-OBJECT:PostLongChar (pcHostName,
                                         ?,
                                         pcUrl,
                                         "":U,
                                         pcContentType,
                                         plcPostData) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a POST http request
        Notes:
        @param pcHostName The hostname to request the resource form
        @param piPort The portnumber to request from
        @param pcUrl The URL to request
        @param plcPostData The LONGCHAR data to be posted
        @param pcContentType The content-type, defaults to application/json
        @return The LONGCHAR response
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR PostLongchar (pcHostName    AS CHARACTER,
                                         piPort        AS INTEGER,
                                         pcUrl         AS CHARACTER,
                                         pcContentType AS CHARACTER,
                                         plcPostData   AS LONGCHAR):

        RETURN THIS-OBJECT:PostLongChar (pcHostName,
                                         piPort,
                                         pcUrl,
                                         "":U,
                                         pcContentType,
                                         plcPostData) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:  Performs a POST http request
        Notes:
        @param pcHostName The hostname to request the resource form
        @param piPort The portnumber to request from
        @param pcUrl The URL to request
        @param pcHttpVersion The http version to support (HTTP/1.0, HTTP/1.1)
        @param pcContentType The content-type, defaults to application/json
        @param plcPostData The LONGCHAR data to be posted
        @return The LONGCHAR response
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR PostLongChar (pcHostName    AS CHARACTER,
                                         piPort        AS INTEGER,
                                         pcUrl         AS CHARACTER,
                                         pcHttpVersion AS CHARACTER,
                                         pcContentType AS CHARACTER,
                                         plcPostData   AS LONGCHAR):

        DEFINE VARIABLE lcRequest      AS LONGCHAR      NO-UNDO .
        DEFINE VARIABLE i              AS INTEGER       NO-UNDO .
        DEFINE VARIABLE cKey           AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE mRequest       AS MEMPTR        NO-UNDO .
        DEFINE VARIABLE cOptions       AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE oPair          AS NameValuePair NO-UNDO .
        DEFINE VARIABLE cConnectString AS CHARACTER     NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 04.01.2013
           Default Ports */
        IF IntegerType:IsNullOrEmpty (piPort) THEN
        DO:
            IF pcUrl BEGINS "https:":U THEN
                piPort = 443 .
            ELSE
                piPort = 80 .
        END.

        IF pcUrl BEGINS "https:":U THEN
            ASSIGN cOptions = "-ssl -nohostverify":U .

        IF CharacterType:IsNullOrEmpty (pcContentType) THEN
            pcContentType = "application/json":U .

        FIX-CODEPAGE (lcRequest) = "utf-8":U .

        IF CharacterType:IsNullOrEmpty (pcHttpVersion) THEN
            pcHttpVersion = "HTTP/1.0":U .

        lcResponse = "":U .

        ASSIGN lcRequest = SUBSTITUTE ("POST &1 &2":U,
                                       pcUrl,
                                       pcHttpVersion) + {&HTTP-NEWLINE} .

        THIS-OBJECT:RequestHeaders:Add ("Content-Type":U, pcContentType).
        THIS-OBJECT:RequestHeaders:Add ("Content-Length":U, STRING(LENGTH(plcPostData, "RAW":U))) .

        DO i = 1 TO THIS-OBJECT:RequestHeaders:Count:
            ASSIGN cKey      = ENTRY (i, THIS-OBJECT:RequestHeaders:KEYS, THIS-OBJECT:RequestHeaders:KeyDelimiter)
                   lcRequest = lcRequest + SUBSTITUTE ("&1: &2":U,
                                                       cKey,
                                                       THIS-OBJECT:RequestHeaders:GetValue (cKey)) + {&HTTP-NEWLINE} .
        END.

        ASSIGN lcRequest      = lcRequest + {&HTTP-NEWLINE} + plcPostData + {&HTTP-NEWLINE}
               cConnectString = SUBSTITUTE ("&3 -H &1 -S &2":U,
                                            pcHostName,
                                            piPort,
                                            cOptions).

        SET-SIZE (mRequest) = LENGTH (lcRequest, "RAW":U) + 1.

        PUT-STRING (mRequest, 1) = lcRequest .

        CREATE SOCKET hSocket .

        hSocket:SET-READ-RESPONSE-PROCEDURE ("ReadResponse":U, hReadResponseProcedure) .

        LogManager:WriteMessage ("Performing Socket Connection using: ":U + cConnectString, "HttpClient":U) .

        hSocket:CONNECT (cConnectString) .

        LogManager:WriteMessage ("Sending request: ":U + TRIM (STRING (SUBSTRING (lcRequest, 1, 200, "CHARACTER":U)), {&HTTP-NEWLINE} ), "HttpClient":U) .

        hSocket:WRITE (mRequest, 1, LENGTH (lcRequest, "CHARACTER":U)) .

        ASSIGN lActive = TRUE .

        DO WHILE lActive:
            THIS-OBJECT:WaitForReadResponse (hSocket) .

            LogManager:WriteMessage ("Post WAIT-FOR READ-RESPONSE, Active: ":U + STRING (lActive), "HttpClient":U) .
        END.

        THIS-OBJECT:HttpResult = THIS-OBJECT:ProcessResponseHeaders (INPUT-OUTPUT lcResponse) .

        THIS-OBJECT:SetNumericHttpResultCode () .

        oPair = THIS-OBJECT:ResponseHeaders:FindByName ("Transfer-Encoding":U) .

        IF VALID-OBJECT (oPair) AND oPair:Value = "chunked":U THEN
            THIS-OBJECT:ProcessResponseChunks (INPUT-OUTPUT lcResponse) .

        RETURN lcResponse .

        FINALLY:
            hSocket:DISCONNECT () NO-ERROR .

            SET-SIZE (mRequest) = 0 .

            GarbageCollectorHelper:DeleteObject (hSocket) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Processes the payload of a Chunked Transfer Encoding response
        Notes:
        @param plcResponse INPUT-OUTPUT The response data where the chunks should be processed
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessResponseChunks (INPUT-OUTPUT plcResponse AS LONGCHAR):

        DEFINE VARIABLE lcChunkHeader AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cLength       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iLength       AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iIndex        AS INTEGER   NO-UNDO .
        DEFINE VARIABLE lcCleaned     AS LONGCHAR  NO-UNDO .

        FIX-CODEPAGE (lcCleaned) = "utf-8":U .

        REPEAT ON ERROR UNDO, THROW:

            iIndex = INDEX (plcResponse, {&HTTP-NEWLINE}) .

            IF iIndex > 0 THEN .
            ELSE DO:
                ASSIGN plcResponse = lcCleaned .
                RETURN .
            END.

            ASSIGN lcChunkHeader = SUBSTRING (plcResponse, 1, iIndex - 1)
                   plcResponse   = SUBSTRING (plcResponse, iIndex + LENGTH ({&HTTP-NEWLINE})).

            ASSIGN cLength = ENTRY (1, lcChunkHeader, ";":U)
                   iLength = MathHelper:Hex2Int (cLength) .

            ASSIGN lcCleaned   = lcCleaned + SUBSTRING (plcResponse,
                                                        1,
                                                        iLength)
                   plcResponse =  SUBSTRING (plcResponse, iLength + LENGTH ({&HTTP-NEWLINE}) + 1).
        END.

        ASSIGN plcResponse = lcCleaned .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes the Reponse Headers into the ReponseHeaderDictionary
        Notes:
        @param plcResponse INPUT-OUTPUT Receives the complete http response and returns it without the headers
        @return The HTTP status code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER ProcessResponseHeaders (INPUT-OUTPUT plcResponse AS LONGCHAR):

        DEFINE VARIABLE cHeaderLine  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cStatus      AS CHARACTER NO-UNDO INITIAL ? .

        THIS-OBJECT:ResponseHeaders:Clear () .

        REPEAT ON ERROR UNDO, THROW:
            iIndex = INDEX (plcResponse, {&HTTP-NEWLINE}) .

            IF iIndex > 0 THEN .
            ELSE
                RETURN cStatus .

            ASSIGN cHeaderLine  = SUBSTRING (plcResponse, 1, iIndex - 1)
                   plcResponse  = SUBSTRING (plcResponse, iIndex + LENGTH ({&HTTP-NEWLINE})).

            IF cHeaderLine = "":U THEN
                RETURN cStatus .

            ASSIGN iIndex = INDEX (cHeaderLine, ":":U) .

            IF iIndex > 0 THEN
                THIS-OBJECT:ResponseHeaders:Add (SUBSTRING (cHeaderLine, 1, iIndex - 1),
                                                 TRIM (SUBSTRING (cHeaderLine, iIndex + 1))) .
            ELSE
                IF cHeaderLine BEGINS "http":U AND cStatus = ? THEN
                    ASSIGN cStatus = cHeaderLine .

        END.

        RETURN cStatus  .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Data is available on a socket or the remote end of a connection has
                 closed its socket; applies only to socket objects.
        Notes:   Invoked from the Socket READ-RESPONSE-PROCEDURE
                 The AVM invokes this procedure whenever it detects that data is
                 available on the socket or that the remote end of the socket has
                 closed its end of the socket. In this procedure, the SELF handle
                 identifies the affected socket object.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ReadResponseHandler ():

        DEFINE VARIABLE mResponse AS MEMPTR   NO-UNDO .
        DEFINE VARIABLE iSize     AS INTEGER  NO-UNDO.

        IF NOT hSocket:CONNECTED () THEN DO:
            LogManager:WriteMessage ("Not connected.":U, "HttpClient":U) .

            ASSIGN lActive = FALSE .
            RETURN .
        END .

        iSize = hSocket:GET-BYTES-AVAILABLE () .

        LogManager:WriteMessage ("Received bytes: ":U + STRING (iSize), "HttpClient":U) .

        IF iSize > 0 THEN DO:
            SET-SIZE (mResponse) = 0 .
            SET-SIZE (mResponse) = iSize + 1 .

            hSocket:READ (mResponse, 1, iSize, READ-AVAILABLE) .

            lcResponse = lcResponse + GET-STRING (mResponse, 1) .
        END.
        ELSE
            ASSIGN lActive = FALSE .

        FINALLY:
            SET-SIZE (mResponse) = 0 .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a basic authorization header to the request
        Notes:
        @param pcUserName The UserName to use for basic authorization
        @param pcPassword The password to use for basic authorization
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetBasicAuthorization (pcUserName AS CHARACTER,
                                              pcPassword AS CHARACTER):

        DEFINE VARIABLE cBase64        AS LONGCHAR NO-UNDO.
        DEFINE VARIABLE mAuthorization AS MEMPTR   NO-UNDO .

        ASSIGN cBase64 = SUBSTITUTE ("&1:&2":U, pcUserName, pcPassword) .

        SET-SIZE (mAuthorization) = LENGTH (cBase64) .

        PUT-STRING (mAuthorization, 1, LENGTH (cBase64)) = cBase64 .

        ASSIGN cBase64 = BASE64-ENCODE (mAuthorization) .

        THIS-OBJECT:RequestHeaders:Add ("Authorization":U, SUBSTITUTE ("Basic &1":U, cBase64)) .

        FINALLY:
            SET-SIZE (mAuthorization) = 0 .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the numeric HttpResultCode property based on the HttpResult
                 property
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetNumericHttpResultCode ():

        IF NUM-ENTRIES (THIS-OBJECT:HttpResult, " ":U) > 1 THEN
            THIS-OBJECT:HttpResultCode = INTEGER (ENTRY (2, THIS-OBJECT:HttpResult, " ":U)) NO-ERROR .
        ELSE
            THIS-OBJECT:HttpResultCode = ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the WAIT-FOR READ-RESPONSE of the Socket handle
        Notes:   Seperate method, to avoid need for compilation with -IOEverywhere 1
                 On 10.2B execution will required -IOEverywhere 1
        @param phSocket The handle of the socket
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID WaitForReadResponse (phSocket AS HANDLE):

        WAIT-FOR "READ-RESPONSE":U OF hSocket .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the HttpClient class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC HttpClient ():

        GarbageCollectorHelper:DeleteObject (hReadResponseProcedure) .

    END DESTRUCTOR .

END CLASS.
