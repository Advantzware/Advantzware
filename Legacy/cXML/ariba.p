/* ariba.p */

/* if certificate is needed, run firefox with URL service-2.ariba.com
   click secure lock, go under service-2.ariba.com secure connection,
   then click more information, from the Page Info popup, select
   View Certificate, then click Details tab, click Export...
   Save it to the cXML folder.
   
   from PROENV (run as administrator), navigate to cXML folder and type
   certutil -import service-2.ariba.com.crt (which generates a hash file
   in %DLC%\certs folder)
   
   Environment Variables:
       set PSC_SSLCLIENT_PROTOCOLS=TLSv1.2
       set PSC_SSLCLIENT_CIPHERS=ECDHE-ECDSA-AES256-GCM-SHA384
*/

DEFINE INPUT  PARAMETER ipcXMLFile     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcXMLResponse AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

DEFINE VARIABLE vcWebHost   AS CHARACTER NO-UNDO INITIAL "service-2.ariba.com".
DEFINE VARIABLE vcWebPort   AS CHARACTER NO-UNDO INITIAL "443".
DEFINE VARIABLE vcWSAgent   AS CHARACTER NO-UNDO INITIAL "service-2.ariba.com".
DEFINE VARIABLE vhWebSocket AS HANDLE    NO-UNDO.

CREATE SOCKET vhWebSocket.

vhWebSocket:CONNECT('-H ' + vcWebHost + ' -S ' + vcWebPort + ' -ssl') NO-ERROR.

IF NOT vhWebSocket:CONNECTED() THEN DO:
    opcReturnValue = 'Connection Failed'.
    RETURN.
END.

vhWebSocket:SET-READ-RESPONSE-PROCEDURE('getWebServerResponse').
opcReturnValue = RETURN-VALUE.
IF opcReturnValue NE '' THEN RETURN.

RUN PostRequest (OUTPUT opcReturnValue).
IF opcReturnValue NE '' THEN RETURN.

WAIT-FOR READ-RESPONSE OF vhWebSocket.

vhWebSocket:DISCONNECT() NO-ERROR.
DELETE OBJECT vhWebSocket.

opcReturnValue = 'Successfully Transmitted'.
RETURN.

PROCEDURE getWebServerResponse:
    DEFINE VARIABLE vcWebResp AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess  AS LOGICAL.
    DEFINE VARIABLE mResponse AS MEMPTR.
  
    IF NOT vhWebSocket:CONNECTED() THEN RETURN 'Connection Dropped'.
  
    lSuccess = TRUE.
    DO WHILE lSuccess AND ERROR-STATUS:GET-MESSAGE(1) EQ '':
        SET-SIZE(mResponse) = 1.
        SET-BYTE-ORDER(mResponse) = BIG-ENDIAN.
        SELF:READ(mResponse,1,1,1) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN 'Connection Dropped:READ'.
        vcWebResp = vcWebResp + GET-STRING(mResponse,1).
        IF SELF:CONNECTED() EQ FALSE THEN LEAVE.
    END.

    OUTPUT TO VALUE(ipcXMLResponse).
    PUT UNFORMATTED vcWebResp.
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE PostRequest:
    DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

    DEFINE VARIABLE viXMLLength AS INTEGER.
    DEFINE VARIABLE vcRequest   AS CHARACTER.
    DEFINE VARIABLE viRequest   AS INTEGER.
    DEFINE VARIABLE viMsg       AS INTEGER.
    DEFINE VARIABLE mRequest    AS MEMPTR.
    DEFINE VARIABLE lSuccess    AS LOGICAL.
  
    DEFINE VARIABLE vcXMLText   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLLine    AS CHARACTER NO-UNDO.
  
  &SCOPED-DEFINE endLine + CHR(13) + CHR(10)
  
    INPUT FROM VALUE(ipcXMLFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cXMLLine.
        vcXMLText = vcXMLText + cXMLLine {&endLine}.
    END. /* repeat */
    INPUT CLOSE.
  
    ASSIGN
        viXMLLength = LENGTH(vcXMLText)
        vcRequest   = 'POST https://'
              + vcWSAgent
              + '/service/transaction/cxml.asp '
              + 'HTTP/1.1' {&endLine}
    + 'Host: ' + vcWebHost {&endLine}
        + 'User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; MS Web Services Client Protocol 1.0.3705.288)' {&endLine}
        + 'Content-type: text/xml; charset="UTF-8"' {&endLine}
        + 'Content-Length: ' + STRING(viXMLLength) {&endLine}
    {&endLine}
    viRequest = LENGTH(vcRequest)
    viMsg = viRequest + viXMLLength + 1
        .

    SET-SIZE(mRequest) = 0.
    SET-SIZE(mRequest) = viMsg.
    SET-BYTE-ORDER(mRequest) = BIG-ENDIAN.
  
    PUT-STRING(mRequest,1) = vcRequest + vcXMLText.
  
    vhWebSocket:WRITE(mRequest,1,viMsg) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        opcReturnValue = 'Connection Failure'.
END PROCEDURE.
