/* qLocalInit.i - used in query local-initialize procedure */

  DEFINE VARIABLE charHandle AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
  
  IF VALID-HANDLE(adm-broker-hdl) THEN
  DO:
    RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'CONTAINER':U,OUTPUT charHandle).
    phandle = WIDGET-HANDLE(charHandle).
    IF VALID-HANDLE(phandle) THEN
    RUN rowIDs IN phandle ({&rowIDsOutput}).
  END.
  RUN adm-open-query.
