/* inv-hea_.w */

{oerep/r-invprt.w}

RUN refreshCallingBrowse.

PROCEDURE refreshCallingBrowse:
  DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.

  phandle = SESSION:FIRST-PROCEDURE.
  DO WHILE VALID-HANDLE(phandle):
    IF INDEX(phandle:FILE-NAME,'w-oeinv.') NE 0 THEN DO:
      RUN refreshBrowse IN phandle.
      RETURN.
    END.
    phandle = phandle:NEXT-SIBLING.
  END. /* do while */
END PROCEDURE.
