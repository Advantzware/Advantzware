/* listtype.i */

PROCEDURE Get-{&VALUTYPE}-Values:
  RUN Find-{&FINDTYPE}-Record.
  IF RETURN-VALUE = "EMPTY" THEN
  RETURN "EMPTY".
  RUN Set-{&VALUTYPE}-Values.
END PROCEDURE.

PROCEDURE Find-{&FINDTYPE}-Record:
  &IF "{&FIRST-EXTERNAL-TABLE}" NE "" &THEN
  CASE list-order:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
    {methods/listobjs/findindx.i 1}
    {methods/listobjs/findindx.i 2}
    {methods/listobjs/findindx.i 3}
    {methods/listobjs/findindx.i 4}
    {methods/listobjs/findindx.i 5}
  END CASE.
  &ENDIF
END PROCEDURE.

PROCEDURE Set-{&VALUTYPE}-Values:
  &IF "{&{&VALUTYPE}VALUES}" NE "" &THEN
  ASSIGN {&{&VALUTYPE}VALUES}.
  &ENDIF
END PROCEDURE.
