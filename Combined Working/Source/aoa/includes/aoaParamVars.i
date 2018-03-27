/* aoaParamVars.i */

DEFINE VARIABLE hContainer AS HANDLE NO-UNDO.

&IF DEFINED(useCustList) NE 0 &THEN
{sys/ref/CustList.i NEW}
&ENDIF
