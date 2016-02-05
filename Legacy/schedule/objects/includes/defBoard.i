/* defBoard.i - used in AppBuilder, if source for Pro Code missing, change
                Board value below to Basic or View */

&IF DEFINED(Board) EQ 0 &THEN
&GLOBAL-DEFINE Board Pro
&ENDIF

DEFINE VARIABLE commaList AS CHARACTER NO-UNDO.
DEFINE VARIABLE continue AS LOGICAL NO-UNDO.

{{&includes}/findProgram.i}
