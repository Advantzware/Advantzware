/* disable.i */

&IF "{&F1}" NE "" &THEN
DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.


ASSIGN
  current-widget = FRAME {&FRAME-NAME}:HANDLE
  current-widget = current-widget:FIRST-CHILD
  current-widget = current-widget:FIRST-CHILD.
DO WHILE current-widget NE ?:
  IF current-widget:NAME NE ? AND
     INDEX("{&F1}",current-widget:NAME) NE 0 THEN
/*   current-widget:HIDDEN = yes. */
  current-widget:SENSITIVE = NO.
  current-widget = current-widget:NEXT-SIBLING.
END.
&ENDIF
{methods/run_link.i "RECORD-SOURCE" "Enable-Navigation"}
