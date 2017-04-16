/* resourceList.i - used in load programs and getResource in board.w */

&IF DEFINED(resourceListDef) EQ 0 &THEN
{{&loads}/resourceListDef.i}
&ENDIF

EMPTY TEMP-TABLE resourceList.
EMPTY TEMP-TABLE priorityList.
IF ID NE '' THEN
DO:
  IF SEARCH('{&data}/' + ID + '/resourceList.dat') EQ ? THEN
  DO:
    OUTPUT TO VALUE('{&data}/' + ID + '/resourceList.dat').
    EXPORT ''.
    OUTPUT CLOSE.
  END.

  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/resourceList.dat')) NO-ECHO.
  IMPORT resourceUse.
  REPEAT:
    IMPORT lvResource.
    CREATE resourceList.
    resourceList.resource = lvResource.
  END.
  INPUT CLOSE.

  IF SEARCH('{&data}/' + ID + '/priorityList.dat') EQ ? THEN
  DO:
    OUTPUT TO VALUE('{&data}/' + ID + '/priorityList.dat').
    EXPORT ''.
    OUTPUT CLOSE.
  END.

  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/priorityList.dat')) NO-ECHO.
  REPEAT:
    IMPORT lvResource lvPriority.
    CREATE priorityList.
    ASSIGN
      priorityList.resource = lvResource
      priorityList.priority = lvPriority.
  END.
  INPUT CLOSE.
END.
