/* config.i - used in configGetPut.i */

PROCEDURE {1}{6}:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  {2} {3} VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  {4} {5}.
  {4} {{&includes}/config{6}.i}
  {4} colorPriority.
  DO i = 1 TO EXTENT(jobBGColor):
    {4} jobLabel[i] jobBGColor[i] customLabel[i] customValue[i] customBGColor[i]
        jobFGColor[i] customFGColor[i].
  END.
  {2} CLOSE.
END PROCEDURE.
