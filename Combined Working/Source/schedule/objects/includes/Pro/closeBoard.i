/* closeBoard.i */

  DEFINE VARIABLE lvLoginID AS CHARACTER NO-UNDO.

  MESSAGE 'Save Board Changes before Closing?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO-CANCEL UPDATE closeBoard.
  IF closeBoard THEN RUN 'saveBoard' IN h_board.
  ELSE IF closeBoard EQ ? THEN RETURN NO-APPLY.
  RUN getLoginID IN h_board (OUTPUT lvLoginID).
  OS-DELETE VALUE(SEARCH('{&data}/' + ID + '/inUse.' + lvLoginID + '.dat')).
