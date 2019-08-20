/*------------------------------------------------------------------------

  File: sys/ref/SelColCorrect.i

  Description: If a column is renamed that might give user an error - This file will Correct the error and
               send a message for all selectable export to excel report.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: Sewa Singh
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.


cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).

      IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN DO:
           MESSAGE "Column '" + string(sl_selected:ENTRY(iCount)) + "' has changed and now it is deleted." 
              VIEW-AS ALERT-BOX INFORMATION.

           ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
      END.
  END.
