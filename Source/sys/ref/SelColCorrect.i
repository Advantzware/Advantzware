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
DEFINE VARIABLE cFieldList AS CHARACTER NO-UNDO.

cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).

      IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN DO:
          IF cFieldList eq "" THEN
	     cFieldList = cFieldList + string(sl_selected:ENTRY(iCount))  .  
	  ELSE
	     cFieldList = cFieldList + "," + string(sl_selected:ENTRY(iCount))  . 	         
      END.
  END.
  DO iCount = 1 TO NUM-ENTRIES(cTmpList):             
      IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN DO:          
           ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
      END.
  END.
IF cFieldList NE "" THEN DO:
     MESSAGE "Columns you have previously selected for inclusion have changed."  
             "Please remove the following columns from your selection list and re-select the new columns:"  SKIP  cFieldList
              VIEW-AS ALERT-BOX INFORMATION.
END.
