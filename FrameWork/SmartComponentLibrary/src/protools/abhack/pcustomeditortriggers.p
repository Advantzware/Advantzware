/*------------------------------------------------------------------------------
         pcustomeditortriggers.p   02-NOV-2009  by formation   
  Purpose: Sample hook to implement your own triggers for editors aside abhack
  Parameters:  <none>
  Notes:    
  1)  will rely on a subscribe anywhere to kill this procedure
  2) there is only one instance at a time of this procedure.  ABHack will RUN this procedure
   to redefine the triggers each time it find out you are in a different source editor.
------------------------------------------------------------------------------*/


DEFINE INPUT  PARAMETER phEditor     AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER phABHackProc AS HANDLE      NO-UNDO.

SUBSCRIBE TO "abhackKillPcustomeditortriggers" ANYWHERE.

PROCEDURE abhackKillPcustomeditortriggers:    
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* I let this one as example */
ON 'F7' OF phEditor DO:
    MESSAGE  "In" THIS-PROCEDURE:FILE-NAME SKIP
     "caught F7 event in editor " phEditor
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

