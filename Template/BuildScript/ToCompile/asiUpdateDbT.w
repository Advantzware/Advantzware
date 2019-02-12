&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: asiUpdateDBt.w
  Description: trasition program - only used in update to 16.8.6 or higher 
  Input Parameters:  <none>
  Output Parameters: <none>
  Author: MYT
  Created: 10/1/2017 and highly modified/adapted over next several months
  Change History:
    12/19/2017 - MYT -  updated to handle 16.6.9 data fixes
                        added Clean Before Install to suppress deletion
                        of existing programs/resources directories prior
                        to install of new (lets customers stay live)
    02/10/2019 - MYT -  removed all except return success                        
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ipcName AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcPort AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcDir AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcFromVer AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcToVer AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiLevel AS INT NO-UNDO.

DEF OUTPUT PARAMETER oplSuccess AS LOG NO-UNDO.

ASSIGN 
    oplSuccess = TRUE.
    
RETURN.


/* ************************  Function Implementations ***************** */

