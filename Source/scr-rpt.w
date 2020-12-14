/*------------------------------------------------------------------------

  File: scr-rpt.w

  Description: Screen Viewer

  Input Parameters: List Name, Title, Font and Orientation

  Output Parameters: <none>

  Author: Ron Stark

  Created: 9.25.2020 (re-created from scr-rpt.w dialog version)

------------------------------------------------------------------------*/

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcListName    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTitle       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiFont        AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcOrientation AS CHARACTER NO-UNDO.

RUN screenViewer.w PERSISTENT (ipcListName, ipcTitle, ipiFont, ipcOrientation).
