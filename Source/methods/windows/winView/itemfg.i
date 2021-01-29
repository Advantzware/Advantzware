/* itemfg.i - window local-view - rstark 9.30.2018 */

//{methods/winReSizeMax.i}

DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

&IF '{&h_folderUom}' NE '' &THEN
RUN sys/ref/nk1look.p (g_company, "FGItemUOM", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
IF lFound AND cReturn EQ "NO" THEN
RUN disable-folder-page IN h_folder (12) .
&ENDIF