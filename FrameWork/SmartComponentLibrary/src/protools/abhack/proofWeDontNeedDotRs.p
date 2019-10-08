DEFINE VARIABLE hed    AS HANDLE   NO-UNDO.
DEFINE VARIABLE hproc  AS HANDLE   NO-UNDO.
DEFINE VARIABLE i      AS INTEGER  NO-UNDO.
DEFINE VARIABLE ietime AS INTEGER  NO-UNDO.

hed = WIDGET-HANDLE('1772'). /* put the handle of a running editor reported by abhack */

ietime = ETIME.
DO i = 1 TO 1000:
    RUN protools/abhack/procEditorList.w PERSISTENT SET hproc (hed).
    APPLY 'close' TO hproc.
END.

/* very similar result with and without .r's */                 
MESSAGE ETIME - ietime
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
