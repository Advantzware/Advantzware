/* setJasper.p */

DEFINE BUFFER bUserPrint FOR user-print.

FOR EACH user-print NO-LOCK
    WHERE user-print.user-id   EQ "nosweat"
      AND user-print.batch     EQ ""
      AND user-print.batch-seq EQ 0
      AND user-print.prgmName  EQ "Jasper"
    :
    CREATE bUserPrint.
    BUFFER-COPY user-print TO bUserPrint
        ASSIGN bUserPrint.user-id = "_default".
END.
