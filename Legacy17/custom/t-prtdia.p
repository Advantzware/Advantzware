{custom/xprint.i}

/* = PrinterDialog = */

DEF VAR v-prtm AS MEMPTR NO-UNDO.
DEF VAR v-prt-list AS CHAR NO-UNDO.

SET-SIZE(v-prtm) = 256.
RUN printerdialog(v-prtm).

v-prt-list = GET-STRING(v-prtm,1).

SET-SIZE(v-prtm) = 0.

MESSAGE /* "Printer name: " ENTRY(1,v-prt-list) SKIP
        "Format Name : " ENTRY(2,v-prt-list) SKIP
        "Dim: " ENTRY(3,v-prt-list) "x" ENTRY(4,v-prt-list) "(tenth mm)" SKIP
        "Orientation:" ENTRY(5,v-prt-list)
        */
    v-prt-list
        VIEW-AS ALERT-BOX.
        
