/* t-fontd.p  */

{custom/xprint.i}

DEF VAR m AS MEMPTR NO-UNDO.
DEF VAR dialogtype AS INT NO-UNDO.
DEF VAR returncode AS INT NO-UNDO.
DEF VAR L AS CHAR NO-UNDO.

dialogType = 3.   /* 1 : screen fonts, 2:print fonts, other: all) */
SET-SIZE(M) = 512.
RUN fontdialog(dialogtype,m,OUTPUT returncode).

l = GET-STRING(m,1).
SET-SIZE(m) = 0.

