/* fg/fgpstmn.p  FG post main program - call fg/fgpstall.w 
                 fg/fgpstall.w is called from fgpstmn.p and r-fgeom.w */
                 
/* DEF NEW SHARED VAR choice AS LOG NO-UNDO. */

/* RUN fg/fgpstall.w (?,""). */

/* rstark 12.1.2016 - corrected to enable being launched from mainmenu.w */

DEFINE VARIABLE ip-post-eom-date AS DATE      NO-UNDO INITIAL ?.
DEFINE VARIABLE ip-run-what      AS CHARACTER NO-UNDO.

{fg/fgpstall.w NEW}
