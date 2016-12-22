/* fg/fgpstmn.p  FG post main program - call fg/fgpstall.w 
                 fg/fgpstall.w is called from fgpstmn.p and r-fgeom.w */
                 
DEF NEW SHARED VAR choice AS LOG NO-UNDO.

RUN fg/fgpstall.w PERSISTENT (?,"").
