/* gl/r-gljrn.w */
DEF INPUT PARAM ip-post AS LOG NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
   {sys/inc/selrptcol.i "GR9" }
END.

IF SelectRptColumn-log THEN RUN gl/r-gljrnN.w PERSISTENT (ip-post).
ELSE RUN gl/r-gljrnA.w PERSISTENT (ip-post).