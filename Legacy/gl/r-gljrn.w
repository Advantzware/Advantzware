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
   {sys/inc/runAOAVer.i "GR9" }
END.

cAOAFile = SEARCH("AOA/r-gljrn.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-gljrn.p.
ELSE RUN gl/r-gljrnN.w PERSISTENT (ip-post).
