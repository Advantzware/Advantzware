/* fgrep/r-valbsc.w */

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
   {sys/inc/runAOAVer.i "IL13" }
END.

cAOAFile = SEARCH("AOA/r-valbsc.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-valbsc.p.
ELSE RUN fgrep/r-valbscN.w PERSISTENT.
