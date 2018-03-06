/* fgrep/r-ageinv.w */

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
   {sys/inc/runAOAVer.i "IR12" }
END.

cAOAFile = SEARCH("AOA/r-ageinv.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-ageinv.p.
ELSE RUN fgrep/r-ageinN.w PERSISTENT.
