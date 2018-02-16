/* fgrep/r-cusinv.w */

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
   {sys/inc/runAOAVer.i "IL12" }
END.

cAOAFile = SEARCH("AOA/r-cusinv.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-cusinv.p.
ELSE RUN fgrep/r-cusinvN.w PERSISTENT.
