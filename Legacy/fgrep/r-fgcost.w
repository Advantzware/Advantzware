/* fgrep/r-fgcost.w */

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
   {sys/inc/runAOAVer.i "IR14" }
END.

cAOAFile = SEARCH("AOA/r-fgcost.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-fgcost.p.
ELSE RUN fgrep/r-fgcosN.w PERSISTENT.
