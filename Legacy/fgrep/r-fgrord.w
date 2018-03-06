/* fgrep/r-fgrord.w */

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
   {sys/inc/runAOAVer.i "IR1" }
END.

cAOAFile = SEARCH("AOA/r-fgrord.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-fgrord.p.
ELSE RUN fgrep/r-fgrordN.w PERSISTENT.
