/* jcrep/r-backlo.w */

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
   {sys/inc/runAOAVer.i "JL2" }
END.

cAOAFile = SEARCH("AOA/r-backlo.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-backlo.p.
ELSE RUN jcrep/r-backloN.w PERSISTENT.
