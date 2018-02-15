/* rmrep/r-inkmch.w */

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
   {sys/inc/runAOAVer.i "MR8" }
END.

cAOAFile = SEARCH("AOA/r-inkmch.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-inkmch.p.
ELSE RUN rmrep/r-inkmcN.w PERSISTENT.
