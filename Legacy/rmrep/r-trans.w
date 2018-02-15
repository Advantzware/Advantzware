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
   {sys/inc/runAOAVer.i "MR5" }
END.

cAOAFile = SEARCH("AOA/r-trans.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-trans.p.
ELSE RUN rmrep/r-transN.w PERSISTENT.
