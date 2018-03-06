/* rmrep/r-consum.w */

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
   {sys/inc/runAOAVer.i "MR10" }
END.

cAOAFile = SEARCH("AOA/r-consum.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-consum.p.
ELSE RUN rmrep/r-consumN.w PERSISTENT.
