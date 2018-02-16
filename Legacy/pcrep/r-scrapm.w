/* pcrep/r-scrapm.w */

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
   {sys/inc/runAOAVer.i "DR12" }
END.

cAOAFile = SEARCH("AOA/r-scrapm.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-scrapm.p.
ELSE RUN pcrep/r-scrapmN.w PERSISTENT.
