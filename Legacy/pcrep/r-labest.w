/* pcrep/r-labest.w */

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
   {sys/inc/runAOAVer.i "DR7" }
END.

cAOAFile = SEARCH("AOA/r-labest.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-labest.p.
ELSE RUN pcrep/r-labestN.w PERSISTENT.
