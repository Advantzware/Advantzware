/* pcrep/r-maceff.w */

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
   {sys/inc/runAOAVer.i "DR1" }
END.

cAOAFile = SEARCH("AOA/r-maceff.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-maceff.p.
ELSE RUN pcrep/r-maceffN.w PERSISTENT.
