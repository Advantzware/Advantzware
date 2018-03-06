/* pcrep/r-macmsf.w */

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
   {sys/inc/runAOAVer.i "JL4" }
END.

cAOAFile = SEARCH("AOA/r-macmsf.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-macmsf.p.
ELSE RUN pcrep/r-macmsfN.w PERSISTENT.
