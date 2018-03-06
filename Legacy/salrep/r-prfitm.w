/* salrep/r-prfitm.w */

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
   {sys/inc/runAOAVer.i "HV" }
END.

cAOAFile = SEARCH("AOA/r-prfitm.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-prfitm.p.
ELSE RUN salrep/r-prfitmN.w PERSISTENT.
