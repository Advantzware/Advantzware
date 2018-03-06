/* salrep/r-lastvs.w */

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
   {sys/inc/runAOAVer.i "HL" }
END.

cAOAFile = SEARCH("AOA/r-lastvs.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-lastvs.p.
ELSE RUN salrep/r-lastvsN.w PERSISTENT.
