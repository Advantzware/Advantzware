/* salrep/r-cusitm.w */

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
   {sys/inc/runAOAVer.i "HZ" }
END.

cAOAFile = SEARCH("AOA/r-cusitm.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-cusitm.p.
ELSE RUN salrep/r-cusitmN.w PERSISTENT.
