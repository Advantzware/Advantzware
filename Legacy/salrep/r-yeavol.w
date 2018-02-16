/* salrep/r-yeavol.w */

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
   {sys/inc/runAOAVer.i "HY" }
END.

cAOAFile = SEARCH("AOA/r-yeavol.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-yeavol.p.
ELSE RUN salrep/r-yeavolN.w PERSISTENT.
