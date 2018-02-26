/* salrep/r-hsaldt.w */

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
   {sys/inc/runAOAVer.i "HR2" }
END.

cAOAFile = SEARCH("AOA/r-hsaldt.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-hsaldt.p.
ELSE RUN salrep/r-hsaldtN.w PERSISTENT.
