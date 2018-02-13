/* salrep/r-slsper.w */

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
   {sys/inc/runAOAVer.i "HR15" }
END.

cAOAFile = SEARCH("AOA/r-slsper.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-slsper.p.
ELSE RUN salrep/r-slsperN.w PERSISTENT.
