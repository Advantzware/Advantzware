/* salrep/r-shpcar.w */

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
   {sys/inc/runAOAVer.i "HR7" }
END.

cAOAFile = SEARCH("AOA/r-shpcar.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-shpcar.p.
ELSE RUN salrep/r-shpcarN.w PERSISTENT.
