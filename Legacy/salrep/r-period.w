/* salrep/r-period.w */

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
   {sys/inc/runAOAVer.i "HC" }
END.

cAOAFile = SEARCH("AOA/r-period.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-period.p.
ELSE RUN salrep/r-periodN.w PERSISTENT.
