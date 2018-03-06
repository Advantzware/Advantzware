/* rmrep/r-rmcost.w */

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
   {sys/inc/runAOAVer.i "MR1" }
END.

cAOAFile = SEARCH("AOA/r-rmcost.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-rmcost.p.
ELSE RUN rmrep/r-rmcostN.w PERSISTENT.
