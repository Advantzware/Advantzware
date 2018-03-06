/* arrep/r-cust.w */

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
   {sys/inc/runAOAVer.i "AR1" }
END.

cAOAFile = SEARCH("AOA/r-cust.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-cust.p.
ELSE RUN arrep/r-custN.w PERSISTENT.
