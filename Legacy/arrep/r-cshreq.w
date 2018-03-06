/* arrep/r-cshreq.w */

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
   {sys/inc/runAOAVer.i "AR14" }
END.

cAOAFile = SEARCH("AOA/r-cshreq.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-cshreq.p.
ELSE RUN arrep/r-cshreqN.w PERSISTENT.
