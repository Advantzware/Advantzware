/* arrep/r-araged.w */

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
   {sys/inc/runAOAVer.i "AR5" }
END.

cAOAFile = SEARCH("AOA/r-araged.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-araged.p.
ELSE RUN arrep/r-aragedN.w PERSISTENT.
