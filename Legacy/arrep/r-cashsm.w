/* arrep/r-cashsm.w */

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
   {sys/inc/runAOAVer.i "AR9" }
END.

cAOAFile = SEARCH("AOA/r-cashsm.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-cashsm.p.
ELSE RUN arrep/r-cashsmN.w PERSISTENT.
