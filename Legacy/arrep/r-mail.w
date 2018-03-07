/* arrep/r-mail.w */

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
   {sys/inc/runAOAVer.i "AR7" }
END.

cAOAFile = SEARCH("AOA/r-mail.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-mail.p.
ELSE RUN arrep/r-mailN.w PERSISTENT.
