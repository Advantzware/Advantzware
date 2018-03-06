/* jcrep/r-clsjob.w */

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
   {sys/inc/runAOAVer.i "JR5" }
END.

cAOAFile = SEARCH("AOA/r-clsjob.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-clsjob.p.
ELSE RUN jcrep/r-clsjobN.w PERSISTENT.
