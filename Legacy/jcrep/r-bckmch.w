/* jcrep/r-bckmch.w */

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
   {sys/inc/runAOAVer.i "JR10" }
END.

cAOAFile = SEARCH("AOA/r-bckmch.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-bckmch.p.
ELSE RUN jcrep/r-bckmchN.w PERSISTENT.
