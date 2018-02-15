/* aprep/r-cashrq.w */

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
   {sys/inc/runAOAVer.i "VR2" }
END.

cAOAFile = SEARCH("AOA/r-cashrq.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-cashrq.p.
ELSE RUN aprep/r-cashqN.w PERSISTENT.
