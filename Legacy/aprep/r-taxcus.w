/* aprep/r-taxcus.w */

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
   {sys/inc/runAOAVer.i "VR9" }
END.

cAOAFile = SEARCH("AOA/r-taxcus.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-taxcus.p.
ELSE RUN aprep/r-taxcusN.w PERSISTENT.
