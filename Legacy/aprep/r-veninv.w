/* aprep/r-veninv.w */

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
   {sys/inc/runAOAVer.i "VR12" }
END.

cAOAFile = SEARCH("AOA/r-veninv.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-veninv.p.
ELSE RUN aprep/r-veninvN.w PERSISTENT.
