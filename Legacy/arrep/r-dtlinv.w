/* arrep/r-dtlinv.w */

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
   {sys/inc/runAOAVer.i "AL3" }
END.

cAOAFile = SEARCH("AOA/r-dtlinv.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-dtlinv.p.
ELSE RUN arrep/r-dtlinvN.w PERSISTENT.
