/* arrep/r-cstshp.w */

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
   {sys/inc/runAOAVer.i "AR13" }
END.

cAOAFile = SEARCH("AOA/r-cstshp.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-cstshp.p.
ELSE RUN arrep/r-cstshpN.w PERSISTENT.
