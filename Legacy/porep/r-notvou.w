/* porep/r-notvou.w */

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
   {sys/inc/runAOAVer.i "PR7" }
END.

cAOAFile = SEARCH("AOA/r-notvou.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-notvou.p.
ELSE RUN porep/r-notvouN.w PERSISTENT.
