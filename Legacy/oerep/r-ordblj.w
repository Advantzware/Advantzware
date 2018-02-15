/* oerep/r-ordblj.w */

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
   {sys/inc/runAOAVer.i "OZ9" }
END.

cAOAFile = SEARCH("AOA/r-ordblj.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-ordblj.p.
ELSE RUN oerep/r-ordbjN.w PERSISTENT.
