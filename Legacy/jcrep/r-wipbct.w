/* jcrep/r-wipbct.w */

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
   {sys/inc/runAOAVer.i "JR12" }
END.

cAOAFile = SEARCH("AOA/r-wipbct.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-wipbct.p.
ELSE RUN jcrep/r-wipbctN.w PERSISTENT.
