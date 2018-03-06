/* ar/r-mishst.w */

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
   {sys/inc/runAOAVer.i "AL2" }
END.

cAOAFile = SEARCH("AOA/r-mishst.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-mishst.p.
ELSE RUN ar/r-mishstN.w PERSISTENT.
