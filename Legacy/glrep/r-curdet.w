/* glrep/r-curdet.w */

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
   {sys/inc/runAOAVer.i "GR2" }
END.

cAOAFile = SEARCH("AOA/r-curdet.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-curdet.p.
ELSE RUN glrep/r-curdetN.w PERSISTENT.
