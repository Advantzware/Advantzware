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
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-curdet.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN glrep/r-curdetN.w PERSISTENT.
ELSE RUN glrep/r-curdetA.w PERSISTENT.
