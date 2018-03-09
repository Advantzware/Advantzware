/* glrep/r-trnjou.w */

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
   {sys/inc/runAOAVer.i "GR3" }
END.

cAOAFile = SEARCH("AOA/r-trnjou.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-trnjou.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN glrep/r-trnjoN.w PERSISTENT.
ELSE RUN glrep/r-trnjoA.w PERSISTENT.
