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
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-trnjou.p.
ELSE RUN glrep/r-trnjoN.w PERSISTENT.
