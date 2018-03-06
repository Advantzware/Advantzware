/* glrep/r-pstchk.w */

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
   {sys/inc/runAOAVer.i "GR8" }
END.

cAOAFile = SEARCH("AOA/r-pstchk.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-pstchk.p.
ELSE RUN glrep/r-pstchN.w PERSISTENT.
