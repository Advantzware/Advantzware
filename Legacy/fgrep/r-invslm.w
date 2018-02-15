/* fgrep/r-shpcpn.w */

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
   {sys/inc/runAOAVer.i "IR5" }
END.

cAOAFile = SEARCH("AOA/r-invslm.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-invslm.p.
ELSE RUN fgrep/r-invslN.w PERSISTENT.
