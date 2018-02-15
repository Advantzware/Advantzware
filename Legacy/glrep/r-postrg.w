/* glrep/r-postrg.w */

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
   {sys/inc/runAOAVer.i "GR6" }
END.

cAOAFile = SEARCH("AOA/r-postrg.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-postrg.p.
ELSE RUN glrep/r-postrgN.w PERSISTENT.
