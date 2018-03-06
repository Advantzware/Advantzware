/* glrep/r-postck.w */

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
   {sys/inc/runAOAVer.i "GR7" }
END.

cAOAFile = SEARCH("AOA/r-postck.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-postck.p.
ELSE RUN glrep/r-postckN.w PERSISTENT.
