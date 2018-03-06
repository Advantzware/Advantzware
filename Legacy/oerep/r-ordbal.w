/* oerep/r-ordbal.w */

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
   {sys/inc/runAOAVer.i "OR8" }
END.

cAOAFile = SEARCH("AOA/r-ordbal.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-ordbal.p.
ELSE RUN oerep/r-ordbaN.w PERSISTENT.
