/* oerep/r-hotsOp.w */

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
   {sys/inc/runAOAVer.i "OZ8" }
END.

cAOAFile = SEARCH("AOA/r-hotsop.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-hotsop.p.
ELSE RUN oerep/r-hotsON.w PERSISTENT.
