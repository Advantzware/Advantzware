/* oe/r-inve&p.w */

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
   {sys/inc/runAOAVer.i "OB4"}
END.

cAOAFile = SEARCH("AOA/r-inve&p.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-inve&p.p.
ELSE RUN oe/r-inve&pN.w PERSISTENT.
