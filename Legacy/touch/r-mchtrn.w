/* touch/r-mchtrn.w */

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
  cocode = gcompany
  locode = gloc
  .
DO TRANSACTION:
   {sys/inc/runAOAVer.i "TR3" }
END.

cAOAFile = SEARCH("AOA/r-mchtrn.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-mchtrn.p.
ELSE RUN touch/r-mchtrnA.w PERSISTENT.
