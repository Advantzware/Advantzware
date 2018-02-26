/* jcrep/r-mchcst.w */

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
   {sys/inc/runAOAVer.i "JR7" }
END.

cAOAFile = SEARCH("AOA/r-mchcst.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-mchcst.p.
ELSE RUN jcrep/r-mchcstN.w PERSISTENT.
