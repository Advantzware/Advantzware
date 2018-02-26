/* fgrep/r-svsqty.w */

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
   {sys/inc/runAOAVer.i "IL4" }
END.

cAOAFile = SEARCH("AOA/r-svsqty.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-svsqty.p.
ELSE RUN fgrep/r-svsqtyN.w PERSISTENT.
