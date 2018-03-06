/* fg\r-phce&p.w */

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
   {sys/inc/runAOAVer.i "IC3" }
END.

cAOAFile = SEARCH("AOA/r-phce&p.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-phce&p.p.
ELSE RUN fg\r-phce&pN.w PERSISTENT.
