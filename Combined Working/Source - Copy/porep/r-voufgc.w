/* porep/r-voufgc.w */

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
   {sys/inc/runAOAVer.i "PR11" }
END.

cAOAFile = SEARCH("AOA/r-voufgc.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-voufgc.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN porep/r-voufgcN.w PERSISTENT.
ELSE RUN porep/r-voufgcA.w PERSISTENT.
