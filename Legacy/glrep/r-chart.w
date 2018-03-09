/* glrep/r-chart.w */

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
   {sys/inc/runAOAVer.i "GR1" }
END.

cAOAFile = SEARCH("AOA/r-chart.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-chart.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN glrep/r-chartN.w PERSISTENT.
ELSE RUN glrep/r-chartA.w PERSISTENT.
