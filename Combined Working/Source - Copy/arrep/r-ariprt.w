/* arrep/r-araged.w */

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
DEFINE VARIABLE vPrgmnameOverride AS CHARACTER NO-UNDO.
assign
 cocode = gcompany
 locode = gloc.

&IF DEFINED(vprgmname) = 0 &THEN 
&global-define vprgmname "r-aiprt."
&ENDIF
vPrgmnameOverride = {&vprgmname}.
RUN oerep/r-invprtoe.w  PERSISTENT (input "ar-inv", INPUT   {&vprgmname}).
