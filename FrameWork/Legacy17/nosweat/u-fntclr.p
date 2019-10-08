/* u-fntclr.p */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

RUN Get_Procedure IN Persistent-Handle ("enhance.",OUTPUT run-proc,no).
IF run-proc NE "" THEN
{methods/smartrun.i "('')"}

 
