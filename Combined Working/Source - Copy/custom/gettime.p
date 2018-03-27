/*gettime.p  get time resources */

{methods/defines/globdefs.i}

IF CONNECTED("asi") THEN DO:
   {sys/inc/tstime.i}
END.

SESSION:TIME-SOURCE = IF connected("asi") and
                         tstime-cha = "Workstation" THEN "local"
                      ELSE "ASI".


