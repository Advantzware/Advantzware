/*gettime.p  get time resources */

{methods/defines/globdefs.i}

DEFINE VARIABLE cTSTime AS CHARACTER NO-UNDO.

IF CONNECTED("asi") THEN DO:
   RUN spGetSettingByName ("TSTime", OUTPUT cTSTime).
END.

SESSION:TIME-SOURCE = IF CONNECTED("asi") 
                      AND cTSTime EQ "Workstation" THEN "local"
                      ELSE "ASI".


