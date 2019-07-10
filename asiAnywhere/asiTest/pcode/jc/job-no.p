
DEF INPUT-OUTPUT PARAM io-job-no  LIKE job.job-no  NO-UNDO.
DEF INPUT-OUTPUT PARAM io-job-no2 LIKE job.job-no2 NO-UNDO.
DEF INPUT PARAM ip-prod-cat AS CHAR NO-UNDO .

{sys/inc/var.i SHARED}
{custom/globdefs.i}
{oe/oe-sysct1.i NEW}


RUN oe/oe-sysct.p.


IF v-job-meth EQ "YYMMSEQ#" THEN DO:
  io-job-no = SUBSTR(STRING(YEAR(TODAY),"9999"),3,2) +
                     STRING(MONTH(TODAY),"99").

  FIND LAST job
      WHERE job.company EQ cocode
        AND job.job-no  BEGINS io-job-no
      USE-INDEX job-no NO-LOCK NO-ERROR.
  io-job-no2 = IF AVAIL job THEN INT(SUBSTR(TRIM(job.job-no),5,2)) ELSE 0 NO-ERROR.

  IF ERROR-STATUS:ERROR THEN io-job-no2 = 0.

  ASSIGN
   io-job-no  = io-job-no + STRING(io-job-no2 + 1,"99")
   io-job-no2 = 0.
END.
/* ELSE IF v-job-meth NE "Order#" THEN io-job-no = "".  */

ELSE IF v-job-meth EQ "Order#" THEN DO:  END.
ELSE IF v-job-meth = "PLine&Order#" AND
     LENGTH(trim(io-job-no)) < 6 THEN 
     DO: 
        FIND FIRST prodl WHERE prodl.procat = ip-prod-cat NO-LOCK NO-ERROR.
        IF AVAIL prodl THEN 
           io-job-no =  FILL(" ",6 - LENGTH( SUBSTRING(prodl.prolin,1,1) + trim(io-job-no)))
                     + SUBSTRING(prodl.prolin,1,1) + trim(io-job-no) .
        
     END.

IF io-job-no NE "" THEN
DO WHILE CAN-FIND(FIRST job
                  WHERE job.company EQ cocode
                    AND job.job-no  EQ io-job-no
                    AND job.job-no2 EQ io-job-no2)    OR
         CAN-FIND(FIRST oe-ord
                  WHERE oe-ord.company EQ cocode
                    AND oe-ord.job-no  EQ io-job-no
                    AND oe-ord.job-no2 EQ io-job-no2) OR
         CAN-FIND(FIRST oe-ordl
                  WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.job-no  EQ io-job-no
                    AND oe-ordl.job-no2 EQ io-job-no2):
  io-job-no2 = io-job-no2 + 1.
END.
