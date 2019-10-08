/*File: pc/cmpltjob.p*/

DEFINE INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-mach-code AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-job-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-job-no2 AS INT NO-UNDO.
DEFINE INPUT PARAMETER ip-blank-no AS INT NO-UNDO.
DEFINE INPUT PARAMETER ip-form-no AS INT NO-UNDO.
DEFINE INPUT PARAMETER ip-pass AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER op-job-found AS LOG NO-UNDO.

op-job-found = CAN-FIND(FIRST cmpltjob WHERE
                    cmpltjob.company EQ ip-company AND
                    cmpltjob.machine EQ ip-mach-code AND
                    cmpltjob.job_number EQ ip-job-no AND
                    cmpltjob.job_sub EQ ip-job-no2 AND
                    cmpltjob.blank_number EQ ip-blank-no AND
                    cmpltjob.form_number EQ ip-form-no AND
                    cmpltjob.pass_sequence EQ ip-pass).
