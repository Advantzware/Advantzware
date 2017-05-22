/* loadBasic.p */

{schedule/scopDir.i}

&SCOPED-DEFINE Board Basic

&SCOPED-DEFINE joinTable , EACH job-sch NO-LOCK ~
WHERE job-sch.company EQ job.company ~
AND job-sch.job-no EQ job.job-no ~
AND job-sch.job-no2 EQ job.job-no2

&SCOPED-DEFINE startDatePhrase AND job-mch.start-date-su EQ job-sch.m-date

{{&loads}/ASI/loadPro.p}
