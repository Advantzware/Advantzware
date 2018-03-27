/* pendingJob.i - used in viewers/pending.w & print/pendingJobs.p */

{{&viewers}/includes/setPendingJob.i}

PROCEDURE getConfiguration:
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version asOfTime.
  INPUT CLOSE.
  RUN VALUE('get' + version).
END PROCEDURE.
