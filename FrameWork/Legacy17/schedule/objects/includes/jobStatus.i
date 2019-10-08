/* jobStatus.i - used in procedure buildLines in print/includes/rptLayout.i */

      WHEN 'jobStatus[{1}]' THEN
      RUN loadField (STRING({&useTable}.jobStatus[{1}],rptFields.fieldFormat)).
