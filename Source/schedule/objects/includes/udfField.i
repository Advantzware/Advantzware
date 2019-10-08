/* udfField.i - used in procedure buildLines in print/includes/rptLayout.i */

      WHEN 'udfField{1}' THEN
      RUN loadField ({&useTable}.udfField{1}).
