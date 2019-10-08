/* userField.i - used in procedure buildLines in print/includes/rptLayout.i */

      WHEN 'userField{1}' THEN
      RUN loadField ({&useTable}.userField{1}).
