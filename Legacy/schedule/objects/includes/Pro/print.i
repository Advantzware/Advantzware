/* print.i - used in procedure buildLines in print.p */

      WHEN '{1}Field{2}' THEN
      RUN loadField (browseColumn.rptLine,
                     browseColumn.rptCol,
                     INTEGER({1}Width[{2}] / 1.5),
                     {1}Label[{2}],
                     ttblJob.{1}Field{2}).
