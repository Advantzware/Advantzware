/* buildJob.i */

      IF intervals:LOOKUP(intervals:SCREEN-VALUE) LE lockButtons AND
         xValue LT FRAME {&FRAME-NAME}:WIDTH-PIXELS - 14 THEN DO:
        lockIdx = lockIdx + 1.
        RUN createLock (lockIdx,xValue,yValue,ROWID(ttblJob),ttblJob.jobLocked).
      END.
      {{&includes}/View/buildJob.i} /* noteIcon */
