/* createJob.i */

        SELECTABLE = YES
    TRIGGERS:
      ON SELECTION
         PERSISTENT RUN gridLine IN THIS-PROCEDURE (pWidget:HANDLE).
    END TRIGGERS.
