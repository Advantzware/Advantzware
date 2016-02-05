/* createJob.i */

        SELECTABLE = YES
    TRIGGERS:
      ON DESELECTION
         PERSISTENT RUN jobDeselection IN THIS-PROCEDURE (pWidget:HANDLE).
      ON SELECTION
         PERSISTENT RUN jobSelection IN THIS-PROCEDURE (pWidget:HANDLE).
    END TRIGGERS.
