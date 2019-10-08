/* createJob.i - used in procedure createJob */

        MOVABLE = YES
        SELECTABLE = YES
        RESIZABLE = YES
      TRIGGERS:
        ON DESELECTION
           PERSISTENT RUN jobDeselection IN THIS-PROCEDURE (pWidget:HANDLE).
        ON END-MOVE
           PERSISTENT RUN jobEndMove IN THIS-PROCEDURE (pWidget:HANDLE).
        ON END-RESIZE
           PERSISTENT RUN jobEndResize IN THIS-PROCEDURE (pWidget:HANDLE).
        ON LEFT-MOUSE-DOWN
           PERSISTENT RUN jobMouseDown IN THIS-PROCEDURE (pWidget:HANDLE).
        ON MOUSE-SELECT-CLICK
           PERSISTENT RUN jobClick IN THIS-PROCEDURE (pWidget:HANDLE).
        ON START-RESIZE
           PERSISTENT RUN jobStartResize IN THIS-PROCEDURE.
        ON START-MOVE
           PERSISTENT RUN jobStartMove IN THIS-PROCEDURE (pWidget:HANDLE).
        ON SELECTION
           PERSISTENT RUN jobSelection IN THIS-PROCEDURE (pWidget:HANDLE).
      END TRIGGERS.
