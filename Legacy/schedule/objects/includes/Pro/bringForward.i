/* bringForward.i - used in trigger for btnBringForward in schedule.w */

RUN bringForward IN h_board (IF btnPackResource:PRIVATE-DATA NE 'Resource' THEN '<ALL>'
                             ELSE btnPackResource:LABEL).
