/* gridLine.i - used in procedure gridLine in board.w */

/* IF ipWidget:TYPE NE 'BUTTON' THEN */
RUN objectName IN containerHandle (IF ipWidget:TYPE EQ 'BUTTON' THEN 'Resource'
                                   ELSE 'Job',ipWidget:NAME).
