/* btnJobNotes.i - used in trigger for btnJobNotes in board.w & resourceDetail.w */

&IF DEFINED(boardType) EQ 0 &THEN
RUN {&prompts}/jobNotes.w ({1},'{&Board}').
&ELSE
RUN {&prompts}/jobNotes.w ({1},boardType).
&ENDIF
