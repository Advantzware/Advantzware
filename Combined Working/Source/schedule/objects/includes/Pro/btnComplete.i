/* btnComplete.i - used in trigger for btnComplete in board.w & resourceDetail.w */

RUN {&prompts}/completeJob.w ({1},'{&Board}').
IF '{&Board}' EQ 'Pro' THEN
RUN setJobColor IN {2} ({1}) NO-ERROR.
