/* getScenario.i - used in procedure getScenario in board.w */

  {{&includes}/getPending.i}
  RUN setJobSequence.
  IF justOpened THEN
  RUN saveScenario ('{&scenarios}/' + ID).
