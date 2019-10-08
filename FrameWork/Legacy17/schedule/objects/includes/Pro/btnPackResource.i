/* btnPackResource.i - used in trigger btnPackResource in schedule.w */

  IF SELF:PRIVATE-DATA EQ 'Resource' THEN
  RUN packResource IN h_board (SELF:LABEL).
  ELSE
  RUN packJob IN h_board (SELF:LABEL).
