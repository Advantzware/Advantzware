/* get_time.i */

ASSIGN
  {&hour} = SUBSTR(STRING({&field},"HH:MM AM"),1,2)
  {&minute} = SUBSTR(STRING({&field},"HH:MM AM"),4,2)
  {&second} = SUBSTR(STRING({&field},"HH:MM:SS AM"),7,2)
  {&ampm} = SUBSTR(STRING({&field},"HH:MM AM"),7,2).
