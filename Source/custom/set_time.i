/* set_time.i */

IF {&ampm} EQ 'PM' AND {&hour} NE '12' THEN
{&hour} = STRING(INTEGER({&hour}) + 12,'99').
IF {&ampm} EQ 'AM' AND {&hour} EQ '12' THEN
{&hour} = '00'.
{&field} = INTEGER({&hour}) * 3600
         + INTEGER({&minute}) * 60
         .
