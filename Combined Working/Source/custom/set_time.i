/* set_time.i */

IF {&ampm} = 'PM' AND {&hour} NE '12' THEN
{&hour} = STRING(INTEGER({&hour}) + 12,'99').
IF {&ampm} = 'AM' AND {&hour} = '12' THEN
{&hour} = '00'.
{&field} = INTEGER({&hour}) * 3600 + INTEGER({&minute}) * 60.
