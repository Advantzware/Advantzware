/* setTime.i */

IF {&ampm} = 'PM' AND {&hour} NE '12' THEN
{&hour} = STRING(INTEGER({&hour}) + 12,'99').
IF {&ampm} = 'AM' AND {&hour} = '12' THEN
{&hour} = IF '{&field}' BEGINS 'end' THEN '24' ELSE '00'.
{&field} = INTEGER({&hour}) * 3600 + INTEGER({&minute}) * 60.
