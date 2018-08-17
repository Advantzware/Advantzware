/* calctime.i */

IF {&file}.start_date EQ {&file}.end_date THEN
    {&file}.total_time = {&file}.end_time - {&file}.start_time.
ELSE
    {&file}.total_time = (86400 - {&file}.start_time)
                       + ({&file}.end_date - {&file}.start_date - 1) * 86400
                       +  {&file}.end_time.
/*if end_date is blank, set total_time to 0*/
IF {&file}.total_time LT 0 OR {&file}.total_time EQ ? THEN
{&file}.total_time = 0.
