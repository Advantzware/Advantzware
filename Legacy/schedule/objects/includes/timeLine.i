/* timeLine.i - used in OCX.Tick trigger in schedule.w */

RUN timeLine IN h_board.
{&WINDOW-NAME}:TITLE = STRING(TIME,'HH:MM:SS am') + ' - ' + winTitle + updatesPending().
