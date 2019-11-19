/* get_time2.i */

DO WITH FRAME {&FRAME-NAME}:
    RUN spCommon_ParseTime (
        {&field},
        {&hour}:HANDLE,
        {&minute}:HANDLE,
        {&second}:HANDLE,
        {&ampm}:HANDLE
        ).
    ASSIGN {&TIME-FIELDS}.
END. /* with frame */
