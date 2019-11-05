/* get_time.i */

DO WITH FRAME {&FRAME-NAME}:
    RUN spParseTime (
        {&field},
        {&hour}:HANDLE,
        {&minute}:HANDLE,
        ?,
        {&ampm}:HANDLE
        ).
    ASSIGN {&TIME-FIELDS}.
END. /* with frame */
