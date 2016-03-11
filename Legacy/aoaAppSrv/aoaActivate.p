/* aoaActivate.p */

OUTPUT TO 'testparam.txt' APPEND.
PUT UNFORMATTED
    '[' STRING(TODAY,'99.99.9999') ','
    STRING(TIME,'hh:mm:ss') '] aoaActivate.p'
    SKIP
    '[' STRING(TODAY,'99.99.9999') ','
    STRING(TIME,'hh:mm:ss') '] '
    'SERVER-CONNECTION-ID: ' SESSION:SERVER-CONNECTION-ID
    SKIP
    '[' STRING(TODAY,'99.99.9999') ','
    STRING(TIME,'hh:mm:ss') '] '
    'SERVER-CONNECTION-CONTEXT: ' SESSION:SERVER-CONNECTION-CONTEXT
    SKIP(1)
    .
OUTPUT CLOSE.
