/* winTitle.i - used in main block of detail viewers */

{&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                     + ' {&Board} (' + ID + ') - v'
                     + DYNAMIC-FUNCTION("sfVersion") + ' - '
                     + STRING(TODAY,'99.99.9999') + asOfTime
                     .
