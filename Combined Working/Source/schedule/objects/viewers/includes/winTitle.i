/* winTitle.i - used in main block of detail viewers */

{system/sysconst.i}
{&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE +
                       ' {&Board} (' + ID + ') - v{&awversion} - ' +
                       STRING(TODAY,'99.99.9999') + asOfTime.
