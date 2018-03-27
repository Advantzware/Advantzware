/* wndwinit.i */

IF g_pageno = 0 THEN
DO:
  IF NOT AVAILABLE config THEN
  FIND FIRST config NO-LOCK NO-ERROR.
  IF AVAILABLE config THEN
  g_pageno = config.start_page_no.
  IF g_pageno = 0 THEN
  RETURN.
END.
RUN SELECT-PAGE IN THIS-PROCEDURE (g_pageno).
g_pageno = 0.

