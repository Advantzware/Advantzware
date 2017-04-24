 /* connect to rfq database - start */

IF NOT CONNECTED('rfq') AND SEARCH('addon\rfq.pf') NE ? THEN
   CONNECT -pf VALUE(SEARCH('addon\rfq.pf')) NO-ERROR.

IF CONNECTED('rfq') THEN
   RUN util/loadvw2.w.
