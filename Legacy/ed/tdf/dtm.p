/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\dtm.
**       By:
** Descript:
10.06.98 by CAH on \\ricky\rv8 Log#0000:
1.  Added 011 for ship_date for ASN 856 transactions.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
{rc/datev.i}
def shared stream s-out.
IF ws_segment <> "DTM" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i  date_qual                       18  3}
      {rc/substr.i  char_date                       21  8}
      {rc/substr.i  char_time                       29  8}
      {rc/substr.i  time_code                       37  2}
      {rc/substr.i  date_time_period_qual           39  3}
      {rc/substr.i  date_time_period                42 35}
      .
  END.
  WHEN "3060" THEN
  DO:
    ASSIGN
      {rc/substr.i  date_qual                       18  3}
      {rc/substr.i  char_date                       21  6}
      {rc/substr.i  char_time                       27  8}
      {rc/substr.i  time_code                       35  2}
      {ed/tdf/substrde.i  ws_century                37  3 0}
      {rc/substr.i  date_time_period_qual           40  3}
      {rc/substr.i  date_time_period                43 35}
      .
  END.
  OTHERWISE /* "3020" */
    DO:
    ASSIGN
      {rc/substr.i  date_qual                         18  3}
      {rc/substr.i  char_date                         21  6}
      {rc/substr.i  char_time                         27  6}
      {rc/substr.i  time_code                         33  2}
      {ed/tdf/substrde.i  ws_century                  35  3 0}
      .
  END.    /* 3020 */
END.
IF ws_century > 0 THEN
char_date = string(ws_century,"99") + char_date.
{rc/xyymmdd.i char_date extra_date#}.
CASE date_qual:
WHEN "001" THEN
ASSIGN cancel_date# = extra_date# cancel_date_qualifier = date_qual.
WHEN "002"  THEN
ASSIGN ship_date#   = extra_date# ship_date_qualifier = date_qual.
when "003"   THEN invoice_date# = extra_date#.
when "007" then assign send_date# = extra_date# 
  extra_date_qualifier = date_qual.
WHEN "009" /* process date */
  THEN 
ASSIGN ship_date#   = extra_date# ship_date_qualifier = date_qual.
WHEN "004" THEN          /* 9809 CAH from Sears 860 */
ASSIGN purchase_order_date# = extra_date#.
WHEN "010"  THEN
ASSIGN ship_date# = extra_date#
  ship_date_qualifier = date_qual.
WHEN "037" /* ship not before */ THEN
ASSIGN ship_date#   = extra_date#     ship_date_qualifier = date_qual.
WHEN "011" THEN
ASSIGN ship_date# = extra_date# ship_date_qualifier = date_qual.
WHEN "012" THEN
ASSIGN discount_due_date# = extra_date#.
WHEN "013" THEN
ASSIGN net_date# = extra_date#.
WHEN "014" THEN
ASSIGN terms_deferred_due# = extra_date#.
when "077" then assign ship_date# = extra_date#
    ship_date_qualifier = date_qual. /* 990507 CAH */
WHEN "097" THEN
ASSIGN send_date# = extra_date#. /* transaction create date */
when "145" then do: end.  /* opening date, e.g. store */
when "146" then do: end.  /* closeing date, e.g. store */
OTHERWISE RUN rc/debugmsg.p
  ("Unrecognized date qualifier: " + date_qual + " for date: " + string(extra_date#,"99/99/9999") ).
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE date_qual:
  WHEN "001" THEN
  ASSIGN extra_date# = cancel_date#.
  WHEN "002" THEN
  ASSIGN extra_date# = ship_date#.
  WHEN "003" THEN extra_date# = invoice_date#. /* 9901 cah FROM SEARS 4010 820 */
  WHEN "004" THEN           /* 9809 CAH from Sears 860 */
  ASSIGN extra_date# = purchase_order_date#.
  WHEN "009" THEN
  ASSIGN extra_date# = ship_date#. /* process date */
  WHEN "010" THEN
  ASSIGN extra_date# = ship_date#.
  WHEN "011" THEN
  ASSIGN extra_date# = ship_date#.
  WHEN "037" THEN
  ASSIGN extra_date# = ship_date#. /* ship not before */
  WHEN "012" THEN
  ASSIGN extra_date# = discount_due_date#.
  WHEN "013" THEN
  ASSIGN extra_date# = net_date#.
  WHEN "014" THEN
  ASSIGN extra_date# = terms_deferred_due#.
  WHEN "077" THEN ASSIGN extra_date# = ship_date#.
  WHEN "097" THEN
  ASSIGN extra_date# = send_date#. /* transaction create date */
  OTHERWISE RUN rc/debugmsg.p
    ("Unrecognized date qualifier: " + date_qual + " for date: " + string(extra_date#,"99/99/9999") ).
END CASE.
/* check mandatory assignments ... */
IF date_qual = ""
  OR extra_date# = ?
  THEN
DO:
  RUN rc/debugmsg.p ("Mandatory elements missing (date_qual or extra_date#), qual = " + date_qual ).
  RETURN error.
END.
char_date = {rc/dt2ymd.i extra_date#}.
ws_century = integer(substring(string(year(extra_date#)),1,2)).
CASE ws_version:
WHEN "4010" THEN
DO:
  ASSIGN
      {rc/substr.i  date_qual                       18  3}
      {rc/substr.i  char_date                       21  8}
      {rc/substr.i  char_time                       29  8}
      {rc/substr.i  time_code                       37  2}
      {rc/substr.i  date_time_period_qual           39  3}
      {rc/substr.i  date_time_period                42 35}
      .
END.      
WHEN "3060" THEN
DO:
  ASSIGN
    {rc/outstr.i  date_qual                       18  3}
    {rc/outstr.i  char_date                       21  6}
    {rc/outstr.i  char_time                       27  8}
    {rc/outstr.i  time_code                       35  2}
    {ed/tdf/outstrde.i  ws_century                37  3 0}
    {rc/outstr.i  date_time_period_qual           40  3}
    {rc/outstr.i  date_time_period                43 35}
    .
END.
OTHERWISE /* "3020" */
  DO:
  ASSIGN
    {rc/outstr.i  date_qual                         18  3}
    {rc/outstr.i  char_date                         21  6}
    {rc/outstr.i  char_time                         27  6}
    {rc/outstr.i  time_code                         33  2}
    {ed/tdf/substrde.i  ws_century                  35  3 0}
    .
END.    /* 3020 */
END.    /* CASE VERSIONS */
END.    /* O */
IF COMMAND MATCHES "*P*" THEN DO:
    DISPLAY STREAM S-OUT
        ws_segment
        extra_date# label "Date" format "99/99/9999" space(0) "/" space(0) date_qual format 'x(03)' no-label
        time_code   label "Time Code"
        date_time_period label "Period" space(0) '/' space(0) date_time_period_qual no-label
    with side-labels width 144 no-box.    
        
END.
