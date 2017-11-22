/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\rc\dt2char.p
**       By: Chris Heins
** Descript: Given date and format, return character string.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM pdate AS date NO-UNDO.
DEF INPUT  PARAM pformat AS char NO-UNDO.   /* c y m d */
DEF OUTPUT PARAM pchar AS char NO-UNDO.
IF pformat = "" OR pformat = ?
  THEN
pformat = "mdyc".
IF pdate = ? THEN
pchar = "".
ELSE
DO:
  IF pformat = "ymd"
    THEN
  pchar =
  substring(string(year(pdate),"99"),3,2)
  + string(month(pdate),"99")
  + string(day(pdate),"99").
  ELSE
  IF pformat = "mdy"
    THEN
  pchar =
  string(month(pdate),"99")
  + string(day(pdate),"99")
  + substring(string(year(pdate),"99"),3,2).
  ELSE
  IF pformat = "cymd"
    THEN
  pchar =
  string(year(pdate),"9999")
  + string(month(pdate),"99")
  + string(day(pdate),"99").
  ELSE
  IF pformat = "mdyc"
    THEN
  pchar =
  string(month(pdate),"99")
  + string(day(pdate),"99")
  + string(year(pdate),"9999").
  ELSE
  DO:
    RUN rc/debugmsg.p ("Unrecognized date format: " + pformat).
  END.
END.
