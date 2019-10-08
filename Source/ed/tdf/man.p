/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\iref.
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF  ws_segment <> "MAN" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/substr.i man_qual               18  02}
      {rc/substr.i man_number             20  45}
      .
  END.
  OTHERWISE /* 3060 */
    DO:
    ASSIGN
      {rc/substr.i man_qual               18  02}
      {rc/substr.i man_number             20  48}
      {rc/substr.i man_number2            68  48}
      .
  END.
END CASE.
CASE man_qual:
WHEN "GM" THEN
ucc128_mark = man_number.
WHEN "SM" THEN
pallet_mark = man_number.
OTHERWISE
  DO:
  RUN rc/debugmsg.p
    ("Unrecognized man_qual: " + man_qual + " man_number: " + man_number).
  RETURN error.
END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF man_qual = "" THEN
  RETURN error.
  CASE man_qual:
  OTHERWISE
    DO:
    RUN rc/debugmsg.p ("Unrecognized man_qual: " + man_qual + " man_number: " + man_number).
    RETURN error.
  END.
END CASE.
/* check mandatory assignments ... */
IF man_number = ""
  THEN
DO:
  RUN rc/debugmsg.p
    ("Mandatory elements missing (man_number) with Qualifier: " + man_qual ).
  RETURN error.
END.
CASE ws_version:
WHEN "3020" THEN
DO:
  ASSIGN
    {rc/outstr.i man_qual               18  02}
    {rc/outstr.i man_number             20  45}
    .
END.
OTHERWISE /* "3060" */
  DO:
  ASSIGN
    {rc/outstr.i man_qual               18  02}
    {rc/outstr.i man_number             20  48}
    {rc/outstr.i man_number2            68  48}
    .
END.
END CASE.
END.    /* O */
