/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\td5.p
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
IF ws_segment <> "TD5" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3060" THEN
  DO:
    ASSIGN
      {rc/substr.i    routing_sequence_code        18  2}
      {rc/substr.i    id_code_qualifier            20 02}
      {rc/substr.i    id_code                      22 20}
      {rc/substr.i    transportation_method_code   42 02}
      {rc/substr.i    routing                      44 35}
      .
  END.
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/substr.i    routing_sequence_code        18  2}
      {rc/substr.i    id_code_qualifier            20 02}
      {rc/substr.i    id_code                      22 17}
      {rc/substr.i    transportation_method_code   39 02}
      {rc/substr.i    routing                      41 35}
      .
  END.
END CASE.
CASE id_code_qualifier:
WHEN "2" THEN
carrier_scac_code = id_code.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE id_code_qualifier:
  WHEN "2" THEN
  id_code = carrier_scac_code.
END CASE.
CASE ws_version:
WHEN "3060" THEN
DO:
  ASSIGN
    {rc/outstr.i    routing_sequence_code        18  2}
    {rc/outstr.i    id_code_qualifier            20 02}
    {rc/outstr.i    id_code                      22 20}
    {rc/outstr.i    transportation_method_code   42 02}
    {rc/outstr.i    routing                      44 35}
    .
END.
WHEN "3020" THEN
DO:
  ASSIGN
    {rc/outstr.i    routing_sequence_code        18  2}
    {rc/outstr.i    id_code_qualifier            20 02}
    {rc/outstr.i    id_code                      22 17}
    {rc/outstr.i    transportation_method_code   39 02}
    {rc/outstr.i    routing                      41 35}
    .
END.
END CASE.
END.    /* O */
