/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\iitd.
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
{rc/datev.i}
IF ws_segment <> "ITD" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    terms_type                          18  02}
      {rc/substr.i    terms_basis                         20  02}
      {ed/tdf/substrde.i  discount_percent                22  07 0}
      {rc/substr.i    discount_due_date                   29  08}
      {ed/tdf/substrde.i    discount_due_days             37  04 0}
      {rc/substr.i    net_date                            41  08}
      {ed/tdf/substrde.i net_days                         49  04 0}
      {ed/tdf/substrde.i discount_amount                  53  11 0}
      {rc/substr.i  misc_elem[9]                          64  08} /* deferred due date */
      {rc/substr.i  misc_elem[10]                         72  11} /* deferred amt due */
      {rc/substr.i  misc_elem[11]                         83  06} /* pct of inv payable */
      {rc/substr.i  terms_description                     89  80}
      {rc/substr.i  misc_elem[13]                        169  03} /* day of month */
      {rc/substr.i  misc_elem[14]                        172  02} /* pay meth */
      {rc/substr.i  misc_elem[15]                        174  11} /* percent */
      .
  END.
  OTHERWISE
    DO:
    ASSIGN
      {rc/substr.i    terms_type                          18  02}
      {rc/substr.i    terms_basis                         20  02}
      {ed/tdf/substrde.i  discount_percent                22  07 0}
      {rc/substr.i    discount_due_date                   29  06}
      {ed/tdf/substrde.i    discount_due_days             35  04 0}
      {rc/substr.i    net_date                            39  06}
      {ed/tdf/substrde.i net_days                         45  04 0}
      {ed/tdf/substrde.i discount_amount                  49  11 0}
      .
  END.
END CASE.
{rc/xyymmdd.i net_date net_date#}
{rc/xyymmdd.i discount_due_date discount_due_date#}.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN
    net_date            = IF net_date# = ? THEN ""
    ELSE {rc/dt2ymd.i net_date#}
    discount_due_date   = IF discount_due_date# = ? THEN ""
    ELSE {rc/dt2ymd.i discount_due_date#}
    .
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/outstr.i    terms_type                          18  02}
      {rc/outstr.i    terms_basis                         20  02}
      {ed/tdf/outstrde.i  discount_percent                22  07 0}
      {rc/outstr.i    discount_due_date                   29  08}
      {ed/tdf/outstrde.i    discount_due_days             37  04 0}
      {rc/outstr.i    net_date                            41  08}
      {ed/tdf/outstrde.i net_days                         49  04 0}
      {ed/tdf/outstrde.i discount_amount                  53  11 0}
      .
  END.
  OTHERWISE
    DO:
    ASSIGN
      {rc/outstr.i    terms_type                          18  02}
      {rc/outstr.i    terms_basis                         20  02}
      {ed/tdf/outstrde.i  discount_percent                22  07 0}
      {rc/outstr.i    discount_due_date                   29  06}
      {ed/tdf/outstrde.i    discount_due_days             35  04 0}
      {rc/outstr.i    net_date                            39  06}
      {ed/tdf/outstrde.i net_days                         45  04 0}
      {ed/tdf/outstrde.i discount_amount                  49  11 0}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    terms_type          LABEL "Type"
    terms_basis         LABEL "Basis"
    discount_percent    LABEL "Disc%"   WHEN discount_percent <> 0
    discount_due_date   LABEL "Disc-Dt" WHEN discount_due_date <> ?
    discount_due_days   LABEL "Disc-Days" WHEN discount_due_days <> 0
    net_date            LABEL "Net-Dt"  WHEN net_date <> ?
    net_days            LABEL "Net-Days" WHEN net_days <> 0
    discount_amount     LABEL "Disc$"   WHEN discount_amount <> 0
    WITH side-labels width 144 no-box.
END.
