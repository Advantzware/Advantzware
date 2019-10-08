/*
10.08.98 by CAH on \\ricky\rv8 Log#0000:
1.  Suppressed this segment for sears ASN.
*/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa  AS char NO-UNDO.
DEF OUTPUT PARAM erc        AS int NO-UNDO.
DEF SHARED var ws_partner AS char NO-UNDO.
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "PO4" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {ed/tdf/substrde.i number_of_inner_packs        18  06  0}  /* per carton */
      {ed/tdf/substrde.i number_of_eaches_per_inner   25  08  0}
      {rc/substr.i unit_of_measure        34  02}
      {rc/substr.i packaging_code         36  05}
      {rc/substr.i weight_qual            41  02}
      {ed/tdf/substrde.i case_weight      43  09  0}
      {rc/substr.i weight_unit_measure           53  02}
      {ed/tdf/substrde.i case_volume      55  09  0}
      {rc/substr.i volume_unit_measure    65  02}
      {ed/tdf/substrde.i case_length      67  08  0}
      {ed/tdf/substrde.i case_width       76  08  0}
      {ed/tdf/substrde.i case_height      85  08  0}
      {rc/substr.i size_unit_measure      94  02}
      {ed/tdf/substrde.i inner_pack       96  06  0}
      {rc/substr.i slp_code              103  02}
      {rc/substr.i item_assigned_id      105  20}
      {rc/substr.i item_assigned_id2     125  20}
      {ed/tdf/substrde.i misc_number1    145  09  8}
      .
  END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF ws_partner = "SEARS" AND header_setid = "856"
    THEN
  RETURN ERROR.
  IF case_weight = 0 AND case_volume = 0
    THEN
  RETURN error.
  CASE ws_version:
  WHEN "3020" THEN
  RETURN error.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {ed/tdf/outstrde.i number_of_inner_packs 18  06  0}  /* per carton */
      {ed/tdf/outstrde.i number_of_eaches_per_inner   25  08  0}
      {rc/outstr.i unit_of_measure        34  02}
      {rc/outstr.i packaging_code         36  05}
      {rc/outstr.i weight_qual            41  02}
      {ed/tdf/outstrde.i case_weight      43  09  0}
      {rc/outstr.i weight_unit_measure           53  02}
      {ed/tdf/outstrde.i case_volume      55  09  0}
      {rc/outstr.i volume_unit_measure    65  02}
      /* 9808 CAH: Getting errors here as
      {ed/tdf/outstrde.i case_length      67  08  0}
      {ed/tdf/outstrde.i case_width       76  08  0}
      {ed/tdf/outstrde.i case_height      85  08  0}
      {rc/outstr.i size_unit_measure      94  02}
      {ed/tdf/outstrde.i inner_pack       96  06  0}
      {rc/outstr.i slp_code              103  02}
      {rc/outstr.i item_assigned_id      105  20}
      {rc/outstr.i item_assigned_id2     125  20}
      {ed/tdf/outstrde.i misc_number1    145  09  8}
      */
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    number_of_inner_packs
    number_of_eaches_per_inner
    unit_of_measure
    packaging_code
    weight_qual
    case_weight
    weight_unit_measure
    case_volume
    volume_unit_measure
    case_length
    case_width
    case_height
    size_unit_measure
    inner_pack
    slp_code
    item_assigned_id
    item_assigned_id2
    misc_number1 LABEL "Misc#1"
    WITH side-labels width 144 no-box.
END.
