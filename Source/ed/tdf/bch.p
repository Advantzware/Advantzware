/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\bch.p
**       By: Chris Heins, RCI (c) 1999 All Rights Reserved.
** Descript: Purchase order change segment processor.
05.20.99 by CAH:
1.  Updated for 4010.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa  AS char NO-UNDO.
DEF OUTPUT PARAM erc        AS int NO-UNDO.
def shared stream s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
{rc/datev.i}
IF ws_segment <> "BCH" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  when "4010" then do:
    ASSIGN
      {rc/substr.i    transaction_purpose_code        18  02}
      {rc/substr.i    transaction_type_code           20  02}
      {rc/substr.i    purchase_order_number           22  22}
      {rc/substr.i    release_number                  44  30}
      {rc/substr.i    change_order_seq_number         74  08}
      {rc/substr.i    purchase_order_date             82  08}
      {rc/substr.i    ref_number                      90  45}
      {rc/substr.i    contract_number                 135 30}
      {rc/substr.i    ref_desc                        165 30}
      {rc/substr.i    extra_date                      195 08}
      {rc/substr.i    misc_elem[11]              203 08}    /* extra date2 */
      {rc/substr.i    misc_elem[12]              211 02}    /* contract type */
      {rc/substr.i    misc_elem[13]              213 02}    /* security level */
      {rc/substr.i    misc_elem[14]              215 02}    /* ack type */
      {rc/substr.i    misc_elem[15]              217 02}    /* transaction type */
      {rc/substr.i    misc_elem[16]              219 02}    /* purchase category */
      .
  end.
  
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/substr.i    transaction_purpose_code        18  02}
      {rc/substr.i    transaction_type_code           20  02}
      {rc/substr.i    purchase_order_number           22  22}
      {rc/substr.i    release_number                  44  30}
      {rc/substr.i    change_order_seq_number         74  08}
      {rc/substr.i    purchase_order_date             82  06}
      {rc/substr.i    ref_number                      88  45}
      {rc/substr.i    contract_number                 133 30}
      {rc/substr.i    ref_desc                        163 30}
      {rc/substr.i    extra_date                      193 06}
      .
  END.
END CASE.
{rc/xyymmdd.i purchase_order_date purchase_order_date#}.
{rc/xyymmdd.i extra_date          extra_date#}.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ...
  IF   invoice_date# = ?
  OR invoice_number = ""
  OR invoice_number = ?
  THEN
  DO:
  RUN rc/debugmsg.p
  ("Mandatory elements missing (invoice_date or invoice_number)" ).
  RETURN error.
  END.
  ... */
  ASSIGN
    purchase_order_date     =
    IF purchase_order_date# = ? THEN "" ELSE
    {rc/dt2ymd.i purchase_order_date#}
    extra_date     =
    IF extra_date# = ? THEN "" ELSE
    {rc/dt2ymd.i extra_date#}
    .
  CASE ws_version:
  when "4010" then do:
    ASSIGN
      {rc/outstr.i    transaction_purpose_code        18  02}
      {rc/outstr.i    transaction_type_code           20  02}
      {rc/outstr.i    purchase_order_number           22  22}
      {rc/outstr.i    release_number                  44  30}
      {rc/outstr.i    change_order_seq_number         74  08}
      {rc/outstr.i    purchase_order_date             82  08}
      {rc/outstr.i    ref_number                      90  45}
      {rc/outstr.i    contract_number                 135 30}
      {rc/outstr.i    ref_desc                        165 30}
      {rc/outstr.i    extra_date                      195 08}
      {rc/outstr.i    misc_elem[11]              203 08}    /* extra date2 */
      {rc/outstr.i    misc_elem[12]              211 02}    /* contract type */
      {rc/outstr.i    misc_elem[13]              213 02}    /* security level */
      {rc/outstr.i    misc_elem[14]              215 02}    /* ack type */
      {rc/outstr.i    misc_elem[15]              217 02}    /* transaction type */
      {rc/outstr.i    misc_elem[16]              219 02}    /* purchase category */
      .
  end.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/outstr.i    transaction_purpose_code        18  02}
      {rc/outstr.i    transaction_type_code           20  02}
      {rc/outstr.i    purchase_order_number           22  22}
      {rc/outstr.i    release_number                  44  30}
      {rc/outstr.i    change_order_seq_number         74  08}
      {rc/outstr.i    purchase_order_date             82  06}
      {rc/outstr.i    ref_number                      88  45}
      {rc/outstr.i    contract_number                 133 30}
      {rc/outstr.i    ref_desc                        163 30}
      {rc/outstr.i    extra_date                      199 06}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    transaction_purpose_code    LABEL "Purpose"
    transaction_type_code       label "Type"
    purchase_order_number       LABEL "PO#"
    release_number              LABEL "Rel#"        FORMAT "x(10)"
    change_order_seq_number     label "Chg#"        format "x(04)"
    purchase_order_date#        LABEL "Date"
    contract_number             LABEL "Contract#"   FORMAT "x(20)"
    skip space(09)    
    ref_number                  label "Ref"
    ref_desc                    label "Ref Descr"
    WITH side-labels width 144 no-box.
END.
