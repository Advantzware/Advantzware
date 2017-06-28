/***************************************************************************\
*****************************************************************************
**  Program: edi/lkupstby.p
**       By: Chris Heins
** Descript: Determine oe-ording by-code (BY) and Ship To (ST) codes.
**
*****************************************************************************
\***************************************************************************/
DEF SHARED VAR ws_partner AS CHAR no-undo.
DEF INPUT   PARAM inv-line_rec   AS RECID NO-UNDO.
DEF OUTPUT  PARAM st_code       AS CHAR NO-UNDO.
DEF OUTPUT  PARAM by_code       AS CHAR NO-UNDO.
FIND inv-line WHERE RECID(inv-line) = inv-line_rec NO-LOCK NO-ERROR.
IF NOT AVAIL inv-line THEN
DO:
  by_code = "".
  st_code = "".
  RETURN.
END.
FIND oe-ord WHERE oe-ord.company = inv-line.company
  AND oe-ord.ord-no = inv-line.ord-no NO-LOCK NO-ERROR.
find inv-head where inv-head.r-no = inv-line.r-no no-lock no-error.
IF AVAIL oe-ord THEN
DO:
  st_code = oe-ord.sold-id.
  /*
  IF by_code <= " " THEN
  by_code = oe-ord.edi-by-code-no.
  */
END.
if avail inv-head
then do:
  by_code = inv-head.sold-no.
end.
if st_code = "" or by_code = "" then do:
    find first edpotran
    where edpotran.partner = ws_partner
    and edpotran.cust-po = inv-line.po-no no-lock no-error.
    if avail edpotran then do:
        find first edpoline of edpotran
        where edpoline.item-no = inv-line.i-no no-lock no-error.
    end.
    if st_code = "" then
    st_code = if avail edpoline and edpoline.st-code > "" then edpoline.st-code
    else if avail edpotran and edpotran.st-code > "" then edpotran.st-code
    else "".
    if by_code = "" then
    by_code = if avail edpoline and edpoline.by-code > "" then edpoline.by-code
    else if avail edpotran and edpotran.by-code > "" then edpotran.by-code
    else "".
end.
/*
IF by_code <= " " THEN
DO:
  FIND FIRST edshipto WHERE edshipto.partner = ws_partner AND
    edshipto.cust = inv-line.cust-no AND
    edshipto.ref-type = "s"
    NO-LOCK NO-ERROR.
  IF AVAIL edshipto THEN
  assign by_code = edshipto.by-code st_code = edshipto.st-code.
  ELSE
  DO:
    FIND FIRST edshipto WHERE edshipto.partner = ws_partner AND
      edshipto.cust = inv-line.cust-no AND
      edshipto.ref-type = "c"
      NO-LOCK NO-ERROR.
    IF AVAIL edshipto THEN
    assign by_code = edshipto.by-code st_code = edshipto.st-code.
  END.
END.
IF by_code <= " " THEN
by_code = " " /* 9607 was 99, custom for federated or may? */.
/* 9602 CAH: Translator was rejecting 1 digit by-code codes ... */
IF SUBSTRING(by_code,1,1) = "X" THEN
by_code = SUBSTRING(by_code,2,4).
by_code = TRIM(by_code).
IF LENGTH(by_code) < 4
and {rc/isdigit.i substring(by_code,1,1)}THEN
by_code = STRING(INTEGER(by_code),"9999").
if st_code = "" then do:
DO:
  FIND FIRST edshipto WHERE edshipto.partner = ws_partner AND
    edshipto.cust = inv-line.cust-no AND
    edshipto.ref-type = "c"
    NO-LOCK NO-ERROR.
  IF AVAIL edshipto THEN
  st_code = by-code.
  ELSE
  DO:
    FIND FIRST edshipto WHERE edshipto.partner = ws_partner AND
      edshipto.cust = oe-ord.sold-id AND
      edshipto.ref-type = "ST"
      NO-LOCK NO-ERROR.
    IF AVAIL edshipto THEN
    st_code = by-code.
  END.
END.
IF st_code <= " " AND AVAIL oe-ord THEN
DO:
  st_code = string(oe-ord.sold-no,"9999").
  /*
  IF st_code <= " " THEN
  st_code = oe-ord.edi-by-code-no.
  */
END.
IF st_code <= " " THEN do:
st_code = " ". /* inv-line.cust-no. */
FIND FIRST edshipto WHERE edshipto.partner = ws_partner AND
  edshipto.cust = inv-line.cust-no AND
  edshipto.ref-type = "c"
  NO-LOCK NO-ERROR.
IF AVAIL edshipto THEN
st_code = by-code.
/* 9607 CAH: Was:  "99".  per N1 BY segment below */
/* 9602 CAH: Translator was rejecting 1 digit by-code codes ... */
end.
end.    /* if st_code = "" */
IF SUBSTRING(st_code,1,1) = "X" THEN
st_code = SUBSTRING(st_code,2,4).
st_code = TRIM(st_code).
IF LENGTH(st_code) < 4
and {rc/isdigit.i substring(st_code,1,1)}THEN
st_code = STRING(INTEGER(st_code),"9999").
*/
