/***************************************************************************\
*****************************************************************************
**  Program: e:\asi\patch\edi\ed\exparin
**       By: Chris Heins, for Advanced Software, Inc. for Sonoco Packaging.
** Descript: (c) 1998 Sonoco, All Rights Reserved.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT PARAM p_file AS char NO-UNDO. /* arinv or inv-head */
DEF INPUT PARAM p_rec  AS RECID NO-UNDO.
DEF STREAM s-export.
/* include files */
{rc/stringv.i}
{ed/tdf/sharedv.i   "new shared"}
/* contants and literals */
DEF VAR FILLER          AS char NO-UNDO     INITIAL "".
DEF var ws_plant        AS char NO-UNDO     INITIAL "A720".
DEF VAR export_fid      AS char NO-UNDO     initial "SONO_INV.TXT".
/* local variables */
DEF var tax_basis       AS decimal NO-UNDO  FORMAT '-99999999.99999999'.
DEF var line_tax        AS decimal NO-UNDO  FORMAT '-99999999.99999999'.
DEF var line_count      AS int NO-UNDO.
DEF var retry_counter   AS int  NO-UNDO.
DEF var fancy_date      AS char NO-UNDO.    /* yyyy-mm-dd */
DEF var ws_due-date     AS date NO-UNDO.
DEF var ws_salesacct    AS char NO-UNDO.
DEF var ws_sman         AS char NO-UNDO.    /* misc has no sman, take 1st */
DEF var ws_po-no        AS char NO-UNDO.
DEF var xx              AS int NO-UNDO.
def var ws_tilde        as char no-undo initial "~~".
IF NOT CAN-DO("ar-inv,inv-head", p_file) THEN
RETURN error.
IF p_file = "ar-inv" THEN
DO:
  FIND ar-inv WHERE RECID(ar-inv) = p_rec EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL ar-inv THEN
  RETURN error.
  
  if not (ar-inv.posted and ar-inv.printed) then return error.
  /* determine if the invoice has any lines, and determine the total taxable */
  line_count = 0.
  FOR EACH ar-invl NO-LOCK
      WHERE ar-invl.x-no = ar-inv.x-no
      and ar-invl.posted:
    line_count = line_count + 1.
    IF ar-invl.tax = TRUE
      THEN
    ASSIGN tax_basis = tax_basis + ar-invl.amt.
  END.
END.    /* ar-inv */
IF p_file = "inv-head" THEN
DO:
  FIND inv-head WHERE RECID(inv-head) = p_rec EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL inv-head THEN
  RETURN error.
  if inv-head.deleted then return error.
  /*
  if not (inv-head.posted and inv-head.printed) then return error.
  */
  ASSIGN
    ws_sman = ?
    ws_po-no = ?.
  /* determine if the invoice has any lines, and determine the total taxable */
  line_count = 0.
  FOR EACH inv-line NO-LOCK
      WHERE inv-line.r-no = inv-head.r-no
      and not (inv-line.deleted):
    line_count = line_count + 1.
    IF inv-line.tax = TRUE
      THEN
    ASSIGN tax_basis = tax_basis + inv-line.amt.
    IF ws_sman = ?
      THEN
    _sman:
    DO xx = 1 TO 3:
      IF inv-line.sman[xx] > "" THEN
      DO:
        ASSIGN ws_sman = inv-line.sman[xx].
        LEAVE _sman.
      END.
    END.  /* sman default */
    IF ws_po-no = ?
      AND inv-line.po-no > ""
      THEN
    ws_po-no = inv-line.po-no.
  END.
  FOR EACH inv-misc NO-LOCK
      WHERE inv-misc.r-no = inv-head.r-no
      and not(inv-line.deleted):
    line_count = line_count + 1.
    IF inv-misc.tax = TRUE
      THEN
    ASSIGN tax_basis = tax_basis + inv-misc.amt.
  END.
END.    /* inv-head */
IF line_count = 0 THEN
RETURN ERROR.
_open:
DO ON error UNDO, RETRY:
  error-status:error = FALSE.
  OUTPUT STREAM s-export TO VALUE(export_fid) append.
  IF OS-ERROR > 0 THEN
  DO:
    HIDE MESSAGE NO-PAUSE.
    MESSAGE error-status:get-message(OS-ERROR).
    PAUSE.
    retry_counter = retry_counter + 1.
    IF retry_counter > 100 THEN
    DO:
      OUTPUT STREAM s-export close.
      RETURN error (string(OS-ERROR)).
    END.
    UNDO _open, RETRY _open.
  END.    /* error handling */
END.    /* _open block */
IF p_file = "ar-inv" THEN
DO:
  fancy_date = string(year(ar-inv.inv-date),"9999") + '-'
  + string(month(ar-inv.inv-date),"99") + '-'
  + string(day(ar-inv.inv-date),"99").
  FIND cust WHERE cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.
  FOR EACH ar-invl EXCLUSIVE-LOCK
      WHERE ar-invl.x-no = ar-inv.x-no
      and ar-invl.posted:
    IF ar-invl.tax
      THEN
    ASSIGN
      line_tax = (ar-invl.amt / tax_basis) * ar-inv.tax-amt
      line_tax = round(line_tax, 2).
    ELSE
    ASSIGN line_tax = 0.
    str_buffa = ""  /* clear the work area */.
    ASSIGN
      {rc/outstr.i    ar-inv.cust-no                  1   10}
      {rc/outstr.i    "if ar-inv.sold-state > '' then ar-inv.sold-state
    else if ar-inv.state > '' then ar-inv.state
    else if avail cust then cust.state else '' "      11 2}
      {rc/outstr.i    ar-invl.sman[1]                 13  4}
      {rc/outstr.i    fancy_date                      17 10}
      {rc/outstr.i    ws_plant                        27  4}  /* w-plt */
      {rc/outstr.i    string(ar-inv.inv-no)           31  9}
      {rc/outstr.i    string(ar-invl.line,'999')      40  3}
      {rc/outstr.i    FILLER                          43  5}  /* w-prod */
      {rc/outstr.i    string(ar-invl.inv-qty,'+9999999')  48  8}
      {rc/outstr.i    "string(100 * ar-invl.amt,'+999999999')" 56 10}
      {rc/outstr.i    "string(100 * line_tax,'+9999999999999')" 66 14}
      {rc/outstr.i    "string(100 * ar-invl.t-freight,'+999999999999')" 80 13}
      {rc/outstr.i    FILLER                          93  13}   /* w-ref-inv */
      {rc/outstr.i    "string(100 * ar-invl.disc,'+999999999999999')" 106 16}
      {rc/outstr.i    "IF ar-inv.type > '' then ar-inv.type else 'I' "
                                                      122 1}  /* w-tran-type */
      {rc/outstr.i    FILLER                          123 1}  /* w-tran-code */
      {rc/outstr.i    'USD'                           124 3}  /* w-curr-type */
      {rc/outstr.i    ar-inv.terms                    127 5}
      {rc/outstr.i    string(ar-inv.due-date,'99/99/9999') 132 10}
      {rc/outstr.i    FILLER                          142 3}  /* w-bill-sys */
      {rc/outstr.i    ar-invl.po-no                   145 30}
      {rc/outstr.i    '000'                           175 3}  /* w-po-line */
      {rc/outstr.i    string(ar-invl.bol-no)          178 30}
      {rc/outstr.i    " 'ar-inv rec# ' + string(ar-inv.x-no)"
                                                      208 30} /* w-ref-doc */
      {rc/outstr.i    FILLER                          238 1}  /* w-ref-do-cd */
      {rc/outstr.i    ws_plant                        239 4}  /* w-plant */
      {rc/outstr.i    substring(ar-invl.actnum,1,5)   243 5}  /* w-dept */
      {rc/outstr.i    substring(ar-invl.actnum,6,6)   248 6}  /* w-account */
      {rc/outstr.i    string(ar-invl.ord-no)          254 8}  /* sales order */
      {rc/outstr.i    FILLER                          262 7}
      {rc/outstr.i    ws_tilde                        269 1}  /* w-end */
      .
    PUT STREAM s-export UNFORMATTED str_buffa SKIP.
  END.
END.    /* ar-inv file */
IF p_file = "inv-head" THEN
DO:
  FIND cust WHERE cust.cust-no = inv-head.cust-no NO-LOCK NO-ERROR.
  FIND terms WHERE terms.company = inv-head.company
    AND terms.t-code = inv-head.terms NO-LOCK NO-ERROR.
  ws_due-date = inv-head.inv-date +
  (IF AVAIL terms THEN
  terms.net-days ELSE
  0).
  fancy_date = string(year(inv-head.inv-date),"9999") + '-'
  + string(month(inv-head.inv-date),"99") + '-'
  + string(day(inv-head.inv-date),"99").
  FIND ar-ctrl WHERE ar-ctrl.company = inv-head.company NO-LOCK NO-ERROR.
  FOR EACH inv-line EXCLUSIVE-LOCK
      WHERE inv-line.r-no = inv-head.r-no
      and not(inv-line.deleted):
    IF inv-line.tax
      THEN
    ASSIGN
      line_tax = (inv-line.amt / tax_basis) * inv-head.t-inv-tax
      line_tax = round(line_tax, 2).
    ELSE
    ASSIGN line_tax = 0.
    FIND FIRST itemfg
      WHERE itemfg.company = inv-line.company
      AND itemfg.i-no = inv-line.i-no NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN
    DO:
      FIND fgcat
        WHERE fgcat.company eq inv-misc.company
        AND fgcat.procat  eq itemfg.procat
        NO-LOCK NO-ERROR.
    END.
    ws_salesacct =
    IF AVAIL fgcat AND fgcat.glacc > ""
      THEN
    fgcat.glacc
    ELSE
    IF AVAIL ar-ctrl
      THEN
    ar-ctrl.sales
    ELSE ""
    .
    str_buffa = ""  /* clear the work area */.
    ASSIGN
      {rc/outstr.i    inv-head.cust-no                1   10}
      {rc/outstr.i    "if inv-head.sold-state > '' then inv-head.sold-state
    else if inv-head.state > '' then inv-head.state
    else if avail cust then cust.state else '' "    11 2}
      {rc/outstr.i    inv-line.sman[1]                13  4}
      {rc/outstr.i    fancy_date                      17 10}
      {rc/outstr.i    ws_plant                        27  4}  /* w-plt */
      {rc/outstr.i    string(inv-head.inv-no)           31  9}
      {rc/outstr.i    string(inv-line.line,'999')      40  3}
      {rc/outstr.i    FILLER                          43  5}  /* w-prod */
      {rc/outstr.i    string(inv-line.inv-qty,'+9999999')  48  8}
      {rc/outstr.i    "string(100 * inv-line.amt,'+999999999')" 56 10}
      {rc/outstr.i    "string(100 * line_tax,'+9999999999999')" 66 14}
      {rc/outstr.i    "string(100 * inv-line.t-freight,'+999999999999')" 80 13}
      {rc/outstr.i    FILLER                          93  13}   /* W-REF-INV */
      {rc/outstr.i    "string(100 * inv-line.disc,'+999999999999999')" 106 16}
      {rc/outstr.i    'I'                             122 1}  /* w-tran-type */
      {rc/outstr.i    FILLER                          123 1}  /* w-tran-code */
      {rc/outstr.i    'USD'                           124 3}  /* w-curr-type */
      {rc/outstr.i    inv-head.terms                  127 5}
      {rc/outstr.i    string(ws_due-date,'99/99/9999') 132 10}
      {rc/outstr.i    FILLER                          142 3}  /* w-bill-sys */
      {rc/outstr.i    inv-line.po-no                  145 30}
      {rc/outstr.i    '000'                           175 3}  /* w-po-line */
      {rc/outstr.i    string(inv-head.bol-no)         178 30}
      {rc/outstr.i    " 'inv-head # ' + string(inv-head.r-no)"
                                                      208 30} /* w-ref-doc */
      {rc/outstr.i    FILLER                          238 1}  /* w-ref-do-cd */
      {rc/outstr.i    ws_plant                        239 4}  /* w-plant */
      {rc/outstr.i    substring(ws_salesacct,1,5)     243 5}  /* w-dept */
      {rc/outstr.i    substring(ws_salesacct,6,6)     248 6}  /* w-account */
      {rc/outstr.i    string(inv-line.ord-no)         254 8} /*w-project */
      {rc/outstr.i    FILLER                          262 7}
      {rc/outstr.i    'E'                             269 1}  /* w-end */
      .
    PUT STREAM s-export UNFORMATTED str_buffa SKIP.
  END.    /* inv-line loop */
  FOR EACH inv-misc EXCLUSIVE
      WHERE inv-misc.r-no = inv-head.r-no
      and inv-misc.posted
      and not(inv-misc.deleted):
    IF inv-misc.tax
      THEN
    ASSIGN
      line_tax = (inv-misc.amt / tax_basis) * inv-head.t-inv-tax
      line_tax = round(line_tax, 2).
    ELSE
    ASSIGN line_tax = 0.
    str_buffa = ""  /* clear the work area */.
    ASSIGN
      {rc/outstr.i    inv-head.cust-no              1   10}
      {rc/outstr.i    "if inv-head.sold-state > '' then inv-head.sold-state
    else if inv-head.state > '' then inv-head.state
    else if avail cust then cust.state else '' "    11 2}
      {rc/outstr.i    ws_sman                       13  4}
      {rc/outstr.i    fancy_date                    17 10}
      {rc/outstr.i    ws_plant                      27  4}  /* w-plt */
      {rc/outstr.i    string(inv-head.inv-no)       31  9}
      {rc/outstr.i    string(inv-misc.line,'999')   40  3}
      {rc/outstr.i    FILLER                        43  5}  /* w-prod */
      {rc/outstr.i    string(0,'+9999999')          48  8}
      {rc/outstr.i    "string(100 * inv-misc.amt,'+999999999')" 56 10}
      {rc/outstr.i    "string(100 * line_tax,'+9999999999999')" 66 14}
      {rc/outstr.i    "string(100 * 0,'+999999999999')" 80 13}
      {rc/outstr.i    FILLER                        93  13} /* W-REF-INV */
      {rc/outstr.i    "string(100 * 0,'+999999999999999')" 106 16}
      {rc/outstr.i    'I'                           122 1}  /* w-tran-type */
      {rc/outstr.i    FILLER                        123 1}  /* w-tran-code */
      {rc/outstr.i    'USD'                         124 3}  /* w-curr-type */
      {rc/outstr.i    inv-head.terms                127 5}
      {rc/outstr.i    string(ws_due-date,'99/99/9999') 132 10}
      {rc/outstr.i    FILLER                        142 3}  /* w-bill-sys */
      {rc/outstr.i    ws_po-no                      145 30}
      {rc/outstr.i    '000'                         175 3}  /* w-po-line */
      {rc/outstr.i    string(inv-head.bol-no)       178 30}
      {rc/outstr.i    " 'inv-head # ' + string(inv-head.r-no)"
                                                    208 30} /* w-ref-doc */
      {rc/outstr.i    FILLER                        238 1}  /* w-ref-do-cd */
      {rc/outstr.i    ws_plant                      239 4}  /* w-plant */
      {rc/outstr.i    substring(inv-misc.actnum,1,5) 243 5}  /* w-dept */
      {rc/outstr.i    substring(inv-misc.actnum,6,6) 248 6}  /* w-account */
      {rc/outstr.i    string(inv-misc.ord-no)       254 8}  /* sales order */
      {rc/outstr.i    FILLER                        262 7}
      {rc/outstr.i    'E'                           269 1}  /* w-end */
      .
    PUT STREAM s-export UNFORMATTED str_buffa SKIP.
  END.    /* inv-misc loop */
END.    /* inv-head file */
OUTPUT STREAM s-export close.
