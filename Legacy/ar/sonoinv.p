/***************************************************************************\
*****************************************************************************
**  Program: e:\ASI\PATCH\AR\sonoinv.p
**       By: Chris Heins, for Advanced Software, Inc. for Sonoco Packaging.
** Descript: (c) 1998 Sonoco, All Rights Reserved.
03.05.99 by CAH on \\ricky\asi\patch Log#0000:
1.  Added write of end of file record, which was previously in the test
driver program.   To invoke this, pass "TOTAL" as the filename, and the
number of records written in the recid.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT PARAM p_file AS char NO-UNDO. /* arinv or inv-head or TOTAL */
DEF INPUT PARAM p_rec  AS RECID NO-UNDO.
DEF OUTPUT PARAM p_count AS integer NO-UNDO.
p_count = 0.
DEF STREAM s-export.
/* include files */
{edi/rc/stringv.i}
/* contants and literals */
def var local-debug     as logical no-undo initial false. /* 9903 CAH
    set this to true to enable debugging messages on frt allocations */
DEF VAR FILLER          AS char NO-UNDO     INITIAL "".
DEF var ws_plant        AS char NO-UNDO     INITIAL "A720".
DEF VAR export_fid      AS char NO-UNDO     initial "salcca.dat".
DEF var ws_bill-sys-code AS char NO-UNDO     initial "CCA".
/* local variables */
DEF var tax_basis       AS decimal NO-UNDO  FORMAT '-99999999.99999999'.
DEF var line_tax        AS decimal NO-UNDO  FORMAT '-99999999.99999999'.
/* 9903 CAH: Added to support allocation of header freight ... */
def var frt_basis       as decimal no-undo  format '-99999999.99999999'.
def var frt_alloc       as decimal no-undo  format '-99999999.99999999'.
def var frt_lines       as decimal no-undo.
def var last_line       as int no-undo.
def var alloc_factor    as decimal no-undo decimals 10.
def var ws_line_frt as decimal no-undo.
def var allocated_frt as decimal no-undo decimals 2.
def var first_line as logical no-undo initial true.
DEF var line_count      AS int NO-UNDO.
DEF var retry_counter   AS int  NO-UNDO.
DEF var fancy_date      AS char NO-UNDO.    /* yyyy-mm-dd */
DEF var ws_due-date     AS date NO-UNDO.
DEF var ws_salesacct    AS char NO-UNDO.
DEF var ws_sman         AS char NO-UNDO.    /* misc has no sman, take 1st */
DEF var ws_po-no        AS char NO-UNDO.
DEF var xx              AS int NO-UNDO.
DEF var ws_tilde        AS char NO-UNDO initial "~~".
DEF var exp_cust-no     AS char NO-UNDO.
DEF VAR N AS INT NO-UNDO.
DEF VAR LOOPS AS INT NO-UNDO.
DEF VAR SIGN AS INT NO-UNDO.
DEF VAR WS_ACTNUM AS CHAR NO-UNDO.
IF NOT CAN-DO("total,ar-inv,inv-head", p_file) THEN
DO:
  BELL.
  RUN rc/debugmsg.p ("invlid file-type: " + p_file).
  RETURN.
END.
IF p_file = "ar-inv" THEN
DO:
  FIND ar-inv WHERE RECID(ar-inv) = p_rec EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL ar-inv THEN
  RETURN.
  IF NOT (/* ar-inv.posted AND 9903 CAH */ ar-inv.printed) THEN
  RETURN.
  /* determine if the invoice has any lines, and determine the total taxable */
  line_count = 0.
  assign
    frt_basis = 0
    frt_lines = 0
    frt_alloc = 0
    last_line = 0
    allocated_frt = 0.


  FOR EACH ar-invl NO-LOCK
      WHERE ar-invl.x-no = ar-inv.x-no
      /* AND ar-invl.posted 9903 CAH */:
    line_count = line_count + 1.
    IF ar-invl.tax = TRUE
      THEN
    ASSIGN tax_basis = tax_basis + ar-invl.amt.
    assign
      frt_basis = frt_basis + ar-invl.amt         /* total amount */
      frt_lines = frt_lines + ar-invl.t-freight.       /* billed by line */
      if ar-invl.amt > 0
      then last_line = max(last_line, ar-invl.line).   /* save for plug */
  END.
  frt_alloc = ar-inv.freight - frt_lines.

  if local-debug then do:
  message "bill" ar-inv.f-bill "header" ar-inv.freight
    "Lines" frt_lines "allocate" frt_alloc "line weights" frt_basis.
  pause.
  end.
END.    /* ar-inv */
IF p_file = "inv-head" THEN
DO:
  FIND inv-head WHERE RECID(inv-head) = p_rec EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL inv-head THEN
  RETURN.
  IF inv-head.deleted THEN
  RETURN.
  if not (/* inv-head.posted and 9903 */ inv-head.printed) then RETURN.
  ASSIGN
    ws_sman = ?
    ws_po-no = ?.
  /* determine if the invoice has any lines, and determine the total taxable */
  line_count = 0.
  FOR EACH inv-line NO-LOCK
      WHERE inv-line.r-no = inv-head.r-no
      AND NOT (inv-line.deleted):
    line_count = line_count + 1.
    IF inv-line.tax = TRUE
      THEN
    ASSIGN tax_basis = tax_basis + inv-line.t-price.
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
      AND NOT(inv-misc.deleted):
    line_count = line_count + 1.
    IF inv-misc.tax = TRUE
      THEN
    ASSIGN tax_basis = tax_basis + inv-misc.amt.
  END.
END.    /* inv-head */
/* 9903 CAH: without the p_file test this was returning error .... */
IF line_count = 0 AND NOT p_file = "total" THEN
RETURN.
IF p_file = "total" and p_rec = 0 then
return.
_open:
DO ON error UNDO, RETRY:
  IF RETRY THEN
  retry_counter = retry_counter + 1.
  /* error-status:error = FALSE. */
  OUTPUT STREAM s-export TO VALUE(export_fid) append.
  /* v8 only ...
  IF OS-ERROR > 0 THEN
  DO:
  HIDE MESSAGE NO-PAUSE.
  MESSAGE error-status:get-message(OS-ERROR).
  PAUSE.
  retry_counter = retry_counter + 1.
  IF retry_counter > 100 THEN
  DO:
  OUTPUT STREAM s-export close.
  RETURN (string(OS-ERROR)).
  END.
  UNDO _open, RETRY _open.
  END.    /* error handling */
  ... */
  IF retry_counter > 100 THEN
  DO:
    OUTPUT STREAM s-export close.
    RETURN.   /*  error (string(OS-ERROR)). */
  END.
END.    /* _open block */
IF p_file = "ar-inv" THEN
DO:
  fancy_date = string(year(ar-inv.inv-date),"9999") + '-'
  + string(month(ar-inv.inv-date),"99") + '-'
  + string(day(ar-inv.inv-date),"99").
  FIND first cust
      WHERE cust.company eq ar-inv.company
	and cust.cust-no eq ar-inv.cust-no
      NO-LOCK NO-ERROR.
  exp_cust-no = ws_bill-sys-code + ar-inv.cust-no.
  IF CUST.TYPE = "SONOCO" THEN
  LOOPS = 2.
  ELSE
  LOOPS = 1.
  N = 1.

  AR_OUTERBLOCK:
  REPEAT N = 1 TO LOOPS:
    IF N = 2 THEN
    ASSIGN
      SIGN = -1.
    ELSE
    ASSIGN
      SIGN = +1.
    assign
    first_line = true
    allocated_frt = 0.
    FOR EACH ar-invl EXCLUSIVE-LOCK
	WHERE ar-invl.x-no = ar-inv.x-no
	/* AND ar-invl.posted 9903 CAH */:
      IF ar-invl.tax
	THEN
      ASSIGN
	line_tax = (ar-invl.amt / tax_basis) * ar-inv.tax-amt
	line_tax = round(line_tax, 2).
      ELSE
      ASSIGN line_tax = 0.
      IF N = 2 THEN
      ASSIGN
	WS_ACTNUM = IF AVAIL CUST THEN CUST.TAX-ID ELSE "99999-999999".
      ELSE
      ASSIGN
	WS_ACTNUM = ar-invl.actnum.

      /* 9903 CAH: Allocate the freight differential between the lines items (frt_alloc)
	based on the weight of each line relative to the total weight (frt_basis).
	If none of the lines have weight,
	and there is allocable freight, then put it against the first line */

      alloc_factor = if frt_basis = 0 then 0 else round(ar-invl.amt / frt_basis, 10).

      ws_line_frt =
	if false /* ar-inv.f-bill = false 9903 f-bill is always 0 on thie record */ then 0
	else if frt_basis > 0 then round(ar-invl.t-freight + (frt_alloc * alloc_factor), 2)
	else if frt_basis = 0 and first_line then frt_alloc
	else 0.

      if local-debug then  message "factor" alloc_factor "line frt" ws_line_frt.

	allocated_frt = allocated_frt + ws_line_frt.

	/* plug any rounding differential to the last line ... */
	if ar-invl.line = last_line then ws_line_frt = ws_line_frt
	    + (frt_alloc - allocated_frt).

      /* end of 9903 insertion */

      str_buffa = ""  /* clear the work area */.
      ASSIGN
	{edi/rc/outstr.i    exp_cust-no                  1   10}
	{edi/rc/outstr.i    "if ar-inv.sold-state > '' then ar-inv.sold-state
    else if ar-inv.state > '' then ar-inv.state
    else if avail cust then cust.state else '' "      11 2}
	{edi/rc/outstr.i    ar-invl.sman[1]                 13  4}
	{edi/rc/outstr.i    fancy_date                      17 10}
	{edi/rc/outstr.i    ws_plant                        27  4}  /* w-plt */
	{edi/rc/outstr.i    string(ar-inv.inv-no)           31  9}
	{edi/rc/outstr.i    string(ar-invl.line,'999')      40  3}
	{edi/rc/outstr.i    FILLER                          43  5}  /* w-prod */
	{edi/rc/outstr.i    string(ar-invl.inv-qty,'+9999999')  48  8}
	{edi/rc/outstr.i    "string(SIGN * 100 * ar-invl.amt,'+999999999')" 56 10}
	{edi/rc/outstr.i    "string(SIGN * 100 * line_tax,'+9999999999999')" 66 14}
	{edi/rc/outstr.i    "string(SIGN * 100 * ws_line_frt,'+999999999999')" 80 13}  /* 9903 CAH changed from line item freight */
	{edi/rc/outstr.i    FILLER                          93  13}   /* w-ref-inv */
	{edi/rc/outstr.i    "string(SIGN * 100 * ar-invl.disc,'+999999999999999')" 106 16}
	{edi/rc/outstr.i    "IF  AVAIL cust AND cust.type = 'SONOCO' THEN 'I' ELSE '' "
	122 1}  /* w-tran-type */
	{edi/rc/outstr.i    FILLER                          123 1}  /* w-tran-code */
	{edi/rc/outstr.i    'USD'                           124 3}  /* w-curr-type */
	{edi/rc/outstr.i    ar-inv.terms                    127 5}
	{edi/rc/outstr.i    string(ar-inv.due-date,'99/99/9999') 132 10}
	{edi/rc/outstr.i    ws_bill-sys-code                142 3}
	{edi/rc/outstr.i    ar-invl.po-no                   145 30}
	{edi/rc/outstr.i    '000'                           175 3}  /* w-po-line */
	{edi/rc/outstr.i    string(ar-invl.bol-no)          178 30}
	{edi/rc/outstr.i    string(ar-invl.ord-no)          208 30} /* w-ref-doc */
	{edi/rc/outstr.i    'S'                              238 1}  /* w-ref-do-cd */
	{edi/rc/outstr.i    ws_plant                        239 4}  /* w-plant */
	{edi/rc/outstr.i    substring(WS_ACTNUM,1,5)   243 5}  /* w-dept */
	{edi/rc/outstr.i    substring(WS_ACTNUM,7,6)   248 6}  /* w-account */
	{edi/rc/outstr.i    FILLER                         254 8}  /* project */
	{edi/rc/outstr.i    FILLER                          262 7}
	{edi/rc/outstr.i    ws_tilde                        269 1}  /* w-end */
	.
      PUT STREAM s-export UNFORMATTED str_buffa SKIP.
      p_count = p_count + 1.
    END.
  END.  /* OUTERBLOCK */
END.    /* ar-inv file */
IF p_file = "inv-head" THEN
DO:
  FIND cust WHERE cust.cust-no = inv-head.cust-no NO-LOCK NO-ERROR.
  FIND terms WHERE terms.company = inv-head.company
    AND terms.t-code = inv-head.terms NO-LOCK NO-ERROR.
  exp_cust-no = ws_bill-sys-code + inv-head.cust-no.
  ws_due-date = inv-head.inv-date +
  (IF AVAIL terms THEN
  terms.net-days ELSE
  0).
  fancy_date = string(year(inv-head.inv-date),"9999") + '-'
  + string(month(inv-head.inv-date),"99") + '-'
  + string(day(inv-head.inv-date),"99").
  FIND ar-ctrl WHERE ar-ctrl.company = inv-head.company NO-LOCK NO-ERROR.
  IF CUST.TYPE = "SONOCO" THEN
  LOOPS = 2.
  ELSE
  LOOPS = 1.
  N = 1.

  assign
    frt_basis = 0
    frt_lines = 0
    frt_alloc = 0
    last_line = 0
    allocated_frt = 0.

  if inv-head.f-bill = true then do:

  for each inv-line no-lock
  WHERE inv-line.r-no = inv-head.r-no
    AND NOT(inv-line.deleted):
	assign
      frt_basis = frt_basis + inv-line.t-weight         /* total weight */
      frt_lines = frt_lines + inv-line.t-freight.       /* billed by line */
      if inv-line.t-weight > 0
      then last_line = max(last_line, inv-line.line).   /* save for plug */
  end.

  frt_alloc = inv-head.t-inv-freight - frt_lines.


  end.
  else frt_alloc = 0.

  if local-debug then do:
  message "bill" inv-head.f-bill "header" inv-head.t-inv-freight
    "Lines" frt_lines "allocate" frt_alloc "line weights" frt_basis.
  pause.
  end.

  INV_OUTERBLOCK:
  REPEAT N = 1 TO LOOPS:
    IF N = 2 THEN
    ASSIGN
      SIGN = -1
      WS_ACTNUM = IF AVAIL CUST THEN CUST.TAX-ID ELSE "99999-999999".
    ELSE
    ASSIGN
      SIGN = +1.
    assign
    first_line = true
    allocated_frt = 0.
    FOR EACH inv-line EXCLUSIVE-LOCK
	WHERE inv-line.r-no = inv-head.r-no
	AND NOT(inv-line.deleted):
      IF inv-line.tax
	THEN
      ASSIGN
	line_tax = (inv-line.t-price / tax_basis) * inv-head.t-inv-tax
	line_tax = round(line_tax, 2).
      ELSE
      ASSIGN line_tax = 0.
      FIND FIRST itemfg
	WHERE itemfg.company = inv-line.company
	AND itemfg.i-no = inv-line.i-no NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN
      DO:
	FIND fgcat
	  WHERE fgcat.company eq inv-line.company
	  AND fgcat.procat  eq itemfg.procat
	  NO-LOCK NO-ERROR.
      END.
      IF N = 1 THEN
      DO:
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
      END.

      /* 9903 CAH: Allocate the freight differential between the lines items (frt_alloc)
	based on the weight of each line relative to the total weight (frt_basis).
	If none of the lines have weight,
	and there is allocable freight, then put it against the first line */

      alloc_factor = if frt_basis = 0 then 0 else round(inv-line.t-weight / frt_basis, 10).

      ws_line_frt =
	if inv-head.f-bill = false then 0
	else if frt_basis > 0 then round(inv-line.t-freight + (frt_alloc * alloc_factor), 2)
	else if frt_basis = 0 and first_line then frt_alloc
	else 0.

      if local-debug then  message "factor" alloc_factor "line frt" ws_line_frt.

	allocated_frt = allocated_frt + ws_line_frt.

	/* plug any rounding differential to the last line ... */
	if inv-line.line = last_line then ws_line_frt = ws_line_frt
	    + (frt_alloc - allocated_frt).

      /* end of 9903 insertion */

      str_buffa = ""  /* clear the work area */.
      ASSIGN
	{edi/rc/outstr.i    exp_cust-no                1   10}
	{edi/rc/outstr.i    "if inv-head.sold-state > '' then inv-head.sold-state
    else if inv-head.state > '' then inv-head.state
    else if avail cust then cust.state else '' "    11 2}
	{edi/rc/outstr.i    inv-line.sman[1]                13  4}
	{edi/rc/outstr.i    fancy_date                      17 10}
	{edi/rc/outstr.i    ws_plant                        27  4}  /* w-plt */
	{edi/rc/outstr.i    string(inv-head.inv-no)           31  9}
	{edi/rc/outstr.i    string(inv-line.line,'999')      40  3}
	{edi/rc/outstr.i    FILLER                          43  5}  /* w-prod */
	{edi/rc/outstr.i    string(inv-line.inv-qty,'+9999999')  48  8}
	{edi/rc/outstr.i    "string(SIGN * 100 * inv-line.t-price,'+999999999')" 56 10}
	{edi/rc/outstr.i    "string(SIGN * 100 * line_tax,'+9999999999999')" 66 14}
	{edi/rc/outstr.i    "string(SIGN * 100 * ws_line_frt,'+999999999999')" 80 13}  /* 9903 CAH was inv-line.t-freight */
	{edi/rc/outstr.i    FILLER                          93  13}   /* W-REF-INV */
	{edi/rc/outstr.i    "string(SIGN * 100 * inv-line.disc,'+999999999999999')" 106 16}
	{edi/rc/outstr.i    "IF AVAIL cust AND cust.type = 'SONOCO' THEN 'I' ELSE '' "                             122 1}  /* w-tran-type */
	{edi/rc/outstr.i    FILLER                          123 1}  /* w-tran-code */
	{edi/rc/outstr.i    'USD'                           124 3}  /* w-curr-type */
	{edi/rc/outstr.i    inv-head.terms                  127 5}
	{edi/rc/outstr.i    string(ws_due-date,'99/99/9999') 132 10}
	{edi/rc/outstr.i    ws_bill-sys-code                142 3}
	{edi/rc/outstr.i    inv-line.po-no                  145 30}
	{edi/rc/outstr.i    '000'                           175 3}  /* w-po-line */
	{edi/rc/outstr.i    string(inv-head.bol-no)         178 30}
	{edi/rc/outstr.i    string(inv-line.ord-no)         208 30} /* w-ref-doc */
	{edi/rc/outstr.i    'S'                             238 1}  /* w-ref-do-cd */
	{edi/rc/outstr.i    ws_plant                        239 4}  /* w-plant */
	{edi/rc/outstr.i    substring(ws_salesacct,1,5)     243 5}  /* w-dept */
	{edi/rc/outstr.i    substring(ws_salesacct,7,6)     248 6}  /* w-account */
	{edi/rc/outstr.i    FILLER                          254 8}  /* w-project */
	{edi/rc/outstr.i    FILLER                          262 7}
	{edi/rc/outstr.i    ws_tilde                        269 1}  /* w-end */
	.
      PUT STREAM s-export UNFORMATTED str_buffa SKIP.
      p_count = p_count + 1.
      if first_line then first_line = false.
    END.    /* inv-line loop */
    FOR EACH inv-misc EXCLUSIVE
	WHERE inv-misc.r-no = inv-head.r-no
	/* AND inv-misc.posted */
	AND NOT(inv-misc.deleted):
      IF inv-misc.tax
	THEN
      ASSIGN
	line_tax = (inv-misc.amt / tax_basis) * inv-head.t-inv-tax
	line_tax = round(line_tax, 2).
      ELSE
      ASSIGN line_tax = 0.
      str_buffa = ""  /* clear the work area */.
      ASSIGN
	{edi/rc/outstr.i    exp_cust-no              1   10}
	{edi/rc/outstr.i    "if inv-head.sold-state > '' then inv-head.sold-state
    else if inv-head.state > '' then inv-head.state
    else if avail cust then cust.state else '' "    11 2}
	{edi/rc/outstr.i    ws_sman                       13  4}
	{edi/rc/outstr.i    fancy_date                    17 10}
	{edi/rc/outstr.i    ws_plant                      27  4}  /* w-plt */
	{edi/rc/outstr.i    string(inv-head.inv-no)       31  9}
	{edi/rc/outstr.i    string(inv-misc.line,'999')   40  3}
	{edi/rc/outstr.i    FILLER                        43  5}  /* w-prod */
	{edi/rc/outstr.i    string(0,'+9999999')          48  8}
	{edi/rc/outstr.i    "string(SIGN * 100 * inv-misc.amt,'+999999999')" 56 10}
	{edi/rc/outstr.i    "string(SIGN * 100 * line_tax,'+9999999999999')" 66 14}
	{edi/rc/outstr.i    "string(SIGN * 100 * 0,'+999999999999')" 80 13}
	{edi/rc/outstr.i    FILLER                        93  13} /* W-REF-INV */
	{edi/rc/outstr.i    "string(SIGN * 100 * 0,'+999999999999999')" 106 16}
	{edi/rc/outstr.i    "IF AVAIL cust AND cust.type = 'SONOCO' THEN 'I' ELSE '' "
	122 1}  /* w-tran-type */
	{edi/rc/outstr.i    FILLER                        123 1}  /* w-tran-code */
	{edi/rc/outstr.i    'USD'                         124 3}  /* w-curr-type */
	{edi/rc/outstr.i    inv-head.terms                127 5}
	{edi/rc/outstr.i    string(ws_due-date,'99/99/9999') 132 10}
	{edi/rc/outstr.i    ws_bill-sys-code              142 3}
	{edi/rc/outstr.i    ws_po-no                      145 30}
	{edi/rc/outstr.i    '000'                         175 3}  /* w-po-line */
	{edi/rc/outstr.i    string(inv-head.bol-no)       178 30}
	{edi/rc/outstr.i    string(inv-misc.ord-no)       208 30} /* w-ref-doc */
	{edi/rc/outstr.i    'S'                           238 1}  /* w-ref-do-cd */
	{edi/rc/outstr.i    ws_plant                      239 4}  /* w-plant */
	{edi/rc/outstr.i    substring(inv-misc.actnum,1,5) 243 5}  /* w-dept */
	{edi/rc/outstr.i    substring(inv-misc.actnum,7,6) 248 6}  /* w-account */
	{edi/rc/outstr.i    FILLER                         254 8}  /* project */
	{edi/rc/outstr.i    FILLER                        262 7}
	{edi/rc/outstr.i    ws_tilde                      269 1}  /* w-end */
	.
      PUT STREAM s-export UNFORMATTED str_buffa SKIP.
      p_count = p_count + 1.
    END.    /* inv-misc loop */
  END.  /* inv_outerloop */
END.    /* inv-head file */
IF p_file = "total"
  THEN
DO:
  xx = integer(p_rec).
  ASSIGN
    str_buffa = ""
    {edi/rc/outstr.i    'ZZEOF' 1   5}
    {edi/rc/outstr.i    string(xx,'9999999999')  6   13}
    {edi/rc/outstr.i    FILLER  19  505}
    {edi/rc/outstr.i    ws_tilde    524 1}.
  PUT STREAM s-export UNFORMATTED str_buffa SKIP.
END.
OUTPUT STREAM s-export close.
