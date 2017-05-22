/* cust.i */
DEF BUFFER b-inv-head FOR inv-head.

&IF '{&CUSTOMER-TOTALS}' NE '' &THEN
IF NOT ll-secure THEN DO:
  RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
  IF NOT ll-secure THEN RETURN "adm-error".
END.
ENABLE ptd-sales total-msf WITH FRAME {&FRAME-NAME}.
APPLY 'ENTRY' TO ptd-sales.
&ELSE
DO WITH FRAME {&FRAME-NAME}:
  ENABLE {&faxFields} tb_po-mand fi_flat-comm fl_custemail.

  IF v-inv-fmt EQ "Hughes" THEN
     ENABLE tb_show-set.

  RELEASE inv-head.
  RELEASE b-inv-head.

  IF cust.inv-meth EQ ? AND cust.cust-no NE "" THEN
  FIND FIRST inv-head NO-LOCK
      WHERE inv-head.company       EQ cust.company
        AND inv-head.cust-no       EQ cust.cust-no
        AND inv-head.inv-no        NE 0
        AND inv-head.multi-invoice EQ YES
      NO-ERROR.
  IF AVAIL inv-head THEN
  FIND FIRST b-inv-head NO-LOCK
      WHERE b-inv-head.company EQ inv-head.company
        AND b-inv-head.cust-no EQ inv-head.cust-no
        AND b-inv-head.inv-no  EQ inv-head.inv-no
        AND NOT b-inv-head.multi-invoice
      NO-ERROR.
  IF NOT AVAIL b-inv-head THEN ENABLE rd_inv-meth.
END.
&ENDIF

