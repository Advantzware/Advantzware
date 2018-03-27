/* ar-cashl.i */
&IF DEFINED(ARCASH) <> 0 &THEN
find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name eq "CASHRCPT"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
    sys-ctrl.company = g_company
    sys-ctrl.name    = "CASHRCPT"
    sys-ctrl.descrip = "Allow Overpayment of Invoices?"
    sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-overpay = sys-ctrl.log-fld.
&endif

RUN inquiry-mode.



