/* -------------------------------------------------- gl/gl-inc1.p  09/97 FWK */
/* GL Income Statement Default Acct#					      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
/*{sys/form/s-top.f}  */

def shared var v-ptd as date initial today no-undo.
def shared var v-s-cos-no like account.actnum no-undo.
def shared var v-e-cos-no like account.actnum no-undo.
def shared var v-s-oper-no like account.actnum no-undo.
def shared var v-e-oper-no like account.actnum no-undo.
def shared var v-s-gen-no like account.actnum no-undo.
def shared var v-e-gen-no like account.actnum no-undo.
def shared var v-s-inc-no like account.actnum no-undo.
def shared var v-e-inc-no like account.actnum no-undo.
def shared var v-s-oth-no like account.actnum no-undo.
def shared var v-e-oth-no like account.actnum no-undo.

def buffer xaccount for account.

/* COS Default Account Numbers */
find first account where account.company eq cocode and
			 account.type eq "T" and
			 account.dscr eq "COST OF SALES" no-lock no-error.
if avail account then
do:
  find next account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
  if avail account then
    assign v-s-cos-no = account.actnum.
  find next xaccount where xaccount.company eq cocode and
			  xaccount.type eq "T" and
			   xaccount.actnum > account.actnum no-lock no-error.
  if avail xaccount then
  do:
    find prev xaccount where xaccount.company eq cocode and
			  xaccount.type ne "T" no-lock no-error.
    if avail xaccount then
      assign v-e-cos-no = xaccount.actnum.
  end.
  else
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-cos-no = account.actnum.
  end.

  if v-s-cos-no > v-e-cos-no then
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-cos-no = account.actnum.
  end.
end.

/* Operating Default Account Numbers */
find first account where account.company eq cocode and
			 account.type eq "T" and
			 account.dscr eq "OPERATING EXPENSES" no-lock no-error.
if avail account then
do:
  find next account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
  if avail account then
    assign v-s-oper-no = account.actnum.
  find next xaccount where xaccount.company eq cocode and
			  xaccount.type eq "T" and
			   xaccount.actnum > account.actnum no-lock no-error.
  if avail xaccount then
  do:
    find prev xaccount where xaccount.company eq cocode and
			  xaccount.type ne "T" no-lock no-error.
    if avail xaccount then
      assign v-e-oper-no = xaccount.actnum.
  end.
  else
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-oper-no = account.actnum.
  end.

  if v-s-oper-no > v-e-oper-no then
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-oper-no = account.actnum.
  end.
end.

/* Gen & Admin Default Account Numbers */
find first account where account.company eq cocode and
			 account.type eq "T" and
			 account.dscr eq "GENERAL & ADMINISTRATIVE" 
			 no-lock no-error.
if avail account then
do:
  find next account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
  if avail account then
    assign v-s-gen-no = account.actnum.
  find next xaccount where xaccount.company eq cocode and
			  xaccount.type eq "T" and
			   xaccount.actnum > account.actnum no-lock no-error.
  if avail xaccount then
  do:
    find prev xaccount where xaccount.company eq cocode and
			  xaccount.type ne "T" no-lock no-error.
    if avail xaccount then
      assign v-e-gen-no = xaccount.actnum.
  end.
  else
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-gen-no = account.actnum.
  end.

  if v-s-gen-no > v-e-gen-no then
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-gen-no = account.actnum.
  end.
end.

/* Income Tax Expense Default Account Numbers */
find first account where account.company eq cocode and
			 account.type eq "T" and
			 account.dscr eq "INCOME TAX EXPENSE" 
			 no-lock no-error.
if avail account then
do:
  find next account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
  if avail account then
    assign v-s-inc-no = account.actnum.
  find next xaccount where xaccount.company eq cocode and
			  xaccount.type eq "T" and
			   xaccount.actnum > account.actnum no-lock no-error.
  if avail xaccount then
  do:
    find prev xaccount where xaccount.company eq cocode and
			  xaccount.type ne "T" no-lock no-error.
    if avail xaccount then
      assign v-e-inc-no = xaccount.actnum.
  end.
  else
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-inc-no = account.actnum.
  end.

  if v-s-inc-no > v-e-inc-no then
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-inc-no = account.actnum.
  end.
end.

/* Other Expense Default Account Numbers */
find first account where account.company eq cocode and
			 account.type eq "T" and
			 account.dscr eq "OTHER EXPENSE" 
			 no-lock no-error.
if avail account then
do:
  find next account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
  if avail account then
    assign v-s-oth-no = account.actnum.
  find next xaccount where xaccount.company eq cocode and
			  xaccount.type eq "T" and
			   xaccount.actnum > account.actnum no-lock no-error.
  if avail xaccount then
  do:
    find prev xaccount where xaccount.company eq cocode and
			  xaccount.type ne "T" no-lock no-error.
    if avail xaccount then
      assign v-e-oth-no = xaccount.actnum.
  end.
  else
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-oth-no = account.actnum.
  end.

  if v-s-oth-no > v-e-oth-no then
  do:
    find last account where account.company eq cocode and
			  account.type ne "T" no-lock no-error.
    if avail account then
      assign v-e-oth-no = account.actnum.
  end.
end.


/* End ---------------------------------- Copr. 1994  Advanced Software, Inc. */

