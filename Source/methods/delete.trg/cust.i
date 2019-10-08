/* cust.i */

def var v-msg as char init "" no-undo.

if v-msg eq "" then do:
  find first eb
      where eb.company eq cust.company
        and eb.cust-no eq cust.cust-no
      use-index cust no-lock no-error.
  if avail eb then do:
    if cust.cust-no eq "" then
    for each eb
        where eb.company eq cust.company
          and eb.cust-no eq "":
      delete eb.
    end.
         
    else v-msg = "estimates exist for this customer".
  end.
end.
      
if v-msg eq "" then do:
  find first ar-inv
      where ar-inv.company eq cust.company
        and ar-inv.cust-no eq cust.cust-no
        and ar-inv.due     ne 0
      no-lock no-error.
  if avail ar-inv then v-msg = "open invoices exist for this customer".
end.

if v-msg eq "" then do:     
  find first oe-ord
      where oe-ord.company eq cust.company
        and oe-ord.cust-no eq cust.cust-no
        and oe-ord.stat    ne "Z" 
/*        and oe-ord.stat    ne "C"  gdm - 11020912*/
        and oe-ord.stat    ne "D"
      no-lock no-error.
  if avail oe-ord then v-msg = /* gdm - 11020912 */
                               IF oe-ord.stat EQ "C" 
                                 THEN "orders exist for this customer"
                                 ELSE "open orders exist for this customer".
end.
      
if v-msg eq "" then do:
  find first job-hdr
      where job-hdr.company eq cust.company
        and job-hdr.cust-no eq cust.cust-no
        and job-hdr.opened
      no-lock no-error.
  if avail job-hdr then v-msg = "open jobs exist for this customer".
end.
      
if v-msg eq "" then do:
  find first ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.posted     eq yes
        and ar-cashl.cust-no    eq cust.cust-no
        and ar-cashl.inv-no     eq 0
        and ar-cashl.memo       eq no
        and ar-cashl.on-account eq yes
      no-lock no-error.
  if avail ar-cashl then v-msg = "cash on-acct exists for this customer".
end.
      
if v-msg eq "" then do:
  FOR EACH shipto OF cust EXCLUSIVE-LOCK:
    DELETE shipto.
  END.
  IF CAN-FIND(FIRST soldto OF cust) THEN
  FOR EACH soldto OF cust EXCLUSIVE-LOCK:
    DELETE soldto.
  END.
  FOR EACH cust-markup OF cust EXCLUSIVE-LOCK:
    DELETE cust-markup.
  END.
end.

else do:
  message "Cannot delete, " + v-msg.
  return error.
end.
