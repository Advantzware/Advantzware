def new shared var cocode as char init "001" no-undo.
def new shared var locode as char init 'main' no-undo.
def new shared var x as int no-undo.
def new shared var y as int no-undo.
def new shared var k as int no-undo.


disable triggers for load of inv-head.

for each inv-head where bol-no eq 26886:
  for each inv-line where inv-line.r-no = inv-head.r-no:
  run n:/rcode/oe/invpost4.p (recid(inv-line), -1).
  delete inv-line.
  end.
  
  for each inv-misc where inv-misc.r-no = inv-head.r-no:
        find first oe-ordm where oe-ordm.company = inv-misc.company and
             oe-ordm.ord-no = inv-misc.ord-no and
             oe-ordm.line = inv-misc.line no-error.
        /** Re-Set billing flag to (I)nvoiced **/
        if avail oe-ordm        and
           oe-ordm.bill  eq "I" and
           inv-misc.bill eq "Y" then oe-ordm.bill = "Y".
        delete inv-misc.
   end. /* each inv-misc */
   
   delete inv-head.
end.
