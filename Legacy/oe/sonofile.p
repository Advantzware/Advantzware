/* -------------------------------------------------- oe/sonofile.p 11/99 FWK */
/* Invoicing  - File to Create invoiceh,l,m,txt  files		              */
/* -------------------------------------------------------------------------- */

def input parameter v-int as int.
def input parameter v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xar-inv for ar-inv.
def buffer xar-invl for ar-invl.

def stream s-head.
def stream s-line.
def stream s-misc.

output stream s-head to invoiceh.txt append.
output stream s-line to invoicel.txt append.
output stream s-misc to invoicem.txt append.

if v-int eq 1 then
do:
  find first xar-inv where recid(xar-inv) eq v-recid no-lock no-error.
  if avail xar-inv then
    export stream s-head xar-inv.
end.
else if v-int eq 2 then
do:
  find first xar-invl where recid(xar-invl) eq v-recid no-lock no-error.
  if avail xar-invl then
    export stream s-line xar-invl.
end.
else if v-int eq 3 then
do:
  find first xar-invl where recid(xar-invl) eq v-recid no-lock no-error.
  if avail xar-invl then
    export stream s-misc xar-invl.
end.

output stream s-head close. 
output stream s-line close.
output stream s-misc close.
