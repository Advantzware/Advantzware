/* --------------------------------------------------- ar/ar-oreg.i 07/99 JLF */
/* applied cash on account register                                           */
/* -------------------------------------------------------------------------- */

if {1}.memo then {1}.amt-paid = {1}.amt-paid * -1.

find first ar-inv
    where ar-inv.company eq cocode
      and ar-inv.inv-no  eq {1}.inv-no
    no-error.
IF NOT AVAIL ar-inv THEN NEXT.
assign
 ar-inv.disc-taken = ar-inv.disc-taken +
		             ({1}.amt-disc * {2})
 ar-inv.paid       = ar-inv.paid +
		             (({1}.amt-paid + {1}.amt-disc) * {2}).

if "{2}" eq "1" then ar-inv.pay-date = ar-cash.check-date.

if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
  ar-inv.due       = ar-inv.net - ar-inv.paid.
else
  ar-inv.due       = ar-inv.gross - ar-inv.paid.

find first cust
    {sys/ref/custW.i}
      and cust.cust-no eq {1}.cust-no
    exclusive-lock.

assign
 cust.on-account = cust.on-account -
		           ({1}.amt-paid * {2})
 cust.acc-bal    = cust.acc-bal    -
		           (({1}.amt-paid + {1}.amt-disc) * {2}).

if ar-inv.due eq 0 then do:
  run sys/inc/daytopay.p (recid({1})). 
  cust.num-inv  = cust.num-inv + (1 * {2}).
end.

if "{2}" eq "1" then
  if cust.lpay-date lt ar-cash.check-date or cust.lpay-date eq ? then
    assign
     cust.lpay-date  = ar-cash.check-date
     cust.lpay       = ar-cash.check-amt.

if cust.acc-bal ge cust.hibal then
  assign
   cust.hibal      = cust.acc-bal
   cust.hibal-date = ar-cash.check-date.

{1}.on-account = "{2}" eq "-1".

if {1}.memo then {1}.amt-paid = {1}.amt-paid * -1.

RELEASE ar-inv.
RELEASE cust.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
