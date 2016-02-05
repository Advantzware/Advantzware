
if {1}.pur-uom eq "" then {1}.pur-uom = itemfg.prod-uom.

assign
 v-binqty = {1}.qty   * (if {1}.qty   lt 0 then -1 else 1)
 v-qty    = {4}.t-qty * (if {4}.t-qty lt 0 then -1 else 1).

if {1}.pur-uom eq "EA" or
   {1}.pur-uom eq {2}  then
  v-tagcost = {1}.std-tot-cost.
else
  run sys/ref/convcuom.p({1}.pur-uom, "EA", 0, 0, 0, 0,
                         {1}.std-tot-cost, output v-tagcost).

if {2} eq "EA"        or
   {1}.pur-uom eq {2} then
  v-cost = {3}.
else
  run sys/ref/convcuom.p({2}, "EA", 0, 0, 0, 0, {3}, output v-cost).

assign
 {1}.qty          = {1}.qty + {4}.t-qty
 {1}.std-tot-cost = ((v-tagcost * v-binqty) + (v-cost * v-qty)) /
                    (v-binqty + v-qty).

if {1}.pur-uom ne "EA" and
   {1}.pur-uom ne {2}  then
  run sys/ref/convcuom.p("EA", {1}.pur-uom, 0, 0, 0, 0,
                         {1}.std-tot-cost, output {1}.std-tot-cost).

if avail job-hdr then
  assign
   v-cost           = job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                      job-hdr.std-var-cost + job-hdr.std-fix-cost
   {1}.std-mat-cost = {1}.std-tot-cost * (job-hdr.std-mat-cost / v-cost)
   {1}.std-lab-cost = {1}.std-tot-cost * (job-hdr.std-lab-cost / v-cost)
   {1}.std-var-cost = {1}.std-tot-cost * (job-hdr.std-var-cost / v-cost)
   {1}.std-fix-cost = {1}.std-tot-cost * (job-hdr.std-fix-cost / v-cost).

else
  assign
   {1}.std-mat-cost = {1}.std-tot-cost
   {1}.std-lab-cost = 0
   {1}.std-var-cost = 0
   {1}.std-fix-cost = 0.

{1}.std-tot-cost = {1}.std-mat-cost + {1}.std-lab-cost +
                   {1}.std-var-cost + {1}.std-fix-cost.
                   
