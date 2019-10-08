/*  defined in procedure's definition section for GUI
def workfile q-sort{&fil} no-undo field qty as dec field rel as int.
*/
for each q-sort{&fil}:
  delete q-sort{&fil}.
end.

do {&sub} = 1 to {&ext}:
  if {&qty}[{&sub}] ne 0 then do:
    create q-sort{&fil}.
    assign
     q-sort{&fil}.qty = {&qty}[{&sub}]
     q-sort{&fil}.rel = if {&rel}[{&sub}] eq 0 then 1 else {&rel}[{&sub}].
  end.
end.

assign
 {&sub} = 0
 {&qty} = 0
 {&rel} = 0.

for each q-sort{&fil} break by q-sort{&fil}.qty BY q-sort{&fil}.rel:
  if first-of(q-sort{&fil}.rel) then
    assign
     {&sub}         = {&sub} + 1
     {&qty}[{&sub}] = q-sort{&fil}.qty
     {&rel}[{&sub}] = q-sort{&fil}.rel.
end.
