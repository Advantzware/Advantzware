/* x = integer  xx = fraction */
if integer({1}) > decimal({1}) then x = integer({1}) - 1.
else x = integer({1}).
xxx = decimal({1}) - x.
find first fraction where fraction.number >= xxx no-lock no-error.
if xxx > 0 then do:
   if {2} = "" then {2} = string(x) + fraction.conv.
               else {2} = {2} + " x " + string(x) + fraction.conv.
end.
else do:
   if {2} = "" then {2} = string(x) + "~"".
               else {2} = {2} + " x " + string(x) + "~"".
end.
