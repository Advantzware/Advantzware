
if month({1}) eq 2 and day({1}) ge 28 then do:
  {2} = date(2, 28, year({1}) - 1).
  if year({2}) modulo 4 eq 0 then {2} = {2} + 1. 
end.

else {2} = date(month({1}), day({1}), year({1}) - 1).
