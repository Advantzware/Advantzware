/* Make sure the decimal is not greater than .15 (16ths of an inch) ----------*/

if ((frame-field eq "{1}" or
     frame-field eq {2}) and
    keyfunction(lastkey) eq "return") or
   keyfunction(lastkey) eq "go"       then do:
       
  if input {3} - trunc(input {3},0) ge .16 then do:
    bell.
    message "ERROR: Cannot have more than .15 as decimal, field is"
            "(inches.16ths)".
    next-prompt {3}.
    next {4}.
  end.
end.
