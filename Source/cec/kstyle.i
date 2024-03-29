/* ---------------------------------------------------- cec/kstyle.i 12/93 cd */
/*                                                                            */
/* -------------------------------------------------------------------------- */

find first sys-ctrl
    where sys-ctrl.company eq cocode
     and sys-ctrl.name    eq "ROUND"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "ROUND"
   sys-ctrl.descrip = "Round Up Scoring Allowances?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.


/* get rid of any blank/space or invalid character */
do i = 1 to length(tmpstore):
   if keycode(substring(tmpstore,i,1)) = 32 or
      index("0123456789.+-*/LWDTFJBOSI",substring(tmpstore,i,1)) = 0 then
      tmpstore = substring(tmpstore,1,i - 1) + substring(tmpstore,i + 1).
end.

assign num = 0     /* zero arrays */
       op  = ""
    nextop = 1.

loop:
do i = 1 to length(tmpstore):

   kar = substring(tmpstore,i,1).
   if index("+-/*",kar) > 0 then
   do:
      assign op[nextop] = kar
             nextop     = nextop + 1.
      next.
   end.
   if index("LWDJTSFBOIXY",kar) > 0 and num[nextop] = 0 then do:
      if kar = "L" then num[nextop] = {&l} .
      if kar = "W" then num[nextop] = {&w} .
      if kar = "D" then num[nextop] = {&d} .
      if kar = "J" then num[nextop] = {&g} .
      if kar = "T" then num[nextop] = {&t} .
      if kar = "S" then num[nextop] = {&k} .
      if kar = "F" then num[nextop] = {&f} .
      if kar = "B" then num[nextop] = {&b} .
      if kar = "O" then num[nextop] = {&o} .
      if kar = "I" then num[nextop] = {&i} .
      next.
   end.
   else if index("LWDJTSFBOIXY",kar) > 0 and num[nextop] ne 0 then do:
      if kar = "L" then num[nextop] = {&l} * num[nextop].
      if kar = "W" then num[nextop] = {&w} * num[nextop].
      if kar = "D" then num[nextop] = {&d} * num[nextop].
      if kar = "J" then num[nextop] = {&g} * num[nextop].
      if kar = "T" then num[nextop] = {&t} * num[nextop].
      if kar = "S" then num[nextop] = {&k} * num[nextop].
      if kar = "F" then num[nextop] = {&f} * num[nextop].
      if kar = "B" then num[nextop] = {&b} * num[nextop].
      if kar = "O" then num[nextop] = {&o} * num[nextop].
      if kar = "I" then num[nextop] = {&i} * num[nextop].
      next.
   end.
   else do:
      curnum = "".
      do while (keycode(kar) >= 48 and keycode(kar) <= 57) or keycode(kar) = 46:
         assign curnum = curnum + kar
                     i = i + 1
                   kar = substring(tmpstore,i,1).
      end.
      i = i - 1.
      if num[nextop] ne 0
      then num[nextop] = decimal(curnum) * num[nextop].
      else num[nextop] = decimal(curnum).
   end.
end.
do i = 1 to EXTENT(num):
  num[i] = num[i] * li-16-32.
  if sys-ctrl.log-fld then do:
    {sys/inc/roundup.i num[i]}
  end.
  ELSE IF v-cecscrn-char NE "Decimal" THEN
       num[i] = truncate(num[i],0).
  num[i] = num[i] / li-16-32.
end.

formule[{&for}] = num[1].
do i = 2 to EXTENT(op):
        if op[i - 1] = "+" then formule[{&for}] = formule[{&for}] + num[i].
   else if op[i - 1] = "-" then formule[{&for}] = formule[{&for}] - num[i].
end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
