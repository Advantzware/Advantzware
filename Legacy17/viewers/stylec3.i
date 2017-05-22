  
  find first reftable where reftable.reftable = "STYFLU" 
                         and reftable.company = style.style
                         and reftable.loc = flute.code
                         and reftable.code = "{1}"  /* joint tab */
                         AND reftable.code2 = ""
                         no-lock no-error
                         .

  find first bf-reftable where bf-reftable.reftable = "STYFLU" 
                         and bf-reftable.company = style.style
                         and bf-reftable.loc = flute.code
                         and bf-reftable.code = "{1}"  /* joint tab */
                         and bf-reftable.code2 = "1"
                         no-lock no-error
                         .
  if avail reftable then
  do i = 1 to 12: /* must be 12, total is in 13 */
    ld-{1}-array[i] = reftable.val[i].
  end.

  if avail bf-reftable then
  do i = 1 to 8: /* must be 8 (12 + 8 = 20) */
    ld-{1}-array[12 + i] = bf-reftable.val[i].
  end.

  find first reftable where reftable.reftable = "STYSCORE" 
                         and reftable.company = style.style
                         and reftable.loc = flute.code
                         and reftable.code = "{1}"  /* joint tab */
                         no-lock no-error.

  lv-{1}-scr-type = "".
  if avail reftable then
  do i = 1 to EXTENT(lv-{1}-scr-type):
    lv-{1}-scr-type[i] = substr(reftable.dscr,i,1).
  end.
