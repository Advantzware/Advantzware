  
  find first reftable where reftable.reftable = "STYFLU" 
                         and reftable.company = style.style
                         and reftable.loc = flute.code
                         and reftable.code = "{1}"  /* joint tab */
                         AND reftable.CODE2 = ""
                         no-error
                         .
  if not avail reftable then do:
     create reftable.
     assign reftable.reftable = "STYFLU"
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = "{1}"
            reftable.code2 = "".  
  end.

  find first bf-reftable where bf-reftable.reftable = "STYFLU" 
                         and bf-reftable.company = style.style
                         and bf-reftable.loc = flute.code
                         and bf-reftable.code = "{1}"  /* joint tab */
                         and bf-reftable.code2 = "1"
                         no-error
                         .
  if not avail bf-reftable then do:
     create bf-reftable.
     assign bf-reftable.reftable = "STYFLU"
            bf-reftable.company = style.style
            bf-reftable.loc = flute.code
            bf-reftable.code = "{1}"
            bf-reftable.code2 = "1".  
  end.

  reftable.val[13] = 0.
  DO i = 1 TO EXTENT(ld-{1}-array):
    IF i LE 12 THEN
      reftable.val[i] = ld-{1}-array[i].
    ELSE
      bf-reftable.val[i - 12] = ld-{1}-array[i].

    reftable.val[13] = reftable.val[13] + ld-{1}-array[i].
  END.

  find first reftable where reftable.reftable = "STYSCORE" 
                         and reftable.company = style.style
                         and reftable.loc = flute.code
                         and reftable.code = "{1}"  /* joint tab */
                         no-error
                         .
  if not avail reftable then do:
     create reftable.
     assign reftable.reftable = "STYSCORE"
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = "{1}".  
  end.

  reftable.dscr = "".
  DO i = 1 TO EXTENT(lv-{1}-scr-type):
    reftable.dscr = reftable.dscr + STRING(lv-{1}-scr-type[i],"X").
  END.
