/* cc_fg-set.i - used in custom/companyCopy.i */

    BUFFER-COPY fg-set EXCEPT company s-no rec_key TO bfg-set
      ASSIGN bfg-set.s-no = getNextFGSet()
             bfg-set.company = ipCompanyTo.

    {custom\rec_key.i bfg-set}

