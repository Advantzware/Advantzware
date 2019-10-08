/* ------------------------------------------------- sys/rep/cust.i 12/98 JLF */
/*                                                                            */
/* cust list printout                                                         */
/*                                                                            */
/* -------------------------------------------------------------------------- */

    for EACH cust
        WHERE cust.company       GE fco
          AND cust.company       LE tco
          AND cust.cust-no GE fcust
          AND cust.cust-no LE tcust
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true)
          AND cust.type          GE ftype
          AND cust.type          LE ttype
          AND cust.sman          GE fsman
          AND cust.sman          LE tsman
          AND cust.date-field[1] GE begin_date
          AND cust.date-field[1] LE end_date
          AND cust.sman          LE tsman
          AND (cust.cust-level   EQ v-level OR v-level EQ 99)
          AND (cust.disc         NE 0 OR NOT tb_disc-only)
          AND (cust.active       NE "I" AND tb_active OR
               cust.active       EQ "I" AND tb_inactive)
        NO-LOCK
        {2}
        BY cust.{1} BY cust.cust-no:

        {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

      if detailed then do with frame cust:
        {sys/ref/cust.v}
        down.
      end.

      else 
      do with frame cust2:
        find first sman
            where sman.company eq cust.company
            and sman.sman    eq cust.sman
            no-lock no-error.

        display cust.cust-no
                cust.name
                cust.type
                cust.active
                cust.addr[1]
                cust.contact
                cust.addr[2]
                cust.sman
                sman.sname when available sman
                cust.city + ", " + cust.state + " " + cust.zip
                              format "x(30)" @ cust.city
                cust.area-code
                cust.phone
                cust.disc
                cust.cust-level.

        down.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' cust.cust-no                        '",'
               '"' cust.NAME                           '",'
               '"' cust.TYPE                           '",'
               '"' cust.active                         '",'
               '"' cust.addr[1]                        '",'
               '"' cust.addr[2]                        '",'
               '"' cust.city                           '",'
               '"' cust.state                          '",'
               '"' cust.zip                            '",'
               '"' IF cust.phone NE "" THEN
                      "(" + cust.area-code + ") "
                      + STRING(cust.phone,"999-9999")
                   ELSE ""                             '",'
               '"' cust.contact                        '",'
               '"' cust.sman                           '",'
               '"' IF AVAIL sman THEN sman.sname
                   ELSE ""                             '",'
               '"' cust.disc                           '",'
               '"' cust.cust-level                     '",'
               '"' cust.tax-id                         '",'
               '"' cust.date-field[2]                  '",'     /*Task# 12111305*/
               SKIP.
      end.
    end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
