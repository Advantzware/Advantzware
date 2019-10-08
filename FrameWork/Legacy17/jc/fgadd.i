/* ----------------------------------------------------- jc/fgadd.i 02/98 JLF */
/*                                                                            */
/* Finish Goods Assigment of Fields                                           */
/*                                                                            */
/* -------------------------------------------------------------------------- */

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq xeb.cust-no
          no-lock no-error.

      create itemfg.
      assign
       itemfg.company     = cocode
       itemfg.loc         = locode
       itemfg.i-code      = "C"
       itemfg.job-date    = today
       itemfg.i-no        = /*input */ xeb.stock-no
       itemfg.est-no      = xest.est-no
       itemfg.i-name      = xeb.part-dscr1
       itemfg.part-dscr1  = xeb.part-dscr2
       itemfg.sell-uom    = "M"
       itemfg.part-no     = xeb.part-no
       itemfg.cust-no     = xeb.cust-no
       itemfg.cust-name   = if avail cust then cust.name else ""
       itemfg.cust-job-no = string(job.job-no) + "-" + string(job.job-no2)
       itemfg.pur-uom     = IF xeb.pur-man THEN "EA" ELSE "M"
       itemfg.prod-uom    = IF xeb.pur-man THEN "EA" ELSE "M"
       itemfg.stocked     = yes
       itemfg.die-no      = xeb.die-no
       itemfg.plate-no    = xeb.plate-no
       itemfg.style       = xeb.style
       itemfg.procat      = xeb.procat
       itemfg.cad-no      = xeb.cad-no
       itemfg.upc-no      = xeb.upc-no
       itemfg.spc-no      = xeb.spc-no
       itemfg.isaset      = xeb.form-no eq 0
       itemfg.pur-man     = xeb.pur-man
       itemfg.alloc       = xeb.set-is-assembled
       itemfg.type-code   = "O".

      IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

      IF v-graphic-char NE "" THEN 
      DO:
         IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
            v-graphic-char = v-graphic-char + "\".

         IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
            itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
      END.
      RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").
      RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT xeb.loc).
      {fg/set-inks1.i itemfg xeb}

      {sys/inc/fgcascnt.i itemfg xeb}
      
      {sys/inc/updfgdim.i "xeb"}
      
      IF xeb.form-no EQ 0 THEN DO:
        itemfg.pur-man = NOT CAN-FIND(FIRST x-eb
                                      WHERE x-eb.company EQ xeb.company 
                                        AND x-eb.est-no  EQ xeb.est-no
                                        AND x-eb.form-no NE 0
                                        AND x-eb.pur-man EQ NO).

        FOR EACH x-eb
            WHERE x-eb.company EQ xeb.company 
              AND x-eb.est-no  EQ xeb.est-no
              AND x-eb.form-no NE 0
            NO-LOCK BREAK BY x-eb.form-no:
          ll-one-part = FIRST(x-eb.form-no) AND LAST(x-eb.form-no).
          LEAVE.
        END.
        IF ll-one-part THEN itemfg.alloc = YES.
      END.
