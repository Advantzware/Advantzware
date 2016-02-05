/* ------------------------------------------------- cec/pr4-mcl.i 10/96 JLF  */
/* Print mclean totals                                                        */
/* -------------------------------------------------------------------------- */
    
    ASSIGN v-yld = 0
           mclean2yld = 0.
    FOR EACH eb FIELDS(yld-qty) NO-LOCK
        WHERE eb.company EQ xest.company
          AND eb.est-no  EQ xest.est-no
          AND eb.form-no EQ v-form-no:
      v-yld = v-yld +
              (IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty) ELSE eb.yld-qty).
    END.
    IF vmclean2 THEN
    FOR EACH eb FIELDS(yld-qty) NO-LOCK
        WHERE eb.company EQ xest.company
          AND eb.est-no  EQ xest.est-no
          AND eb.form-no > 0 AND eb.form-no <= v-form-no :
        mclean2yld = mclean2yld +
              (IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty) ELSE eb.yld-qty).
    END.
    /*IF vmclean2 THEN v-yld = mclean2yld.*/

    assign
     vmcl = if {1} lt 6  then 1  else
            if {1} lt 11 then 6  else
            if {1} lt 16 then 11 else
            if {1} lt 21 then 16 else
            if {1} lt 28 then 21 else 28
     vhld = vmcl
     v-fac-hdr = "in Cost/M" + (if v-rollfac then "SF" else "")
     v-fac-hdr = fill(" ",11 - length(trim(v-fac-hdr))) + trim(v-fac-hdr).

    put "*** T O T A L S"
        space(4)
        v-fac-hdr format "x(11)".

    do v = vmcl to vhld + 4:
      if qtty[v] ne 0 then
      DO:
         IF cerunc-dec EQ 0 THEN
            put qtty[v] *
                (if vmclean2 then v-yld else 1) format ">>,>>>,>>9".
         ELSE
            put qtty[v] *
                (if vmclean2 then v-yld else 1) format ">,>>>,>>>,>>9".
      END.
    end.

    put skip
        space(17)
        "# of Releases" format "x(13)".

    do v = vmcl to vhld + 4:
      if qtty[v] ne 0 then
      DO:
         IF cerunc-dec EQ 0 THEN
            put rels[v] format ">>,>>>,>>9".
         ELSE
            put rels[v] format ">,>>>,>>>,>>9".
      END.
    end.

    IF cerunc-dec EQ 0 THEN
       PUT SKIP
           fill("-",30) format "x(30)".
    ELSE
       put skip
           fill("-",32) format "x(32)".
    

    do v = vmcl to vhld + 4:
      if qtty[v] ne 0 then put unformatted fill("-",10) format "x(10)".
    end.

    put skip.

    mclean-loop:
    for each mclean where mclean.form-no eq v-form-no BY mclean.total-field.
        
      if mclean.rec-type eq ""            or
         (not mclean.descr begins "    ") then v-skip-pct = no.
      
      if v-skip-pct then next.

      do v = vmcl to vhld + 4:
        if mclean.cost[v] ne 0 then leave.
        if v ge vhld + 4 then next mclean-loop.
      end.

      if vsuthrlnd and mclean.descr eq "SELLING PRICE" then put skip(1).

      if mclean.descr eq "OVERALL" then put skip(3).
      
      if mclean.rec-type   ne ""          and
         (not mclean.descr begins "    ") then do:
         
        v-skip-pct = yes.
        find first bmclean
            where bmclean.rec-type eq mclean.rec-type
              and bmclean.form-no  eq v-form-no
              and bmclean.descr    begins "    "
            no-error.

        if avail bmclean then
        do v = vmcl + 1 to vhld + 4:
          if bmclean.cost[v] ne bmclean.cost[v - 1] and
             qtty[v] ne 0                           then do:
            v-skip-pct = no.
            leave.
          end.
        end.
        if v-skip-pct THEN 
           put trim(mclean.descr) + " - " +
              trim(string(bmclean.cost[vmcl],"->>>9.99%")) format "x(30)".
        
        else
         PUT mclean.descr.
      end.

      else PUT mclean.descr.

      do v = vmcl to vhld + 4:
        if qtty[v] ne 0 then
        DO:
           IF cerunc-dec EQ 0 THEN
              put mclean.cost[v] / (IF vmclean2 and (not mclean.descr begins "    ")
                                   then v-yld else 1).
           ELSE
              put mclean.cost[v] / (IF vmclean2 and (not mclean.descr begins "    ")
                                    then v-yld else 1) FORMAT "->,>>>,>>>.99".
        END.
      end.
      
      put skip.
      
      if mclean.descr eq "Direct Material" then
      for each bmclean
          where bmclean.rec-type eq "DM":

        substr(bmclean.descr,1,5) = fill(" ",5).
        put bmclean.descr.
        do v = vmcl to vhld + 4:
          if qtty[v] ne 0 then
            put bmclean.cost[v] / (if vmclean2 then v-yld else 1).
        end.
        put skip.
      end.
    end.
