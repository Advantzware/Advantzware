/* ------------------------------------------------- cec/pr4-mcl.i 10/96 JLF  */
/* Print mclean totals                                                        */
/* -------------------------------------------------------------------------- */
    
    v-yld = 0.
    FOR EACH eb NO-LOCK
        WHERE eb.company EQ xest.company
          AND eb.est-no  EQ xest.est-no:
      v-yld = v-yld +
              (IF eb.cust-% LT 0 THEN (-1 / eb.cust-%) ELSE eb.cust-%).
    END.

    assign
     vmcl = if {1} lt 6  then 1  else
            if {1} lt 11 then 6  else
            if {1} lt 16 then 11 else
            if {1} lt 21 then 16 else
            if {1} lt 28 then 21 else 28
     vhld = vmcl.

    put "*** T O T A L S     in Cost/M ".

    do v = vmcl to vhld + 4:
      if qtty[v] ne 0 then put qtty[v] *
                (if vmclean2 then v-yld else 1) format ">>,>>>,>>9".
    end.

    put skip
        space(17)
        "# of Releases" format "x(13)".

    do v = vmcl to vhld + 4:
      if qtty[v] ne 0 then put rels[v]          format ">>,>>>,>>9".
    end.

    put skip
        fill("-",30) format "x(30)".

    do v = vmcl to vhld + 4:
      if qtty[v] ne 0 then put unformatted fill("-",10) format "x(10)".
    end.

    put skip.

    mclean-loop:
    for each mclean.

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
        if v-skip-pct then
          put trim(mclean.descr) + " - " +
              trim(string(bmclean.cost[vmcl],"->>>9.99%")) format "x(30)".
        else
          put mclean.descr.
      end.

      else
        put mclean.descr.

      do v = vmcl to vhld + 4:
        if qtty[v] ne 0 then
          put mclean.cost[v] / (if vmclean2 and (not mclean.descr begins "    ")
                                then v-yld else 1).
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
