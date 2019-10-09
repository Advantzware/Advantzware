/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/oe/rep
**       by: Christopher A. Heins, 07.14.95
** Descript: Salesman Performance daily, period and year to date.
**
*****************************************************************************
\***************************************************************************/

      find first oe-ordm
          where recid(oe-ordm) eq tt-report.rec-id
          no-lock no-error.
      if avail oe-ordm then do:
        find first oe-ord of oe-ordm no-lock.

        assign
         i     = int(tt-report.key-03)
         v-pct = oe-ordm.s-pct[i] / 100
         v-amt = oe-ordm.amt * v-pct.

        create w-data.
        assign
         w-data.sman     = tt-report.key-01
         w-data.ord-no   = oe-ord.ord-no
         w-data.line     = oe-ordm.line
         w-data.misc     = yes
         w-data.procat   = "P/M"
         w-data.qty      = 0
         w-data.sqft     = 0
         w-data.t-sqft   = 0
         w-data.t-tons   = 0
         w-data.item-n   = oe-ordm.dscr
         w-data.cost     = oe-ordm.cost * v-pct
         w-data.price    = v-amt
         w-data.revenue  = v-amt
         w-data.comm     = oe-ordm.s-comm[i].

        FIND FIRST prep WHERE prep.company = oe-ordm.company AND
             prep.CODE = oe-ordm.charge  NO-LOCK NO-ERROR.
        IF AVAIL prep THEN do:
            IF prep.fgcat <> "" THEN
                w-data.procat = prep.fgcat .
            ELSE
                w-data.procat = "P" .
        END.
        ELSE
            w-data.procat = "M" .


      end.

      else do:
        find first oe-ordl
            where recid(oe-ordl) eq tt-report.rec-id
            no-lock no-error.

        if avail oe-ordl then do:
          find first oe-ord of oe-ordl no-lock.

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-ordl.i-no
              no-lock no-error.

          assign
           i      = int(tt-report.key-03)
           v-pct  = oe-ordl.s-pct[i] / 100
           v-qty  = oe-ordl.qty * v-pct
           v-amt  = oe-ordl.t-price * v-pct
           v-tons = if avail itemfg then (itemfg.weight-100 * v-qty / 100 / 2000) else 0.

          IF AVAIL itemfg AND itemfg.isaset THEN
          DO:
             v-sqft = 0.

             FOR EACH fg-set FIELDS(part-no part-qty) WHERE
                 fg-set.company = itemfg.company AND
                 fg-set.set-no = itemfg.i-no
                 NO-LOCK,
                 FIRST b-itemfg FIELDS(t-sqft) WHERE
                       b-itemfg.company EQ itemfg.company AND
                       b-itemfg.i-no EQ fg-set.part-no
                       NO-LOCK:

                 v-sqft = v-sqft + (v-qty * (IF fg-set.part-qty GE 0 THEN fg-set.part-qty ELSE (-1 / fg-set.part-qty))
                                    * b-itemfg.t-sqft / 1000).
             END.
          END.
          ELSE
             ASSIGN
                v-sqft = if avail itemfg then (itemfg.t-sqft * v-qty / 1000) else 0.

          create w-data.
          assign
           w-data.sman     = tt-report.key-01
           w-data.ord-no   = oe-ord.ord-no
           w-data.line     = oe-ordl.line
           w-data.misc     = no
           v-n-lines       = v-n-lines + 1

           qm              = oe-ordl.qty / 1000
           w-data.procat   = if avail itemfg then itemfg.procat else ?
           w-data.item-n   = if avail itemfg then itemfg.i-name else ?
           w-data.qty      = v-qty
           w-data.margin   = oe-ordl.q-qty.

          IF NOT oe-ordl.is-a-component THEN
            ASSIGN
             w-data.sqft     = if avail itemfg then itemfg.t-sqft else ?
             w-data.t-sqft   = v-sqft
             w-data.t-tons   = v-tons
             w-data.price    = oe-ordl.price
             w-data.revenue  = v-amt
             w-data.cost     = oe-ordl.cost * qm
             w-data.comm     = oe-ordl.s-comm[i].
/*
          release est.
          find first job-hdr
              where job-hdr.company eq cocode
                and job-hdr.ord-no  eq oe-ordl.ord-no
                and job-hdr.job-no  eq oe-ordl.job-no
                and job-hdr.job-no2 eq oe-ordl.job-no2
                and job-hdr.i-no    eq oe-ordl.i-no
              no-lock no-error.
          if avail job-hdr then
            find first est
                where est.e-num eq job-hdr.e-num
                no-lock no-error.
          if avail est         and
             est.est-type ne 3 and
             est.est-type ne 4 and
             est.est-type ne 8 then do:
            assign
             mat         = job-hdr.std-mat-cost * qm
             lab         = (job-hdr.std-lab-cost +
                            job-hdr.std-var-cost +
                            job-hdr.std-fix-cost) * qm

             w-data.cost = w-data.cost +
                           if ce-ctrl.spec-%[1] gt 0 and
                              ce-ctrl.spec-%[1] le 1 then
                             (oe-ordl.cost * qm * ce-ctrl.spec-%[1])
                           else ce-ctrl.spec-%[1]

             w-data.cost = w-data.cost +
                           if ce-ctrl.spec-%[2] gt 0 and
                              ce-ctrl.spec-%[2] le 1 then
                             (oe-ordl.cost * qm * ce-ctrl.spec-%[2])
                           else ce-ctrl.spec-%[2]

             w-data.cost = w-data.cost +
                           if ce-ctrl.spec-%[3] gt 0 and
                              ce-ctrl.spec-%[3] le 1 then
                             (oe-ordl.cost * qm * ce-ctrl.spec-%[3])
                           else ce-ctrl.spec-%[3].

            do v = 1 to 6:
              if ce-ctrl.mat-cost[v] ge mat then do:
                w-data.cost = w-data.cost + (mat * ce-ctrl.mat-pct[v] / 100).
                leave.
              end.
            end.

            do v = 1 to 6:
              if ce-ctrl.lab-cost[v] ge lab then do:
                w-data.cost = w-data.cost + (lab * ce-ctrl.lab-pct[v] / 100).
                leave.
              end.
            end.

            if est.est-type eq 1 or est.est-type eq 5 then do:
              release style.
              find first eb
                  where eb.e-num    eq job-hdr.e-num
                    and eb.stock-no eq job-hdr.i-no
                  no-lock no-error.
              if avail eb then
              find first style
                  where style.company eq cocode
                    and style.style   eq eb.style
                  no-lock no-error.
              if avail style and style.royalty ne 0 then
                w-data.cost = w-data.cost +
                              if style.royalty gt 0 and
                                 style.royalty le 1 then
                                (oe-ordl.cost * qm * style.royalty)
                              else (style.royalty * qm).
            end.
          end.

          assign
           w-data.cost = w-data.cost * v-pct
           w-data.cost = w-data.cost + (v-amt * oe-ord.s-comm[i] / 100).
*/
        end.
      end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

