
          assign
           v-sman-no = ""
           v-misc    = NO
           v-prod-cat = "".

          RELEASE itemfg.
          RELEASE prep.
          RELEASE fgcat.

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq {2}ar-invl.i-no
              no-lock no-error.
          
          if not avail itemfg then do:

            FIND FIRST prep WHERE
                 prep.company EQ cocode AND
                 prep.code    EQ {2}ar-invl.i-name
                 NO-LOCK NO-ERROR.

            IF AVAIL prep THEN
               v-prod-cat = prep.fgcat.
            ELSE
            DO:
               find first fgcat
                   where fgcat.company eq cocode
                     and fgcat.glacc   eq {2}ar-invl.actnum
                   no-lock no-error.

               IF AVAIL fgcat THEN
                  v-prod-cat = fgcat.procat.
               ELSE
                  ASSIGN
                     v-prod-cat = "MISC"
                     v-misc = YES.
            END.
          end.
          ELSE
             v-prod-cat = itemfg.procat.
          
          IF NOT v-prod-line-mode AND
             NOT(v-prod-cat GE begin_fg-cat AND
             v-prod-cat LE end_fg-cat) THEN NEXT.
          ELSE
             IF v-prod-line-mode AND 
                NOT can-find(FIRST tt-fg-cat WHERE
                                   tt-fg-cat.fg-cat = v-prod-cat) THEN
                NEXT.

          do i = 1 to 3:
             v-sman-no = if {2}ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                         else {2}ar-invl.sman[i].
            
             if NOT(v-sman-no GE begin_slsmn AND v-sman-no LE END_slsmn) OR
                 (i ne 1 and
                 (v-sman-no eq "" or {2}ar-invl.s-pct[i] eq 0)) then next.

             IF NOT v-prod-line-mode THEN
             DO:
                FIND FIRST tt-data WHERE
                     tt-data.cust-no = cust.cust-no AND
                     tt-data.sman-no = v-sman-no AND
                     tt-data.prod-cat = v-prod-cat
                     NO-ERROR.
               
                IF NOT AVAIL tt-data THEN
                DO:
                   CREATE tt-data.
                   ASSIGN tt-data.cust-no = cust.cust-no
                          tt-data.cust-name = cust.NAME
                          tt-data.sman-no = v-sman-no
                          tt-data.prod-cat = v-prod-cat.
                   RELEASE tt-data.
                END.
             END. /*NOT v-prod-line-mode*/
             ELSE
             DO:
                FIND FIRST tt-fg-cat WHERE
                     tt-fg-cat.fg-cat = v-prod-cat.

                FIND FIRST tt-data WHERE
                     tt-data.cust-no = cust.cust-no AND
                     tt-data.sman-no = v-sman-no AND
                     tt-data.prod-cat = tt-fg-cat.prodline
                     NO-ERROR.
                 
                IF NOT AVAIL tt-data THEN
                DO:
                   CREATE tt-data.
                   ASSIGN tt-data.cust-no = cust.cust-no
                          tt-data.cust-name = cust.NAME
                          tt-data.sman-no = v-sman-no
                          tt-data.prod-cat = tt-fg-cat.prodline.
                   RELEASE tt-data.
                END.
             END.

             create b-tt-report.
            
             assign
              b-tt-report.rec-id  = recid({1})
              b-tt-report.row-id  = ROWID({2}ar-invl)
              b-tt-report.key-01  = if avail itemfg then "1"
                                    else
                                    if v-misc then "2" else "3"
              b-tt-report.key-02  = v-sman-no
              b-tt-report.prod-cat  = v-prod-cat
              b-tt-report.key-09 = cust.cust-no. 
          end.
