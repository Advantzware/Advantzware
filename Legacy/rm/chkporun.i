/* rm/chkporun.i   Check PO qty overrun */
    
    DEF VAR pox AS INT NO-UNDO.
    DEF VAR v-rec-qty AS DEC NO-UNDO.
    DEF VAR ld AS DEC NO-UNDO.

    DEF BUFFER xrm-rctd FOR rm-rctd.


    RELEASE po-ordl.

    find first po-ord where po-ord.company eq cocode
                        and po-ord.po-no   eq int(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        no-lock no-error.
    pox = if avail po-ord then po-ord.po-no else 0.
    if pox ne 0 then do:
        find first po-ordl
            where po-ordl.company   eq cocode
              and po-ordl.po-no     eq pox
              and po-ordl.i-no      eq rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
              and po-ordl.job-no    eq rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
              and po-ordl.job-no2   eq int(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
              and po-ordl.s-num     eq int(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
              AND po-ordl.item-type EQ YES
            no-lock no-error.
        if not avail po-ordl then pox = 0.
    end.
      
    if avail po-ordl then do:
        FIND FIRST item
            WHERE item.company EQ po-ordl.company
              AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

        ld = input rm-rctd.qty.

        IF input rm-rctd.pur-uom NE po-ordl.cons-uom THEN
          run sys/ref/convquom.p((input rm-rctd.pur-uom), po-ordl.cons-uom,
                                 item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                 ld, output ld).

        v-rec-qty = po-ordl.t-rec-qty + ld.
        
        for each xrm-rctd
            where xrm-rctd.company   eq cocode
              and xrm-rctd.i-no      eq rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
              and xrm-rctd.rita-code eq "R"
              and xrm-rctd.po-no     eq (rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
              and xrm-rctd.job-no    eq rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
              and xrm-rctd.job-no2   eq int(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
              and xrm-rctd.s-num     eq int(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
              AND RECID(xrm-rctd) <> RECID(rm-rctd)   NO-LOCK:

           ld = xrm-rctd.qty.

           IF xrm-rctd.pur-uom NE po-ordl.cons-uom THEN
             run sys/ref/convquom.p(rm-rctd.pur-uom, po-ordl.cons-uom,
                                    item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    ld, output ld).

           v-rec-qty = v-rec-qty + ld.
        end.

        if v-rec-qty gt po-ordl.cons-qty * (1 + (po-ordl.over-pct / 100)) THEN
        do:          
           message "The PO qty + overrun has been exceeded. Do you want to re-enter?"
                  VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
           IF ll-ans  THEN DO:
               APPLY "entry" TO rm-rctd.qty.
               RETURN NO-APPLY.
           END.          
        end.
        ELSE IF rmunderover-cha EQ "UnderRuns and OverRun" AND v-rec-qty LT po-ordl.cons-qty - (po-ordl.cons-qty * po-ordl.under-pct / 100) THEN
        DO:
           MESSAGE "The PO qty is less than the underrun. Do you want to re-enter?"
              VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans2 AS LOG.
           IF ll-ans2 THEN DO:
               APPLY "entry" TO rm-rctd.qty.
               RETURN NO-APPLY.
           END.
        END.
    end.
