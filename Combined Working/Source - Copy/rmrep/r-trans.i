     if first-of(rm-rcpth.rita-code)  then v-first[1] = yes.
     if first-of(rm-rcpth.trans-date) then v-first[2] = yes.

     assign
      v-job-no = fill(" ",6 - length(trim(rm-rdtlh.job-no))) +
                 trim(rm-rdtlh.job-no) + "-" + string(rm-rdtlh.job-no2,"99")
      v-value  = rm-rdtlh.cost * rm-rdtlh.qty.

     if v-job-no begins "-" then v-job-no = "".
     
     /*BV - 07121305*/
     IF tb_issue-detail THEN DO:

        ASSIGN 
             cCust = ""
             cINo = ""
             cIName = "".
         FIND FIRST bf-job-hdr 
                WHERE bf-job-hdr.company EQ rm-rdtlh.company
                    AND bf-job-hdr.job-no EQ rm-rdtlh.job-no
                    AND bf-job-hdr.job-no2 EQ rm-rdtlh.job-no2
                    AND bf-job-hdr.frm     EQ rm-rdtlh.s-num
                NO-LOCK NO-ERROR.
        IF NOT AVAIL bf-job-hdr THEN
            FIND FIRST bf-job-hdr 
                WHERE bf-job-hdr.company EQ rm-rdtlh.company
                    AND bf-job-hdr.job-no EQ rm-rdtlh.job-no
                    AND bf-job-hdr.job-no2 EQ rm-rdtlh.job-no2
                NO-LOCK NO-ERROR.
        IF AVAIL bf-job-hdr THEN DO:
            cIno = bf-job-hdr.i-no.
            FIND FIRST bf-cust WHERE bf-cust.company EQ bf-job-hdr.company
                AND bf-cust.cust-no EQ bf-job-hdr.cust-no NO-LOCK NO-ERROR.
            IF AVAIL bf-cust THEN cCust = bf-cust.name. 
        END.
        IF cINo NE "" THEN DO:
            FIND FIRST bf-itemfg 
                WHERE bf-itemfg.company EQ rm-rdtlh.company
                    AND bf-itemfg.i-no EQ cINo
                NO-LOCK NO-ERROR.
            IF AVAIL bf-itemfg THEN
                cIName = bf-itemfg.i-name.
        END.
        RELEASE bf-job-hdr.
        RELEASE bf-itemfg.
        RELEASE bf-cust.
     END. /*tb_issue-detail*/

     display rm-rcpth.trans-date when first-of(rm-rcpth.trans-date)
             rm-rcpth.i-no
             rm-rcpth.i-name
             rm-rcpth.po-no
             rm-rcpth.rita-code
             rm-rdtlh.tag
             v-job-no
             rm-rdtlh.qty
             rm-rdtlh.loc
             rm-rdtlh.loc-bin
             rm-rdtlh.loc2
             rm-rdtlh.loc-bin2
             rm-rdtlh.cost
             v-value
         with frame itemx.
     down with frame itemx.

     IF tb_excel THEN DO:
        IF tb_issue-detail THEN
            EXPORT STREAM excel DELIMITER "," 
                (IF FIRST-OF(rm-rcpth.trans-date) THEN STRING(rm-rcpth.trans-date) ELSE "") 
                rm-rcpth.i-no
                rm-rcpth.i-name
                rm-rcpth.po-no
                rm-rcpth.rita-code
                v-job-no
                cCust
                cINo
                cIName
                rm-rdtlh.tag              
                rm-rdtlh.qty
                rm-rdtlh.loc
                rm-rdtlh.loc-bin
                rm-rdtlh.loc2
                rm-rdtlh.loc-bin2
                rm-rdtlh.cost
                v-value
                SKIP.
        ELSE
            EXPORT STREAM excel DELIMITER "," 
                (IF FIRST-OF(rm-rcpth.trans-date) THEN STRING(rm-rcpth.trans-date) ELSE "") 
                rm-rcpth.i-no
                rm-rcpth.i-name
                rm-rcpth.po-no
                rm-rcpth.rita-code
                v-job-no
                rm-rdtlh.tag              
                rm-rdtlh.qty
                rm-rdtlh.loc
                rm-rdtlh.loc-bin
                rm-rdtlh.loc2
                rm-rdtlh.loc-bin2
                rm-rdtlh.cost
                v-value
                SKIP.
     END.
     assign
      v-qty[1] = v-qty[1] + rm-rdtlh.qty
      v-val[1] = v-val[1] + v-value.

     if last-of(rm-rcpth.rita-code) then do:
        if not v-first[1] then do:
          underline rm-rdtlh.qty
                    v-value
              with frame itemx.
       
          display " TYPE TOTALS" @ rm-rcpth.i-name
                  v-qty[1]       @ rm-rdtlh.qty
                  v-val[1]       @ v-value
              with frame itemx.
        end.
       
        if not last-of(rm-rcpth.trans-date) then put skip(1).
       
        assign
         v-qty[2] = v-qty[2] + v-qty[1]
         v-val[2] = v-val[2] + v-val[1]
       
         v-qty[1] = 0
         v-val[1] = 0.
     end.

     if last-of(rm-rcpth.trans-date) then do:
        if not v-first[2] then do:
          underline rm-rdtlh.qty
                    v-value
              with frame itemx.
       
          display " DATE TOTALS" @ rm-rcpth.i-name
                  v-qty[2]       @ rm-rdtlh.qty
                  v-val[2]       @ v-value
              with frame itemx.
        end.
       
        put skip(2).
       
        assign
         v-qty[3] = v-qty[3] + v-qty[2]
         v-val[3] = v-val[3] + v-val[2]
       
         v-qty[2] = 0
         v-val[2] = 0.
     end.

     v-first[1] = no.
     if last-of(rm-rcpth.rita-code) then v-first[2] = no.

     if last(rm-rcpth.trans-date) then do:
        underline rm-rdtlh.qty
                  v-value
            with frame itemx.
       
        display "GRAND TOTALS" @ rm-rcpth.i-name
                v-qty[3]       @ rm-rdtlh.qty
                v-val[3]       @ v-value
              with frame itemx.
     end.

     v-code = rm-rcpth.rita-code.

     if v-code ne "T" then do:
     
        find first costtype
            where costtype.company   eq cocode
              and costtype.loc       eq rm-rdtlh.loc
              and costtype.cost-type eq item.cost-type
            no-lock no-error.

        if v-code eq "R" then
        do:
           create tt-report.
           assign
              tt-report.term-id = ""
              tt-report.key-01  = if avail costtype then costtype.inv-asset
                                  else "Cost Type not found"
              tt-report.key-02  = string(v-value,"->>,>>>,>>9.99").
        end.
        
        else
        do:
           create tt-report.
           assign
             tt-report.term-id = ""
             tt-report.key-01  = if avail costtype then costtype.cons-exp
                                 else "Cost Type not found"
             tt-report.key-02  = string(v-value,"->>,>>>,>>9.99").    
        end.
     end.
