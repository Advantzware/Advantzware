     if first-of(rm-rcpth.po-no)  then v-first[1] = yes.
     if first-of(rm-rcpth.i-no) then v-first[2] = yes.

     assign
      v-job-no = fill(" ",6 - length(trim(rm-rdtlh.job-no))) +
                 trim(rm-rdtlh.job-no) + "-" + string(rm-rdtlh.job-no2,"99")
      v-value  = rm-rdtlh.cost * rm-rdtlh.qty.

     if v-job-no begins "-" then v-job-no = "".
     
     /*BV - 07121305*/
     /*IF tb_issue-detail THEN DO:*/

        ASSIGN 
             cCust = ""
             cINo = ""
             cIName = ""
             cVender = "" 
             ld-poqty  = 0 
             ld-rqty = 0
             ld-under-per = 0 .
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
   /*  END. /*tb_issue-detail*/*/

     FIND FIRST po-ordl 
         WHERE po-ordl.company EQ cocode
           AND po-ordl.po-no EQ int(rm-rcpth.po-no) 
           AND po-ordl.job-no EQ rm-rcpth.job-no
           AND po-ordl.job-no2 EQ rm-rcpth.job-no2
           AND po-ordl.i-no EQ rm-rcpth.i-no 
	       AND (po-ordl.LINE EQ rm-rcpth.po-line OR rm-rcpth.po-line EQ 0  )
	       AND (po-ordl.s-num EQ rm-rdtlh.s-num ) NO-LOCK NO-ERROR .
    
     ld-rqty = rm-rdtlh.qty.
     IF AVAIL po-ordl  THEN DO:
         ld-poqty = po-ordl.ord-qty.
        
       IF po-ordl.pr-qty-uom NE po-ordl.cons-uom THEN
           RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, po-ordl.cons-uom,
                              0, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                              ld-poqty,
                              OUTPUT ld-poqty).
       
       
/*        ld-rqty = po-ordl.t-rec-qty. */
      
       ld-under-per =  ((po-ordl.t-rec-qty - ld-poqty) / ld-poqty) * 100 .

     END.

     IF AVAIL po-ordl THEN
         FIND FIRST vend WHERE vend.company = cocode AND
             vend.vend-no = po-ordl.vend-no NO-LOCK NO-ERROR.
     
     IF AVAIL vend THEN 
         ASSIGN cVender = vend.NAME .
          
            ASSIGN
              v-bwt    = item.basis-w
              v-wid    = if item.r-wid eq 0 then item.s-wid else item.r-wid
              v-len    = if item.r-wid eq 0 then item.s-len else 12
              v-dep    = item.s-dep.
     
            if avail po-ordl then 
                assign
                    v-wid = po-ordl.s-wid
                    v-len = po-ordl.s-len.
            ELSE IF ITEM.i-code EQ "E" THEN do:
               FIND FIRST job-mat WHERE
                 job-mat.company EQ rm-rcpth.company AND
                 job-mat.job-no EQ rm-rcpth.job-no AND
                 job-mat.job-no2 EQ rm-rcpth.job-no2 AND
                 job-mat.i-no EQ rm-rcpth.i-no AND
                 job-mat.frm EQ rm-rdtlh.s-num
                 NO-LOCK NO-ERROR.

            IF AVAIL job-mat THEN
               ASSIGN
                  v-wid = job-mat.wid
                  v-len = job-mat.len.
            END.
       
     if rm-rcpth.pur-uom eq "TON" then
           v-ton = rm-rdtlh.qty.
         else
           run sys/ref/convquom.p (rm-rcpth.pur-uom, "TON",
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rdtlh.qty, output v-ton).
          ASSIGN cReason = "".
          
              FIND FIRST rejct-cd NO-LOCK WHERE rejct-cd.CODE EQ rm-rdtlh.reject-code[1] NO-ERROR.
             IF rm-rdtlh.reject-code[1] NE "" THEN
               ASSIGN cReason = rm-rdtlh.reject-code[1] + IF AVAIL rejct-cd AND rejct-cd.dscr NE "" THEN ( " - " + rejct-cd.dscr) ELSE "".
             ELSE cReason = "".

		 dShtWid     = 0  .
                   dShtLen     = 0  .
                  IF ITEM.i-code EQ "R" THEN do:
                      IF item.industry = "1" THEN
                          ASSIGN
                          dShtWid     = ITEM.case-w 
                          dShtLen     = ITEM.case-l.
                      ELSE
                          ASSIGN
                              dShtWid     = ITEM.s-wid 
                              dShtLen     = ITEM.s-len.
                  END.
                  ELSE DO:
                      IF AVAIL po-ordl THEN
                      FIND FIRST job-mat NO-LOCK
                          WHERE job-mat.company EQ cocode
                          AND job-mat.job-no  EQ po-ordl.job-no
                          AND job-mat.job-no2 EQ po-ordl.job-no2
                          AND job-mat.i-no EQ ITEM.i-no
                          AND job-mat.frm EQ po-ordl.s-num
                          AND job-mat.blank-no EQ po-ordl.b-num NO-ERROR .
                      IF AVAIL po-ordl AND AVAIL job-mat THEN
                          ASSIGN
                          dShtWid     = job-mat.wid 
                          dShtLen     = job-mat.len .
                  END.
            cShtSize = (trim(string(dShtLen,">,>>99.99")) + " X " + trim(string(dShtWid,">,>>99.99")) ).

      ASSIGN cDisplay = ""
             cTmpField = ""
             cVarValue = ""
             cExcelDisplay = ""
             cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
             cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             
                 CASE cTmpField:               
                     WHEN "trans-date" THEN cVarValue = STRING(rm-rcpth.trans-date,"99/99/99") .
                     WHEN "i-no" THEN cvarValue = STRING(rm-rcpth.i-no,"x(10)") .
                     WHEN "i-name" THEN cVarValue = string(rm-rcpth.i-name,"x(30)") .
                     WHEN "po-no" THEN cVarValue = STRING(rm-rcpth.po-no,"x(8)") .
                     WHEN "rita-code" THEN cVarValue = STRING(rm-rcpth.rita-code,"x(2)").
                     WHEN "v-job-no" THEN cVarValue =     STRING(v-job-no,"x(10)").
                     WHEN "tag" THEN cVarValue = STRING(rm-rdtlh.tag,"x(20)").
                     WHEN "qty" THEN cVarValue = STRING(ld-rqty,"->>>>>9.99<<").
                     WHEN "loc" THEN cVarValue = STRING(rm-rdtlh.loc,"x(5)") .
                     WHEN "loc-bin" THEN cVarValue = STRING(rm-rdtlh.loc-bin,"x(8)").
                     WHEN "loc2" THEN cVarValue =     STRING(rm-rdtlh.loc2,"x(6)").
                     WHEN "loc-bin2" THEN cVarValue = STRING(rm-rdtlh.loc-bin2,"x(8)").
                     WHEN "cost" THEN cVarValue = STRING(rm-rdtlh.cost,"->>>>>9.99<<<<").
                     WHEN "v-value" THEN cVarValue = STRING(v-value,"->>,>>>.99").

                     WHEN "poqty" THEN cVarValue = IF AVAIL po-ordl THEN STRING(ld-poqty,"->>>>,>>>.99") ELSE "".
                     WHEN "due" THEN cVarValue = IF AVAIL po-ordl THEN STRING(po-ordl.due-date,"99/99/99") ELSE "".
                     WHEN "vend" THEN cVarValue = STRING(cVender,"x(20)").
                     WHEN "per" THEN cVarValue = IF AVAIL po-ordl THEN STRING(ld-under-per,"->>>,>>9.99%") ELSE "".

                     WHEN "form" THEN cVarValue = IF AVAIL po-ordl THEN STRING(po-ordl.s-num,">>>>") ELSE "".
                     WHEN "cust" THEN cVarValue = STRING(cCust,"x(25)").
                     WHEN "fgitem" THEN cVarValue = STRING(cINo,"x(15)").
                     WHEN "itemdesc" THEN cVarValue = STRING(cIName,"x(30)").
                     WHEN "ovrpct" THEN cVarValue = IF AVAIL po-ordl THEN STRING(po-ordl.over-pct,">>9.99%") ELSE "".
                     WHEN "undpct" THEN cVarValue = IF AVAIL po-ordl THEN STRING(po-ordl.under-pct,">>9.99%") ELSE "".
                     WHEN "porcptqty" THEN cVarValue = STRING(ld-porqty,"->>>>>9.99<<").
                     WHEN "tons" THEN cVarValue = STRING(v-ton,"->>>>>9.9999").
                     WHEN "Reason" THEN cVarValue =  string(cReason,"x(30)")      .
                     WHEN "Reason-cd" THEN cVarValue = IF AVAIL rm-rdtlh AND rm-rdtlh.reject-code[1] NE "" THEN string(rm-rdtlh.reject-code[1],"x(2)") ELSE ""    .
                     WHEN "Reason-dscr" THEN cVarValue = IF AVAIL rejct-cd AND rejct-cd.dscr NE "" THEN string(rejct-cd.dscr,"x(25)") ELSE ""   .
		     	    WHEN "sheet-size" THEN cVarValue = string(cShtSize,"x(20)")    .
                 END CASE.
                 
                 cExcelVarValue = cVarValue.  
                 cDisplay = cDisplay + cVarValue +
                     FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
         END.
         PUT UNFORMATTED cDisplay SKIP.
         IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                 cExcelDisplay SKIP.
         END.

     assign
      v-qty[1] = v-qty[1] + ld-rqty 
      v-val[1] = v-val[1] + v-value
      v-cost[1] = v-cost[1] + rm-rdtlh.cost
      v-t-ton[1] = v-t-ton[1] + v-ton
      /*v-poqty[1] = v-poqty[1] + ld-poqty*/  .
     
     IF LAST-OF(rm-rcpth.po-no) THEN
         ASSIGN v-poqty[1] = v-poqty[1] + ld-poqty .

     if last-of(rm-rcpth.po-no) then do:
        if not v-first[1] AND tb_subtot then do:
        /*  underline rm-rdtlh.qty
                    v-value
              with frame itemx.
       
          display " TYPE TOTALS" @ rm-rcpth.i-name
                  v-qty[1]       @ rm-rdtlh.qty
                  v-val[1]       @ v-value
              with frame itemx. */

             ASSIGN cDisplay = ""
             cTmpField = ""
             cVarValue = ""
             cExcelDisplay = ""
             cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
             cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             
                 CASE cTmpField:               
                     WHEN "trans-date" THEN cVarValue = "" .
                     WHEN "i-no" THEN cvarValue = "" .
                     WHEN "i-name" THEN cVarValue = "" .
                     WHEN "po-no" THEN cVarValue = "" .
                     WHEN "rita-code" THEN cVarValue = "".
                     WHEN "v-job-no" THEN cVarValue = "".
                     WHEN "tag" THEN cVarValue = "".
                     WHEN "qty" THEN cVarValue = STRING(v-qty[1],"->>>>>9.99<<").
                     WHEN "loc" THEN cVarValue = "" .
                     WHEN "loc-bin" THEN cVarValue = "".
                     WHEN "loc2" THEN cVarValue = "".
                     WHEN "loc-bin2" THEN cVarValue = "".
                     WHEN "cost" THEN cVarValue = STRING(v-cost[1],"->>>>>9.99<<<<").
                     WHEN "v-value" THEN cVarValue = STRING(v-val[1],"->>,>>>.99").
                     WHEN "poqty" THEN cVarValue =  STRING(v-poqty[1],"->>>>,>>>.99") .
                     WHEN "due" THEN cVarValue = "".
                     WHEN "vend" THEN cVarValue = "".
                     WHEN "per" THEN cVarValue = "".
                     WHEN "form" THEN cVarValue = "".
                     WHEN "cust" THEN cVarValue = "".
                     WHEN "fgitem" THEN cVarValue = "".
                     WHEN "itemdesc" THEN cVarValue = "" .
                     WHEN "ovrpct" THEN cVarValue = "".
                     WHEN "undpct" THEN cVarValue = "".
                     WHEN "tons" THEN cVarValue = STRING(v-t-ton[1],"->>>>>9.9999").
                     WHEN "Reason" THEN cVarValue =  ""      .
                     WHEN "Reason-cd" THEN cVarValue = "".
                     WHEN "Reason-dscr" THEN cVarValue =  ""   .
                     WHEN "sheet-size" THEN cVarValue = ""    .
                 END CASE.
                 
                 cExcelVarValue = cVarValue.  
                 cDisplay = cDisplay + cVarValue +
                     FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
         END.
         PUT str-line SKIP.
         PUT UNFORMATTED "         PO# TOTALS "  substring(cDisplay,21,300) SKIP.
         /*IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                 cExcelDisplay SKIP.
         END.*/
        end.
       
        if not last-of(rm-rcpth.i-no) then put skip(1).
       
        assign
         v-qty[2] = v-qty[2] + v-qty[1]
         v-val[2] = v-val[2] + v-val[1]        
         v-cost[2] = v-cost[2] + v-cost[1]
         v-poqty[2] = v-poqty[2] + v-poqty[1]
         v-t-ton[2] = v-t-ton[2] + v-t-ton[1]
       
         v-qty[1] = 0
         v-val[1] = 0
         v-cost[1] = 0
         v-poqty[1] = 0
         v-t-ton[1] = 0.
     end.

     if last-of(rm-rcpth.i-no) then do:
        if not v-first[2] AND tb_subtot then do:
        /*  underline rm-rdtlh.qty
                    v-value
              with frame itemx.
       
          display " DATE TOTALS" @ rm-rcpth.i-name
                  v-qty[2]       @ rm-rdtlh.qty
                  v-val[2]       @ v-value
              with frame itemx.*/
             ASSIGN cDisplay = ""
             cTmpField = ""
             cVarValue = ""
             cExcelDisplay = ""
             cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
             cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             
                 CASE cTmpField:               
                     WHEN "trans-date" THEN cVarValue = "" .
                     WHEN "i-no" THEN cvarValue = "" .
                     WHEN "i-name" THEN cVarValue = "" .
                     WHEN "po-no" THEN cVarValue = "" .
                     WHEN "rita-code" THEN cVarValue = "".
                     WHEN "v-job-no" THEN cVarValue = "".
                     WHEN "tag" THEN cVarValue = "".
                     WHEN "qty" THEN cVarValue = STRING(v-qty[2],"->>>>>9.99<<").
                     WHEN "loc" THEN cVarValue = "" .
                     WHEN "loc-bin" THEN cVarValue = "".
                     WHEN "loc2" THEN cVarValue = "".
                     WHEN "loc-bin2" THEN cVarValue = "".
                     WHEN "cost" THEN cVarValue = STRING(v-cost[2],"->>>>>9.99<<<<").
                     WHEN "v-value" THEN cVarValue = STRING(v-val[2],"->>,>>>.99").
                     WHEN "poqty" THEN cVarValue = STRING(v-poqty[2],"->>>>,>>>.99") .
                     WHEN "due" THEN cVarValue = "".
                     WHEN "vend" THEN cVarValue = "".
                     WHEN "per" THEN cVarValue = "".
                     WHEN "form" THEN cVarValue = "".
                     WHEN "cust" THEN cVarValue = "".
                     WHEN "fgitem" THEN cVarValue = "".
                     WHEN "itemdesc" THEN cVarValue = "" .
                     WHEN "ovrpct" THEN cVarValue = "".
                     WHEN "undpct" THEN cVarValue = "".
                     WHEN "tons" THEN cVarValue = STRING(v-t-ton[2],"->>>>>9.9999").
                     WHEN "Reason" THEN cVarValue =  ""     .
                     WHEN "Reason-cd" THEN cVarValue = "".
                     WHEN "Reason-dscr" THEN cVarValue = ""   .
                     WHEN "sheet-size" THEN cVarValue = ""    .
                 END CASE.
                 
                 cExcelVarValue = cVarValue.  
                 cDisplay = cDisplay + cVarValue +
                     FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
         END.
         PUT str-line SKIP.
         PUT UNFORMATTED "         ITEM TOTALS"  substring(cDisplay,21,300) SKIP.
         /*IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                 cExcelDisplay SKIP.
         END.*/
        end.
       
        put skip(2).
       
        assign
         v-qty[3] = v-qty[3] + v-qty[2]
         v-val[3] = v-val[3] + v-val[2]
         v-cost[3] = v-cost[3] + v-cost[2]
         v-poqty[3] = v-poqty[3] + v-poqty[2]  
         v-t-ton[3] = v-t-ton[3] + v-t-ton[2]
       
         v-qty[2] = 0
         v-val[2] = 0
         v-cost[2] = 0
         v-poqty[2] = 0
         v-t-ton[2] = 0 .
     end.

     v-first[1] = no.
     if last-of(rm-rcpth.po-no) then v-first[2] = no.

     if last(rm-rcpth.i-no) then do:
        /*underline rm-rdtlh.qty
                  v-value
            with frame itemx.
       
        display "GRAND TOTALS" @ rm-rcpth.i-name
                v-qty[3]       @ rm-rdtlh.qty
                v-val[3]       @ v-value
              with frame itemx.*/
          ASSIGN cDisplay = ""
             cTmpField = ""
             cVarValue = ""
             cExcelDisplay = ""
             cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
             cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             
                 CASE cTmpField:               
                     WHEN "trans-date" THEN cVarValue = "" .
                     WHEN "i-no" THEN cvarValue = "" .
                     WHEN "i-name" THEN cVarValue = "" .
                     WHEN "po-no" THEN cVarValue = "" .
                     WHEN "rita-code" THEN cVarValue = "".
                     WHEN "v-job-no" THEN cVarValue = "".
                     WHEN "tag" THEN cVarValue = "".
                     WHEN "qty" THEN cVarValue = STRING(v-qty[3],"->>>>>9.99<<").
                     WHEN "loc" THEN cVarValue = "" .
                     WHEN "loc-bin" THEN cVarValue = "".
                     WHEN "loc2" THEN cVarValue = "".
                     WHEN "loc-bin2" THEN cVarValue = "".
                     WHEN "cost" THEN cVarValue = STRING(v-cost[3],"->>>>>9.99<<<<").
                     WHEN "v-value" THEN cVarValue = STRING(v-val[3],"->>,>>>.99").
                     WHEN "poqty" THEN cVarValue =  STRING(v-poqty[3],"->>>>,>>>.99").
                     WHEN "due" THEN cVarValue = "".
                     WHEN "vend" THEN cVarValue = "".
                     WHEN "per" THEN cVarValue = "".
                     WHEN "form" THEN cVarValue = "".
                     WHEN "cust" THEN cVarValue = "".
                     WHEN "fgitem" THEN cVarValue = "".
                     WHEN "itemdesc" THEN cVarValue = "" .
                     WHEN "ovrpct" THEN cVarValue = "".
                     WHEN "undpct" THEN cVarValue = "".
                     WHEN "tons" THEN cVarValue = STRING(v-t-ton[3],"->>>>>9.9999").
                     WHEN "Reason" THEN cVarValue =  ""      .
                     WHEN "Reason-cd" THEN cVarValue = "".
                     WHEN "Reason-dscr" THEN cVarValue =  ""   .
                     WHEN "sheet-size" THEN cVarValue = ""    .
                 END CASE.
                 
                 cExcelVarValue = cVarValue.  
                 cDisplay = cDisplay + cVarValue +
                     FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
         END.
         PUT str-line SKIP.
         PUT UNFORMATTED "         GRAND TOTALS"  substring(cDisplay,22,300) SKIP.
         /*IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                 cExcelDisplay SKIP.
         END.*/
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

