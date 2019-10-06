/* jcrep/r-wipjobN.i*/

DEF VAR v-wgt AS DEC NO-UNDO.
DEF VAR v-shtWid AS DEC NO-UNDO.
DEF VAR v-shtLen AS DEC NO-UNDO.
DEF VAR v-sheets AS DEC NO-UNDO.
DEF VAR v-msf AS DEC NO-UNDO.
DEF VAR v-ton AS DEC NO-UNDO.
DEF VAR v-mlf AS DEC NO-UNDO.
DEF VAR v-costTon AS DEC NO-UNDO.
DEF VAR v-total AS DEC NO-UNDO.
DEF VAR v-lf-qty AS DEC NO-UNDO.
DEF VAR v-vendor AS cha NO-UNDO.

FOR EACH job FIELDS(job job-no job-no2 est-no) WHERE
      job.company EQ cocode AND
      job.opened EQ YES NO-LOCK:

    FOR EACH job-mch WHERE
          job-mch.company EQ cocode and
          job-mch.job     EQ job.job AND
          job-mch.job-no  eq job.job-no and
          job-mch.job-no2 eq job.job-no2 AND
          job-mch.m-code  GE begin_mach AND
          job-mch.m-code  LE end_mach AND
          job-mch.dept    GE begin_dept AND
          job-mch.dept    LE end_dept AND
          job-mch.run-complete EQ NO
          /*USE-INDEX job*/ NO-LOCK,
    EACH job-mat FIELDS(i-no basis-w wid len qty std-cost) WHERE
                job-mat.company eq cocode AND
                job-mat.job-no = job.job-no AND
                job-mat.job-no2 = job.job-no2 AND
                job-mat.frm EQ job-mch.frm AND
                job-mat.frm GT 0 AND job-mat.len GT 0
                no-lock,
    first ITEM WHERE item.company eq cocode AND
                      item.i-no eq job-mat.i-no AND
                      ITEM.mat-type = "B"
                      NO-LOCK 
                BREAK BY job.job-no BY job.job-no2 BY job-mat.i-no:
              
       /* formular  
       MSF           (Formula =  ((Sheets x (Sht W x Sht L / 144))   / 1,000 
       Tons          (Formula=  (MSF x Basic Wt)  /  2000 Lbs
       MLF           (1000's of LF =  (Sheets x (Sht L / 12")   / 1000)
       Cost/Ton     =   Total Value / Tons
       Total Value  =  Board Cost from Bin Tab times Sheets 
       */
     IF FIRST-OF(job-mat.i-no) THEN ASSIGN v-sheets = 0
                                           v-msf = 0
                                           v-total = 0.
                                           
     IF item.r-wid > 0 THEN
     DO:
       v-lf-qty = job-mat.qty.
       
          IF ITEM.cons-uom NE "LF" THEN
             RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                   (if item.r-wid eq 0 THEN item.s-len
                                                       ELSE 12),
                                   (if item.r-wid eq 0 THEN item.s-wid
                                                       ELSE item.r-wid),
                                   item.s-dep,                    
                                   job-mat.qty, OUTPUT v-lf-qty).
    END.

       ASSIGN v-wgt = job-mat.basis-w
              v-shtWid = job-mat.wid
              v-shtLen = job-mat.len
              v-sheets = v-sheets + v-lf-qty /*job-mat.qty*/  /* mch-act.qty ?*/
              v-total = v-total + v-lf-qty * job-mat.std-cost /* rm-bin.cost ? */
              .

             
 IF LAST-OF(job-mat.i-no) THEN DO:
   ASSIGN v-msf = v-sheets * (v-shtwid * v-shtlen / 144) / 1000
          v-ton = v-msf * v-wgt / 2000 /* Lb */
          v-mlf = v-sheets * (v-shtLen / 12 ) / 1000
          v-costTon = v-total / v-ton
          .
          
   BUFFER bf-item:FIND-BY-ROWID(ROWID(item), NO-LOCK) .
   ASSIGN cDisplay = ""
          cExcelDisplay = "".
   
   DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 hField =  BUFFER bf-item:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     cDisplay = cDisplay + 
                               IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                 (cTmpField + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                               ELSE IF LENGTH(cTmpField) <  int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                 (FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                               ELSE cTmpField.
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                 END.
            END.
            ELSE DO: 
              cVarValue = "".
              CASE cTmpField:               
                 WHEN "v-job-no" THEN cVarValue = string(job.job-no + "-" + string(job.job-no2,"99")).
                 WHEN "v-vendor" THEN cVarValue = STRING(v-vendor,"X(20)").
                 WHEN "v-wgt" THEN cVarValue = STRING(v-wgt,"->>,>>9.99").
                 WHEN "v-shtWid" THEN cVarValue = STRING(v-shtWid,"->>,>>9.99<<").
                 WHEN "v-shtLen" THEN cVarValue = STRING(v-shtLen,"->>,>>9.99<<").
                 WHEN "v-sheets" THEN cVarValue =  STRING(v-sheets,"->>>,>>>,>>9.99").
                 WHEN "v-msf" THEN cVarValue =     STRING(v-msf,"->>>,>>>,>>9.99").
                 WHEN "v-ton" THEN cVarValue =     STRING(v-ton,"->>>,>>>,>>9.99").
                 WHEN "v-mlf" THEN cVarValue =     STRING(v-mlf,"->>>,>>>,>>9.99").
                 WHEN "v-costTon" THEN cVarValue = STRING(v-costTon,"->>>,>>>,>>9.99").
                 WHEN "v-total" THEN cVarValue = STRING(v-total,"->>>,>>>,>>9.99").
              END CASE.

              cExcelVarValue = cVarValue.  
              cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
            END.
   END.
        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
 END.  /* last-of */

 END.
END.  /* each job */


