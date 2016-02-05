/* --------------------------------------------------- gl/gl-jreg.i 03/97 JLF */
/*                                                                            */
/* g/l journal entry edit register                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */
        
ASSIGN post-line-error = NO.

IF {2} = 1 THEN DO:

FOR EACH gl-jrn WHERE gl-jrn.company EQ cocode
     AND gl-jrn.posted  EQ (ip-post EQ ?)
     AND gl-jrn.recur   EQ NO
     AND gl-jrn.journal GE fjrnl
     AND gl-jrn.journal LE tjrnl
     AND gl-jrn.tr-date GE begin_date
     AND gl-jrn.tr-date LE end_date
    NO-LOCK,

    EACH gl-jrnl OF gl-jrn NO-LOCK
        BREAK BY gl-jrn.journal 
        BY gl-jrnl.actnum
        BY gl-jrn.tr-date
        WITH FRAME a{2} WIDTH 141 STREAM-IO:

    IF FIRST-OF(gl-jrn.journal) THEN DO:
        /*DISPLAY gl-jrn.journal LABEL "Journal".*/
        ASSIGN
           v-jrnal-2 = STRING(gl-jrn.journal)
           tbal = tbal + gl-jrn.tr-amt. /* 9508 cah */
        ASSIGN
            cred-tot = 0
            deb-tot = 0 .
    END.
    ELSE 
       v-jrnal-2 = "".

    FIND FIRST account WHERE 
               account.company = cocode AND
               account.actnum  = gl-jrnl.actnum NO-LOCK NO-ERROR.
    IF NOT AVAIL account OR account.TYPE = "T" THEN DO: 
        ASSIGN post-line-error = YES.
        MESSAGE "The account number below is either invalid or a Title account - cannot post!".
        IF tb_excel THEN 
            PUT STREAM s-temp UNFORMATTED
                "The account number below is either invalid or a Title account - cannot post!"
            SKIP.
    END.

    ASSIGN
        deb = 0
        cred = 0
        lv-rev = IF gl-jrn.reverse THEN "R"       ELSE
                 IF gl-jrn.from-reverse THEN "PR" ELSE " ".

    IF gl-jrnl.tr-amt > 0 THEN 
        deb = gl-jrnl.tr-amt.
    ELSE 
        cred = 0 - gl-jrnl.tr-amt.

        ASSIGN
            cred-tot = cred-tot + cred 
            deb-tot = deb-tot + deb  
            tot-cred-t = tot-cred-t + cred .
            tot-deb-t = tot-deb-t + deb  .

                 ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "jur"    THEN cVarValue = string(TRIM(v-jrnal-2)) .
                         WHEN "descri"   THEN cVarValue = string(gl-jrnl.dscr,"x(28)").
                         WHEN "date"   THEN cVarValue = STRING(gl-jrn.tr-date).
                         WHEN "acco"  THEN cVarValue = STRING(account.actnum,"x(22)") .
                         WHEN "desc"   THEN cVarValue = STRING(account.dscr,"x(21)") .
                         WHEN "debi"  THEN cVarValue = IF deb NE 0 THEN STRING(deb,"->>,>>>,>>9.99") ELSE "" .
                         WHEN "credit"   THEN cVarValue = IF cred NE 0 THEN STRING(cred,"->>,>>>,>>9.99") ELSE ""  .
                         WHEN "rev"   THEN cVarValue = STRING(lv-rev,"X(3)") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM s-temp UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
    
    /*DISPLAY 
        gl-jrnl.dscr LABEL "Description"
        gl-jrn.tr-date COLUMN-LABEL "Date"
        account.actnum LABEL "Account"
        account.dscr   FORMAT "x(22)"
        deb  (TOTAL BY gl-jrn.journal) WHEN deb  NE 0
        cred (TOTAL BY gl-jrn.journal) WHEN cred NE 0
        lv-rev FORMAT "X(2)".

    IF tb_excel THEN 
        PUT STREAM s-temp UNFORMATTED
            TRIM(v-jrnal-2)                    + "," +
            TRIM(STRING(gl-jrnl.dscr))      + "," +
            TRIM(STRING(gl-jrn.tr-date))    + "," +
            TRIM(STRING(account.actnum))    + "," +
            TRIM(STRING(account.dscr))      + "," +
            TRIM(STRING(deb))               + "," +
            TRIM(STRING(cred))              + "," +
            TRIM(lv-rev)         
        SKIP. */
         IF LAST-OF(gl-jrn.journal) THEN DO:
             PUT str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "jur"    THEN cVarValue = "" .
                         WHEN "descri"   THEN cVarValue = "".
                         WHEN "date"   THEN cVarValue = "".
                         WHEN "acco"  THEN cVarValue = "" .
                         WHEN "desc"   THEN cVarValue = ""  .
                         WHEN "debi"  THEN cVarValue = STRING(deb-tot,"->>,>>>,>>9.99") .
                         WHEN "credit"   THEN cVarValue = STRING(cred-tot,"->>,>>>,>>9.99") .
                         WHEN "rev"   THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "        Total "   SUBSTRING(cDisplay,15,300)  SKIP(1).

         END.  /* last of journal */

         IF LAST(gl-jrn.journal) THEN DO:
             PUT str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "jur"    THEN cVarValue = "" .
                         WHEN "descri"   THEN cVarValue = "".
                         WHEN "date"   THEN cVarValue = "".
                         WHEN "acco"  THEN cVarValue = "" .
                         WHEN "desc"   THEN cVarValue = ""  .
                         WHEN "debi"  THEN cVarValue = STRING(tot-deb-t,"->>,>>>,>>9.99") .
                         WHEN "credit"   THEN cVarValue = STRING(tot-cred-t,"->>,>>>,>>9.99") .
                         WHEN "rev"   THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "       Grand Total "   SUBSTRING(cDisplay,20,300)  SKIP(1).
            IF glBalanceCheck THEN
                glOutOfBalance = tot-deb-t NE tot-cred-t.
            
         END.  /* last journal */

    ASSIGN
        deb = 0 
        cred = 0
        v-postable = YES.
END. /* for each */
end. /* If*/
ELSE DO:
FOR EACH gl-jrn WHERE gl-jrn.company EQ cocode
     AND gl-jrn.posted  EQ (ip-post EQ ?)
     AND gl-jrn.recur   EQ NO
     AND gl-jrn.journal GE fjrnl
     AND gl-jrn.journal LE tjrnl
     AND gl-jrn.tr-date GE begin_date
     AND gl-jrn.tr-date LE end_date
     NO-LOCK,

    EACH gl-jrnl OF gl-jrn NO-LOCK
        BREAK BY gl-jrn.journal 
        BY gl-jrnl.line
        BY gl-jrnl.actnum
        BY gl-jrn.tr-date
        WITH FRAME b{2} WIDTH 141 STREAM-IO:

    IF FIRST-OF(gl-jrn.journal) THEN DO:
        DISPLAY gl-jrn.journal LABEL "Journal".
        ASSIGN
           v-jrnal-2 = STRING(gl-jrn.journal)
           tbal = tbal + gl-jrn.tr-amt. /* 9508 cah */
        ASSIGN
            cred-tot = 0
            deb-tot = 0 .
    END.
    ELSE 
       v-jrnal-2 = "".

    FIND FIRST account WHERE 
               account.company = cocode AND
               account.actnum  = gl-jrnl.actnum NO-LOCK NO-ERROR.
    IF NOT AVAIL account OR account.TYPE = "T" THEN DO: 
        ASSIGN post-line-error = YES.
        MESSAGE "The account number below is either invalid or a Title account - cannot post!".
        IF tb_excel THEN 
            PUT STREAM s-temp UNFORMATTED
                "The account number below is either invalid or a Title account - cannot post!"
            SKIP.
    END.

    ASSIGN
        deb = 0
        cred = 0
        lv-rev = IF gl-jrn.reverse THEN "R"       ELSE
                 IF gl-jrn.from-reverse THEN "PR" ELSE " ".

    IF gl-jrnl.tr-amt > 0 THEN 
        deb = gl-jrnl.tr-amt.
    ELSE 
        cred = 0 - gl-jrnl.tr-amt.

    ASSIGN
            cred-tot = cred-tot + cred 
            deb-tot = deb-tot + deb  
            tot-cred-t = tot-cred-t + cred .
            tot-deb-t = tot-deb-t + deb.

                 ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "jur"    THEN cVarValue = string(TRIM(v-jrnal-2)) .
                         WHEN "descri"   THEN cVarValue = string(gl-jrnl.dscr,"x(28)").
                         WHEN "date"   THEN cVarValue = STRING(gl-jrn.tr-date).
                         WHEN "acco"  THEN cVarValue = STRING(account.actnum,"x(22)") .
                         WHEN "desc"   THEN cVarValue = STRING(account.dscr,"x(21)") .
                         WHEN "debi"  THEN cVarValue = IF deb NE 0 THEN STRING(deb,"->>,>>>,>>9.99") ELSE "" .
                         WHEN "credit"   THEN cVarValue = IF cred NE 0 THEN STRING(cred,"->>,>>>,>>9.99") ELSE ""  .
                         WHEN "rev"   THEN cVarValue = STRING(lv-rev,"X(3)") .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM s-temp UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
    
    /*DISPLAY 
        gl-jrnl.dscr LABEL "Description"
        gl-jrn.tr-date COLUMN-LABEL "Date"
        account.actnum LABEL "Account"
        account.dscr   FORMAT "x(22)"
        deb  (TOTAL BY gl-jrn.journal) WHEN deb  NE 0
        cred (TOTAL BY gl-jrn.journal) WHEN cred NE 0
        lv-rev FORMAT "X(2)".

    IF tb_excel THEN 
        PUT STREAM s-temp UNFORMATTED
            TRIM(v-jrnal-2)                    + "," +
            TRIM(STRING(gl-jrnl.dscr))      + "," +
            TRIM(STRING(gl-jrn.tr-date))    + "," +
            TRIM(STRING(account.actnum))    + "," +
            TRIM(STRING(account.dscr))      + "," +
            TRIM(STRING(deb))               + "," +
            TRIM(STRING(cred))              + "," +
            TRIM(lv-rev)         
        SKIP. */

      IF LAST-OF(gl-jrn.journal) THEN DO:
             PUT str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "jur"    THEN cVarValue = "" .
                         WHEN "descri"   THEN cVarValue = "".
                         WHEN "date"   THEN cVarValue = "".
                         WHEN "acco"  THEN cVarValue = "" .
                         WHEN "desc"   THEN cVarValue = ""  .
                         WHEN "debi"  THEN cVarValue = STRING(deb-tot,"->>,>>>,>>9.99") .
                         WHEN "credit"   THEN cVarValue = STRING(cred-tot,"->>,>>>,>>9.99") .
                         WHEN "rev"   THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "        Total " SUBSTRING(cDisplay,15,300)  SKIP(1).

         END.  /* last of journal */

         IF LAST(gl-jrn.journal) THEN DO:
             PUT str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "jur"    THEN cVarValue = "" .
                         WHEN "descri"   THEN cVarValue = "".
                         WHEN "date"   THEN cVarValue = "".
                         WHEN "acco"  THEN cVarValue = "" .
                         WHEN "desc"   THEN cVarValue = ""  .
                         WHEN "debi"  THEN cVarValue = STRING(tot-deb-t,"->>,>>>,>>9.99") .
                         WHEN "credit"   THEN cVarValue = STRING(tot-cred-t,"->>,>>>,>>9.99") .
                         WHEN "rev"   THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "       Grand Total "   SUBSTRING(cDisplay,20,300)  SKIP(1).
            IF glBalanceCheck THEN
                glOutOfBalance = tot-deb-t NE tot-cred-t.
         END.  /* last journal */

     ASSIGN
        deb = 0 
        cred = 0
        v-postable = YES.
END. /* for each */

END.

IF post-line-error THEN ASSIGN v-postable = NO.
 
/* end ----------------------------------- Copr. 1997  Advanced Software Inc. */
