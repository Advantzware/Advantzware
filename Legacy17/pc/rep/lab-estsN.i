

if last-of(work-rep.{1}) then
do:
   /*display fill("-", 113) format "x(113)" at 18 skip
           "*" at 10 work-rep.{1} "TOTALS"
           {3}r-act-hrs at 32
           {3}m-act-hrs
           {3}dt-chg-hrs
           {3}dt-nochg-hrs
           ({3}r-act-hrs + {3}m-act-hrs +
           {3}dt-chg-hrs + {3}dt-nochg-hrs)
           {3}r-std-hrs
           {3}m-std-hrs
           ({3}r-std-hrs + {3}m-std-hrs) (({3}r-act-hrs + {3}m-act-hrs +
           {3}dt-chg-hrs + {3}dt-nochg-hrs) - ({3}r-std-hrs + {3}m-std-hrs))
                    format ">>>>>>9.99-"
           skip
           fill("=", 113) format "x(113)" at 18
         with frame {3}sum stream-io width 132 no-box no-labels down.

   IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
          SKIP(1)
          '"' ""                                        '",'
          '"' ""                                        '",'
          '"' work-rep.{1} + " TOTALS"                  '",'
          '"' ""                                        '",'
          '"' ""                                        '",'
          '"' STRING({3}r-act-hrs)                      '",'
          '"' STRING({3}m-act-hrs)                      '",'
          '"' STRING({3}dt-chg-hrs)                     '",'
          '"' STRING({3}dt-nochg-hrs)                   '",'
          '"' STRING({3}r-act-hrs + {3}m-act-hrs +
               {3}dt-chg-hrs + {3}dt-nochg-hrs)         '",'
          '"' STRING({3}r-std-hrs)                      '",'
          '"' STRING({3}m-std-hrs)                      '",'
          '"' ({3}r-std-hrs + {3}m-std-hrs)             '",'
          '"' STRING((({3}r-act-hrs + {3}m-act-hrs +
                       {3}dt-chg-hrs + {3}dt-nochg-hrs) -
                      ({3}r-std-hrs + {3}m-std-hrs)),">>>>>>9.99-") '",'
          SKIP(1). */

    PUT SKIP str-line SKIP .

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                                      
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "chrg"         THEN cVarValue = "" .
                         WHEN "mach"        THEN cVarValue = "" .
                         WHEN "job"           THEN cVarValue = "" .
                         WHEN "frm"          THEN cVarValue = "" .
                         WHEN "blnk"          THEN cVarValue = "" .
                         WHEN "act-run"       THEN cVarValue = STRING({3}r-act-hrs,">>>>>>9.99").
                         WHEN "act-mr"       THEN cVarValue = STRING({3}m-act-hrs,">>>>>>9.99").                 
                         WHEN "dwn-chrg"      THEN cVarValue = STRING({3}dt-chg-hrs,">>>>>>9.99").
                         WHEN "dwn-no"        THEN cVarValue = STRING({3}dt-nochg-hrs,">>>>>>9.99").
                         WHEN "ttl-act"       THEN cVarValue = STRING(({3}r-act-hrs + {3}m-act-hrs + {3}dt-chg-hrs + {3}dt-nochg-hrs),">>>>>>9.99-") .
                         WHEN "est-run"       THEN cVarValue = STRING({3}r-std-hrs,">>>>>>9.99").
                         WHEN "est-mr"        THEN cVarValue = STRING({3}m-std-hrs,">>>>>>9.99").
                         WHEN "ttl-est"         THEN cVarValue = string(({3}r-std-hrs + {3}m-std-hrs),">>>>>>9.99-"). 
                         WHEN "lbr-var"         THEN cVarValue = STRING((({3}r-act-hrs + {3}m-act-hrs + {3}dt-chg-hrs + {3}dt-nochg-hrs) - ({3}r-std-hrs + {3}m-std-hrs)),">>>>>>9.99-") . 
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " *   "   work-rep.{1}  "   TOTALS" substring(cDisplay,18,300) SKIP(1).
            IF tb_excel THEN DO:
             cExcelDisplay = cExcelDisplay.
             PUT STREAM excel UNFORMATTED  
              work-rep.{1} ", TOTALS " + substring(cExcelDisplay,3,300) SKIP.
         END.

   assign {3}r-act-hrs    = 0
          {3}m-act-hrs    = 0
          {3}dt-chg-hrs   = 0
          {3}dt-nochg-hrs = 0
          {3}r-std-hrs    = 0
          {3}m-std-hrs    = 0.
end.
if last-of(work-rep.{2}) then
do:
 /*  display "**" + " " + TRIM(SUBSTRING(work-rep.{2},1,20)) + " TOTALS" FORMAT "x(30)" AT 2
           {4}r-act-hrs at 32
           {4}m-act-hrs
           {4}dt-chg-hrs
           {4}dt-nochg-hrs
           ({4}r-act-hrs + {4}m-act-hrs +
           {4}dt-chg-hrs + {4}dt-nochg-hrs)
           {4}r-std-hrs
           {4}m-std-hrs
           ({4}r-std-hrs + {4}m-std-hrs) (({4}r-act-hrs + {4}m-act-hrs +
           {4}dt-chg-hrs + {4}dt-nochg-hrs) - ({4}r-std-hrs + {4}m-std-hrs))
                    format ">>>>>>9.99-"
           skip
           fill("*", 130) format "x(130)" at 1
         with frame {4}sum stream-io width 132 no-box no-labels down.

   IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
          SKIP(1)
          '"' ""                                        '",'
          '"' ""                                        '",'
          '"' ("**" + " " +
              TRIM(SUBSTRING(work-rep.{2},1,20)) +
              " TOTALS")                                '",'
          '"' ""                                        '",'
          '"' ""                                        '",'
          '"' STRING({4}r-act-hrs)                      '",'
          '"' STRING({4}m-act-hrs)                      '",'
          '"' STRING({4}dt-chg-hrs)                     '",'
          '"' STRING({4}dt-nochg-hrs)                   '",'
          '"' (STRING({4}r-act-hrs + {4}m-act-hrs +
               {4}dt-chg-hrs + {4}dt-nochg-hrs))        '",'
          '"' STRING({4}r-std-hrs)                      '",'
          '"' STRING({4}m-std-hrs)                      '",'
          '"' ({4}r-std-hrs + {4}m-std-hrs)             '",'
          '"' STRING((({4}r-act-hrs + {4}m-act-hrs +
                       {4}dt-chg-hrs + {4}dt-nochg-hrs) -
                       ({4}r-std-hrs + {4}m-std-hrs)),">>>>>>9.99-") '",'
          SKIP. */

   
    PUT SKIP str-line SKIP .

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                                      
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "chrg"         THEN cVarValue = "" .
                         WHEN "mach"        THEN cVarValue = "" .
                         WHEN "job"           THEN cVarValue = "" .
                         WHEN "frm"          THEN cVarValue = "" .
                         WHEN "blnk"          THEN cVarValue = "" .
                         WHEN "act-run"       THEN cVarValue = STRING({4}r-act-hrs,">>>>>>9.99").
                         WHEN "act-mr"       THEN cVarValue = STRING({4}m-act-hrs,">>>>>>9.99").                 
                         WHEN "dwn-chrg"      THEN cVarValue = STRING({4}dt-chg-hrs,">>>>>>9.99").
                         WHEN "dwn-no"        THEN cVarValue = STRING({4}dt-nochg-hrs,">>>>>>9.99").
                         WHEN "ttl-act"       THEN cVarValue = STRING(({4}r-act-hrs + {4}m-act-hrs + {4}dt-chg-hrs + {4}dt-nochg-hrs),">>>>>>9.99-") .
                         WHEN "est-run"       THEN cVarValue = STRING({4}r-std-hrs,">>>>>>9.99").
                         WHEN "est-mr"        THEN cVarValue = STRING({4}m-std-hrs,">>>>>>9.99").
                         WHEN "ttl-est"         THEN cVarValue = string(({4}r-std-hrs + {4}m-std-hrs),">>>>>>9.99-"). 
                         WHEN "lbr-var"         THEN cVarValue = STRING((({4}r-act-hrs + {4}m-act-hrs + {4}dt-chg-hrs + {4}dt-nochg-hrs) - ({4}r-std-hrs + {4}m-std-hrs)),">>>>>>9.99-") . 
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " *   "    "CHARGE TOTALS" substring(cDisplay,19,300) SKIP(1).

        /*    PUT fill("*", 130) format "x(130)" at 1 . */

            IF tb_excel THEN DO:
             cExcelDisplay = cExcelDisplay.
             PUT STREAM excel UNFORMATTED  
              " CHARGE TOTALS " + substring(cExcelDisplay,3,300) SKIP.
         END.

   assign {4}r-act-hrs    = 0
          {4}m-act-hrs    = 0
          {4}dt-chg-hrs   = 0
          {4}dt-nochg-hrs = 0
          {4}r-std-hrs    = 0
          {4}m-std-hrs    = 0.
end.
