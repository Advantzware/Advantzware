
for each {1}
    where {1}.company eq cocode
      /*and {1}.i-code  ge b-itemcode#
      and {1}.i-code  le e-itemcode#*/
      AND ( {1}.i-code EQ b-itemcode# OR   b-itemcode# = "B" OR v-type = "F" )
      and {1}.procat  ge b-category#
      and {1}.procat  le e-category#
      and (("{1}" eq "item" and index(v-mattype-list,{1}.mat-type) gt 0
                            and {2} ge b-flute#
                            and {2} le e-flute#)
       or  ("{1}" eq "itemfg"))
    no-lock,
   
    each po-ordl
    where po-ordl.company eq cocode
      and po-ordl.i-no    eq {1}.i-no
      and (("{1}" eq "item"   and po-ordl.item-type) or
           ("{1}" eq "itemfg" and not po-ordl.item-type))
      AND po-ordl.actnum  GE begin_acct
      AND po-ordl.actnum  LE end_acct
    no-lock,
    
    first po-ord
    where po-ord.company eq cocode
      and po-ord.po-no   eq po-ordl.po-no
      and po-ord.vend-no ge b-vendor#
      and po-ord.vend-no le e-vendor#
      and po-ord.po-date ge b-date#
      and po-ord.po-date le e-date#
    no-lock
    
    break by po-ord.vend-no
          by po-ordl.i-no:

    {custom/statusMsg.i " 'Processing PO#  '  + string(po-ord.po-no) "}
          
  if first-of(po-ord.vend-no) then do:
    find first vend
        where vend.company eq cocode
          and vend.vend-no eq po-ord.vend-no
        no-lock no-error.
    assign
     vendor# = caps(po-ord.vend-no)
     name#   = if avail vend then vend.name else "* Invalid *".

    IF FIRST(po-ord.vend-no) THEN DO:
      VIEW FRAME r-top.
  /*    IF detail-flag# THEN VIEW FRAME f-header. */
    END. 
     
    if detail-flag# AND NOT FIRST(po-ord.vend-no) then page. 
  end.

  if po-ordl.pr-qty-uom eq "MSF" then
    v-msf = po-ordl.ord-qty.
    
  else do:
    assign
     v-wid = po-ordl.s-wid
     v-len = po-ordl.s-len
     v-dep = 0
     v-bwt = 0.
     
    if "{1}" eq "item" then
      assign
       v-bwt = item.basis-w
       v-dep = item.s-dep.
       
    run sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                           v-bwt, v-len, v-wid, v-dep,
                           po-ordl.ord-qty, output v-msf).
  end.                         

  assign
   x            = month(po-ord.po-date)
   v-cost       = po-ordl.cost * v-msf
   ytd-msf#[1]  = ytd-msf#[1] + v-msf
   ytd-cost#[1] = ytd-cost#[1] + v-cost.

  if po-ord.po-date ge p-date# then
    assign
     mtd-msf#[1]  = mtd-msf#[1] + v-msf
     mtd-cost#[1] = mtd-cost#[1] + v-cost.

  if (detail-flag#     and last-of(po-ordl.i-no))   or
     (not detail-flag# and last-of(po-ord.vend-no)) then do:
      
  /*  item# = if detail-flag# then caps(po-ordl.i-no)
                            else (po-ord.vend-no +
                            IF AVAIL vend THEN "-" + vend.name
                            ELSE "").
     
    display item#
            mtd-msf#[1]
            mtd-cost#[1]
            ytd-msf#[1]
            ytd-cost#[1]
        with frame f-detail.
    down with frame f-detail. */
    
  /*  IF tb_excel THEN
    DO:
       IF detail-flag# THEN
       DO:
       
          PUT STREAM excel UNFORMATTED
              '"' IF first-of(po-ord.vend-no) THEN
                     vendor# + " " + name#
                  ELSE ""                         '",'.
       END.

       PUT STREAM excel UNFORMATTED
         '"' item#                                  '",'
         '"' STRING(mtd-msf#[1],"->>>,>>>,>>9.99")  '",'
         '"' STRING(mtd-cost#[1],"->>>,>>>,>>9.99") '",'
         '"' STRING(ytd-msf#[1],"->>>,>>>,>>9.99")  '",'
         '"' STRING(ytd-cost#[1],"->>>,>>>,>>9.99") '",'
         SKIP.
    END. */

        ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".
            
            
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "vend"     THEN cVarValue = STRING(po-ord.vend-no)  .
                     WHEN "name"     THEN cVarValue = IF AVAIL vend THEN STRING(vend.NAME,"x(30)") ELSE "" .
                     WHEN "ino"      THEN cVarValue = STRING(po-ordl.i-no) .
                     WHEN "ptd-msf"  THEN cVarValue = STRING(mtd-msf#[1],"->>>,>>>,>>9.99") .
                     WHEN "ptd-cst"  THEN cVarValue = STRING(mtd-cost#[1],"->>>,>>>,>>9.99") .
                     WHEN "ytd-msf"  THEN cVarValue = STRING(ytd-msf#[1],"->>>,>>>,>>9.99") .
                     WHEN "ytd-cs"   THEN cVarValue = STRING(ytd-cost#[1],"->>>,>>>,>>9.99") .
                    
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
     mtd-msf#[2]  = mtd-msf#[2]  + mtd-msf#[1]
     mtd-cost#[2] = mtd-cost#[2] + mtd-cost#[1]
     ytd-msf#[2]  = ytd-msf#[2]  + ytd-msf#[1]
     ytd-cost#[2] = ytd-cost#[2] + ytd-cost#[1]
     
     mtd-msf#[1]  = 0
     mtd-cost#[1] = 0
     ytd-msf#[1]  = 0
     ytd-cost#[1] = 0.
  end.
  
  if last(po-ord.vend-no) then do:
    put skip(1).
  
  /*  display "        Grand Totals" @ item#
            mtd-msf#[2]            @ mtd-msf#[1]
            mtd-cost#[2]           @ mtd-cost#[1]
            ytd-msf#[2]            @ ytd-msf#[1]
            ytd-cost#[2]           @ ytd-cost#[1]
        with frame f-detail.
    down with frame f-detail.
    
    put skip(1).

    IF tb_excel THEN
    DO:
       PUT STREAM excel UNFORMATTED
           SKIP(1).

       IF detail-flag# THEN
          PUT STREAM excel UNFORMATTED
              '"' ""                         '",'.

       PUT STREAM excel UNFORMATTED
         '"' "Grand Totals"                         '",'
         '"' STRING(mtd-msf#[2],"->>>,>>>,>>9.99")  '",'
         '"' STRING(mtd-cost#[2],"->>>,>>>,>>9.99") '",'
         '"' STRING(ytd-msf#[2],"->>>,>>>,>>9.99")  '",'
         '"' STRING(ytd-cost#[2],"->>>,>>>,>>9.99") '",'
         SKIP.
    END. */
    PUT SKIP str-line SKIP .
        ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".
            
            
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "vend"     THEN cVarValue = "".
                     WHEN "name"     THEN cVarValue = "".
                     WHEN "ino"      THEN cVarValue = "".
                     WHEN "ptd-msf"  THEN cVarValue = STRING(mtd-msf#[2],"->>>,>>>,>>9.99") .
                     WHEN "ptd-cst"  THEN cVarValue = STRING(mtd-cost#[2],"->>>,>>>,>>9.99") .
                     WHEN "ytd-msf"  THEN cVarValue = STRING(ytd-msf#[2],"->>>,>>>,>>9.99") .
                     WHEN "ytd-cs"   THEN cVarValue = STRING(ytd-cost#[2],"->>>,>>>,>>9.99") .
                    
                END CASE.
                  
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        
        PUT UNFORMATTED  "Grand Totals:" substring(cDisplay,14,300) SKIP(1).
        IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                  "Grand Totals " + SUBSTRING(cExcelDisplay,3,300) SKIP.
        END.
  end.
end.

