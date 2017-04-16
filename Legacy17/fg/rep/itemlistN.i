/* ---------------------------------------------- fg/rep/itemlist.i 11/98 JLF */
/* FINISHED GOODS - INVENTORY MASTER FILE LIST                                */
/* -------------------------------------------------------------------------- */


	where itemfg.company eq cocode
	  and itemfg.i-no    ge v-ino[1]
	  and itemfg.i-no    le v-ino[2]
	  and itemfg.cust-no ge v-cust[1]
	  and itemfg.cust-no le v-cust[2]
          and (if lselected then can-find(first ttCustList where ttCustList.cust-no eq itemfg.cust-no
             AND ttCustList.log-fld no-lock) else true)
	  and itemfg.procat  ge v-cat[1]
	  and itemfg.procat  le v-cat[2]
	no-lock:
    {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"}
      v-qtyoh = 0.
      v-fgitm = "".
     iInkCount = 0.
    FOR EACH itemfg-ink OF itemfg NO-LOCK:
        iInkCount = iInkCount + 1.
        IF iInkCount LE 12 THEN
            ASSIGN
                cInkCode[iInkCount] = itemfg-ink.rm-i-no
                cInkDesc[iInkCount] = itemfg-ink.dscr.
    END.    
    IF itemfg.est-no NE "" THEN
        v-fgitm =  itemfg.est-no .
        FIND LAST bf-job
            WHERE bf-job.company EQ itemfg.company
              AND bf-job.est-no EQ itemfg.est-no
        NO-LOCK NO-ERROR.
        IF AVAIL bf-job THEN DO: 
            cJobNo = bf-job.job-no.
            FIND FIRST oe-ordl WHERE oe-ordl.company = itemfg.company
                AND oe-ordl.i-no = itemfg.i-no 
                AND oe-ordl.job-no = bf-job.job-no 
            NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl THEN
              ASSIGN cPo  =   oe-ordl.po-no .
        END.
/*       FIND LAST fg-bin                       */
/*       where fg-bin.company eq itemfg.company */
/*         and fg-bin.i-no    eq itemfg.i-no    */
/*         NO-LOCK NO-ERROR.                    */
/*                                              */
/*       IF AVAIL fg-bin THEN                   */
/*           ASSIGN job =   fg-bin.job-no .     */
     
      IF AVAIL bf-job THEN
        

      IF v-fgitm EQ "" AND AVAIL oe-ordl THEN
          v-fgitm = oe-ordl.est-no.
    
     IF v-fgitm NE "" THEN do:
         FIND FIRST est WHERE est.company = itemfg.company
             AND est.est-no = FILL(" ",8 - LENGTH(TRIM(v-fgitm))) + TRIM(v-fgitm) NO-LOCK NO-ERROR.
         FIND FIRST eb WHERE eb.company = est.company
         AND eb.est-no EQ est.est-no 
           AND eb.stock-no = itemfg.i-no NO-LOCK NO-ERROR.
         FIND FIRST ef WHERE ef.company = est.company
             AND ef.est-no EQ est.est-no 
             AND ef.form-no = eb.form-no
         NO-LOCK NO-ERROR.

         IF AVAIL eb THEN DO:
             ASSIGN 
             casew          =   eb.cas-len 
             casel          =   eb.cas-wid
             cased          =   eb.cas-dep
             unit           =   eb.tr-cnt
/*              ink-cod1       =   string(eb.i-code[1])    */
/*              ink-cod2       =   string(eb.i-code[2])    */
/*              ink-cod3       =   string(eb.i-code[3])    */
/*              ink-cod4       =   string(eb.i-code[4])    */
/*              ink-cod5       =   string(eb.i-code[5])    */
/*              ink-cod6       =   string(eb.i-code[6])    */
/*              ink-cod7       =   string(eb.i-code[7])    */
/*              ink-cod8       =   string(eb.i-code[8])    */
/*              ink-cod9       =   string(eb.i-code[8])    */
/*              ink-cod10      =   string(eb.i-code[10])   */
/*              /*ink-cod11      =   string(eb.i-code[11]) */
/*              ink-cod12      =   string(eb.i-code[12])*/ */
/*              ink-nam1       =   eb.i-dscr[1]            */
/*              ink-nam2       =   eb.i-dscr[2]            */
/*              ink-nam3       =   eb.i-dscr[3]            */
/*              ink-nam4       =   eb.i-dscr[4]            */
/*              ink-nam5       =   eb.i-dscr[5]            */
/*              ink-nam6       =   eb.i-dscr[6]            */
/*              ink-nam7       =   eb.i-dscr[7]            */
/*              ink-nam8       =   eb.i-dscr[8]            */
/*              ink-nam9       =   eb.i-dscr[9]            */
/*              ink-nam10      =   eb.i-dscr[10]           */
            /* ink-nam11      =   eb.i-dscr[11]
             ink-nam12      =   eb.i-dscr[12] */ .
         IF iInkCount = 0 THEN
                DO iInkCount = 1 TO 10:
                    ASSIGN 
                        cInkCode[iInkCount] = eb.i-code2[iInkCount]
                        cInkDesc[iInkCount] = eb.i-dscr2[iInkCount].
                END.
         END.
         IF AVAIL ef THEN
             ASSIGN
             board-code     =   ef.board
             board-name     =   ef.brd-dscr
             caliper        =   ef.cal .
     END.

      for each fg-bin
	  where fg-bin.company eq itemfg.company
	    and fg-bin.i-no    eq itemfg.i-no
        NO-LOCK
	  use-index co-ino:

	    if (v-custown and (fg-bin.loc eq "CUST" or fg-bin.cust-no gt "")) OR 
	       ((fg-bin.loc ge v-loc[1] and fg-bin.loc le v-loc[2]) and
	        fg-bin.cust-no eq "" and fg-bin.loc ne "CUST") then
          v-qtyoh = v-qtyoh + fg-bin.qty.
      end. /* each fg-bin */

      if v-zbal or v-qtyoh ne 0 then do:

   /* display itemfg.i-no itemfg.i-name itemfg.procat when pcat = yes
		itemfg.sell-uom
		itemfg.cust-no itemfg.cust-name v-qtyoh
	    with frame itemx.
	put "CUST PART#: " at 3 itemfg.part-no
	    " DESC LINE 1: " itemfg.part-dscr1
	    " DESC LINE 2: " itemfg.part-dscr2 skip(1).
	down with frame itemx.*/

    
    ASSIGN itemname = itemfg.i-name 
           dscr1 = itemfg.part-dscr1
           dscr2 = itemfg.part-dscr2
           ino = itemfg.i-no
           cutpart = itemfg.part-no.

    if index(itemname,'"',1) > 0 then assign
        itemname = replace(itemname,'"'," "). 

     if index(itemname,',',1) > 0 then assign
        itemname = replace(itemname,','," "). 

     if index(dscr1,'"',1) > 0 then assign
        dscr1 = replace(dscr1,'"'," "). 

     if index(dscr1,',',1) > 0 then assign
        dscr1 = replace(dscr1,','," "). 

     if index(dscr2,'"',1) > 0 then assign
        dscr2 = replace(dscr2,'"'," "). 

     if index(dscr2,',',1) > 0 then assign
        dscr2 = replace(dscr2,','," "). 

     if index(ino,'"',1) > 0 then assign
        ino = replace(ino,'"'," "). 

     if index(ino,',',1) > 0 then assign
        ino = replace(ino,','," "). 
     
     if index(cutpart,'"',1) > 0 then assign
        cutpart = replace(cutpart,'"'," "). 

     if index(cutpart,',',1) > 0 then assign
        cutpart = replace(cutpart,','," "). 


   /* IF tb_excel THEN 
      PUT STREAM excel UNFORMATTED
          '"' ino                                           '",'
          '"' itemname                                      '",'
          '"' (IF pcat THEN itemfg.procat ELSE "")          '",'
          '"' itemfg.sell-uom                               '",'
          '"' itemfg.cust-no                                '",'
          '"' itemfg.cust-name                              '",'
          '"' STRING(v-qtyoh,"->>,>>>,>>9")                 '",'
          '"' cutpart                                       '",'
          '"' dscr1                                         '",'
          '"' dscr2                                         '",'
          SKIP. */


     ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
       
       BUFFER b-itemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
          CASE cTmpField:               
                WHEN "i-no" THEN cVarValue = itemfg.i-no .
                WHEN "i-name" THEN cvarValue = itemfg.i-name.
                WHEN "procat" THEN cVarValue = itemfg.procat .
                WHEN "sell-uom" THEN cvarValue = itemfg.sell-uom.
                WHEN "cust-no" THEN cVarValue = itemfg.cust-no .
                WHEN "cust-name" THEN cvarValue = itemfg.cust-name.
                WHEN "part-no" THEN cVarValue = string(itemfg.part-no,"x(15)") .
                WHEN "part-dscr1" THEN cvarValue = string(itemfg.part-dscr1,"x(28)").
                WHEN "part-dscr2" THEN cVarValue = string(itemfg.part-dscr2,"x(28)") .
                WHEN "part-dscr3" THEN cVarValue = string(itemfg.part-dscr3,"x(28)") .
                WHEN "style"   THEN cvarValue = itemfg.style.
                WHEN "group"   THEN cvarValue = string(itemfg.spare-char-1,"x(15)").
                WHEN "l-score" THEN cVarValue = string(itemfg.l-score[50], ">>>,>>9.99<<<<") .
                WHEN "w-score" THEN cvarValue = string(itemfg.w-score[50], ">>>,>>9.99<<<<") . 
                WHEN "d-score" THEN cVarValue = string(itemfg.d-score[50], ">>>,>>9.99<<<<") .
                WHEN "casew"    THEN     cVarValue =   string(casew, ">>9.9999") .    
                WHEN "casel"    THEN     cvarValue =   string(casel, ">>9.9999") .    
                WHEN "cased"    THEN     cVarValue =   string(cased, ">>9.9999") .    
                WHEN "count"     THEN    cVarValue =   string(itemfg.case-count, ">>>>9") .     
                WHEN "ink-cod1" THEN     cvarValue =   cInkCode[1] . 
                WHEN "ink-cod2" THEN     cVarValue =   cInkCode[2] . 
                WHEN "ink-cod3" THEN     cVarValue =   cInkCode[3] . 
                WHEN "ink-cod4" THEN     cvarValue =   cInkCode[4] . 
                WHEN "ink-cod5" THEN     cVarValue =   cInkCode[5] . 
                WHEN "ink-cod6" THEN     cVarValue =   cInkCode[6] . 
                WHEN "ink-cod7" THEN     cvarValue =   cInkCode[7] . 
                WHEN "ink-cod8" THEN     cVarValue =   cInkCode[8] . 
                WHEN "ink-cod9"  THEN    cVarValue =   cInkCode[9] . 
                WHEN "ink-cod10" THEN    cvarValue =   cInkCode[10] .
                WHEN "ink-nam1"  THEN    cVarValue =   cInkDesc[1] . 
                WHEN "ink-nam2"  THEN    cVarValue =   cInkDesc[2] . 
                WHEN "ink-nam3"  THEN    cvarValue =   cInkDesc[3] . 
                WHEN "ink-nam4"  THEN    cVarValue =   cInkDesc[4] . 
                WHEN "ink-nam5"  THEN    cVarValue =   cInkDesc[5] . 
                WHEN "ink-nam6"  THEN    cvarValue =   cInkDesc[6] . 
                WHEN "ink-nam7"  THEN    cVarValue =   cInkDesc[7] . 
                WHEN "ink-nam8"  THEN    cVarValue =   cInkDesc[8] . 
                WHEN "ink-nam9"  THEN    cvarValue =   cInkDesc[9] . 
                WHEN "ink-nam10" THEN    cVarValue =   cInkDesc[10] .
                WHEN "board-code" THEN   cVarValue  =  board-code .
                WHEN "board-name" THEN   cVarValue =   board-name .
                WHEN "caliper"  THEN     cVarValue =   string(caliper, "9.99999") .   
                WHEN "job" THEN  cVarValue =   cJobNo . 
                WHEN "po"  THEN  cVarValue =   cPO  .
                WHEN "v-qtyoh" THEN cVarValue = string(v-qtyoh, "->>>,>>9.9<<<<<") .


                /*WHEN "v-relDate" THEN cVarValue = IF AVAIL tt-oe-rel THEN string(tt-oe-rel.rel-date) ELSE "".
                WHEN "v-relQty" THEN cVarValue = /*IF AVAIL tt-oe-rel THEN string(tt-oe-rel.tot-qty,"->>>>>>,>>9") ELSE ""*/
                                                 STRING(v-qty-allo,"->>>>>>,>>9") .
                WHEN "v-rctDate" THEN cVarValue = IF trans-date <> ? THEN STRING(trans-date, "99/99/9999") ELSE "".
                WHEN "v-qty-onh" THEN cVarValue =     STRING(v-qty-onh,"->>>>>>,>>9").
                WHEN "li-ship-qty" THEN cVarValue = STRING(li-ship-qty,"->>>>>>,>>9").
                WHEN "v-qty-ord" THEN cVarValue = STRING(oe-ordl.qty,"->>>>>>,>>9").
                WHEN "v-price" THEN cVarValue = STRING(oe-ordl.price,"->>>,>>9.99").
                WHEN "v-ext" THEN cVarValue = STRING(v-ext,"->>>,>>>,>>9.99").
                WHEN "v-rfq" THEN cVarValue = STRING(v-rfq,"x(10)").*/
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

      end.
    end. /* each itemfg */

/* end ---------------------------------- copr. 1998  advanced software, inc. */
