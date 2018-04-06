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

	display itemfg.i-no itemfg.i-name itemfg.procat when pcat = yes
		itemfg.sell-uom
		itemfg.cust-no itemfg.cust-name v-qtyoh
	    with frame itemx.
	put "CUST PART#: " at 3 itemfg.part-no
	    " DESC LINE 1: " itemfg.part-dscr1
	    " DESC LINE 2: " itemfg.part-dscr2 skip(1).
	down with frame itemx.

    
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


    IF tb_excel THEN 
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
          SKIP.

      end.
    end. /* each itemfg */

/* end ---------------------------------- copr. 1998  advanced software, inc. */
