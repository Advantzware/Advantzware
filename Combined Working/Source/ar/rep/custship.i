/* ------------------------------------------------- sys/rep/cust.i 12/98 JLF */
/*                                                                            */
/* cust list printout                                                         */
/*                                                                            */
/* -------------------------------------------------------------------------- */
DO:
  IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    excelHeader = 'Type,Code,Name,Address1,Address2,City,State,Zip,' +
                  'Phone,Fax,Contact,Email,Rep,Sales Rep Name,Territory'.
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
  END. /* if tb_excel */
for EACH cust NO-LOCK
      WHERE cust.company GE fco
        AND cust.company LE tco
        AND cust.cust-no GE fcust
        AND cust.cust-no LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
        AND ttCustList.log-fld no-lock) else true)
        AND cust.type GE ftype
        AND cust.type LE ttype
        AND cust.sman  GE fsman
        AND cust.sman LE tsman
        AND cust.date-field[1] GE begin_date
        AND cust.date-field[1] LE end_date
        AND cust.sman LE tsman
        AND (cust.cust-level EQ v-level OR v-level EQ 99)
        AND (cust.active NE 'I' AND tb_active
         OR cust.active EQ 'I' AND tb_inactive) {2}
        BY cust.{1} BY cust.cust-no:
    
      {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
    FIND FIRST sman NO-LOCK
         WHERE sman.company EQ cust.company
           AND sman.sman EQ cust.sman NO-ERROR.
    IF CAN-DO('Customer,Both',rd_showCode) THEN DO WITH FRAME fRpt:
      DISPLAY
        'Customer' @ recType
        cust.cust-no
        cust.name
        cust.area-code + cust.phone @ phoneField
        cust.contact
        sman.sman WHEN AVAILABLE sman
        sman.sname WHEN AVAILABLE sman
        sman.territory WHEN AVAILABLE sman.
      DOWN.
      DISPLAY
        cust.addr[1] @ cust.name
        cust.fax @ phoneField
        cust.email @ cust.contact.
      DOWN.
      IF cust.addr[2] NE '' THEN DO:
        DISPLAY cust.addr[2] @ cust.name.
        DOWN.
      END. /* if addr[2] */
      DISPLAY
        cust.city + ', ' +
        cust.state + ' ' +
        cust.zip FORMAT 'x(30)' @ cust.name.
      DOWN.
      
      IF tb_excel THEN DO:
        PUT STREAM excel UNFORMATTED
          '"Customer",'
          '"' cust.cust-no '",'
          '"' cust.name '",'
          '"' cust.addr[1] '",'
          '"' cust.addr[2] '",'
          '"' cust.city '",'
          '"' cust.state '",'
          '"' cust.zip '",'
          '"' cust.area-code FORMAT '(999)' ' ' cust.phone FORMAT '999-9999' '",'
          '"' cust.fax FORMAT '(999) 999-9999' '",'
          '"' cust.contact '",'
          '"' cust.email '",'.
        IF AVAILABLE sman THEN
        PUT STREAM excel UNFORMATTED
          '"' sman.sman '",'
          '"' sman.sname '",'
          '"' sman.territory '"'.
        PUT STREAM excel UNFORMATTED SKIP.
      END. /* if tb_excel */
    END. /* if tb_showcode customer,both */
    IF CAN-DO('ShipTo,Both',rd_showCode) THEN DO:
      FOR EACH shipto OF cust NO-LOCK WITH FRAME fRpt:
        DISPLAY
          'Ship To' @ recType
          shipto.ship-id @ cust.cust-no
          shipto.ship-name @ cust.name
          shipto.area-code + shipto.phone @ phoneField
          cust.contact
          sman.sman WHEN AVAILABLE sman
          sman.sname WHEN AVAILABLE sman
          sman.territory WHEN AVAILABLE sman.
        DOWN.
        DISPLAY
          shipto.ship-addr[1] @ cust.name
          shipto.fax @ phoneField
          cust.email @ cust.contact.
        DOWN.
        IF shipto.ship-addr[2] NE '' THEN DO:
          DISPLAY shipto.ship-addr[2] @ cust.name.
          DOWN.
        END. /* if addr[2] */
        DISPLAY
          shipto.ship-city + ', ' +
          shipto.ship-state + ' ' +
          shipto.ship-zip FORMAT 'x(30)' @ cust.name.
        DOWN.
        IF tb_excel THEN DO:
          PUT STREAM excel UNFORMATTED
            '"Ship To",'
            '"' shipto.ship-id '",'
            '"' shipto.ship-name '",'
            '"' shipto.ship-addr[1] '",'
            '"' shipto.ship-addr[2] '",'
            '"' shipto.ship-city '",'
            '"' shipto.ship-state '",'
            '"' shipto.ship-zip '",'
            '"' shipto.area-code FORMAT '(999)' ' ' shipto.phone FORMAT '999-9999' '",'
            '"' shipto.fax FORMAT '(999) 999-9999' '",'
            '"' cust.contact '",'
            '"' cust.email '",'.
          IF AVAILABLE sman THEN
          PUT STREAM excel UNFORMATTED
            '"' sman.sman '",'
            '"' sman.sname '",'
            '"' sman.territory '"'.
          PUT STREAM excel UNFORMATTED SKIP.
        END. /* if tb_excel */
      END. /* each shipto */
    END. /* if tb_showcode shipto,both */
  END. /* each cust */
  
  IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
    OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)).
  END.
END. /* do block */

/* end ---------------------------------- copr. 1998  advanced software, inc. */
