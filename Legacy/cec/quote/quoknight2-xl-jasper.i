  /* Quote# */
  
  rptExcel:set-report-param("imgPath", ls-full-img1).
     
  rptExcel:set-report-param("q_num", v-first-q-no).

  /* Customer ID */
  
  rptExcel:set-report-param("cust_id", xquo.cust-no).

  /* Contact */
  
  rptExcel:set-report-param("contact", xquo.contact).

  /* Telephone */
  
  rptExcel:set-report-param("phone", cust.area-code + cust.phone).
    
  /* Fax */
  
  rptExcel:set-report-param("fax", cust.fax).

  /* Sold To */
  
  rptExcel:set-report-param("sold_to1", bill[1]).
  rptExcel:set-report-param("sold_to2", bill[2]).
  rptExcel:set-report-param("sold_to3", bill[3]).
  rptExcel:set-report-param("sold_to4", bill[4]).

  /* Ship To */
  
  rptExcel:set-report-param("ship_to1", shipto[1]).
  rptExcel:set-report-param("ship_to2", shipto[2]).
  rptExcel:set-report-param("ship_to3", shipto[3]).
  rptExcel:set-report-param("ship_to4", shipto[4]).

  /* Quote Date */
  
  rptExcel:set-report-param("q_date", xquo.quo-date).

  /* SalesPerson */
  
  rptExcel:set-report-param("s_person", sman.sname).

  /* Terms */
  
  rptExcel:set-report-param("terms", terms.dscr).

  /* FOB */
  
  rptExcel:set-report-param("fob", cust.fob-code).

  /* Over-Under */
  
  rptExcel:set-report-param("ovr_und", v-over-under).
