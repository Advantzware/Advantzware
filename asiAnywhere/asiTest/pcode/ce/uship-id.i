
find first cust
    where (cust.company = cocode)
      and cust.cust-no eq eb.cust-no
    no-lock no-error.
    
find first shipto
    where (shipto.company eq cocode
  and  shipto.cust-no eq cust.cust-no)
      and shipto.ship-id eq eb.ship-id
    no-lock no-error.

if AVAIL cust AND
   AVAIL shipto AND
   ({1} or
    (eb.ship-id ne "TEMP" and
     (eb.ship-id ne lv-hld-ship or
      eb.cust-no ne lv-hld-cust))) then
FOR EACH xeb
    WHERE xeb.company    EQ eb.company
      AND xeb.est-no     EQ eb.est-no
      AND ((est.est-type NE 4 AND est.est-type NE 8) OR
           ROWID(xeb) EQ ROWID(eb)):
          
  assign
   xeb.carrier      = cust.carrier
   xeb.dest-code    = cust.del-zone
   xeb.chg-method   = cust.frt-pay
   xeb.ship-id      = shipto.ship-id
   xeb.ship-no      = shipto.ship-no
   xeb.ship-name    = shipto.ship-name
   xeb.ship-addr[1] = shipto.ship-addr[1]
   xeb.ship-addr[2] = shipto.ship-addr[2]
   xeb.ship-city    = shipto.ship-city
   xeb.ship-state   = shipto.ship-state
   xeb.ship-zip     = shipto.ship-zip.
         
  release carrier.
         
  if shipto.carrier ne "" then
  find first carrier
      where (carrier.company eq cocode)
        and carrier.carrier eq shipto.carrier
      no-lock no-error.
  if avail carrier then
    assign
     xeb.carrier   = shipto.carrier
     xeb.dest-code = shipto.dest-code.
            
  else  
  find first carrier
      where (carrier.company eq cocode)
        and carrier.carrier eq cust.carrier
      no-lock no-error.
                
  if avail carrier then do:
    xeb.carr-dscr = carrier.dscr.

    IF xeb.dest-code EQ "" OR
       NOT CAN-FIND(FIRST carr-mtx
                    WHERE carr-mtx.company  EQ xeb.company
                      AND carr-mtx.loc      EQ xeb.loc
                      AND carr-mtx.carrier  EQ xeb.carrier
                      AND carr-mtx.del-zone EQ xeb.dest-code) THEN DO:
      FIND FIRST carr-mtx
          WHERE carr-mtx.company  EQ xeb.company
            AND carr-mtx.loc      EQ xeb.loc
            AND carr-mtx.carrier  EQ xeb.carrier
          NO-LOCK NO-ERROR.
      IF AVAIL carr-mtx THEN xeb.dest-code = carr-mtx.del-zone.
    END.
  end.
end.   
