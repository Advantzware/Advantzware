
find first cust
    {sys/look/custW.i}
      and cust.cust-no eq eb.cust-no
    no-lock no-error.
    
find first shipto
    {sys/look/shiptoW.i}
      and shipto.ship-id eq eb.ship-id
    no-lock no-error.

if AVAIL cust AND
   AVAIL shipto AND
   ({1} or
    (eb.ship-id ne "TEMP" and
     (eb.ship-id ne lv-hld-ship or
      eb.cust-no ne lv-hld-cust OR
      (avail(ef) AND ef.board NE lv-hld-board)))) then
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
      {sys\look/carrierW.i}
        and carrier.carrier eq shipto.carrier
      no-lock no-error.
  if avail carrier then
    assign
     xeb.carrier   = shipto.carrier
     xeb.dest-code = shipto.dest-code.
            
  else  
  find first carrier
      {sys\look/carrierW.i}
        and carrier.carrier eq cust.carrier
      no-lock no-error.
                
  if avail carrier then do:
    xeb.carr-dscr = carrier.dscr.

    IF xeb.dest-code EQ "" OR
       NOT CAN-FIND(FIRST carr-mtx
                    WHERE carr-mtx.company  EQ xeb.company
                      AND carr-mtx.loc      EQ xeb.loc
                      AND carr-mtx.carrier  EQ xeb.carrier
                      AND carr-mtx.del-zone EQ xeb.dest-code) OR 
      (avail(ef) AND ef.board NE lv-hld-board 
                AND AVAIL sys-ctrl 
                AND sys-ctrl.char-fld = "FreightClass")
        THEN DO:
      FIND FIRST carr-mtx
          WHERE carr-mtx.company  EQ xeb.company
            AND carr-mtx.loc      EQ xeb.loc
            AND carr-mtx.carrier  EQ xeb.carrier
          NO-LOCK NO-ERROR.
      IF AVAIL carr-mtx THEN xeb.dest-code = carr-mtx.del-zone.
    END.
    /* Indicates to always copy estimate freight class from item */
    find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "CEDeliveryZone" 
                        no-lock no-error.
    IF avail(ef) AND (ef.board NE lv-hld-board OR eb.ship-id ne lv-hld-ship) AND AVAIL sys-ctrl AND sys-ctrl.char-fld = "FreightClass" THEN DO:
      FIND FIRST ITEM WHERE ITEM.company = xeb.company
                        AND ITEM.i-no = ef.board 
                      NO-LOCK NO-ERROR. 
      IF AVAIL(ITEM) AND item.mat-type = "B" AND ITEM.spare-char-1 GT "" THEN DO:
        FIND FIRST carr-mtx WHERE carr-mtx.company  EQ xeb.company
                              AND carr-mtx.loc      EQ xeb.loc
                              AND carr-mtx.carrier  EQ xeb.carrier
                              AND carr-mtx.del-zone EQ ITEM.spare-char-1
                             NO-LOCK NO-ERROR.
        IF AVAIL carr-mtx THEN DO:
          ASSIGN xeb.dest-code = ITEM.spare-char-1.    
        END.

      END.
    END.
  END.
end.   
