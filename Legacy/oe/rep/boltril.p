/* ----------------------------------------------- oe/rep/boltril.p  11/98 FWK */
/* PRINT TriLakes BOL                                                          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll FOR oe-boll.
def buffer xoe-bolh FOR oe-bolh.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char format "x(7)" no-undo.
def var v-tot-pkgs as int format ">>9" no-undo.
def var v-pkgs as int no-undo.
def var v-pall as int no-undo.
def var v-pallets as int no-undo.
def var v-coname like cust.NAME no-undo.
def var v-coaddr like company.addr no-undo.
def var v-cocity like company.city no-undo.
def var v-costate like company.state no-undo.
def var v-cozip like company.zip no-undo.
def var v-printlines as int no-undo.
def var part-comp as char format "x" no-undo.
def var v-sum-qty like oe-boll.qty no-undo.
def var v-tot-wgt    like oe-bolh.tot-wt.
def var v-tot-cwt    like oe-bolh.cwt.
def var v-lines as int.
DEF VAR lv-qty LIKE oe-boll.qty NO-UNDO.
DEF VAR lv-weight LIKE oe-boll.weight NO-UNDO.
DEF VAR lv-pkgs LIKE v-pkgs NO-UNDO.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.

DEF TEMP-TABLE tt-oe-boll LIKE oe-boll
    FIELD ord-qty LIKE oe-ordl.qty
    FIELD pkgs LIKE v-pkgs
    FIELD i-name LIKE itemfg.i-name
    FIELD prt-lines LIKE v-lines.

DEF BUFFER b-tt-oe-boll FOR tt-oe-boll.

FORM
  tt-oe-boll.ord-qty format ">>>>>>>>>"
  space(1) tt-oe-boll.qty  format ">>>>>>>>>"
  space(0) tt-oe-boll.cases format ">>>>"
  space(0) tt-oe-boll.qty-case format ">>>>>>"
  space(2) tt-oe-boll.po-no format "x(36)"
  space(2) tt-oe-boll.ord-no
  space(2) tt-oe-boll.p-c
  space(1) tt-oe-boll.weight format ">>>>>>9" skip
  with frame ln-s down no-box no-labels stream-io width 90.

FORM
  tt-oe-boll.i-name at 28 skip
  with frame ln-s2 down no-box no-labels stream-io width 90.


find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

do:     /* production mode */

  for each report where report.term-id eq v-term-id,
      first oe-bolh where recid(oe-bolh) eq report.rec-id
      break by oe-bolh.bol-no:

    if first-of(oe-bolh.bol-no) then do:
      v-tot-wgt = 0.
      for each xoe-bolh where xoe-bolh.company eq oe-bolh.company
                          and xoe-bolh.bol-no  eq oe-bolh.bol-no
                        no-lock:
        v-tot-wgt = v-tot-wgt + xoe-bolh.tot-wt.  
      end.

      find first cust where cust.company eq cocode and
        cust.cust-no eq oe-bolh.cust-no no-lock no-error.

      RUN oe/custxship.p (oe-bolh.company,
                          oe-bolh.cust-no,
                          oe-bolh.ship-id,
                          BUFFER shipto).

      find carrier where carrier.company eq oe-bolh.company and
        carrier.carrier eq oe-bolh.carrier no-lock no-error.
      
      v-carrier = if avail carrier THEN carrier.dscr ELSE "".
      
      assign 
       v-frt-pay-dscr = if index("pb",oe-bolh.frt-pay) gt 0 then "Prepaid" else
                        if oe-bolh.frt-pay eq "c" then "Collect" else ""
       v-time         = string(time,"hh:mm am").

      /* calculate total number of pallets */
      assign
       v-pallets = 0
       v-tot-cwt = 0.
      
      FOR EACH xoe-boll
          WHERE xoe-boll.company EQ oe-bolh.company
            AND xoe-boll.b-no    EQ oe-bolh.b-no:
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq xoe-boll.i-no
              and fg-bin.loc     eq xoe-boll.loc
              and fg-bin.loc-bin eq xoe-boll.loc-bin
              and fg-bin.tag     eq xoe-boll.tag
              and fg-bin.job-no  eq xoe-boll.job-no
              and fg-bin.job-no2 eq xoe-boll.job-no2
            no-lock no-error.
            
        assign
         v-pall = if avail fg-bin then
                    (if fg-bin.cases-unit ne 0 then
                       fg-bin.cases-unit else 1) *
                    (if fg-bin.units-pallet ne 0 then
                       fg-bin.units-pallet else 1)
                  else 1
         v-pkgs = xoe-boll.cases + int(xoe-boll.partial gt 0)
         v-pkgs = trunc(v-pkgs / v-pall,0) + int(v-pkgs modulo v-pall gt 0)
         v-pall = v-pall * xoe-boll.qty-case.
         
        if v-pkgs * v-pall gt xoe-boll.qty then
          v-pkgs = trunc(xoe-boll.qty / v-pall,0).

        IF v-pkgs LE 0 THEN v-pkgs = 1.
          
        assign
         v-pallets = v-pallets + v-pkgs
         v-tot-cwt = v-tot-cwt + xoe-boll.qty.
      end.
      
      v-tot-cwt = v-tot-wgt / (v-tot-cwt / 100).
      
      if first-of(oe-bolh.bol-no) then do:

        if oe-ctrl.pr-broker and avail cust and shipto.broker then
          v-coname = cust.name.
        else
        do:
          find first company where company.company eq cocode no-lock no-error.
          if avail company then
            assign v-coname = company.name
                   v-coaddr[1] = company.addr[1]
                   v-coaddr[2] = company.addr[2]
                   v-cocity = company.city
                   v-costate = company.state
                   v-cozip = company.zip.
        end.

        if avail shipto then
          assign
           v-ship-name    = shipto.ship-name
           v-ship-addr[1] = shipto.ship-addr[1]
           v-ship-addr[2] = shipto.ship-addr[2]
           v-ship-city    = shipto.ship-city
           v-ship-state   = shipto.ship-state
           v-ship-zip     = shipto.ship-zip.
        else
          assign
           v-ship-name    = ""
           v-ship-addr[1] = ""
           v-ship-addr[2] = ""
           v-ship-city    = ""
           v-ship-state   = ""
           v-ship-zip     = "".

        if v-print-hdgs then do:
          format header
                 skip(1)
                 "Date:" oe-bolh.bol-date "/" v-time
                 " Company:" v-coname format "x(25)"
                 " BOL #:" oe-bolh.bol-no skip
                 v-coaddr[1] at 37
                 v-coaddr[2] at 37
                 v-cocity at 37 "," v-costate v-cozip skip(2)
                 "Carrier:" v-carrier "Trailer:" at 52
                 oe-bolh.trailer
                 skip(1)
                 "SHIP TO:" at 12
                 v-ship-name  at 12
                 v-ship-addr[1] at 12
                 v-ship-addr[2] at 12
                 v-ship-city at 12
                 v-ship-state
                 v-ship-zip
                 skip(2)
                 v-frt-pay-dscr to 77 skip
                 "Class: 70" to 77 skip
                 "Item 29275 Sub4" to 77 skip
                 "# Pallets:" v-pallets
                 " Weight:" v-tot-wgt
                 " Cwt:" v-tot-cwt FORM ">>>>9.99"
                 " Frt Pay:" oe-bolh.frt-pay skip
                 skip(1)
                 "Shipping Instructions:" at 16 skip
                 oe-bolh.ship-i[1] at 16 format "x(60)" skip
                 oe-bolh.ship-i[2] at 16 format "x(60)" skip
                 oe-bolh.ship-i[3] at 16 format "x(60)" skip
                 oe-bolh.ship-i[4] at 16 format "x(60)"
                 skip(2)
                 "  Qty Ord   Qty Ship Pk Qty/Pk      "
                 "P.O. No. / Item / Description  "
                 "Order# P/C  Wt." skip
                 "--------- ---------- -- ------"
                 "-------------------------------------"
                 "------ --- -------" skip
              with frame hd-top-comp no-box no-labels page-top stream-io width 90.
          VIEW frame hd-top-comp.
        end.

        else do:
          format header
                 skip(3)
                 v-time at 4
                 oe-bolh.bol-date at 16
                 v-coname at 29 format "x(20)"
                 oe-bolh.bol-no at 71
                 skip(4)
                 v-carrier at 8
                 oe-bolh.trailer at 69
                 skip(1)
                 v-ship-name  at 12
                 v-ship-addr[1] at 12
                 v-ship-addr[2] at 12
                 v-ship-city at 12
                 v-ship-state
                 v-ship-zip
                 skip(4)
                 v-frt-pay-dscr to 77
                 "Class: 70" to 77 skip
                 "Item 29275 Sub4" to 77 skip
                 v-pallets at 6
                 v-tot-wgt at 55
                 v-tot-cwt to 67
                 oe-bolh.frt-pay skip
                 oe-bolh.tot-pallets at 16 " Total Pallets"
                 skip(1)
                 oe-bolh.ship-i[1] at 16 format "x(60)" skip
                 oe-bolh.ship-i[2] at 16 format "x(60)" skip
                 oe-bolh.ship-i[3] at 16 format "x(60)" skip
                 oe-bolh.ship-i[4] at 16 format "x(60)"
                 skip(7)
              with frame hd-top no-box no-labels page-top stream-io width 90.
          VIEW frame hd-top.
        end.
      end.

      PAGE.
    end. /* first-of(oe-bolh.bol-no) */

    if first-of(oe-bolh.bol-no) then v-printlines = 0.

    FOR EACH tt-oe-boll:
      DELETE tt-oe-boll.
    END.

    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no,
        FIRST itemfg
        WHERE itemfg.company EQ oe-boll.company
          AND itemfg.i-no    EQ oe-boll.i-no
        NO-LOCK:

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ oe-boll.i-no
            AND fg-bin.loc     EQ oe-boll.loc
            AND fg-bin.loc-bin EQ oe-boll.loc-bin
            AND fg-bin.tag     EQ oe-boll.tag
            AND fg-bin.job-no  EQ oe-boll.job-no
            AND fg-bin.job-no2 EQ oe-boll.job-no2
          NO-LOCK NO-ERROR.

      assign
       v-pall = if avail fg-bin then
                  (if fg-bin.cases-unit ne 0 then
                     fg-bin.cases-unit else 1) *
                  (if fg-bin.units-pallet ne 0 then
                     fg-bin.units-pallet else 1)
                else 1
       v-pkgs = oe-boll.cases + int(oe-boll.partial gt 0)
       v-pkgs = trunc(v-pkgs / v-pall,0) + int(v-pkgs modulo v-pall gt 0)
       v-pall = v-pall * oe-boll.qty-case.
             
      if v-pkgs * v-pall gt oe-boll.qty then
        v-pkgs = trunc(oe-boll.qty / v-pall,0).

      IF oe-boll.cases NE 0 THEN DO:
        FIND FIRST tt-oe-boll
            WHERE tt-oe-boll.company  EQ oe-boll.company
              AND tt-oe-boll.ord-no   EQ oe-boll.ord-no
              AND tt-oe-boll.i-no     EQ oe-boll.i-no
              AND tt-oe-boll.line     EQ oe-boll.line
              AND tt-oe-boll.po-no    EQ oe-boll.po-no
              AND tt-oe-boll.qty-case EQ oe-boll.qty-case
            NO-ERROR.
        IF NOT AVAIL tt-oe-boll THEN DO:
          CREATE tt-oe-boll.
          BUFFER-COPY oe-boll TO tt-oe-boll
          ASSIGN
           tt-oe-boll.qty     = 0
           tt-oe-boll.cases   = 0
           tt-oe-boll.partial = 0
           tt-oe-boll.weight  = 0
           tt-oe-boll.pkgs    = 0
           tt-oe-boll.i-name  = itemfg.i-name.
        END.
     
        ASSIGN
         tt-oe-boll.qty    = tt-oe-boll.qty + oe-boll.qty
         tt-oe-boll.cases  = tt-oe-boll.cases + oe-boll.cases
         tt-oe-boll.weight = tt-oe-boll.weight + oe-boll.weight
         tt-oe-boll.pkgs   = tt-oe-boll.pkgs + v-pkgs.
      END.

      IF oe-boll.partial NE 0 THEN DO:
        FIND FIRST tt-oe-boll
            WHERE tt-oe-boll.company  EQ oe-boll.company
              AND tt-oe-boll.ord-no   EQ oe-boll.ord-no
              AND tt-oe-boll.i-no     EQ oe-boll.i-no
              AND tt-oe-boll.line     EQ oe-boll.line
              AND tt-oe-boll.po-no    EQ oe-boll.po-no
              AND tt-oe-boll.qty-case EQ oe-boll.partial
            NO-ERROR.
        IF NOT AVAIL tt-oe-boll THEN DO:
          CREATE tt-oe-boll.
          BUFFER-COPY oe-boll TO tt-oe-boll
          ASSIGN
           tt-oe-boll.qty      = 0
           tt-oe-boll.cases    = 0
           tt-oe-boll.qty-case = oe-boll.partial
           tt-oe-boll.partial  = 0
           tt-oe-boll.weight   = 0
           tt-oe-boll.pkgs     = 0
           tt-oe-boll.i-name   = itemfg.i-name.
        END.

        tt-oe-boll.cases = tt-oe-boll.cases + 1.

        IF oe-boll.cases EQ 0 THEN
          ASSIGN
           tt-oe-boll.qty    = tt-oe-boll.qty + oe-boll.qty
           tt-oe-boll.weight = tt-oe-boll.weight + oe-boll.weight
           tt-oe-boll.pkgs   = tt-oe-boll.pkgs + v-pkgs.
      END.

      oe-boll.printed = YES.
    END.

    FOR EACH tt-oe-boll
        BREAK BY tt-oe-boll.ord-no
              BY tt-oe-boll.i-no
              BY tt-oe-boll.line
              BY tt-oe-boll.po-no
              BY tt-oe-boll.qty-case * tt-oe-boll.cases DESC:

      IF FIRST-OF(tt-oe-boll.po-no) THEN DO:
        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ tt-oe-boll.company
              AND oe-ordl.ord-no  EQ tt-oe-boll.ord-no
              AND oe-ordl.i-no    EQ tt-oe-boll.i-no
              AND oe-ordl.line    EQ tt-oe-boll.line
            NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl THEN tt-oe-boll.ord-qty = oe-ordl.qty.

        tt-oe-boll.prt-lines = 2.
        IF v-print-hdgs THEN tt-oe-boll.prt-lines = tt-oe-boll.prt-lines + 1.

        FOR EACH b-tt-oe-boll
            WHERE b-tt-oe-boll.company EQ tt-oe-boll.company
              AND b-tt-oe-boll.ord-no  EQ tt-oe-boll.ord-no
              AND b-tt-oe-boll.i-no    EQ tt-oe-boll.i-no
              AND b-tt-oe-boll.line    EQ tt-oe-boll.line
              AND b-tt-oe-boll.po-no   EQ tt-oe-boll.po-no
              AND ROWID(b-tt-oe-boll)  NE ROWID(tt-oe-boll):

          ASSIGN
           tt-oe-boll.qty       = tt-oe-boll.qty + b-tt-oe-boll.qty
           tt-oe-boll.weight    = tt-oe-boll.weight + b-tt-oe-boll.weight
           tt-oe-boll.pkgs      = tt-oe-boll.pkgs + b-tt-oe-boll.pkgs
           tt-oe-boll.prt-lines = tt-oe-boll.prt-lines + 1.

          IF b-tt-oe-boll.p-c THEN tt-oe-boll.p-c = YES.
        END.
      END.
    END.

    FOR EACH tt-oe-boll WHERE tt-oe-boll.cases NE 0,
        FIRST itemfg
        WHERE itemfg.company EQ tt-oe-boll.company
          AND itemfg.i-no    EQ tt-oe-boll.i-no
        NO-LOCK
        BREAK BY tt-oe-boll.ord-no
              BY tt-oe-boll.i-no
              BY tt-oe-boll.line
              BY tt-oe-boll.po-no
              BY tt-oe-boll.qty-case * tt-oe-boll.cases DESC:
   
      if v-printlines ge 25 then do:
        put skip(27 - v-printlines).
        put "CUSTOMER___________________" to 28
            "CARRIER____________________" to 80 skip.
        put "PER________________________" to 28
            "PER________________________" to 80 skip.
        put "DATE_______________________" to 28
            "DATE_______________________" to 80 skip.
        page.
       view frame hd-top-comp.
       v-printlines = 0.
      end.
   
      IF FIRST-OF(tt-oe-boll.po-no) THEN DO:
        IF v-printlines + tt-oe-boll.prt-lines GE 27 THEN DO:
          v-printlines = 0.
          PAGE.
        END.

        DISPLAY tt-oe-boll.ord-qty
                tt-oe-boll.qty
                STRING(tt-oe-boll.po-no,"x(15)") + "/" +
                       tt-oe-boll.i-no @ tt-oe-boll.po-no
                tt-oe-boll.ord-no
                tt-oe-boll.p-c
                tt-oe-boll.weight
            WITH FRAME ln-s.

        lv-pkgs = tt-oe-boll.pkgs.
      END.

      DISPLAY tt-oe-boll.cases
              tt-oe-boll.qty-case
          WITH FRAME ln-s.

      DOWN WITH FRAME ln-s.

      v-printlines = v-printlines + 1.

      IF LAST-OF(tt-oe-boll.po-no) THEN DO:
        DISPLAY tt-oe-boll.i-name
          WITH FRAME ln-s2. 

        if v-print-hdgs then do:
          put space(27) itemfg.part-dscr1 skip.
          v-printlines = v-printlines + 1.
        end.
            
        put space(27)
            trim(string(lv-pkgs,">,>>9")) + " Pallet" +
            (if lv-pkgs ne 1 then "s" else "") format "x(13)" skip
            skip(1).
          
        v-printlines = v-printlines + 3.
      END.
    end. /* for each tt-oe-boll */

    if last-of(oe-bolh.bol-no) then do:
      put skip(27 - v-printlines).
      put "CUSTOMER___________________" to 28   
          "CARRIER____________________" to 80 skip.
      put "PER________________________" to 28
          "PER________________________" to 80 skip.
      put "DATE_______________________" to 28
          "DATE_______________________" to 80 skip.
    end.

    oe-bolh.printed = yes.
  end. /* for each oe-bolh */
end.    /* production mode */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
