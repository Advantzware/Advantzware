/* Release Posting Exception Report ----------------------------------------- */

{sys/form/r-top3w.f}

DEF VAR lv-reason AS CHAR FORMAT "x(9)" NO-UNDO.

FORM HEADER SKIP(1) WITH FRAME r-top.
 
FORMAT
  oe-relh.rel-date
  space(2)
  oe-relh.release#   format ">>>>>>>>"
  space(2)
  oe-relh.carrier
  space(4)
  oe-relh.trailer
  space(2)
  oe-relh.cust-no
  space(4)
  oe-relh.ship-id
  space(2)
  lv-reason  AT 106
  skip(1)
  
  header "Date        Release#  Carrier  Trailer   Cust#       Ship#   " skip
         "----------  --------  -------  --------  --------    --------"

  with stream-io width 132 no-labels no-box no-underline frame relh.

format
  space(5)
  oe-rell.i-no
  itemfg.i-name    format "x(20)"
  oe-rell.po-no
  oe-rell.ord-no
  oe-rell.rel-no   format ">>9" space(0) "-" space(0)
  oe-rell.b-ord-no format "99"
  oe-rell.loc
  oe-rell.loc-bin
  oe-rell.tag
  oe-rell.cases    format "->>>,>>9"
  oe-rell.qty-case format "->>>,>>9"
  oe-rell.partial  format "->>>,>>9"

  header
  space(5) "Item#           Item Name            P.O. #            Ord#  Rel.# Whse. Bin Loc  Tag         Cases Qty/Case  Partial" skip
  space(5) "--------------- -------------------- --------------- ------ ------ ----- -------- -------- -------- -------- --------"
  with stream-io width 132 DOWN no-labels no-box no-underline frame rell.


  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = "Release - Insufficient Inventory Report"
   {sys/inc/ctrtext.i str-tit2 112}
 
   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}
  
  display with frame r-top.
  
  for each tt-except,

      first oe-relh
      where oe-relh.company eq cocode
        and oe-relh.r-no    eq tt-except.r-no
      no-lock

     break by tt-except.r-no
           by tt-except.ord-no
           by tt-except.rel-no
           by tt-except.b-ord-no:

    if first-of(tt-except.r-no) then do:
      lv-reason = IF oe-relh.deleted       THEN "*DELETED*" ELSE
                  IF tt-except.reason EQ 1 THEN "*IN USE*"  ELSE "*NO INV*".

      display oe-relh.rel-date
              oe-relh.release#
              oe-relh.carrier
              oe-relh.trailer
              oe-relh.cust-no
              oe-relh.ship-id
              lv-reason
          with frame relh.
      down with frame relh.
    end.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq tt-except.i-no
        no-lock no-error.

    display tt-except.i-no       @ oe-rell.i-no
            itemfg.i-name        when avail itemfg
            tt-except.po-no      @ oe-rell.po-no
            tt-except.ord-no     @ oe-rell.ord-no
            tt-except.rel-no     @ oe-rell.rel-no
            tt-except.b-ord-no   @ oe-rell.b-ord-no
            tt-except.loc        @ oe-rell.loc
            tt-except.loc-bin    @ oe-rell.loc-bin
            tt-except.tag        @ oe-rell.tag
            tt-except.cases      @ oe-rell.cases
            tt-except.qty-case   @ oe-rell.qty-case
            tt-except.partial    @ oe-rell.partial
        with frame rell.
    down with frame rell.
    
    put skip(1).
  END.
