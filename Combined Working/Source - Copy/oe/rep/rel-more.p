
/* ---------------------------------------------- oe/rep/rel-more.p 09/98 JLF */
/* Print Multiple Release/Picking tickets per header                          */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

def buffer b-report for report.

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

def stream last-page.

form header
     skip(3)
     v-date                         at 66
     skip(2)
     trim(string(page-number - v-last-page,">9")) + " of " +
     trim(string(v-page-tot,">9"))  at 66 format "x(8)"
     skip(2)
     v-salesman                     at 66
     skip(5)
     v-name                         at 11
     v-addr[1]                      at 11
     v-addr[2]                      at 11
     v-c-s-z                        at 11
     skip(4)
     v-carrier                      at 3
     v-weight                       to 38
/*   v-pallets                      to 59 */
     skip(2)

    with frame hd-top no-box no-labels STREAM-IO width 85 page-top.


find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

    {sa/sa-sls01.i}

    {oe/rep/foreachr.i} no-lock,

        each oe-rell
        where oe-rell.company eq oe-relh.company
          and oe-rell.r-no    eq oe-relh.r-no
        USE-INDEX r-no no-lock:

      find first oe-rel
          where oe-rel.company  eq oe-rell.company
            and oe-rel.ord-no   eq oe-rell.ord-no
            and oe-rel.line     eq oe-rell.line
            and oe-rel.link-no  eq oe-rell.r-no
            and oe-rel.ship-no  eq oe-relh.ship-no
            and oe-rel.po-no    eq oe-rell.po-no
            and oe-rel.i-no     eq oe-rell.i-no
          no-lock no-error.

      if not avail oe-rel then
        find first oe-rel
            where oe-rel.company  eq oe-rell.company
              and oe-rel.ord-no   eq oe-rell.ord-no
              and oe-rel.line     eq oe-rell.line
              and oe-rel.rel-date eq oe-relh.rel-date
              and oe-rel.ship-no  eq oe-relh.ship-no
              and oe-rel.po-no    eq oe-rell.po-no
              and oe-rel.i-no     eq oe-rell.i-no
            no-lock no-error.

      create report.
      assign
       report.term-id = v-term
       report.key-01  = "2"
       report.key-02  = oe-relh.cust-no
       report.key-03  = oe-relh.ship-id
       report.key-04  = string(year(oe-relh.rel-date),"9999") +
                        string(month(oe-relh.rel-date),"99")  +
                        string(day(oe-relh.rel-date),"99")
       report.key-05  = oe-relh.carrier
       report.key-06  = if avail oe-rel and oe-rel.po-no ne "" then oe-rel.po-no
                        else oe-relh.po-no
       report.key-07  = string(oe-rell.ord-no,"9999999999")
       report.rec-id  = recid(oe-rell).
    end.

    for each report
        where report.term-id eq v-term
          and report.key-01  eq "2"
        no-lock

        break by report.key-02
              by report.key-03
              by report.key-04
              by report.key-05:

      if last-of(report.key-05) then do:
        create b-report.
        assign
         b-report.term-id = v-term
         b-report.key-01  = "1"
         b-report.key-02  = report.key-02
         b-report.key-03  = report.key-03
         b-report.key-04  = report.key-04
         b-report.key-05  = report.key-05.
      end.
    end.

    release b-report.

    output stream last-page to value(tmp-dir + "rel-more.txt") page-size VALUE(v-lines-per-page).

    for each b-report
        where b-report.term-id eq v-term
          and b-report.key-01  eq "1",

        first cust
        where cust.company eq cocode
          and cust.cust-no eq b-report.key-02
        no-lock

        break by b-report.key-01:

      RUN oe/custxship.p (cust.company,
                          b-report.key-02,
                          b-report.key-03,
                          BUFFER shipto).

      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq b-report.key-05
          no-lock no-error.

      assign
       v-date     = date(int(substr(b-report.key-04,5,2)),
                    int(substr(b-report.key-04,7,2)),
                    int(substr(b-report.key-04,1,4)))
       v-weight   = 0
       v-pallets  = 0
       v-name     = shipto.ship-name
       v-addr[1]  = shipto.ship-addr[1]
       v-addr[2]  = shipto.ship-addr[2]
       v-c-s-z    = shipto.ship-city  + ", " +
                    shipto.ship-state + "  " +
                    shipto.ship-zip
       v-carrier  = if avail carrier then carrier.dscr else "".

      view frame hd-top.
      view stream last-page frame hd-top.  /* Print headers */

      page stream last-page.

      {oe/rep/rel-more.i "stream last-page"}

      v-page-tot = page-number (last-page) - v-last-page.

      page.

      {oe/rep/rel-more.i}

      v-last-page = page-number.
      delete b-report.
    end. /* for each oe-relh */

    output stream last-page close.
