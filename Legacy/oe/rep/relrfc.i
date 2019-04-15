/* ---------------------------------------------- oe/rep/relrfc.i  08/08/2018 YSK*/
/* Print OE Release/Picking tickets    for RFC                       */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

DEFINE BUFFER ref-lot-no FOR reftable.

define workfile w-oe-rell like oe-rell
    field seq    as   integer
    field set-no like fg-set.set-no.

define workfile w-bin 
    field w-loc like fg-bin.loc
    field w-bin like fg-bin.loc-bin
    field w-tag like fg-bin.tag
    field w-qty like fg-bin.qty extent 2
    field w-c-c like fg-bin.case-count
    field w-cas as   decimal
    field w-pal as   decimal
    field w-par like oe-ordl.part-dscr1
    field w-x   as   log
    FIELD w-i-no AS cha
    FIELD w-po-no AS cha 
    FIELD w-count-pro AS INTEGER .

define buffer b-cust    for cust.
define buffer b-ship    for shipto.
define buffer b-w-bin   for w-bin.
define buffer bff-w-bin for w-bin.

define        variable v-frt-pay-dscr    as character format "x(11)" no-undo.
define        variable v-bin             as character no-undo.
define        variable v-print           as log       no-undo.
define        variable v-part-info       like itemfg.i-name no-undo.
define        variable v-qty             like oe-rell.qty no-undo.
define        variable v-rel-qty         like v-qty NO-UNDO.

define        variable v-units-hdr       as character format "x(5)" extent 2.
define        variable v-zone-hdr        as character format "x(10)".
define        variable v-zone            like shipto.dest-code.
define        variable v-part-dscr       like oe-ordl.i-name.
/* === with xprint ====*/
DEFINE        VARIABLE v-term            AS cha       NO-UNDO.
DEFINE        VARIABLE ls-image1         AS cha       NO-UNDO.
DEFINE        VARIABLE ls-image2         AS cha       NO-UNDO.
DEFINE        VARIABLE v-csr             AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE s-print-what-item AS cha       NO-UNDO.
DEFINE SHARED VARIABLE s-print-loc-from  AS cha       NO-UNDO.
DEFINE SHARED VARIABLE s-print-loc-to    AS cha       NO-UNDO.
DEFINE SHARED VARIABLE s-print-bin-from  AS cha       NO-UNDO.
DEFINE SHARED VARIABLE s-print-bin-to    AS cha       NO-UNDO.

format w-oe-rell.ord-no                 
    "<c7>" w-par          format "x(20)"
    "<c24>" v-bin         format "x(20)"
    "<c49.5>" w-x         format "X/"
    "<c52.7>" w-pal       format "->>>>"
    "<c57.5>" w-cas         format "->>>>"
    "<c62.5>" w-c-c         format "->>>>>>>>"
    "<c72.5>" w-qty[1]      format "->>>>>>>>"
    
    with down frame rel-mid no-box no-labels STREAM-IO width 150.



DEFINE VARIABLE v-tel           AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax           AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact       AS cha       FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-line-total    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-quo-total     AS DECIMAL   NO-UNDO.
define variable v-t-tax         as decimal   extent 3 NO-UNDO.
define variable v-bot-lab       as character format "x(63)" extent 3 NO-UNDO.
DEFINE VARIABLE v-q-no          LIKE oe-ord.q-no NO-UNDO.
DEFINE VARIABLE v-printline     AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-ship-i        AS cha       FORM "x(60)" EXTENT 4 NO-UNDO.

DEFINE VARIABLE ll-display-comp AS LOG       NO-UNDO.  /* display company address */
DEFINE VARIABLE ll-consol-rells AS LOG       NO-UNDO.
DEFINE VARIABLE lv-comp-name    AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-email        AS cha       FORM "x(56)" NO-UNDO.

DEFINE VARIABLE lv-comp-color   AS cha       NO-UNDO.
DEFINE VARIABLE lv-other-color  AS cha       INIT "BLACK" NO-UNDO.
DEFINE BUFFER xitemfg FOR itemfg.
DEFINE        VARIABLE lv-comp-unit       AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE v-print-components AS LOG     NO-UNDO.
DEFINE SHARED VARIABLE s-print-part-no    AS LOG     NO-UNDO.
ASSIGN 
    tmpstore = fill("-",130).

DEFINE VARIABLE cRtnChar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS CHARACTER FORMAT "x(200)" NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).

ASSIGN 
    ls-full-img1 = cRtnChar + ">" .

find first sys-ctrl where sys-ctrl.company eq cocode
    and sys-ctrl.name    eq "RELPRINT" no-lock no-error.
ASSIGN
    ll-display-comp = AVAILABLE sys-ctrl AND sys-ctrl.log-fld
    ll-consol-rells = AVAILABLE sys-ctrl AND sys-ctrl.int-fld NE 0.

FIND first sys-ctrl where sys-ctrl.company eq cocode
    and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.
ASSIGN 
    v-comp-add1  = ""
    v-comp-add2  = ""
    v-comp-add3  = ""
    v-comp-add4  = ""
    v-comp-add5  = ""
    lv-email     = ""
    lv-comp-name = "".

IF ll-display-comp THEN 
DO:
    FIND FIRST cust WHERE cust.company = cocode AND
        cust.active = "X" NO-LOCK NO-ERROR.
    /*  ASSIGN v-comp-add1 = company.addr[1]
             v-comp-add2 = company.addr[2]
             v-comp-add3 = company.city + ", " + company.st + "  " + company.zip
             v-comp-add4 = "Phone:  " + IF AVAIL cust THEN string(cust.area-code,"(999)") + string(cust.phone,"999-9999") ELSE "" 
             v-comp-add5 = "Fax     :  " + IF AVAIL cust THEN string(cust.fax,"(999)999-9999") ELSE ""
             lv-email    = "Email:  " + IF AVAIL cust THEN cust.email ELSE ""
             lv-comp-name = company.NAME   
             .
    */ 
    IF AVAILABLE cust THEN
        ASSIGN v-comp-add1  = cust.addr[1]
            v-comp-add2  = cust.addr[2]
            v-comp-add3  = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email     = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            .

END.

format
    tt-rell.ord-no
    tt-rell.po-no at 8
    tt-rell.loc-bin  AT 23  FORM "x(5)"
    tt-rell.i-no at 29  oe-ordl.i-name at 44
    /*tt-rell.cases format ">>>>>9" to 73                    */
    oe-ordl.qty format "->>>>>>>9" to 83
    tt-rell.qty format "->>>>>>>9" SKIP
    tt-rell.tag AT 29 FORM "x(15)"
    oe-ordl.part-dscr1 at 44 format "x(30)" skip
    oe-ordl.part-no AT 29
    oe-ordl.part-dscr2 at 44 format "x(30)"
    /*locbin[2] at 23     
    locbin[3] at 23
    
    locbin[4] at 23*/
    with down frame relprint no-box no-labels STREAM-IO width 110.
/*
format
  tt-rell.ord-no
  tt-rell.po-no at 12
  tt-rell.i-no at 28
  oe-ordl.qty format "->>>>>>>9" to 58
  tt-rell.qty format "->>>>>>>9" to 68
  with down frame ln-s no-box no-labels STREAM-IO width 85.
*/


find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
v-printline = 0.

if v-zone-p then v-zone-hdr = "Route No.:".

{oe/rep/foreachr.i},

first cust
where cust.company eq cocode
and cust.cust-no eq oe-relh.cust-no
no-lock

break by {1} by oe-relh.release#:

RUN oe/custxship.p (oe-relh.company,
    oe-relh.cust-no,
    oe-relh.ship-id,
    BUFFER shipto).

assign
    v-tot-qty = 0
    v-weight  = 0
    v-pallets = 0
    v-zone    = shipto.dest-code
    v-csr     = oe-relh.user-id.

find first carrier
    where carrier.company eq cocode
    and carrier.carrier eq oe-relh.carrier
    no-lock no-error.
      
assign
    v-carrier   = if available carrier then carrier.dscr else ""
    v-frt-terms = "".

FOR EACH xoe-rell
    where xoe-rell.company eq oe-relh.company
    and xoe-rell.r-no    eq oe-relh.r-no
    USE-INDEX r-no NO-LOCK,
    FIRST oe-ord
    where oe-ord.company eq xoe-rell.company
    and oe-ord.ord-no  eq xoe-rell.ord-no
    no-lock:

    case oe-ord.frt-pay:
        when "P" THEN 
            v-frt-terms = "Prepaid".
        when "C" THEN 
            v-frt-terms = "Collect".
        when "B" THEN 
            v-frt-terms = "Bill".
        when "T" THEN 
            v-frt-terms = "Third Party".
    end case.

    LEAVE.
END.
/* =======
      FOR EACH tt-rell:
        DELETE tt-rell.
      END.

      /** Calculate the total weight of the released items. **/
      for each xoe-rell
          where xoe-rell.company eq cocode
            and xoe-rell.r-no    eq oe-relh.r-no:
        find first xoe-ordl
            where xoe-ordl.company eq cocode
              and xoe-ordl.ord-no  eq xoe-rell.ord-no
              and xoe-ordl.line    eq xoe-rell.line
              and xoe-ordl.i-no    eq xoe-rell.i-no
            use-index ord-no no-lock no-error.
        if avail xoe-ordl then
          assign
           v-tot-qty = v-tot-qty + xoe-rell.qty
           v-weight = v-weight + (if xoe-ordl.t-weight ne ? then
                                  (round(xoe-ordl.t-weight /
                                   xoe-ordl.qty, 2) * xoe-rell.qty) else 0).

        if avail xoe-ordl and xoe-ordl.est-no ne "" then do:
          find first eb
              where eb.company  eq xoe-ordl.company
                and eb.est-no   eq xoe-ordl.est-no
                and eb.form-no  eq xoe-ordl.form-no
                and eb.blank-no eq xoe-ordl.blank-no
              no-lock no-error.

          if xoe-ordl.form-no eq 0                             and
             (xoe-ordl.est-type eq 2 or xoe-ordl.est-type eq 6) then do:
            for each fg-set
                where fg-set.company eq xoe-ordl.company
                  and fg-set.set-no  eq xoe-ordl.i-no
                no-lock:
              v-set-qty = v-set-qty + fg-set.QtyPerSet.
            end.
            if v-set-qty eq 0 then v-set-qty = 1.
            for each eb
                where eb.company eq xoe-ordl.company
                  and eb.est-no  eq xoe-ordl.est-no
                  and eb.form-no ne 0
                no-lock:
              find fg-set
                  where fg-set.company eq xoe-ordl.company
                    and fg-set.set-no  eq xoe-ordl.i-no
                    and fg-set.part-no eq eb.stock-no
                  no-lock no-error.

              assign
               v-part-qty = (if avail fg-set and fg-set.QtyPerSet ne 0 then
                             fg-set.QtyPerSet else 1) / v-set-qty
               v-pallets = v-pallets +
                           (if xoe-rell.qty-case ne 0 then
                              round((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                            else
                            if eb.cas-cnt ne 0 then
                              round((round((v-tot-qty * v-part-qty) /
                                         eb.cas-cnt, 2) / eb.cas-pal) + .49, 0)
                            else
                              round((round((v-weight * v-part-qty) /
                                         eb.cas-wt, 2) / eb.cas-pal) + .49, 0)).
            end. /* each eb */
          end. /* do */
          else
          if avail eb then do:
            assign
             v-pallets = v-pallets +
                         (if xoe-rell.qty-case ne 0 then
                            round((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                          else
                          if eb.cas-cnt ne 0 then
                            round((round(v-tot-qty / eb.cas-cnt, 2) /
                                                       eb.cas-pal) + .49, 0)
                          else
                            round((round(v-weight / eb.cas-wt, 2) /
                                                       eb.cas-pal) + .49, 0)).
          end. /* do */
        end. /* est-no ne "" */

        IF ll-consol-rells THEN DO:
          IF (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty-case, xoe-rell.cases).

          IF xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases), 1).
        END.

        ELSE DO:
          CREATE tt-rell.
          BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell.
        END.

        xoe-rell.printed = YES.
      end. /* each xoe-rell */
========*/

/* from relcntbx.p */
FOR EACH w-oe-rell:
    DELETE w-oe-rell.
END.
         
for each oe-rell
    where oe-rell.company eq cocode
    and oe-rell.r-no    eq oe-relh.r-no,

    first itemfg of oe-rell no-lock:
          
    create w-oe-rell.
    buffer-copy oe-rell to w-oe-rell.
        
    assign
        i                = 0 
        w-oe-rell.seq    = i
        w-oe-rell.set-no = oe-rell.i-no
        oe-rell.printed  = yes.

    /* gdm - 03230907 */
    IF v-print-components AND
        itemfg.isaset       AND 
        itemfg.alloc NE YES 
        THEN 
        for each fg-set
            where fg-set.company eq cocode
            and fg-set.set-no  eq oe-rell.i-no
            no-lock:
            
            {sys/inc/part-qty.i v-part-qty fg-set}
          
            create w-oe-rell.
            buffer-copy oe-rell to w-oe-rell.
          
            assign
                i                = i + 1
                w-oe-rell.seq    = i
                w-oe-rell.set-no = oe-rell.i-no
                w-oe-rell.i-no   = fg-set.part-no
                w-oe-rell.qty    = w-oe-rell.qty * v-part-qty.
        end.

    v-weight = v-weight + (oe-rell.qty * itemfg.weight-100 / 100).
end.

{oe/rep/relrfc2.i}

for each w-bin:
    delete w-bin.
end.

for each w-oe-rell,
      
    first oe-ordl
    where oe-ordl.company eq cocode
    and oe-ordl.ord-no  eq w-oe-rell.ord-no
    and oe-ordl.i-no    eq w-oe-rell.set-no
    and oe-ordl.line    eq w-oe-rell.line
    no-lock,
      
    first itemfg of w-oe-rell no-lock
                
    break by w-oe-rell.set-no
    by w-oe-rell.seq
    by w-oe-rell.i-no
    by w-oe-rell.po-no:

    v-rel-qty = v-rel-qty + w-oe-rell.qty.
        
    if last-of(w-oe-rell.po-no) then 
    do:
        for each w-bin:
            delete w-bin.
        end.
        
        i = 0.
        for each fg-bin
            where fg-bin.company  eq cocode
            and fg-bin.i-no     eq w-oe-rell.i-no
            /*  and (fg-bin.job-no  eq oe-ordl.job-no  or oe-ordl.job-no eq "")
              and (fg-bin.job-no2 eq oe-ordl.job-no2 or oe-ordl.job-no eq "") */
            AND ((s-print-what-item = "R" /*AND (fg-bin.tag = w-oe-rell.tag OR w-oe-rell.tag = "")*/ ) 
            OR (s-print-what-item = "I" AND fg-bin.loc >= s-print-loc-from AND fg-bin.loc <= s-print-loc-to
            AND fg-bin.loc-bin >= s-print-bin-from AND fg-bin.loc-bin <= s-print-bin-to)
            )
            and fg-bin.qty      gt 0
            no-lock:

            IF s-print-what-item = "R" AND
                NOT CAN-FIND(FIRST oe-rell
                WHERE oe-rell.company  EQ w-oe-rell.company
                AND oe-rell.r-no     EQ w-oe-rell.r-no
                AND oe-rell.ord-no   EQ w-oe-rell.ord-no
                AND oe-rell.i-no     EQ w-oe-rell.i-no
                AND oe-rell.line     EQ w-oe-rell.line
                AND oe-rell.rel-no   EQ w-oe-rell.rel-no
                AND oe-rell.b-ord-no EQ w-oe-rell.b-ord-no
                AND oe-rell.po-no    EQ w-oe-rell.po-no
                AND oe-rell.loc      EQ fg-bin.loc
                AND oe-rell.loc-bin  EQ fg-bin.loc-bin
                AND oe-rell.tag      EQ fg-bin.tag)
                THEN NEXT.

            /*    FIND FIRST w-bin WHERE w-bin.w-i-no = fg-bin.i-no
                                   AND w-bin.w-po-no = w-oe-rell.po-no
                                   AND w-bin.w-tag = fg-bin.tag NO-LOCK NO-ERROR.
                IF AVAIL w-bin THEN DO: */
            create w-bin.
            assign
                w-tag    = fg-bin.tag
                w-loc    = fg-bin.loc
                w-bin    = fg-bin.loc-bin
                w-qty[1] = fg-bin.qty
                w-qty[2] = fg-bin.qty 
                w-c-c    = fg-bin.case-count
                w-x      = CAN-FIND(FIRST oe-rell
                                 WHERE oe-rell.company  EQ w-oe-rell.company
                                   AND oe-rell.r-no     EQ w-oe-rell.r-no
                                   AND oe-rell.ord-no   EQ w-oe-rell.ord-no
                                   AND oe-rell.i-no     EQ w-oe-rell.i-no
                                   AND oe-rell.line     EQ w-oe-rell.line
                                   AND oe-rell.rel-no   EQ w-oe-rell.rel-no
                                   AND oe-rell.b-ord-no EQ w-oe-rell.b-ord-no
                                   AND oe-rell.po-no    EQ w-oe-rell.po-no
                                   AND oe-rell.loc      EQ fg-bin.loc
                                   AND oe-rell.loc-bin  EQ fg-bin.loc-bin
                                   AND oe-rell.tag      EQ fg-bin.tag)
                w-i-no   = fg-bin.i-no
                w-po-no  = w-oe-rell.po-no
                i        = i + 1.
             
            assign
                w-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                     (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                     (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
                w-pal = w-qty[1] / w-pal.

            {sys/inc/roundup.i w-pal}
        end.
          
        for each w-bin break by w-loc by w-bin:
            v-qty = v-qty + w-qty[1].
            
            if last-of(w-bin) then 
            do:
                for each b-w-bin
                    where b-w-bin.w-loc eq w-bin.w-loc
                    and b-w-bin.w-bin eq w-bin.w-bin:
                    b-w-bin.w-qty[2] = v-qty.
                end.
              
                v-qty = 0.
            end.
        end.
        i = 0 .
        for each w-bin  by w-qty[2] descending by w-qty[1] descending:  /* Strang problem in w-qty[2] sorting */
            ASSIGN 
                w-par       = "" 
                w-count-pro = i 
                i           = i + 1 .
        END.
          
        if i eq 0 then 
        do:
            find first b-cust
                where b-cust.company eq cocode
                and b-cust.active  eq "X" 
                no-lock no-error.
            if available b-cust then 
            do:
                find first b-ship
                    where b-ship.company eq cocode
                    and b-ship.cust-no eq b-cust.cust-no
                    no-lock no-error.
                if available b-ship then 
                do:
                    create w-bin.
                    assign   
                        w-loc = b-ship.loc
                        w-bin = b-ship.loc-bin
                        i     = i + 1.
                end.   
            end.
        end.
         
        do i = i to 7:
            create w-bin.
            FOR EACH bff-w-bin NO-LOCK BY w-count-pro DESCENDING  :
                w-bin.w-count-pro = bff-w-bin.w-count-pro + 1 .
                LEAVE.
            END.
        end.
           
        for each w-bin where w-par eq "" BY w-count-pro by w-qty[2] descending by w-qty[1] descending:
            w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-no
            else itemfg.part-no.
            leave.
        end. 
        
        for each w-bin where w-par eq "" BY w-count-pro by w-qty[2] descending by w-qty[1] descending:
            w-par = /*if w-oe-rell.seq eq 0 then*/ oe-ordl.i-name
            /*else itemfg.i-name*/.
            leave.
        end.
        
        for each w-bin where w-par eq "" BY w-count-pro by w-qty[2] descending by w-qty[1] DESCENDING:
            w-par = /*if w-oe-rell.seq eq 0 AND*/ IF oe-ordl.part-dscr1 NE "" then oe-ordl.part-dscr1
            else itemfg.part-dscr1.
            leave.
        end.
            
        for each w-bin where w-par eq "" BY w-count-pro by w-qty[2] descending by w-qty[1] DESCENDING:
            w-par = /*if w-oe-rell.seq eq 0 AND */ IF oe-ordl.part-dscr2 NE "" then oe-ordl.part-dscr2
            else itemfg.part-dscr2.
            LEAVE.
        end.
          
        for each w-bin where w-par eq "" BY w-count-pro by w-qty[2] descending by w-qty[1] DESCENDING:
            w-par = /*if w-oe-rell.seq eq 0 AND*/ IF oe-ordl.part-dscr3 NE "" then oe-ordl.part-dscr3
            else itemfg.part-dscr3.
            leave.
        end.
          
        for each w-bin where w-par eq "" BY w-count-pro by w-qty[2] descending by w-qty[1] descending:
            w-par = w-oe-rell.i-no.
            leave.
        end.
         
        for each w-bin where w-par eq "" BY w-count-pro by w-qty[2] descending by w-qty[1] descending:
            if w-oe-rell.po-no ne "" then w-par = "PO#: " + w-oe-rell.po-no.
            leave.
        end.

        for each w-bin where w-par eq ""  BY w-count-pro by w-qty[2] descending by w-qty[1] descending:
            

            ASSIGN 
                w-par = w-oe-rell.lot-no.
            leave.
        end.

        j = 6.
        for each w-bin break by w-qty[2] descending by w-qty[1] descending:
            if w-par eq "" and w-loc eq "" and w-bin eq "" then delete w-bin.
            else j = j + 1.
        end.

        for each w-bin break by w-qty[2] descending by w-qty[1] descending:
            if last(w-qty[2]) and (w-loc ne "" or w-bin ne "") then j = j + 1. 
        end.
          
        v-print = yes.
        if available oe-rel then 
        do i = 1 to 4:
            if oe-rel.ship-i[i] ne "" then 
            do:
                if v-print then j = j + 1.
                assign
                    j       = j + 1
                    v-print = no.
            end.
        end.
          
        for each w-bin break by w-qty[2] descending by w-qty[1] descending:
            assign
                w-cas = w-qty[1] / w-c-c
                v-bin = trim(substr(w-tag,16,5)) + "/" +
                     trim(w-loc)              + "/" +
                     trim(w-bin).
            
            if w-cas eq ? then w-cas = 0.
           
            if trim(v-bin) eq "//" then v-bin = "".
          
            {sys/inc/roundup.i w-cas}

            IF v-printline > 44 THEN 
            DO:
                PAGE.
                v-printline = 0.
                {oe/rep/relrfc2.i}
            END.

            display {2}
                w-oe-rell.ord-no    
                when first(w-qty[2])
                w-par
                v-bin
                w-x
                w-pal
                w-cas
                w-c-c
                w-qty[1]
                   
                with frame rel-mid. 
               
            v-printline = v-printline + 1.

            if last(w-qty[2]) then 
            do:
                if w-loc ne "" or w-bin ne "" then down {2} with frame rel-mid.

                IF v-printline > 44 THEN 
                DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/relrfc2.i}
                END.

                  
                display {2}
                    "Rel Qty"       @ w-c-c
                    v-rel-qty format "->>>>>>>>"        @ w-qty[1]

                    with frame rel-mid. 

                v-printline = v-printline + 1.
            end.
            
            down {2} with frame rel-mid.
            
        end.  /* for eacn w-bin*/
        v-rel-qty = 0.
    end.  /* last-of(w-oe-rell.po-no) */
end. /* for each w-oe-rell */

ASSIGN 
    v-ship-i[1] = oe-relh.ship-i[1]
    v-ship-i[2] = oe-relh.ship-i[2]
    v-ship-i[3] = oe-relh.ship-i[3]
    v-ship-i[4] = oe-relh.ship-i[4].

/*Leave shipping notes at bottom of new page*/
IF v-printline > 37 AND
    (v-ship-i[1] NE "" OR
    v-ship-i[2] NE "" OR
    v-ship-i[3] NE "" OR
    v-ship-i[4] NE "") THEN 
DO:
    PAGE.
    v-printline = 0.
    {oe/rep/relrfc2.i}
END.

IF v-ship-i[1] NE "" OR
    v-ship-i[2] NE "" OR
    v-ship-i[3] NE "" OR
    v-ship-i[4] NE "" THEN
    PUT "<FMS Sans Serif><R50><C1><P12><B>     Shipping Instructions: </B> <P9> "SKIP(1)
        "<R51><C1>" v-ship-i[1] AT 7 
        "<R52><C1>" v-ship-i[2] AT 7 
        "<R53><C1>" v-ship-i[3] AT 7 
        "<R54><C1>" v-ship-i[4] AT 7.
ELSE
    PUT "<FMS Sans Serif><R50><C1><P12><B></B> <P9> "SKIP(1).

PUT "<R56><C1>"
    "__________________________________________________________________________________________________________________"  SKIP 

    "<|10><C1><R58><#8><FROM><C80><R60><RECT> " 
    "<=8> Pulled By                                         Checked By                                        # of Units                                         Total Weight/Cube" SKIP
    "<R58><C20><FROM><R60><C20><Line>" 
    "<R58><C40><FROM><R60><C40><Line>" 
    "<R58><C60><FROM><R60><C60><Line>".
             
PAGE.

ASSIGN
    v-printline = 0.
RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    
end. /* for each oe-relh */

RETURN.

PROCEDURE create-tt-rell.
    DEFINE INPUT PARAMETER ip-qty-case LIKE oe-rell.qty-case NO-UNDO.
    DEFINE INPUT PARAMETER ip-cases    LIKE oe-rell.cases NO-UNDO.


    IF ip-qty-case LT 0 THEN
        ASSIGN
            ip-qty-case = ip-qty-case * -1
            ip-cases    = ip-cases * -1.

    FIND FIRST tt-rell
        WHERE tt-rell.i-no     EQ xoe-rell.i-no
        AND tt-rell.po-no    EQ xoe-rell.po-no
        AND tt-rell.ord-no   EQ xoe-rell.ord-no
        AND tt-rell.line     EQ xoe-rell.line
        AND tt-rell.qty-case EQ ip-qty-case
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE tt-rell THEN 
    DO:
    
        CREATE tt-rell.
        BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell
            ASSIGN
            tt-rell.qty-case = ip-qty-case
            tt-rell.cases    = 0
            tt-rell.qty      = 0
            tt-rell.partial  = 0.
    END.

    ASSIGN
        tt-rell.cases = tt-rell.cases + ip-cases
        tt-rell.qty   = tt-rell.qty + (ip-qty-case * ip-cases).

    IF xoe-rell.p-c THEN tt-rell.p-c = YES.

END PROCEDURE.
