/* ---------------------------------------------- oe/rep/bolxapc.i 12/05 YSK */
/* PRINT APC  BOL                                                           */
/* -------------------------------------------------------------------------- */

DEF VAR v-tot-cnt AS INT.

assign
 v-tot-wt    = 0
 v-tot-cases = 0
 v-tot-palls = 0.

for each report where report.term-id eq v-term-id,

    first oe-boll where recid(oe-boll) eq report.rec-id,

    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

    first itemfg
    where itemfg.company eq oe-boll.company
      and itemfg.i-no    eq oe-boll.i-no
    no-lock

    break by report.key-01 /* oe-boll.i-no*/
          by report.key-02 /* oe-boll.ord-no*/
          BY oe-boll.line
          BY oe-boll.po-no
          BY oe-boll.job-no
          BY oe-boll.job-no2:

    IF FIRST-OF(oe-boll.job-no2) THEN DO:
       ASSIGN v-ship-qty = 0
              v-weight   = 0.
    END.

  ASSIGN
   v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
                (if oe-boll.partial gt 0 then 1 else 0)
   v-pal-cnt  = oe-boll.qty-case.
   v-tot-cnt =  v-tot-cnt +  v-tot-pkgs.

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ oe-boll.company
        AND fg-bin.i-no    EQ oe-boll.i-no
        AND fg-bin.job-no  EQ oe-boll.job-no
        AND fg-bin.job-no2 EQ oe-boll.job-no2
        AND fg-bin.loc     EQ oe-boll.loc
        AND fg-bin.loc-bin EQ oe-boll.loc-bin
        AND fg-bin.tag     EQ oe-boll.tag
      NO-LOCK NO-ERROR.

  IF AVAIL fg-bin THEN
    v-pal-cnt = v-pal-cnt                                                     *
                (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
                (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet).


  v-pal-cnt = oe-boll.qty / v-pal-cnt.


  {sys/inc/roundup.i v-pal-cnt}

  v-tot-palls = v-tot-palls + v-pal-cnt.


  if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
    if not avail w2 then 
      create w2.
    assign
     w2.cas-cnt = oe-boll.qty-case
     w2.cases   = w2.cases + oe-boll.cases.
  end.

  if oe-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
    if not avail w2 then 
       create w2.
    assign
     w2.cas-cnt = oe-boll.partial 
     w2.cases   = w2.cases + 1. 
  end.

  release oe-rel.
  find first oe-rell
      where oe-rell.company eq cocode
        and oe-rell.r-no    eq oe-boll.r-no
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        and oe-rell.i-no    eq oe-boll.i-no
        and oe-rell.line    eq oe-boll.line
      no-lock no-error.
  if avail oe-rell then do:
    find first oe-relh of oe-rell no-lock.
    find first oe-rel
        where oe-rel.company eq cocode
          and oe-rel.ord-no  eq oe-rell.ord-no
          and oe-rel.line    eq oe-rell.line
          and oe-rel.link-no eq oe-rell.r-no
          and oe-rel.ship-no eq oe-relh.ship-no
          and oe-rel.i-no    eq oe-rell.i-no
        no-lock no-error.
    if not avail oe-rel then
      find first oe-rel
          where oe-rel.company  eq cocode
            and oe-rel.ord-no   eq oe-rell.ord-no
            and oe-rel.line     eq oe-rell.line
            and oe-rel.rel-date eq oe-relh.rel-date
            and oe-rel.ship-no  eq oe-relh.ship-no
            and oe-rel.i-no     eq oe-rell.i-no
          no-lock no-error.
  end.
    
    find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq int(report.key-02)
         and oe-ordl.i-no    eq report.key-01
         no-lock no-error.

    IF v-printline >= 36 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolxapc2.i}
    END.

    v-job-no = "".
    if avail oe-ordl and oe-ordl.job-no ne "" then
       v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
               trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).

    ASSIGN v-ship-qty = v-ship-qty + oe-boll.qty
           v-weight   = v-weight + oe-boll.weight.

    IF lv-bolfmt-int = 1 THEN DO:  /* show summary per item */
     IF LAST-OF(oe-boll.job-no2) THEN DO:
         i = 0.
         
         FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
            v-tot-cases = v-tot-cases + (w2.cases * w2.cas-cnt).
             i = i + 1.
             IF i eq 1 THEN ASSIGN v-part-dscr = oe-ordl.part-no
                            v-job-po    = oe-boll.po-no.
             ELSE if i eq 2 THEN 
                  ASSIGN v-part-dscr = oe-ordl.part-dscr1 /*i-name*/
                         v-job-po    = if oe-ordl.job-no eq "" then "" else
                                (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
             else if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.
             ELSE if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
             
             IF v-printline >= 38 THEN DO:
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bolxapc2.i}
             END.
             IF FIRST(w2.cases * w2.cas-cnt) THEN 
                PUT {1} /*TRIM(string(oe-ordl.qty,"->>,>>>,>>>")) FORM "x(15)"*/
                    oe-ordl.part-no
                    v-job-po  AT 17 FORM "x(15)" 
                    oe-boll.i-no  SPACE(1)
                    oe-ordl.i-name FORM "x(22)"
                    w2.cases       FORM "->>>>9" " @" SPACE(0)
                    w2.cas-cnt     FORM "->>>>>9"
                    /*v-ship-qty      TO 85 FORM "->>>>>9"
                    oe-boll.p-c     AT 88
                    v-weight        AT 91   */
                    SKIP.
              ELSE PUT {1} 
                       /*v-job-po FORM "x(15)" AT 17*/
                       oe-ordl.ord-no SPACE(33)
                       oe-ordl.part-dscr1 /*v-part-dscr*/ FORM "x(30)" 
                       w2.cases   FORM "->>>>9" " @" SPACE(0)
                       w2.cas-cnt FORM "->>>>>9" SKIP.
              v-printline = v-printline + 1.
              /*IF (i > 1 OR (i = 1 AND last(w2.cases * w2.cas-cnt))) AND v-part-dscr <> "" THEN do:
                 PUT {1} v-part-dscr FORM "x(30)" AT 33 SKIP.
                 v-printline = v-printline + 1.
              END.*/
              IF last(w2.cases * w2.cas-cnt) THEN DO:
                 IF FIRST(w2.cases * w2.cas-cnt) THEN do:
                    PUT {1} 
                       /*v-job-po FORM "x(15)" AT 17*/
                       oe-ordl.ord-no
                       oe-ordl.part-dscr1 /*v-part-dscr*/ FORM "x(30)" 
                       /*w2.cases  AT 71 FORM "->>>9" " @ "
                       w2.cas-cnt FORM "->>>>>9"*/ SKIP.
                    v-printline = v-printline + 1.
                 END.
                 PUT {1} "===================" AT 70 SKIP
                     SPACE(69)
                     v-tot-pkgs FORM "->>>>9"  " =" SPACE(0)
/*                      oe-boll.qty FORM "->>>>>z" SPACE(3) */
                     v-tot-cases FORM "->>>>>z" SPACE(2)
                     oe-boll.p-c /*v-part-comp*/  SPACE(1)
                     v-weight FORM "->>>,>>9"  SKIP.
                     v-printline = v-printline + 2.
                     
              END.
              
/*               v-tot-cases = v-tot-cases + w2.cases. */
              DELETE w2.
         END.
         put {1} skip(1).
         ASSIGN
            v-tot-cases = 0
            v-tot-pkgs  = 0
            v-printline = v-printline + 1.
     END.
  END.
  /* end of summary mods */
  ELSE DO:
     DISPLAY  {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          oe-boll.po-no 
          oe-boll.i-no 
          oe-ordl.i-name  FORM "x(18)"
          oe-boll.cases FORM "->>>,>>>"  "@" SPACE(0)
          oe-boll.qty-case FORM "->>>>>Z" SKIP          
          oe-ordl.part-dscr1 FORM "x(30)" SPACE(38)
          v-1    FORM "->>,>>9"  when oe-boll.partial gt 0 "@" SPACE(0)
          oe-boll.partial   when oe-boll.partial gt 0 FORM "->>>>>z"  SKIP 
          space(69) "==================="  SKIP
          SPACE(68)
          v-tot-pkgs  FORM "->>,>>9"  "=" SPACE(0)
          oe-boll.qty FORM "->>>>>z" SPACE(2)
          oe-boll.p-c /*v-part-comp*/  SPACE(0)
          oe-boll.weight FORM "->>>,>>9" SKIP
          with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
     down {1} with frame bol-mid1.
     put {1} skip(1).
     ASSIGN
      v-printline = v-printline + 5
      v-tot-pkgs  = 0
      v-tot-cases = 0.
  END.
  assign
   v-tot-wt    = v-tot-wt + oe-boll.weight.
/*    v-tot-pkgs  = 0. */

  if oe-boll.weight eq 0 then
    v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
