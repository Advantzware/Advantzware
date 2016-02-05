/* --------------------------------------------- oe/rep/bollprnt22.i 10/09 GDM */
/* N-K BOLFMT = Loylang - FORM for Loylang                                    */
/* -------------------------------------------------------------------------- */
assign
 v-tot-wt    = 0
 v-tot-cases = 0
 v-tot-palls = 0 .

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
  
    IF FIRST-OF(report.key-02) THEN 
       ASSIGN v-ship-qty = 0
              v-weight   = 0
              v-ord-qty  = 0.
       
  ASSIGN
   v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
                if oe-boll.partial gt 0 then 1 else 0
   v-pal-cnt  = oe-boll.qty-case.

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

  /*v-tot-palls = v-tot-palls + v-pal-cnt.*/
   v-tot-palls = v-tot-palls + v-tot-pkgs.

  if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.qty-case
     w2.cases   = w2.cases + oe-boll.cases.
  end.

  if oe-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.partial
     w2.cases   = w2.cases + 1.
  end.
    
  find first oe-ordl where oe-ordl.company eq cocode
       and oe-ordl.ord-no  eq int(report.key-02)
       and oe-ordl.i-no    eq report.key-01
       no-lock no-error.

  find first oe-rel where oe-rel.company eq cocode
       and oe-rel.ord-no  eq int(report.key-02)
       and oe-rel.i-no    eq report.key-01
       and oe-rel.rel-no  eq oe-boll.rel-no
       no-lock no-error.

  find first reftable where reftable.reftable eq "oe-rel.lot-no" 
       and reftable.company  EQ string(oe-rel.r-no,"9999999999")
       no-lock no-error.

/*  IF v-printline >= 36 THEN DO:
     v-printline = 0.
     PAGE {1}.
     {oe/rep/bollprnt12.i}
  END. */

  v-job-no = "".
  if avail oe-ordl and oe-ordl.job-no ne "" then
     v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
                trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).

  ASSIGN v-ship-qty = v-ship-qty + oe-boll.qty
         v-weight   = v-weight + oe-boll.weight
         v-ord-qty = v-ord-qty + oe-ordl.qty.

  IF LAST(report.key-01) THEN
  if v-print-components THEN do:
        for each fg-set
            where fg-set.company eq cocode
              and fg-set.set-no  eq oe-boll.i-no
            no-lock,
            
            first xitemfg
            where xitemfg.company eq cocode
              and xitemfg.i-no    eq fg-set.part-no
            no-lock
            
            break by fg-set.set-no:
            
          {sys/inc/part-qty.i v-part-qty fg-set}
          
          put {1}
              xitemfg.part-no
              fg-set.part-no                  AT 33
              xitemfg.i-name                        FORMAT "x(22)"
              oe-boll.qty * v-part-qty        TO 80 FORMAT "->>>,>>9"
              skip.
        
        /*  v-printline = v-printline + 2.*/
        end.
 END.

  IF lv-bolfmt-int = 1 THEN DO:  /* show summary per item */
    IF LAST-OF(report.key-02) THEN DO:
       
       i = 0.
       v-recnt = 0 .
         .

       FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
           v-recnt = v-recnt + 1.
       END.

       FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
         i = i + 1.
         IF i eq 1 THEN ASSIGN v-job-po    = oe-boll.po-no                                                              
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))
                               v-fgitem    = oe-boll.i-no
                               v-part-dscr = oe-ordl.i-name. 
         ELSE
         if i eq 2 THEN ASSIGN v-part-dscr = oe-ordl.part-dscr1
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))
                               v-job-po    = IF AVAIL reftable THEN reftable.code 
                                                               ELSE ""
                               v-fgitem    = oe-ordl.part-no.
         ELSE
         if i eq 3 then ASSIGN v-part-dscr = oe-ordl.part-dscr2
                               v-job-var = ""
                               v-job-po  = ""
                               v-fgitem  = "".
         ELSE
         if i eq 4 then ASSIGN v-part-dscr = ""
                               v-job-var = ""
                               v-job-po  = ""
                               v-fgitem  = "".  
              
       /*  IF v-printline >= 38 THEN DO:
           v-printline = 0.
           PAGE {1}.
           {oe/rep/bollprnt12.i}
         END. */
        
           create tt-w2.
           assign
               tt-w2.cas-cnt = w2.cas-cnt
               tt-w2.cases   = w2.cases .

         IF FIRST(w2.cases * w2.cas-cnt) THEN 
           PUT {1} oe-ordl.ord-no FORM ">>>>>9"
                   v-job-po FORM "x(15)" AT 11
                   v-fgitem  AT 27 
                   v-part-dscr FORMAT "x(30)" AT 42
                   w2.cases    AT 72 FORMAT "->>>9" " @"
                   w2.cas-cnt    FORM "->>>>9"
                   SKIP.
         ELSE PUT {1} 
                  v-job-var FORM "x(9)"
                  v-job-po  FORM "x(15)" AT 11
                  v-fgitem   AT 27
                  v-part-dscr FORMAT "x(30)" AT 42
                  w2.cases  AT 72 FORMAT "->>>9" " @"
                  w2.cas-cnt FORMAT "->>>>9" SKIP.

         v-printline = v-printline + 1.
         
         IF LAST(w2.cases * w2.cas-cnt) THEN DO:
           IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
             PUT {1} 
                 v-job-var  FORMAT "x(9)"
                 IF AVAIL reftable 
                   THEN reftable.code ELSE "" FORMAT "x(15)" AT 11
                 oe-ordl.part-no AT 27
                 oe-ordl.part-dscr1 FORMAT "x(30)" AT 42
                 SKIP.
             v-printline = v-printline + 1.
           END.
      
           PUT {1}
               "================" AT 72 SKIP
               v-tot-pkgs AT 72 FORM "->>>9"  " ="
               v-ship-qty FORM "->>>>9" SPACE(4)
               oe-boll.p-c SPACE(1)
               v-weight  FORMAT ">>>>>9" SKIP.
      
           ASSIGN
              v-printline = v-printline + 2
            /*  v-tot-pkgs  = 0*/ .
      
           IF v-print-dept THEN
           DO:
              FIND FIRST job-hdr WHERE
                   job-hdr.company eq cocode AND
                   job-hdr.job-no  EQ oe-ordl.job-no AND
                   job-hdr.job-no2 EQ oe-ordl.job-no2
                   NO-LOCK NO-ERROR.

              IF AVAIL job-hdr THEN
              DO:
                 FIND FIRST job WHERE
                      job.company eq cocode AND
                      job.job     eq job-hdr.job AND
                      job.job-no  eq job-hdr.job-no AND
                      job.job-no2 eq job-hdr.job-no2
                      NO-LOCK NO-ERROR.

                 IF AVAIL job THEN
                 DO:
                     ASSIGN j = 0 .
                    FOR EACH notes WHERE
                        notes.rec_key EQ job.rec_key AND
                        CAN-DO(v-depts,notes.note_code)
                        NO-LOCK
                        BY notes.note_code:
                       
                       
                        v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                        {SYS/INC/ROUNDUP.I v-tmp-lines}
                       
                        IF notes.note_text <> "" THEN
                           DO i = 1 TO v-tmp-lines:
                       
                              /*IF v-printline >= 38 THEN DO:
                                 v-printline = 0.
                                 PAGE {1}.
                                 {oe/rep/bollprnt12.i}
                              END.*/
                             
                             
                              PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                              v-printline = v-printline + 1.
                               j = j + 1 .
                                IF j >= 2 THEN LEAVE.
                           END.
                            IF j >= 2 THEN LEAVE.
                    END.
                    RELEASE job.
                 END.
                 RELEASE job-hdr.
              END.
           END.
         END.
         v-tot-cases = v-tot-cases + w2.cases.
         DELETE w2.
       END.
   /*    IF v-printline >= 38 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bollprnt12.i}
       END.*/
       /*PUT {1} SKIP.*/
      v-printline = v-printline + 1.
       PUT "<FArial><P9> " 
             "<C1>"                          
             "__________________________________________________________________________________________________________________" SKIP(1) 
             "<C1>" "<B>  Signature of Receipt </B>"  SKIP
             "<C7>" "Customer ________________________________________                       Date _______________________________________"  SKIP(2)
            "<FCourier New><P10>"
    .   
      PUT {1} SKIP(1).
      v-printline = v-printline + 5.
    /*  IF v-printline >= 38 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bollprnt12.i}
       END.*/
     {oe/rep/bollprnt13.i}    
    
    IF NOT LAST(report.key-01) THEN do:
      v-printline = 0.
      PAGE {1}.
      {oe/rep/bollprnt12.i} .
    END.

    END.
  END.
  /* end of summary mods */
  ELSE DO:
      IF LAST-OF(report.key-02) THEN DO:
       
       i = 0.
       v-recnt = 0 .
         .

       FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
           v-recnt = v-recnt + 1.
       END.

       FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
         i = i + 1.
         IF i eq 1 THEN ASSIGN v-job-po    = oe-boll.po-no                                                              
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))
                               v-fgitem    = oe-boll.i-no
                               v-part-dscr = oe-ordl.i-name. 
         ELSE
         if i eq 2 THEN ASSIGN v-part-dscr = oe-ordl.part-dscr1
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))
                               v-job-po    = IF AVAIL reftable THEN reftable.code 
                                                               ELSE ""
                               v-fgitem    = oe-ordl.part-no.
         ELSE
         if i eq 3 then ASSIGN v-part-dscr = oe-ordl.part-dscr2
                               v-job-var = ""
                               v-job-po  = ""
                               v-fgitem  = "".
         ELSE
         if i eq 4 then ASSIGN v-part-dscr = ""
                               v-job-var = ""
                               v-job-po  = ""
                               v-fgitem  = "".  
              
       /*  IF v-printline >= 38 THEN DO:
           v-printline = 0.
           PAGE {1}.
           {oe/rep/bollprnt12.i}
         END. */
        
           create tt-w2.
           assign
               tt-w2.cas-cnt = w2.cas-cnt
               tt-w2.cases   = w2.cases .

         IF FIRST(w2.cases * w2.cas-cnt) THEN 
           PUT {1} oe-ordl.ord-no FORM ">>>>>9"
                   v-job-po FORM "x(15)" AT 11
                   v-fgitem  AT 27 
                   v-part-dscr FORMAT "x(30)" AT 42
                   w2.cases    AT 72 FORMAT "->>>9" " @"
                   w2.cas-cnt    FORM "->>>>9"
                   SKIP.
         ELSE PUT {1} 
                  v-job-var FORM "x(9)"
                  v-job-po  FORM "x(15)" AT 11
                  v-fgitem   AT 27
                  v-part-dscr FORMAT "x(30)" AT 42
                  w2.cases  AT 72 FORMAT "->>>9" " @"
                  w2.cas-cnt FORMAT "->>>>9" SKIP.

         v-printline = v-printline + 1.
         
         IF LAST(w2.cases * w2.cas-cnt) THEN DO:
           IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
             PUT {1} 
                 v-job-var  FORMAT "x(9)"
                 IF AVAIL reftable 
                   THEN reftable.code ELSE "" FORMAT "x(15)" AT 11
                 oe-ordl.part-no AT 27
                 oe-ordl.part-dscr1 FORMAT "x(30)" AT 42
                 SKIP.
             v-printline = v-printline + 1.
           END.
      
           PUT {1}
               "================" AT 72 SKIP
               v-tot-pkgs AT 72 FORM "->>>9"  " ="
               v-ship-qty FORM "->>>>9" SPACE(4)
               oe-boll.p-c SPACE(1)
               v-weight  FORMAT ">>>>>9" SKIP.
      
           ASSIGN
              v-printline = v-printline + 2
            /*  v-tot-pkgs  = 0*/ .
      
           IF v-print-dept THEN
           DO:
              FIND FIRST job-hdr WHERE
                   job-hdr.company eq cocode AND
                   job-hdr.job-no  EQ oe-ordl.job-no AND
                   job-hdr.job-no2 EQ oe-ordl.job-no2
                   NO-LOCK NO-ERROR.

              IF AVAIL job-hdr THEN
              DO:
                 FIND FIRST job WHERE
                      job.company eq cocode AND
                      job.job     eq job-hdr.job AND
                      job.job-no  eq job-hdr.job-no AND
                      job.job-no2 eq job-hdr.job-no2
                      NO-LOCK NO-ERROR.

                 IF AVAIL job THEN
                 DO:
                     ASSIGN j = 0 .
                    FOR EACH notes WHERE
                        notes.rec_key EQ job.rec_key AND
                        CAN-DO(v-depts,notes.note_code)
                        NO-LOCK
                        BY notes.note_code:
                       
                       
                        v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                        {SYS/INC/ROUNDUP.I v-tmp-lines}
                       
                        IF notes.note_text <> "" THEN
                           DO i = 1 TO v-tmp-lines:
                       
                              /*IF v-printline >= 38 THEN DO:
                                 v-printline = 0.
                                 PAGE {1}.
                                 {oe/rep/bollprnt12.i}
                              END.*/
                             
                             
                              PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                              v-printline = v-printline + 1.
                               j = j + 1 .
                                IF j >= 2 THEN LEAVE.
                           END.
                            IF j >= 2 THEN LEAVE.
                    END.
                    RELEASE job.
                 END.
                 RELEASE job-hdr.
              END.
           END.
         END.
         v-tot-cases = v-tot-cases + w2.cases.
         DELETE w2.
       END.
   /*    IF v-printline >= 38 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bollprnt12.i}
       END.*/
       /*PUT {1} SKIP.*/
      v-printline = v-printline + 1.
       PUT "<FArial><P9> " 
             "<C1>"                          
             "__________________________________________________________________________________________________________________" SKIP(1) 
             "<C1>" "<B>  Signature of Receipt </B>"  SKIP
             "<C7>" "Customer ________________________________________                       Date _______________________________________"  SKIP(2)
            "<FCourier New><P10>"
    .   
      PUT {1} SKIP(1).
      v-printline = v-printline + 5.
    /*  IF v-printline >= 38 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bollprnt12.i}
       END.*/
     {oe/rep/bollprnt13.i}    
    
    IF NOT LAST(report.key-01) THEN do:
      v-printline = 0.
      PAGE {1}.
      {oe/rep/bollprnt12.i} .
    END.
    END.
  END. 
/*  if v-print-components THEN do:
        for each fg-set
            where fg-set.company eq cocode
              and fg-set.set-no  eq oe-boll.i-no
            no-lock,
            
            first xitemfg
            where xitemfg.company eq cocode
              and xitemfg.i-no    eq fg-set.part-no
            no-lock
            
            break by fg-set.set-no:
            MESSAGE "test " + string(xitemfg.i-name) VIEW-AS ALERT-BOX ERROR .
          {sys/inc/part-qty.i v-part-qty fg-set}
          
          put {1}
              xitemfg.part-no
              fg-set.part-no                  AT 33
              xitemfg.i-name                        FORMAT "x(22)"
              oe-boll.qty * v-part-qty        TO 80 FORMAT "->>>,>>9"
              skip(1).
        
          v-printline = v-printline + 2.
        end. 
 END. */
  v-tot-wt = v-tot-wt + oe-boll.weight.

  if oe-boll.weight eq 0 then
    v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).
  
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
