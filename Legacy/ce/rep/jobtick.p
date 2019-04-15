/* ----------------------------------------------- ce/rep/jobtick.p 03/99 JLF */
/*  factory ticket                                                            */
/* -------------------------------------------------------------------------- */

{jcrep/r-ticket.i "shared"}

{ce/rep/jobtick.i "new shared"}

def var v-str      as   char extent 3 no-undo.
def var v-sh-dims  as   char format "x(25)" no-undo.
def var v-est-type like est.est-type no-undo.
def var v-blank    like job-hdr.blank-no no-undo.
def var v-dim      like eb.len extent 3 no-undo.
def var v-cad-no   like eb.cad-no no-undo.
def var v-run      as   dec no-undo.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF var v-dept-note AS cha FORM "x(212)" EXTENT 4 NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-leaf LIKE ef.leaf EXTENT 9 NO-UNDO.
DEF VAR lv-leaf-dscr LIKE ef.leaf-dscr EXTENT 9 NO-UNDO.
DEF VAR lv-leaf-snum LIKE ef.leaf-snum FORMAT ">>>" EXTENT 9 NO-UNDO.
DEF VAR lv-leaf-bnum LIKE ef.leaf-bnum FORMAT ">>>" EXTENT 9 NO-UNDO.
DEF VAR lv-leaf-l LIKE ef.leaf-l EXTENT 9 NO-UNDO.
DEF VAR lv-leaf-w LIKE ef.leaf-w EXTENT 9 NO-UNDO.
DEF VAR v-output AS LOG NO-UNDO.

DEF WORKFILE w-list FIELD w-int AS INT FIELD w-chr AS CHAR.

DEF BUFFER xjob-hdr FOR job-hdr.
     

do transaction:
  {sys/inc/cerun.i F}
end.
{custom/formtext.i NEW}         
{sys/inc/notes.i}

view frame head.


    find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

    for each job-hdr
        where job-hdr.company               eq cocode
          and job-hdr.ftick-prnt            eq reprint

          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no,

        first cust
        where cust.company                  eq cocode
          and cust.cust-no                  eq job-hdr.cust-no
        no-lock,
        
        first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
        no-lock,

        first b-job-mch
        where b-job-mch.company             eq cocode
          and b-job-mch.job                 eq job-hdr.job
          and b-job-mch.job-no              eq job-hdr.job-no
          and b-job-mch.job-no2             eq job-hdr.job-no2
          and b-job-mch.frm                 eq job-hdr.frm
        no-lock,

        first itemfg
        where itemfg.company                eq cocode
          and itemfg.i-no                   eq job-hdr.i-no
        no-lock

        break by job-hdr.job-no
              by job-hdr.job-no2
              by job-hdr.job
              by job-hdr.frm
              by job-hdr.blank-no:
              
      v-blank = job-hdr.blank-no.        

      release est.
      release ef.
      release eb.

      if job.est-no ne "" then do:
        for each est
            where est.company  eq cocode
              and est.est-no   eq job.est-no
              and est.est-type le 4
            no-lock,
            first ef
            where ef.company   eq est.company
              and ef.est-no    eq est.est-no
              and ef.form-no   eq job-hdr.frm
            no-lock,
            first eb
            where eb.company   eq ef.company
              and eb.est-no    eq ef.est-no
              and eb.form-no   eq ef.form-no
              and (eb.blank-no eq job-hdr.blank-no or
                   est.est-type lt 3)
            no-lock:

          v-blank = eb.blank-no.  
          leave.
        end.
          
        if not avail eb then next.
      end.  
        
      assign
       v-est-type = if avail est then est.est-type else 0
       v-hopf      = if cerunf eq "HOP" and avail ef then ef.n-out else 1
       v-date-ent  = job.start-date.
        
      if first-of(job-hdr.job) then do:
        for each w-item-id:
          delete w-item-id.
        end.

        i = 0.
        for each b-job-hdr no-lock
            where b-job-hdr.company eq cocode
              and b-job-hdr.job     eq job-hdr.job
              and b-job-hdr.job-no  eq job-hdr.job-no
              and b-job-hdr.job-no2 eq job-hdr.job-no2:
          find first w-item-id where w-item-id.i-no eq b-job-hdr.i-no no-error.
          if not avail w-item-id then do:
            create w-item-id.
            assign
             i              = i + 1
             w-item-id.i-no = b-job-hdr.i-no
             w-item-id.id   = i.
          end.
        end.
      end.  

      release oe-ord.
      for each oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.i-no    eq job-hdr.i-no
            and oe-ordl.ord-no  ne job-hdr.ord-no
          no-lock,
          first oe-ord
          where oe-ord.company eq cocode
            and oe-ord.ord-no  eq oe-ordl.ord-no
          no-lock
          by oe-ord.ord-no desc:

        leave.
      end.

      v-last-ord = if avail oe-ord then oe-ordl.ord-no else 0.

      release oe-ord.
      release oe-rel.
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq job-hdr.ord-no
            and oe-ordl.i-no    eq job-hdr.i-no
          no-error.

      if avail oe-ordl then do:
        find first oe-ord
            where oe-ord.company eq cocode
              and oe-ord.ord-no  eq oe-ordl.ord-no
            no-lock no-error.

        if avail oe-ord         and
           (not oe-ctrl.p-fact) and
           (oe-ord.stat eq "h" OR oe-ord.priceHold)  then do:
          if first-of(job-hdr.frm) then v-break = yes.
          next.
        end.

        assign
         oe-ordl.ftick-prnt = yes
         v-date-ent         = oe-ord.ord-date.
        
        FOR EACH oe-rel
            WHERE oe-rel.company EQ cocode
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
            NO-LOCK
            BY oe-rel.rel-date:
          RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
          IF INDEX("LBSI",lv-stat) GT 0 THEN LEAVE.
        END.
      end.

      if first-of(job-hdr.frm)                            or
         v-break                                          or
         (first-of(job-hdr.blank-no) and v-est-type eq 3) then do:
        v-break = no.

        assign
         v-slsmn   = ""
         v-ovrun   = ""
         v-unrun   = ""
         v-cust    = ""
         v-qc-inst = ""
         v-form    = if v-est-type eq 3 then job-hdr.blank-no 
                                        else job-hdr.frm.
        if avail oe-ord then do:
          do i = 1 to 3:
            if oe-ord.sman[i] ne "" then
              assign
               v-slsmn = " " + v-slsmn + oe-ord.sman[i] + ","
               v-slsmn = trim(v-slsmn).
          end.

          if v-slsmn ne ""                                  and
             substr(v-slsmn,length(trim(v-slsmn)),1) eq "," then
            substr(v-slsmn,length(trim(v-slsmn)),1) = "".

          assign
           v-ovrun = trim(string(oe-ordl.over-pct,">>9.9<%"))
           v-unrun = trim(string(oe-ordl.under-pct,">>9.9<%")).
        end.

        else
          assign
           v-slsmn = cust.sman
           v-ovrun = trim(string(cust.over-pct,">>9.9<%"))
           v-unrun = trim(string(cust.under-pct,">>9.9<%")).

        i = 0.

        if cust.name ne "" then
          assign
           i         = i + 1
           v-cust[i] = cust.name.
        if cust.addr[1] ne "" then
          assign
           i         = i + 1
           v-cust[i] = cust.addr[1].
        if cust.addr[2] ne "" then
          assign
           i         = i + 1
           v-cust[i] = cust.addr[2].
        assign
         i         = i + 1
         v-cust[i] = trim(cust.city) + ", " + cust.state + "  " + cust.zip.

        i = 4.

        if avail oe-rel then do:
          find first shipto
              where shipto.company eq cocode
                and shipto.cust-no eq oe-rel.cust-no
                and shipto.ship-id eq oe-rel.ship-id
              no-lock no-error.  
          if avail shipto and shipto.ship-name ne "" then
            assign
             i         = i + 1
             v-cust[i] = shipto.ship-name.
          if oe-rel.ship-addr[1] ne "" then
            assign
             i         = i + 1
             v-cust[i] = oe-rel.ship-addr[1].
          if oe-rel.ship-addr[2] ne "" then
            assign
             i         = i + 1
             v-cust[i] = oe-rel.ship-addr[2].
          assign
           i         = i + 1
           v-cust[i] = trim(oe-rel.ship-city) + ", " +
                       oe-rel.ship-state + "  " + oe-rel.ship-zip.
        end.
        
        else
        if avail eb then do:
          if eb.ship-name ne "" then
            assign
             i         = i + 1
             v-cust[i] = eb.ship-name.
          if eb.ship-addr[1] ne "" then
            assign
             i         = i + 1
             v-cust[i] = eb.ship-addr[1].
          if eb.ship-addr[2] ne "" then
            assign
             i         = i + 1
             v-cust[i] = eb.ship-addr[2].
          assign
           i         = i + 1
           v-cust[i] = trim(eb.ship-city) + ", " +
                       eb.ship-state + "  " + eb.ship-zip.
        end.
        
        for each b-job-hdr no-lock
            where b-job-hdr.company eq cocode
              and b-job-hdr.job     eq job-hdr.job
              and b-job-hdr.job-no  eq job-hdr.job-no
              and b-job-hdr.job-no2 eq job-hdr.job-no2
            by b-job-hdr.frm      desc
            by b-job-hdr.blank-no desc:
            
          v-forms = if v-est-type eq 3 then b-job-hdr.blank-no
                                       else b-job-hdr.frm.
          leave.
        end.

        for each job-mat
            where job-mat.company eq cocode
              and job-mat.job     eq job-hdr.job
              and job-mat.job-no  eq job-hdr.job-no
              and job-mat.job-no2 eq job-hdr.job-no2
              and job-mat.frm     eq job-hdr.frm
            no-lock,

            first item
            where item.company   eq cocode
              and item.i-no      eq job-mat.i-no
              and (item.mat-type eq "I" or item.mat-type eq "V")
            no-lock:

          find first w-bot
              where w-bot.ink eq job-mat.i-no
                 or w-bot.ink eq ""
              no-error.
          if not avail w-bot then do:
            i = 0.
            for each w-bot by w-bot.seq desc:
              i = w-bot.seq.
              leave.
            end.

            create w-bot.
            w-bot.seq = i + 1.
          end.

          assign
           w-bot.ink   = job-mat.i-no
           w-bot.ink-d = item.i-name
           w-bot.ink-q = job-mat.qty.
        end.

        v-page-tot = 0.
        for each b-job-hdr no-lock
            where b-job-hdr.company eq cocode
              and b-job-hdr.job     eq job-hdr.job
              and b-job-hdr.job-no  eq job-hdr.job-no
              and b-job-hdr.job-no2 eq job-hdr.job-no2
              and b-job-hdr.frm     eq job-hdr.frm,
            
            first b-itemfg no-lock
            where b-itemfg.company eq cocode
              and b-itemfg.i-no    eq b-job-hdr.i-no:

          v-page-tot = v-page-tot + 1.
          
          find first b-eb
              where b-eb.company   eq job.company
                and b-eb.est-no    eq job.est-no
                and b-eb.form-no   eq b-job-hdr.frm
                and (b-eb.blank-no eq b-job-hdr.blank-no or v-est-type gt 4)
              no-lock no-error.
          v-cad-no = if avail b-eb then b-eb.cad-no else itemfg.cad-no.

          if v-cad-no ne "" then do:
            find first w-bot
                where w-bot.cad eq v-cad-no
                   or w-bot.cad eq ""
                no-error.
            if not avail w-bot then do:
              i = 0.
              for each w-bot by w-bot.seq desc:
                i = w-bot.seq.
                leave.
              end.

              create w-bot.
              w-bot.seq = i + 1.
            end.

            assign
             w-bot.cad   = v-cad-no
             w-bot.cad-a = w-bot.cad-a +
                           trim(string(b-job-hdr.blank-no,">9")) + ",".
          end.
        end.
        
        for each job-mat
            where job-mat.company                eq cocode
              and job-mat.job                    eq job-hdr.job
              and job-mat.job-no                 eq job-hdr.job-no
              and job-mat.job-no2                eq job-hdr.job-no2
              and job-mat.frm                    eq job-hdr.frm
            no-lock,
            first item
            where item.company                   eq cocode
              and item.i-no                      eq job-mat.rm-i-no
              and index("BPR1234",item.mat-type) gt 0
            no-lock:
          leave.  
        end.

        for each job-mch
            where job-mch.company   eq cocode
              and job-mch.job       eq job-hdr.job
              and job-mch.job-no    eq job-hdr.job-no
              and job-mch.job-no2   eq job-hdr.job-no2
              and job-mch.frm       eq job-hdr.frm
              and (v-est-type       ne 3                or
                   job-mch.blank-no eq 0                or
                   job-mch.blank-no eq job-hdr.blank-no) 
            use-index seq-idx no-lock,
            
            first mach
            where mach.company eq cocode
              and mach.m-code  eq job-mch.m-code
            no-lock:

          find first w-bot
              where w-bot.mch eq job-mch.m-code
                 or w-bot.mch eq ""
              no-error.
          if not avail w-bot then do:
            i = 0.
            for each w-bot by w-bot.seq desc:
              i = w-bot.seq.
              leave.
            end.

            create w-bot.
            w-bot.seq = i + 1.
          end.
          
          v-run = job-mch.run-qty.
          
          if avail job-mat and mach.therm and (mach.p-type eq "R" OR job-mch.dept EQ "LM") then
            v-run = v-run * (job-mat.len / 12).
          
          {sys/inc/roundup.i v-run}
            
          assign
           w-bot.mch    = job-mch.m-code
           w-bot.units  = w-bot.units  + v-run
           w-bot.mr-hr  = w-bot.mr-hr  + job-mch.mr-hr
           w-bot.speed  = w-bot.speed  + (job-mch.speed * v-run).
        end.

        i = 0.
        for each w-bot:
          i = i + 1.
        end.

        v-page-tot = max(v-page-tot / 10,i / 10).
        {sys/inc/roundup.i v-page-tot}

        j = 0.
        for each w-inst:
          delete w-inst.
        end.
      
        /*v-page-tot = v-page-tot + v-page-tot modulo 2.*/

        IF AVAIL est THEN DO:
           FOR EACH tt-formtext:
              DELETE tt-formtext.
           END.

          lv-text = "".
          FOR EACH notes WHERE notes.rec_key = job.rec_key
                        AND (notes.note_form_no = ef.form-no OR notes.note_form_no = 0)
                        AND LOOKUP(notes.note_code,"QC,Q1") > 0 NO-LOCK:
              lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
          END.

          DO li = 1 TO 6:
             CREATE tt-formtext.
             ASSIGN tt-line-no = li
                    tt-length  = 40.
          END.
 
          RUN custom/formtext.p (lv-text).
          i = 0.
          v-dept-note = "".
          FOR EACH tt-formtext:
            i = i + 1.
            IF  i <= 6 THEN v-qc-inst[i] = tt-formtext.tt-text.      
          END.
        END.
        
        /*v-page-tot = v-page-tot + v-page-tot modulo 2.*/

        RUN bottom-of-page (NOT FIRST(job-hdr.frm)).
      end.

      assign
       v-str    = ""
       v-dim[1] = itemfg.l-score[50]
       v-dim[2] = itemfg.w-score[50]
       v-dim[3] = itemfg.d-score[50].

      if avail eb then do:
        if v-dim[1] eq 0 then v-dim[1] = eb.len.
        if v-dim[2] eq 0 then v-dim[2] = eb.wid.
        if v-dim[3] eq 0 then v-dim[3] = eb.dep.
      end.

      find first fgcat
          where fgcat.company eq cocode
            and fgcat.procat  eq itemfg.procat
          no-lock no-error.
      do i = 1 to 3:
        if avail fgcat and fgcat.comm gt 0 then
          run sys/inc/dec-frac.p (v-dim[i], 64, output v-str[i]).
        else  
          v-str[i] = string(v-dim[i],">>>9.9<<<").
      end.

      find first w-item-id where w-item-id.i-no eq job-hdr.i-no.
      v-item-id = w-item-id.id.

      IF NOT job-hdr.ftick-prnt THEN DO WHILE TRUE:
        li = li + 1.
        FIND xjob-hdr EXCLUSIVE-LOCK
            WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
            NO-ERROR NO-WAIT.
        IF AVAIL xjob-hdr THEN xjob-hdr.ftick-prnt = YES.
        IF li GE 1000 OR xjob-hdr.ftick-prnt THEN LEAVE.
      END.
       
      display w-item-id.id              at 3
              job-hdr.i-no              at 9
              itemfg.part-no            at 27   format "x(15)"
              eb.part-no when avail eb and itemfg.part-no eq ""
                                        @ itemfg.part-no
              oe-ordl.part-no when avail oe-ordl and oe-ordl.part-no ne ""
                                        @ itemfg.part-no
              itemfg.i-name             at 45
              eb.part-dscr1 when avail eb and itemfg.i-name eq ""
                                        @ itemfg.i-name
              oe-ordl.i-name when avail oe-ordl and oe-ordl.i-name ne ""
                                        @ itemfg.i-name
              trim(v-str[1]) + " x " +
              trim(v-str[2]) + " x " +
              trim(v-str[3])            at 78   format "x(30)"
              itemfg.style              at 111
              eb.style when avail eb and itemfg.style eq ""
                                        @ itemfg.style
              oe-ord.po-no when avail oe-ord
                                        at 119
              oe-ordl.po-no when avail oe-ordl and oe-ordl.po-no ne ""
                                        @ oe-ord.po-no
              v-last-ord when v-last-ord ne 0
                                        at 137
              v-item-id                 at 146
              job-hdr.n-on              at 152  format ">>9"
              eb.num-up when avail eb and job-hdr.n-on eq 0
                                        @ job-hdr.n-on
              job-hdr.qty               at 157  format ">>>,>>>,>>>,>>9"
              eb.spc-no when avail eb   at 175
              itemfg.spc-no when not avail eb
                                        @ eb.spc-no
              eb.upc-no when avail eb   at 193
              itemfg.upc-no when not avail eb
                                        @ eb.upc-no
              oe-ordl.prom-date when avail oe-ordl
                                        at 211

          with no-box no-labels frame d1 stream-io width 225 no-attr-space.
          
      if avail eb then
      for each w-bot where w-bot.ink ne "":
        do i = 1 to 20:
          if eb.i-code2[i] eq w-bot.ink then do:
            w-bot.ink-a = w-bot.ink-a + trim(string(w-item-id.id,">>9")) + ",".
            leave.
          end.
        end.
      end.

      if last-of(job-hdr.frm)                            or
         (last-of(job-hdr.blank-no) and v-est-type eq 3) or   
         line-counter ge 38                              then do while true:
        assign
         
         v-break = /*last-of(job-hdr.frm)or v-odd-p*/ TRUE.

        do while line-counter lt 40:
          put skip(1).
        end.

        
          ASSIGN
           lv-leaf      = ""
           lv-leaf-dscr = ""
           lv-leaf-snum = 0
           lv-leaf-bnum = 0
           lv-leaf-l    = 0
           lv-leaf-w    = 0
           li           = 0.

          for each job-mat
              where job-mat.company                eq cocode
                and job-mat.job                    eq job-hdr.job
                and job-mat.job-no                 eq job-hdr.job-no
                and job-mat.job-no2                eq job-hdr.job-no2
                and job-mat.frm                    eq job-hdr.frm
              no-lock,
              first item
              where item.company                   eq cocode
                and item.i-no                      eq job-mat.rm-i-no
                and index("WLF",item.mat-type)     gt 0
              no-lock:
            li = li + 1.
            IF li LE EXTENT(lv-leaf) THEN
              ASSIGN
               lv-leaf[li]      = item.i-no
               lv-leaf-dscr[li] = item.i-name
               lv-leaf-snum[li] = job-mat.frm
               lv-leaf-bnum[li] = job-mat.blank-no
               lv-leaf-l[li]    = job-mat.len
               lv-leaf-w[li]    = job-mat.wid.
          end.

          v-dim = 0.
          
          release job-mat.
          release item.
          
          for each job-mat
              where job-mat.company                eq cocode
                and job-mat.job                    eq job-hdr.job
                and job-mat.job-no                 eq job-hdr.job-no
                and job-mat.job-no2                eq job-hdr.job-no2
                and job-mat.frm                    eq job-hdr.frm
              no-lock,
              first item
              where item.company                   eq cocode
                and item.i-no                      eq job-mat.rm-i-no
                and index("BPR1234",item.mat-type) gt 0
              no-lock:
            leave.  
          end.
                
          if avail job-mat and avail item then
            assign
             v-str = ""
             v-dim[1] = if job-mat.wid eq 0 then item.s-wid else job-mat.wid
             v-dim[2] = if job-mat.len eq 0 then item.s-len else job-mat.len
             v-dim[3] = item.r-wid.

          if avail ef then do:
            v-dim[1] = ef.nsh-wid.
            v-dim[2] = ef.nsh-len.
            v-dim[3] = ef.roll-wid.
            if not ef.roll then v-dim[3] = 0.
          end.

          do i = 1 to 3:
            if avail fgcat and fgcat.comm gt 0 then
              run sys/inc/dec-frac.p (v-dim[i], 64, output v-str[i]).
            else  
              v-str[i] = string(v-dim[i],">>>9.9<<<").
          end.

          if v-dim[3] eq 0 then v-str[3] = "".

          v-sh-dims = "W: " + trim(v-str[1]) + "   " +
                      "L: " + trim(v-str[2]).

          DISPLAY "Leaf & Window Material"  AT 182
                  "RM Item#"                AT 166
                  "Description"             AT 177
                  "Sht"                     AT 198
                  "Blk"                     AT 202
                  " Length"                 AT 206
                  "  Width"                 AT 214
                  "Sheet Size:"             TO 25
                  v-sh-dims                 AT 27
                  "Description:"            TO 76
                  item.i-name when avail item
                                            AT 78
                  "Die Number:"             TO 135
                  eb.die-no WHEN AVAIL eb   AT 137
                  itemfg.die-no WHEN NOT AVAIL eb
                                            @ eb.die-no
                  lv-leaf[1]                WHEN lv-leaf[1] NE ""
                                            AT 166
                  lv-leaf-dscr[1]           WHEN lv-leaf[1] NE ""
                                            AT 177
                  lv-leaf-snum[1]           WHEN lv-leaf[1] NE ""
                                            AT 198
                  lv-leaf-bnum[1]           WHEN lv-leaf[1] NE ""
                                            AT 202
                  lv-leaf-l[1]              WHEN lv-leaf[1] NE ""
                                            AT 206
                  lv-leaf-w[1]              WHEN lv-leaf[1] NE ""
                                            AT 214
                  lv-leaf[2]                WHEN lv-leaf[2] NE ""
                                            AT 166
                  lv-leaf-dscr[2]           WHEN lv-leaf[2] NE ""
                                            AT 177
                  lv-leaf-snum[2]           WHEN lv-leaf[2] NE ""
                                            AT 198
                  lv-leaf-bnum[2]           WHEN lv-leaf[2] NE ""
                                            AT 202
                  lv-leaf-l[2]              WHEN lv-leaf[2] NE ""
                                            AT 206
                  lv-leaf-w[2]              WHEN lv-leaf[2] NE ""
                                            AT 214
                  "Sheets:"                 TO 25
                  b-job-mch.run-qty * v-hopf AT 27   FORMAT ">>,>>>,>>9"
                  "Caliper:"                TO 76
                  item.cal WHEN AVAIL item  AT 78
                  "Die Size:"               TO 135
                  ef.die-in WHEN AVAIL ef   AT 137
                  lv-leaf[3]                WHEN lv-leaf[3] NE ""
                                            AT 166
                  lv-leaf-dscr[3]           WHEN lv-leaf[3] NE ""
                                            AT 177
                  lv-leaf-snum[3]           WHEN lv-leaf[3] NE ""
                                            AT 198
                  lv-leaf-bnum[3]           WHEN lv-leaf[3] NE ""
                                            AT 202
                  lv-leaf-l[3]              WHEN lv-leaf[3] NE ""
                                            AT 206
                  lv-leaf-w[3]              WHEN lv-leaf[3] NE ""
                                            AT 214
                  lv-leaf[4]                WHEN lv-leaf[4] NE ""
                                            AT 166
                  lv-leaf-dscr[4]           WHEN lv-leaf[4] NE ""
                                            AT 177
                  lv-leaf-snum[4]           WHEN lv-leaf[4] NE ""
                                            AT 198
                  lv-leaf-bnum[4]           WHEN lv-leaf[4] NE ""
                                            AT 202
                  lv-leaf-l[4]              WHEN lv-leaf[4] NE ""
                                            AT 206
                  lv-leaf-w[4]              WHEN lv-leaf[4] NE ""
                                            AT 214
                  "Item Number:"            TO 25
                  item.i-no WHEN AVAIL item AT 27
                  "Roll Width:"             TO 76
                  trim(v-str[3])            AT 78   format "x(10)"
                  "Cuts:"                   TO 98
                  ef.n-cuts WHEN AVAIL ef   AT 100
                  "Die Prev Job:"           TO 135
                  lv-leaf[5]                WHEN lv-leaf[5] NE ""
                                            AT 166
                  lv-leaf-dscr[5]           WHEN lv-leaf[5] NE ""
                                            AT 177
                  lv-leaf-snum[5]           WHEN lv-leaf[5] NE ""
                                            AT 198
                  lv-leaf-bnum[5]           WHEN lv-leaf[5] NE ""
                                            AT 202
                  lv-leaf-l[5]              WHEN lv-leaf[5] NE ""
                                            AT 206
                  lv-leaf-w[5]              WHEN lv-leaf[5] NE ""
                                            AT 214
                  lv-leaf[6]                WHEN lv-leaf[6] NE ""
                                            AT 166
                  lv-leaf-dscr[6]           WHEN lv-leaf[6] NE ""
                                            AT 177
                  lv-leaf-snum[6]           WHEN lv-leaf[6] NE ""
                                            AT 198
                  lv-leaf-bnum[6]           WHEN lv-leaf[6] NE ""
                                            AT 202
                  lv-leaf-l[6]              WHEN lv-leaf[6] NE ""
                                            AT 206
                  lv-leaf-w[6]              WHEN lv-leaf[6] NE ""
                                            AT 214
                  lv-leaf[7]                WHEN lv-leaf[7] NE ""
                                            AT 166
                  lv-leaf-dscr[7]           WHEN lv-leaf[7] NE ""
                                            AT 177
                  lv-leaf-snum[7]           WHEN lv-leaf[7] NE ""
                                            AT 198
                  lv-leaf-bnum[7]           WHEN lv-leaf[7] NE ""
                                            AT 202
                  lv-leaf-l[7]              WHEN lv-leaf[7] NE ""
                                            AT 206
                  lv-leaf-w[7]              WHEN lv-leaf[7] NE ""
                                            AT 214
                  lv-leaf[8]                WHEN lv-leaf[8] NE ""
                                            AT 166
                  lv-leaf-dscr[8]           WHEN lv-leaf[8] NE ""
                                            AT 177
                  lv-leaf-snum[8]           WHEN lv-leaf[8] NE ""
                                            AT 198
                  lv-leaf-bnum[8]           WHEN lv-leaf[8] NE ""
                                            AT 202
                  lv-leaf-l[8]              WHEN lv-leaf[8] NE ""
                                            AT 206
                  lv-leaf-w[8]              WHEN lv-leaf[8] NE ""
                                            AT 214
                  lv-leaf[9]                WHEN lv-leaf[9] NE ""
                                            AT 166
                  lv-leaf-dscr[9]           WHEN lv-leaf[9] NE ""
                                            AT 177
                  lv-leaf-snum[9]           WHEN lv-leaf[9] NE ""
                                            AT 198
                  lv-leaf-bnum[9]           WHEN lv-leaf[9] NE ""
                                            AT 202
                  lv-leaf-l[9]              WHEN lv-leaf[9] NE ""
                                            AT 206
                  lv-leaf-w[9]              WHEN lv-leaf[9] NE ""
                                            AT 214
              WITH NO-BOX NO-LABELS FRAME d2 STREAM-IO WIDTH 225 NO-ATTR-SPACE.

        do while line-counter lt 54:
          put skip(1).
        end.

        j = 0.
        for each w-bot:
          RUN reformat-w-bot (INPUT-OUTPUT w-bot.ink-a).

          RUN reformat-w-bot (INPUT-OUTPUT w-bot.cad-a).

          w-bot.speed = w-bot.speed / w-bot.units.

          display w-bot.ink             at 3
                  w-bot.ink-d           at 15
                    when w-bot.ink ne ""
                  w-bot.ink-a           at 47
                    when w-bot.ink ne ""
                  w-bot.ink-q           at 97
                    when w-bot.ink ne ""
                  w-bot.cad             at 113
                  w-bot.cad-a           at 130
                    when w-bot.cad ne ""
                  w-bot.mch             at 183
                  w-bot.units           at 191
                    when w-bot.mch ne ""
                  w-bot.mr-hr           at 203  format ">,>>9.99"
                    when w-bot.mch ne ""
                  w-bot.speed           at 214  format ">>>,>>9"
                    when w-bot.mch ne ""
                  "" when w-bot.speed eq ? or w-bot.speed eq 0
                                        @ w-bot.speed

              with no-box no-labels frame d3 stream-io width 225 no-attr-space.

          delete w-bot.

          j = j + 1.

          if j ge 10 then leave.
        end.

        if page-number - v-last-page eq v-page-tot then do:
          v-last-page = page-number.
          leave.
        end.

        RUN bottom-of-page (YES).

        IF not v-break then leave.
      end.
    end.

/*on last page*/
IF v-output THEN
   RUN bottom-of-page(YES).

RETURN.

PROCEDURE reformat-w-bot.
  DEF INPUT-OUTPUT PARAM io-w-bot LIKE w-bot.ink-a NO-UNDO.

  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-list FOR w-list.


  IF io-w-bot NE "" THEN DO:
    FOR EACH w-list:
      DELETE w-list.
    END.

    SUBSTR(io-w-bot,LENGTH(TRIM(io-w-bot)),1) = "".

    DO li = 1 TO NUM-ENTRIES(io-w-bot):
      IF INT(ENTRY(li,io-w-bot)) NE 0 THEN DO:
        CREATE w-list.
        ASSIGN
         w-int = INT(ENTRY(li,io-w-bot))
         w-chr = "-".
      END.
    END.

    io-w-bot = "".

    FOR EACH w-list BREAK BY w-int:
      IF LAST(w-int) THEN w-chr = "".

      ELSE
      IF NOT CAN-FIND(FIRST b-list WHERE b-list.w-int EQ w-list.w-int + 1) THEN w-chr = ",".
    END.

    FOR EACH w-list BREAK BY w-int:
      IF FIRST(w-int)                                     OR
         w-chr EQ ","                                     OR
         SUBSTR(io-w-bot,LENGTH(TRIM(io-w-bot)),1) EQ "," OR
         LAST(w-int)                                      THEN
        io-w-bot = io-w-bot + TRIM(STRING(w-int,">>>>")) + TRIM(w-chr).
    END.
  END.

END PROCEDURE.

PROCEDURE bottom-of-page.
  DEF INPUT PARAM ip-put AS LOG NO-UNDO.

  v-output = YES.

  IF ip-put THEN DO:
    DO WHILE LINE-COUNTER LT 65:
      PUT SKIP(1).
    END.

    PUT "FCD-0124 Rev.2" TO 224 SKIP.
  END.
      
  PAGE.
  

END PROCEDURE.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
