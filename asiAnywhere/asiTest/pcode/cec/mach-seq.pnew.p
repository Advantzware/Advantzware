/* -------------------------------------------------- cec/mach-seq.p 4/92 cd  */
/* create machine routing sequence                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-form-no LIKE ef.form-no NO-UNDO.
DEF INPUT PARAM v-qty LIKE est-op.qty NO-UNDO.
DEF INPUT PARAM ip-build-combo AS LOG NO-UNDO.

{sys/inc/var.i shared}

def var save_id as recid.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def new shared var qty     as INT NO-UNDO.
def new shared var maxco   as int no-undo.
def new shared var v-2     as log init no.
def new shared var ll-corr as log no-undo.

def shared var xcal    as de no-undo.
def shared var sh-wid  as de no-undo.
def shared var sh-len  as de no-undo.

{ce/mach-lst.i new}

def workfile w-routing field m-code like mach.m-code.

{est/d-machex.i NEW}

def var v as int no-undo.
def var v1 as int no-undo.
def var v-msf as dec no-undo.
def var v-run as dec no-undo.
def var v-on-f as int no-undo.
def var v-rc-sw as log init no no-undo.
def var v-def-r as log init yes no-undo.
def var v-defr-valid as log init yes.
def var v-yld as dec.
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR v-gsh-wid LIKE xef.gsh-len NO-UNDO.
DEF VAR v-gsh-len LIKE xef.gsh-len NO-UNDO.
DEF VAR ll-label AS LOG NO-UNDO.

&SCOPED-DEFINE where-machine                                                     ~
               WHERE (mach.company EQ cocode                                  ~
                 AND  NOT CAN-FIND(FIRST reftable                             ~
                                   WHERE reftable.reftable EQ "mach.obsolete" ~
                                     AND reftable.company  EQ mach.company    ~
                                     AND reftable.loc      EQ mach.loc        ~
                                     AND reftable.code     EQ mach.m-code     ~
                                     AND reftable.val[1]   EQ 1)) 


session:set-wait-state("general").

{cec/msfcalc.i}

{sys/inc/cepanel.i}

{sys/inc/cercrout.i}

ASSIGN
   qty = if v-qty eq 0 then xest.est-qty[1] else v-qty
   ll-label = CAN-FIND(FIRST sys-ctrl
                    WHERE sys-ctrl.company EQ xest.company
                      AND sys-ctrl.name    EQ "MACHFILE"
                      AND sys-ctrl.log-fld).

blok:
FOR EACH xef
    WHERE xef.company  EQ xest.company
      AND xef.est-no   EQ xest.est-no
      AND (xef.form-no EQ ip-form-no OR ip-form-no EQ 0):

  ASSIGN
   xef.op-lock = NO
   ll-corr     = NO.

  RUN cec/isitfoam.p (ROWID(xef), OUTPUT ll-foam).

  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.form-no EQ xef.form-no
        AND xeb.pur-man EQ NO
      NO-LOCK,

      FIRST style
      where (style.company eq cocode)
        AND style.style EQ xeb.style
      NO-LOCK
    
      BREAK BY xeb.blank-no:

    find first item
        where (item.company = cocode and 
       (item.mat-type = "I" or item.mat-type = "V"))
          and item.i-no eq xeb.i-code[1]
        no-lock no-error.
    if avail item then find first e-item of item no-lock no-error.
    maxco = (xeb.i-col + xeb.i-coat ) / xeb.i-pass.
    {sys/inc/roundup.i maxco}
    
    assign
     v     = 0
     v-yld = if xest.form-qty eq 1 then 1 else
              (if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty)
     v-msf = v-qty * v-yld * xeb.t-len * xeb.t-wid
     v-msf = (if v-corr then (v-msf * .007) else (v-msf / 144)) / 1000.

    {sys/inc/roundup.i v-msf}

    find first routing-mtx
        where (routing-mtx.company eq cocode
  and  routing-mtx.loc     eq locode
  and  routing-mtx.style   eq style.style)
          and routing-mtx.msf ge int(v-msf)
        no-lock no-error.

    if avail routing-mtx then do:
      do v = 1 to 10:
        if (routing-mtx.dim-type  eq "BLANK" and
            routing-mtx.bl-len[v] ge xeb.t-len)     or
           (routing-mtx.dim-type  eq "SHEET" and
            routing-mtx.bl-len[v] ge xef.nsh-len)   then leave.
      end.
      v1 = v.
      do v = 1 to 10:
        if (routing-mtx.dim-type  eq "BLANK" and
            routing-mtx.bl-wid[v] ge xeb.t-wid)     or
           (routing-mtx.dim-type  eq "SHEET" and
            routing-mtx.bl-wid[v] ge xef.nsh-wid)   then leave.
      end.
      v = ((v - 1) * 10) + v1.
    end.
  
    if v ge 1 and v le 100 then
    find first routing
        where (routing.company eq cocode
  and  routing.loc     eq locode)
          and routing.r-code eq routing-mtx.r-code[v]
        no-lock no-error.

    if ll-foam then
      run cec/foammach.p (if avail routing then recid(routing) else ?,
                          output v-def-r).

    xcal = xef.cal.
  
    if avail routing then do:
      for each w-routing:
        delete w-routing.
      end.
  
      do v = 1 to 10:
        if routing.m-code[v] ne "" then do:
          find first mach
              {&where-machine}
                and mach.m-code eq routing.m-code[v]
              no-lock no-error.

          IF NOT AVAIL mach                                     OR
             (style.type EQ "F"                              AND
              (mach.dept[1] EQ "RC" OR mach.dept[1] EQ "DC") AND
              v-def-r)                                          THEN NEXT.

          create w-routing.
          w-routing.m-code = routing.m-code[v].
        end.
      END.
  
      for each w-routing:
        find first mach
            {&where-machine}
              and mach.m-code eq w-routing.m-code
            NO-LOCK no-error.
        if not avail mach OR
           (NOT first(xeb.blank-no) AND mach.p-type NE "B") then next.

        DO i = 1 TO IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
                       mach.dept[3] EQ "PR" OR mach.dept[3] EQ "PR" THEN
                      IF xeb.i-pass EQ 0 THEN 1 ELSE xeb.i-pass
                    ELSE
                    IF mach.dept[1] EQ "RC" THEN 2 ELSE 1:
          v-on-f = IF i EQ 1 THEN xef.n-out ELSE xef.n-out-l.

          DO WHILE TRUE:
            IF mach.dept[1] EQ "RC" THEN RUN cec/rc-mach.p (BUFFER mach, v-on-f, YES).

            ELSE DO: 
              IF mach.p-type EQ "B" THEN
                ASSIGN
                 sh-len = xeb.t-wid
                 sh-wid = xeb.t-len.

              ELSE
              IF xef.lam-dscr EQ "R" THEN
                ASSIGN
                 sh-wid = xef.nsh-wid
                 sh-len = xef.nsh-len.
              ELSE
                ASSIGN
                 sh-wid = xef.nsh-len
                 sh-len = xef.nsh-wid.

              {cec/mach-seq.i sh-len sh-wid xcal &defr=defr}
            END.

            LEAVE.
          END.

          IF AVAIL mach                                         AND
             (mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
              mach.dept[3] EQ "PR" OR mach.dept[3] EQ "PR")     AND
             (maxco GT mach.max-col OR
              (AVAIL ITEM AND mach.pr-type NE item.press-type)) THEN                                       
            RELEASE mach.

          v-defr-valid = AVAIL mach.
      
          if not avail mach then
          find first mach
              {&where-machine}
                and mach.m-code eq w-routing.m-code
              no-lock no-error.
          if not avail mach then leave.

          create m-lst.
          assign
           m-lst.f-no       = xef.form-no
           m-lst.seq        = (10 * mach.d-seq) + (i - 1)
           m-lst.dept       = mach.dept[1]
           m-lst.bl         = mach.p-type eq "B"
           m-lst.m-code     = mach.m-code
           m-lst.dscr       = mach.m-dscr
           m-lst.defr       = yes
           m-lst.defr-valid = v-defr-valid.
     
          if m-lst.bl then m-lst.b-no = xeb.blank-no.

          IF LOOKUP(mach.dept[1],"CR,RC") GT 0 THEN DO:
            m-lst.n-out = if i eq 2 then xef.n-out-l else xef.n-out.

            IF cercrout                                           and
               xef.n-out le 1 and xef.gsh-wid le xef.nsh-wid      AND
               xef.n-out-l le 1 and xef.gsh-len le xef.nsh-len    THEN
              LEAVE.

            ELSE
            if (xef.n-out le 1 and xef.gsh-wid le xef.nsh-wid and i eq 1)   OR
               (xef.n-out-l le 1 and xef.gsh-len le xef.nsh-len and i eq 2) THEN
              delete m-lst.
          END.
        end.
      end.
  
      IF NOT ll-foam THEN DO:
        assign
         i = 0
         j = 0.

        for each m-lst
            where m-lst.dept eq "RC"
              and m-lst.f-no eq xef.form-no:
          i = i + 1.  
        end.
  
        if i gt 2 then
        for each m-lst
            where m-lst.dept eq "RC"
              and m-lst.f-no eq xef.form-no:
          j = j + 1.
    
          if j ne 1 and j ne i then delete m-lst.
        end.
      END.
    end.
  END.

  RELEASE xeb.

  FIND FIRST xeb
      WHERE xeb.company EQ xef.company
        AND xeb.est-no  EQ xef.est-no
        AND xeb.form-no EQ xef.form-no
      NO-LOCK NO-ERROR.

  FIND FIRST style
      where (style.company eq cocode)
        AND style.style EQ xeb.style
      NO-LOCK NO-ERROR.

  IF xeb.pur-man THEN DO:      /* Purchased FG */
    RELEASE mach.
    DO i = 1 TO 7:
      IF style.m-code[i] EQ "" THEN NEXT.
      FIND FIRST mach
          {&where-machine}
            AND mach.m-code EQ style.m-code[i]
          NO-LOCK NO-ERROR.
      IF AVAIL mach AND (mach.dept[1] NE "FO") AND
                        (mach.dept[2] NE "FO") AND
                        (mach.dept[3] NE "FO") AND
                        (mach.dept[4] NE "FO")
      THEN RELEASE mach.
      /*{cec/mach-seq.i xef.gsh-len xef.gsh-wid xcal}*/
      IF AVAIL mach THEN LEAVE.
    END.
    IF NOT AVAIL mach THEN
    FOR EACH mach
        {&where-machine}
          AND mach.dept[1] EQ "FO"
        NO-LOCK:
      /*{cec/mach-seq.i xef.gsh-len xef.gsh-wid xcal}*/
      LEAVE.
    END.
    IF AVAIL mach THEN DO:
      CREATE m-lst.
      ASSIGN
       m-lst.f-no   = xef.form-no
       m-lst.seq    = 10 * mach.d-seq
       m-lst.dept   = "FO"
       m-lst.bl     = YES
       m-lst.m-code = mach.m-code.
    END.
  END.

  ELSE DO:                     /* Manufactured FG */
    FIND FIRST style
        where (style.company eq cocode)
          AND style.style EQ xeb.style
        NO-LOCK NO-ERROR.

    if xef.lam-dscr eq "R"                         /*or
       (xef.lam-dscr ne "R" and xef.xgrain eq "S")*/ then
      assign
       sh-wid = xef.nsh-wid
       sh-len = xef.nsh-len.
    else
      assign
       sh-wid = xef.nsh-len
       sh-len = xef.nsh-wid.

    IF NOT ll-foam THEN DO:
      /* CTS - added corrugator logic */
      /* need corrugator? */
      /* if xef.roll then */
      if can-find(first item-bom
                  where item-bom.company  eq cocode
                    and item-bom.parent-i eq xef.board) then do:
        /* find corrugator entered in style file, if any */
        if avail mach then release mach.
        do i = 1 to 7:
          if style.m-code[i] = "" then next.
          find first mach {&where-machine}            and
                     mach.m-code  = style.m-code[i] no-lock no-error.
          if avail mach and (mach.dept[1] ne "CR") and
                            (mach.dept[2] ne "CR") and
                            (mach.dept[3] ne "CR") and
                            (mach.dept[4] ne "CR")
          then release mach.

          IF mach.p-type EQ "R" OR
              (mach.p-type EQ "B" AND ll-label) THEN
              ASSIGN
                 v-gsh-len = xef.gsh-wid
                 v-gsh-wid = xef.gsh-len.
           ELSE
              ASSIGN
                 v-gsh-wid = xef.gsh-wid
                 v-gsh-len = xef.gsh-len.

          {cec/mach-seq.i v-gsh-len v-gsh-wid xcal}
        end.
        if not avail mach then
        for each mach
            {&where-machine}
              and mach.dept[1] eq "CR"
            no-lock:

           IF mach.p-type EQ "R" OR
              (mach.p-type EQ "B" AND ll-label) THEN
              ASSIGN
                 v-gsh-len = xef.gsh-wid
                 v-gsh-wid = xef.gsh-len.
           ELSE
              ASSIGN
                 v-gsh-wid = xef.gsh-wid
                 v-gsh-len = xef.gsh-len.

           {cec/mach-seq.i v-gsh-len v-gsh-wid xcal}
        end.
        if avail mach then do:
          create m-lst.
          assign
           m-lst.f-no   = xef.form-no
           m-lst.seq    = 10 * mach.d-seq
           m-lst.dept   = "CR"
           m-lst.bl     = no
           m-lst.m-code = mach.m-code
           m-lst.n-out  = xef.n-out
           ll-corr      = YES.
        end.
      end.
      /* CTS end */

      /* need sheeter? */
      if xef.roll AND NOT ll-corr then do:
        /* find sheeter entered in style file, if any */
        if avail mach then release mach.
        do i = 1 to 7:
          if style.m-code[i] = "" then next.
          find first mach {&where-machine}            and
                     mach.m-code  = style.m-code[i] no-lock no-error.
          if avail mach and (mach.dept[1] ne "RS") and
                            (mach.dept[2] ne "RS") and
                            (mach.dept[3] ne "RS") and
                            (mach.dept[4] ne "RS")
          then release mach.
          {cec/mach-seq.i xef.gsh-len xef.gsh-wid xcal}
        end.
        if not avail mach then
        for each mach
            {&where-machine}
              and mach.dept[1] eq "RS"
            no-lock:
          {cec/mach-seq.i xef.gsh-len xef.gsh-wid xcal}
        end.
        if avail mach then do:
          create m-lst.
          assign m-lst.f-no   = xef.form-no
                 m-lst.seq    = 10 * mach.d-seq
                 m-lst.dept   = "RS"
                 m-lst.bl     = no
                 m-lst.m-code = mach.m-code.
        end.
      end.

      /* find ink & coater */
      if xeb.i-pass > 0 then do:
        find first item
            where (item.company = cocode and 
       (item.mat-type = "I" or item.mat-type = "V"))
              and item.i-no eq xeb.i-code[1]
            no-lock no-error.
        if avail item then find first e-item of item no-lock no-error.
        maxco = (xeb.i-col + xeb.i-coat ) / xeb.i-pass.
        {sys/inc/roundup.i maxco}

        if xeb.i-coat > 0 then
        do k = 1 to xeb.i-coat:
          /* find coater entered in style file, if any */
          if avail mach then release mach.
          do i = 1 to 7:
            if style.m-code[i] = "" then next.
            for each mach
                {&where-machine}
                  and mach.m-code  eq style.m-code[i]
                  and mach.dept[1] eq "CT"
                no-lock:
              {cec/mach-seq.i sh-len sh-wid xcal}
            end.
          end.
          /* find 1st valid machine in mach file */
          if not avail mach then
          for each mach
              {&where-machine}
                and mach.dept[1] eq "CT"
              no-lock:
            {cec/mach-seq.i sh-len sh-wid xcal}
          end.
          if avail mach then do:
            create m-lst.
            assign
             m-lst.f-no   = xef.form-no
             m-lst.seq = (10 * mach.d-seq) + k
             m-lst.bl  = no
             m-lst.dept   = "CT"
             m-lst.m-code = mach.m-code
             m-lst.dscr = mach.m-dscr
             m-lst.pass-no = k.

            maxco = xeb.i-col / xeb.i-pass.
            {sys/inc/roundup.i maxco}
          end.
        end. /* avail item... */

        /* find press */
        if avail item then k = 0.
        prez: repeat:
          k = k + 1.
          /* find machine entered in style file, if any */
          if k = 1 and avail mach then release mach.
          do i = 1 to 7:
            if style.m-code[i] = "" then next.
            for each mach
                {&where-machine}
                  and mach.m-code  eq style.m-code[i]
                  and mach.max-col ge maxco
                no-lock:
              {cec/mach-seq.i sh-len sh-wid xcal}
            end.
            if (not avail mach) or
               (avail item and mach.pr-type ne item.press-type) then next.
            if avail mach then leave.
          end.
          /* find layout machine in mach file */
          if not avail mach then do:
            find first mach {&where-machine} and mach.m-code = xef.m-code
                no-lock no-error.
            if avail mach and mach.dept[1] ne "PR" then release mach.
          end.
          /* find 1st valid machine in mach file */
          if not avail mach then
          for each mach
              {&where-machine}
                and mach.max-col ge maxco
              by mach.max-col:
            if avail item and mach.pr-type ne item.press-type then next.
            {cec/mach-seq.i sh-len sh-wid xcal}
          end.

          if avail mach then do:
            if mach.p-type = "R" or xef.roll = no then do:
              find first m-lst where m-lst.dept = "RS" no-error.
              if avail m-lst then delete m-lst.
            end.
            if k = 1 and mach.coater = true and mach.max-col > maxco then do:
              find first m-lst where m-lst.seq > 30 and m-lst.seq < 40 no-error.
              if avail m-lst then do:
                delete m-lst.
                k = k - 1.
                maxco = maxco + 1.
                next prez.
              end.
            end.
            create m-lst.
            assign
             m-lst.f-no    = xef.form-no
             m-lst.seq     = (10 * mach.d-seq) + k
             m-lst.bl      = no
             m-lst.dept    = "PR"
             m-lst.m-code  = mach.m-code
             m-lst.dscr    = mach.m-dscr
             m-lst.pass-no = k.
          end.
          if k >= xeb.i-pass then leave.
        end. /* avail item... */
      end. /* if i-pass > 0 */
      else do:
        k = 1.
        /* find machine entered in style file, if any */
        do i = 1 to 7:
          find first mach {&where-machine} and mach.m-code = style.m-code[i]
              no-lock no-error.
          if avail mach then leave.
        end.
        /* find 1st valid machine in mach file */
        if not avail mach then
        find first mach {&where-machine} and mach.m-code = xef.m-code
            no-lock no-error.
        if avail mach and mach.dept[1] = "PR" then do:
          if mach.p-type = "R" or xef.roll = no then do:
            find first m-lst where m-lst.dept = "RS" no-error.
            if avail m-lst then delete m-lst.
          end.
          create m-lst.
          assign
           m-lst.f-no    = xef.form-no
           m-lst.seq     = (10 * mach.d-seq) + k
           m-lst.bl      = no
           m-lst.m-code  = mach.m-code
           m-lst.dscr    = mach.m-dscr
           m-lst.pass-no = k.
          do i = 1 to 4 :
            if mach.dept[i] ne "" or mach.dept[i] ne "PR" then do:
              m-lst.dept    = mach.dept[i].
              leave.
            end.
          end.
        END.
      END.
    END. /* NOT ll-foam */

    RUN cec/mach-sq1.p.

    IF NOT ll-foam THEN RUN cec/mach-sq4.p.
  END.   /* Manufactured FG */
END. /* for each xef */

RUN cec/mach-sq2.p (AVAIL routing, ip-build-combo).

SESSION:SET-WAIT-STATE ("").

/*RUN est/d-machex.w.*/

/* end ---------------------------------- copr. 1992  advanced software, inc. */
