DEF INPUT PARAMETER v-rctd-rowid AS ROWID NO-UNDO.
/*
DEF INPUT PARAMETER v-from-job AS CHAR NO-UNDO.
DEF INPUT PARAMETER v-to-job AS CHAR NO-UNDO. */
DEF VAR begin_userid AS CHAR NO-UNDO.
DEF VAR end_userid AS CHAR NO-UNDO.
DEF VAR ip-post AS LOG NO-UNDO.
DEF VAR v-post-date AS DATE NO-UNDO.
DEF VAR ll-auto AS LOG NO-UNDO.

ASSIGN
  begin_userid = USERID("nosweat")
  end_userid   = USERID("nosweat")
  v-post-date = TODAY.

/*IF INDEX(PROGRAM-NAME(1),"rm/r-rmtpst") NE 0 OR
   INDEX(PROGRAM-NAME(1),"rm/r-rmte&p") NE 0 THEN */
   ip-post = YES.

/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}
    
    DEF TEMP-TABLE tt-email NO-UNDO
        FIELD vend-no      AS CHAR 
        FIELD po-no        AS INT
        FIELD item-no      AS char
        FIELD item-name    AS CHAR
        FIELD po-qty       AS DEC
        FIELD recvd-qty    AS DEC
        FIELD total-recvd-qty AS DEC
        FIELD cons-uom        AS CHAR
        FIELD overrun-pct AS DEC
        FIELD underrun-pct AS DEC
        FIELD allow-qty    AS DEC  
        FIELD under-qty    AS DEC
        FIELD undovr       AS CHAR
        INDEX po-no po-no ASC item-no ASC.

assign
 cocode = gcompany
 locode = gloc.

DEF VAR ll-valid AS LOG NO-UNDO.

def new shared var pox like po-ordl.po-no.

def var v-types as char format "x(10)" no-undo.
def var v-autoissue as log.
def var v-dunne as log init no.
DEF VAR v-rmtags-log AS LOG NO-UNDO.
DEF VAR lv-post AS LOG NO-UNDO.
DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.

DEF BUFFER b-job-hdr FOR job-hdr.
DEF BUFFER b-rh FOR rm-rcpth.
DEF BUFFER b-rd FOR rm-rdtlh.

{jc/jcgl-sh.i NEW}
    find first sys-ctrl
        where sys-ctrl.company eq cocode
          and sys-ctrl.name    eq "AUTOISSU"
        no-lock no-error.
    if not avail sys-ctrl then do transaction:
      create sys-ctrl.
      assign
       sys-ctrl.company = cocode
       sys-ctrl.name    = "AUTOISSU"
       sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs?".
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
    end.
    v-autoissue = sys-ctrl.log-fld.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company eq cocode
        AND sys-ctrl.name eq "RMTAGS"
        NO-LOCK NO-ERROR.

    IF NOT AVAIL sys-ctrl THEN
       DO TRANSACTION:
          CREATE sys-ctrl.
          ASSIGN
             sys-ctrl.company = cocode
             sys-ctrl.name    = "RMTAGS"
             sys-ctrl.descrip = "Number of RM Loadtags to Print & Create Wip Tags"
             sys-ctrl.char-fld = ""
             sys-ctrl.int-fld = 1
             sys-ctrl.log-fld = FALSE. /* true create wip/false do not */
       END.

    ASSIGN v-rmtags-log = sys-ctrl.log-fld.

DO TRANSACTION:
  {sys/inc/rmpostgl.i}
END.

DEF TEMP-TABLE tt-rctd NO-UNDO LIKE rm-rctd FIELD tt-row-id AS ROWID
                              FIELD rm-row-id AS ROWID
                              FIELD has-rec   AS LOG INIT NO
                              FIELD seq-no    AS INT
                              INDEX seq-no seq-no i-no.

DEF TEMP-TABLE tt-mat NO-UNDO FIELD frm LIKE job-mat.frm
                              FIELD qty LIKE job-mat.qty
                              INDEX frm frm.




  for each rm-rctd
      where rowid(rm-rctd) = v-rctd-rowid
         /* .company   eq cocode */
        and rm-rctd.rita-code ne "C"
      no-lock,
      first item
      where item.company eq cocode
        and item.i-no    eq rm-rctd.i-no
      no-lock
      break by rm-rctd.rita-code:

    if first-of(rm-rctd.rita-code) then v-types = v-types + rm-rctd.rita-code.

    IF NOT ll-auto AND rm-rctd.rita-code EQ "R" THEN
      ll-auto = (item.mat-type EQ "I" AND TRIM(rm-rctd.po-no) EQ "") OR
                (item.i-code EQ "E" AND TRIM(rm-rctd.po-no) NE "").
  end.

  if ll-auto                 and
     index(v-types,"R") gt 0 and
     index(v-types,"I") eq 0 then v-types = trim(v-types) + "I".
    
  RUN create-post-records.

  ll-auto = v-autoissue.
   DO TRANSACTION:  
      {sys/inc/rmemails.i}
      {sys/inc/postdate.i}
   END.
  
  /** Initiate Posting **/
  IF ip-post THEN DO: 
    lv-post = CAN-FIND(FIRST tt-rctd WHERE tt-rctd.has-rec).
    IF lv-post THEN
    FOR EACH tt-rctd
        WHERE tt-rctd.has-rec
          AND NOT CAN-FIND(FIRST rm-rctd WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id):
      lv-post = NO.
      LEAVE.
    END.

    IF lv-post THEN DO:
      FOR EACH tt-rctd
          WHERE tt-rctd.has-rec
            AND CAN-FIND(FIRST rm-rcpth WHERE rm-rcpth.r-no EQ tt-rctd.r-no),
          FIRST rm-rctd WHERE rm-rctd.r-no EQ tt-rctd.r-no:
        lv-r-no = rm-rctd.r-no.
        DO TRANSACTION:
          rm-rctd.r-no = 0.
        END.
        DO TRANSACTION:
          rm-rctd.r-no = lv-r-no.
        END.
        tt-rctd.r-no = rm-rctd.r-no.
      END.

      FOR EACH tt-rctd
          WHERE tt-rctd.has-rec
            AND NOT CAN-FIND(FIRST rm-rctd WHERE rm-rctd.r-no EQ tt-rctd.r-no),
          FIRST rm-rcpth NO-LOCK WHERE rm-rcpth.r-no EQ tt-rctd.r-no:
        MESSAGE "Sorry, these RM Transactions cannot be processed because 1 or " +
                "more have already been posted by UserID: " +
                TRIM(rm-rcpth.user-id) + "..."
            VIEW-AS ALERT-BOX ERROR.

        lv-post = NO.
        LEAVE.
      END.
    END.

    IF ip-post THEN DO:
       FIND FIRST period
           WHERE period.company EQ cocode
             AND period.pst     LE v-post-date
             AND period.pend    GE v-post-date
           NO-LOCK NO-ERROR.
       IF NOT AVAIL period THEN DO:
         MESSAGE "No period exists for this date..."
                 VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
       END.
    END.

    IF lv-post THEN do:
      RUN post-rm.

      lv-post = v-dunne.

    END.
  END.

  RUN util/fxissues.p.

/** end initiate posting **/


PROCEDURE create-post-records :
/* ------------------------------------------------ rm/rep/rm-post.p 10/93 cd */
/* raw materials posting - part 1 : printout                                  */
/* -------------------------------------------------------------------------- */   

def var v-type-prt as ch format "X(11)" init "".
def var v-ext-cost as de.
def var v-tot-qty as dec format "->>,>>>,>>9.99<<".
def var v-tot-cost as dec format "->,>>>>,>>9.99<<".
def var v-grd-qty as dec format "->>,>>>,>>9.99<<".
def var v-grd-cost as dec format "->,>>>>,>>9.99<<".
def var v-po-no like rm-rctd.po-no.
def var v-inkissue as log.
def var v-whse like rm-rctd.loc.
DEF VAR v-int AS INT NO-UNDO.
DEF VAR ll-one-item AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".

/* gdm - 11030903 */
DEF VAR v-itmcnt  AS INT NO-UNDO.
DEF VAR v-gitmcnt AS INT NO-UNDO.

DEF VAR v-totRita   AS CHAR                           NO-UNDO.
DEF VAR v-totLabl   AS CHAR FORMAT "x(50)"            NO-UNDO.
DEF VAR v-tot-qtyI  AS DEC  FORMAT "->>,>>>,>>9.99<<" NO-UNDO.
DEF VAR v-tot-costI AS DEC  FORMAT "->,>>>>,>>9.99<<" NO-UNDO.
DEF VAR v-grd-qtyI  AS DEC  FORMAT "->>,>>>,>>9.99<<" NO-UNDO.
DEF VAR v-grd-costI AS DEC  FORMAT "->,>>>>,>>9.99<<" NO-UNDO.


def buffer b-rm-rctd for rm-rctd.
DEF BUFFER b-item FOR ITEM.
DEF BUFFER b-tt-rctd FOR tt-rctd.


    FOR EACH tt-rctd:
      DELETE tt-rctd.
    END.
    FOR EACH rm-rctd 
        WHERE rowid(rm-rctd) = v-rctd-rowid
        /*.company   EQ cocode
          AND 
          AND rm-rctd.job-no    GE v-from-job
          AND rm-rctd.job-no    LE v-to-job */
          AND INDEX(v-types,rm-rctd.rita-code) GT 0 
          AND rm-rctd.rita-code NE "C"
        NO-LOCK:
      CREATE tt-rctd.
      BUFFER-COPY rm-rctd TO tt-rctd
      ASSIGN
       tt-rctd.rm-row-id = ROWID(rm-rctd)
       tt-rctd.has-rec   = YES
       tt-rctd.seq-no    = 1.
    END.

    if index(v-types,"R") gt 0 then
    auto-issue:
    for each tt-rctd
        where tt-rctd.rita-code eq "R"
          and tt-rctd.job-no    ne ""
        no-lock,
        first item
        where item.company eq cocode
          and item.i-no    eq tt-rctd.i-no
        no-lock.

      release po-ordl.

      v-po-no = trim(tt-rctd.po-no).
      if v-po-no ne "" then do:
        do x = 1 to length(v-po-no):
          if substr(v-po-no,x,1) lt "0" or
             substr(v-po-no,x,1) gt "9" then next auto-issue.
        end.

        find first po-ordl
            where po-ordl.company   eq cocode
              and po-ordl.i-no      eq tt-rctd.i-no
              and po-ordl.po-no     eq int(v-po-no)
              and po-ordl.job-no    eq tt-rctd.job-no
              and po-ordl.job-no2   eq tt-rctd.job-no2
              and po-ordl.item-type eq yes
            use-index item-ordno no-lock no-error.
      end.

      IF item.mat-type NE "I" OR AVAIL po-ordl THEN
        IF (item.i-code EQ "E" AND
            NOT AVAIL po-ordl)      OR
           (item.i-code EQ "R" AND
            NOT v-autoissue)        THEN NEXT auto-issue.

      EMPTY TEMP-TABLE tt-mat.
      
      RELEASE job.
      IF tt-rctd.job-no NE "" AND tt-rctd.s-num EQ ? THEN
      FIND FIRST job
        WHERE job.company EQ cocode
          AND job.job-no  EQ tt-rctd.job-no
          AND job.job-no2 EQ tt-rctd.job-no2
        NO-LOCK NO-ERROR.

      IF AVAIL job THEN DO:
        ld = 0.

        FOR EACH job-mat
            WHERE job-mat.company EQ job.company
              AND job-mat.job     EQ job.job
              AND job-mat.job-no  EQ job.job-no
              AND job-mat.job-no2 EQ job.job-no2
              AND job-mat.rm-i-no EQ tt-rctd.i-no
            NO-LOCK
            BY job-mat.frm:
          CREATE tt-mat.
          ASSIGN
           tt-mat.frm = job-mat.frm
           tt-mat.qty = job-mat.qty
           ld         = ld + job-mat.qty.
        END.

        FOR EACH tt-mat:
          tt-mat.qty = tt-rctd.qty * (tt-mat.qty / ld).
          IF tt-rctd.pur-uom EQ "EA" THEN DO:
            {sys/inc/roundup.i tt-mat.qty} 
          END.
        END.

        ld = 0.
        FOR EACH tt-mat:
          ld = ld + tt-mat.qty.
        END.

        IF ld NE tt-rctd.qty THEN
        FOR EACH tt-mat:
          tt-mat.qty = tt-mat.qty + (tt-rctd.qty - ld).
          LEAVE.
        END.
      END.

      ELSE DO:
        CREATE tt-mat.
        ASSIGN
         tt-mat.frm = tt-rctd.s-num
         tt-mat.qty = tt-rctd.qty.
      END.

      FOR EACH tt-mat:
        CREATE b-tt-rctd.
        BUFFER-COPY tt-rctd EXCEPT rec_key TO b-tt-rctd
        ASSIGN
         b-tt-rctd.rita-code = "I"
         b-tt-rctd.tt-row-id = ROWID(tt-rctd)
         b-tt-rctd.seq-no    = 2
         b-tt-rctd.s-num     = tt-mat.frm
         b-tt-rctd.qty       = tt-mat.qty.

        DELETE tt-mat.
      END.
    end.

    issue-adder-for-board:
    for each tt-rctd
        where tt-rctd.rita-code eq "I"
          and tt-rctd.job-no    ne ""
        no-lock,
        first job
        where job.company eq cocode
          and job.job-no  eq tt-rctd.job-no
          and job.job-no2 eq tt-rctd.job-no2
        no-lock,

        first item
        where item.company  eq cocode
          and item.i-no     eq tt-rctd.i-no
          and item.mat-type eq "B"
        no-lock:

      {rm/rm-addcr.i E b-tt-rctd b-tt-rctd b-}
        ASSIGN
         b-tt-rctd.tt-row-id = ROWID(tt-rctd)
         b-tt-rctd.seq-no    = 3.

      END.
    end.

    assign
     v-grd-qty  = 0
     v-grd-cost = 0.

    FOR EACH tt-rctd WHERE INDEX(v-types,tt-rctd.rita-code) GT 0 
        BREAK BY tt-rctd.loc                                             
              BY tt-rctd.i-no                                            
              BY tt-rctd.job-no                                          
              BY tt-rctd.job-no2 
              BY tt-rctd.loc-bin                           
              BY tt-rctd.tag
              BY RECID(tt-rctd)
              
        WITH FRAME itemx:                                                   

      if first-of(tt-rctd.loc) then do:
        v-whse = tt-rctd.loc.
      end.

      find first item no-lock
          where item.company eq cocode
            and item.i-no    eq tt-rctd.i-no
          no-error.

      release costtype.
      if avail item then
      find first costtype no-lock
          where costtype.company   eq cocode
            and costtype.cost-type eq item.cost-type
          no-error.

      release po-ord.
      if int(tt-rctd.po-no) ne 0 and tt-rctd.rita-code eq "R" then                                         
      find first po-ord no-lock
          where po-ord.company eq cocode
            and po-ord.po-no   eq int(tt-rctd.po-no)
          no-error.

      release po-ordl.
      if avail po-ord then
      find first po-ordl no-lock
          where po-ordl.company   eq cocode
            and po-ordl.po-no     eq po-ord.po-no
            and po-ordl.i-no      eq tt-rctd.i-no
            and po-ordl.job-no    eq tt-rctd.job-no
            and po-ordl.job-no2   eq tt-rctd.job-no2
            and po-ordl.s-num     eq tt-rctd.s-num
            and po-ordl.b-num     eq tt-rctd.b-num
            and po-ordl.deleted   eq no
            and po-ordl.item-type eq yes
          no-error.

      v-ext-cost = tt-rctd.cost * tt-rctd.qty.
       
      IF rmpostgl AND AVAIL costtype AND costtype.inv-asset NE ""  AND
         v-ext-cost NE 0 AND v-ext-cost NE ?                       THEN DO:

        if tt-rctd.rita-code EQ "R"  AND  
           costtype.ap-accrued NE "" THEN DO:

          /* Debit RM Asset */
          FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.inv-asset NO-LOCK NO-ERROR.
          IF NOT AVAIL work-gl THEN DO:
            CREATE work-gl.
            work-gl.actnum = costtype.inv-asset.
          END.
          work-gl.debits = work-gl.debits + v-ext-cost.

          /* Credit RM AP Accrued */
          FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.ap-accrued NO-LOCK NO-ERROR.
          IF NOT AVAIL work-gl THEN DO:
            CREATE work-gl.
            work-gl.actnum = costtype.ap-accrued.
          END.
          work-gl.credits = work-gl.credits + v-ext-cost.
        END.

        ELSE
        IF tt-rctd.rita-code EQ "I" AND
           tt-rctd.job-no NE ""     THEN DO:

          FOR EACH job-hdr
              WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ tt-rctd.job-no
                AND job-hdr.job-no2 EQ tt-rctd.job-no2
              NO-LOCK,
              FIRST job OF job-hdr NO-LOCK
              BREAK BY job-hdr.frm:
            ll-one-item = FIRST(job-hdr.frm) AND LAST(job-hdr.frm).
            LEAVE.
          END.

          FOR EACH job-hdr
              WHERE job-hdr.company     EQ cocode
                AND job-hdr.job-no      EQ tt-rctd.job-no
                AND job-hdr.job-no2     EQ tt-rctd.job-no2
                AND ((job-hdr.frm       EQ tt-rctd.s-num AND
                      (job-hdr.blank-no EQ tt-rctd.b-num OR tt-rctd.b-num EQ 0))
                 OR  ll-one-item)
              NO-LOCK,
              FIRST job OF job-hdr NO-LOCK,
              FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ job-hdr.i-no
              NO-LOCK,
              FIRST prodl
              WHERE prodl.company EQ cocode
                AND prodl.procat  EQ itemfg.procat
                AND CAN-FIND(FIRST prod
                             WHERE prod.company EQ cocode
                               AND prod.prolin  EQ prodl.prolin)
              NO-LOCK,
              FIRST prod
              WHERE prod.company EQ cocode
                AND prod.prolin  EQ prodl.prolin
                AND prod.wip-mat NE ""
              NO-LOCK:

            ld = ROUND(v-ext-cost * (IF ll-one-item        OR
                                        tt-rctd.b-num NE 0 OR
                                        job-hdr.sq-in LE 0 OR
                                        job-hdr.sq-in EQ ? THEN 1
                                     ELSE (job-hdr.sq-in / 100)),2).

            /* Debit FG Wip Material */
            FIND FIRST work-gl
                WHERE work-gl.job     EQ job-hdr.job
                  AND work-gl.job-no  EQ job-hdr.job-no
                  AND work-gl.job-no2 EQ job-hdr.job-no2
                  AND work-gl.actnum  EQ prod.wip-mat 
                NO-LOCK NO-ERROR.
            IF NOT AVAIL work-gl THEN DO:
              CREATE work-gl.
              ASSIGN
               work-gl.job     = job-hdr.job
               work-gl.job-no  = job-hdr.job-no
               work-gl.job-no2 = job-hdr.job-no2
               work-gl.actnum  = prod.wip-mat.
            END.
            work-gl.debits = work-gl.debits + ld.

            /* Credit RM Asset */
            FIND FIRST work-gl
                WHERE work-gl.job     EQ job-hdr.job
                  AND work-gl.job-no  EQ job-hdr.job-no
                  AND work-gl.job-no2 EQ job-hdr.job-no2
                  AND work-gl.actnum  EQ costtype.inv-asset
                NO-LOCK NO-ERROR.
            IF NOT AVAIL work-gl THEN DO:
              CREATE work-gl.
              ASSIGN
               work-gl.job     = job-hdr.job
               work-gl.job-no  = job-hdr.job-no
               work-gl.job-no2 = job-hdr.job-no2
               work-gl.actnum  = costtype.inv-asset.
            END.
            work-gl.credits = work-gl.credits + ld.
          END.
        END.
      END.

      if tt-rctd.rita-code eq "R" or
         tt-rctd.rita-code eq "A"
        then assign v-tot-qty = v-tot-qty + tt-rctd.qty
                    v-tot-cost = v-tot-cost + (tt-rctd.cost * tt-rctd.qty).
      
      if tt-rctd.rita-code eq "I" 
        then assign v-tot-qtyI  = v-tot-qtyI  + tt-rctd.qty
                    v-tot-costI = v-tot-costI + (tt-rctd.cost * tt-rctd.qty).
      
      ASSIGN v-itmcnt  = v-itmcnt  + 1
             v-gitmcnt = v-gitmcnt + 1.

      if last-of(tt-rctd.i-no) then do:       

        assign
         v-grd-qty   = v-grd-qty   + v-tot-qty 
         v-grd-cost  = v-grd-cost  + v-tot-cost
         v-grd-qtyI  = v-grd-qtyI  + v-tot-qtyI 
         v-grd-costI = v-grd-costI + v-tot-costI
         v-tot-qty   = 0
         v-tot-cost  = 0
         v-tot-qtyI  = 0
         v-tot-costI = 0
         v-itmcnt    = 0.      
      end. 
    end. /* each tt-rctd */
    
END PROCEDURE.


PROCEDURE post-rm :
/* --------------------------------------------------- rm/rm-post.p 10/94 rd  */
/* raw materials inventory control receipt maintenance                        */
/* -------------------------------------------------------------------------- */

def buffer xrm-rctd     for rm-rctd.
def buffer xrm-bin      for rm-bin.
def buffer b-rm-rctd    for rm-rctd.
def buffer b-item       for item.
def buffer b-po-ordl    for po-ordl.
def buffer b-job-mat    for job-mat.

def var v-avg-cst   as log.
def var v-next_r-no like rm-rctd.r-no.
def var v_r-no like rm-rctd.r-no.
def var v-conv-qty as dec.
def var v-reduce-qty like po-ordl.ord-qty.
def var no-of-items as int no-undo.
def var ld-cvt-qty as dec no-undo.
def var v-trnum like gl-ctrl.trnum no-undo.

def var v-r-qty     as   dec                    no-undo.
def var v-i-qty     as   dec                    no-undo.
def var v-t-qty     as   dec                    no-undo.
def var cost        as   dec                    no-undo.
def var out-qty     as   dec                    no-undo.
def var v-bwt       like item.basis-w           no-undo.
def var v-len       like item.s-len             no-undo.
def var v-wid       like item.s-wid             no-undo.
def var v-dep       like item.s-dep             no-undo.
def var v-recid     as   recid                  no-undo.
DEF VAR li          AS   INT                    NO-UNDO.

DEF VAR v-rmemail-file AS cha NO-UNDO.

find first rm-ctrl where rm-ctrl.company eq cocode no-lock no-error.
v-avg-cst = rm-ctrl.avg-lst-cst.

    SESSION:SET-WAIT-STATE ("general").

    transblok:
    FOR EACH tt-rctd
        WHERE CAN-FIND(FIRST item WHERE item.company EQ cocode
                                    AND item.i-no    EQ tt-rctd.i-no)
          AND INDEX(v-types,tt-rctd.rita-code) gt 0 
        BREAK BY tt-rctd.seq-no
              BY tt-rctd.i-no
              BY tt-rctd.r-no
              BY RECID(tt-rctd)
        
        TRANSACTION:

      RELEASE rm-rctd.
      RELEASE item.
      li = 0.

      DO WHILE (NOT AVAIL rm-rctd OR NOT AVAIL item) AND li LT 1000:
        li = li + 1.

        FIND rm-rctd EXCLUSIVE-LOCK WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id
            NO-WAIT NO-ERROR.
      
        FIND FIRST item EXCLUSIVE-LOCK
            WHERE item.company EQ rm-rctd.company
              AND item.i-no    EQ rm-rctd.i-no
            USE-INDEX i-no NO-WAIT NO-ERROR.
      END.
      IF NOT AVAIL rm-rctd OR NOT AVAIL item THEN NEXT transblok.

      IF rm-rctd.rita-code EQ "I" AND INT(rm-rctd.po-no) NE 0 THEN
      FOR EACH xrm-rctd
          WHERE xrm-rctd.company   EQ cocode
            AND xrm-rctd.i-no      EQ rm-rctd.i-no
            AND xrm-rctd.rita-code EQ "R"
            AND xrm-rctd.po-no     EQ rm-rctd.po-no
            AND xrm-rctd.r-no      LT rm-rctd.r-no
          NO-LOCK:
            
        UNDO transblok, NEXT transblok.
      END.

      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) +
                               TRIM(rm-rctd.job-no)
            AND job.job-no2 EQ rm-rctd.job-no2
          NO-ERROR.

      /** Find Bin & if not avail then create it **/
      FIND FIRST rm-bin
          WHERE rm-bin.company EQ rm-rctd.company
            AND rm-bin.loc     EQ rm-rctd.loc
            AND rm-bin.i-no    EQ rm-rctd.i-no
            AND rm-bin.loc-bin EQ rm-rctd.loc-bin
            AND rm-bin.tag     EQ rm-rctd.tag
          NO-ERROR.
      IF NOT AVAIL rm-bin THEN DO:
        CREATE rm-bin.
        ASSIGN
         rm-bin.company = rm-rctd.company
         rm-bin.loc     = rm-rctd.loc
         rm-bin.loc-bin = rm-rctd.loc-bin
         rm-bin.tag     = rm-rctd.tag
         rm-bin.i-no    = rm-rctd.i-no.
      END. /* not avail rm-bin */

      ld-cvt-qty = rm-rctd.qty.

      IF rm-rctd.pur-uom NE item.cons-uom AND item.cons-uom NE "" THEN
        RUN sys/ref/convquom.p (rm-rctd.pur-uom, item.cons-uom,
                              item.basis-w,
                              (if item.r-wid eq 0 then item.s-len else 12), 
                              (if item.r-wid eq 0 then item.s-wid else item.r-wid),
                              item.s-dep,
                              ld-cvt-qty, OUTPUT ld-cvt-qty).
        
      if rm-rctd.rita-code eq "R" then do:        /** RECEIPTS **/
        {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        assign
         rm-bin.qty     = rm-bin.qty + ld-cvt-qty
         item.last-cost = rm-rctd.cost
         item.q-onh     = item.q-onh + ld-cvt-qty.
           
        {rm/rm-poupd.i 2}
          
        item.q-avail = item.q-onh + item.q-ono - item.q-comm.
      end. /* R */

      else
      if rm-rctd.rita-code eq "I" then do:  /** ISSUES **/

         IF rm-rctd.tag NE "" THEN
            FOR EACH b-rd FIELDS(r-no rita-code tag2) WHERE
                b-rd.company   EQ cocode AND
                b-rd.tag       EQ rm-rctd.tag AND
                b-rd.loc       EQ rm-rctd.loc AND
                b-rd.loc-bin   EQ rm-rctd.loc-bin AND
                b-rd.rita-code EQ "R" AND
                b-rd.tag2      NE ""
                NO-LOCK
                USE-INDEX tag,
                FIRST b-rh WHERE
                      b-rh.r-no EQ b-rd.r-no AND
                      b-rh.rita-code EQ b-rd.rita-code AND
                      b-rh.i-no      EQ rm-rctd.i-no
                      NO-LOCK:
           
                rm-rctd.tag2 = b-rd.tag2.
            END.

         if avail job and job.job-no ne "" then do:
            run rm/mkjobmat.p (recid(rm-rctd),rm-rctd.company, output v-recid).
              
            find job-mat where recid(job-mat) eq v-recid no-error.
              
            if not avail job-mat then do:
               bell.
               message " Job Mat Record not found for "
                       string(job.job-no + "-" + string(job.job-no2,"99") +
                              "  " + rm-rctd.i-no)
                       VIEW-AS ALERT-BOX.
               undo transblok, next transblok.
            end.
           
            assign
             v-bwt = job-mat.basis-w
             v-len = job-mat.len
             v-wid = job-mat.wid
             v-dep = item.s-dep.
           
            if v-len eq 0 then v-len = item.s-len.
           
            if v-wid eq 0 then
              v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.
           
            if v-bwt eq 0 then v-bwt = item.basis-w.
           
            if index("RL",job.stat) ne 0 then job.stat = "W".
              
            {rm/rmmatact.i}            /* Create Actual Material */
              
            out-qty = rm-rctd.qty.
            IF rm-rctd.pur-uom NE job-mat.qty-uom AND rm-rctd.pur-uom NE "" THEN
               RUN sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.qty-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      rm-rctd.qty, output out-qty).
           
            cost = rm-rctd.cost.
            IF rm-rctd.pur-uom NE job-mat.sc-uom AND rm-rctd.pur-uom NE "" THEN
               RUN sys/ref/convcuom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      rm-rctd.cost, OUTPUT cost).
           
            assign
             mat-act.qty-uom = job-mat.qty-uom
             mat-act.cost    = cost
             mat-act.qty     = mat-act.qty     + out-qty
             job-mat.qty-iss = job-mat.qty-iss + out-qty
             job-mat.qty-all = job-mat.qty-all - out-qty
             item.q-comm     = item.q-comm     - rm-rctd.qty.
              
            run sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rctd.qty, output out-qty).
           
            mat-act.ext-cost = mat-act.ext-cost + (cost * out-qty).
           
            /* Don't relieve more than were allocated */
            if job-mat.qty-all lt 0 then do:
              run sys/ref/convquom.p(job-mat.qty-uom, rm-rctd.pur-uom,
                                     v-bwt, v-len, v-wid, v-dep,
                                     job-mat.qty-all, output out-qty).
              assign
               job-mat.qty-all = 0
               item.q-comm     = item.q-comm - out-qty.
            end.
           
            /*job-mat.all-flg = (job-mat.qty-all gt 0).*/
            if item.q-comm lt 0 then item.q-comm = 0.
           
            IF item.mat-type EQ "B" THEN RUN rm/rm-addcr.p (ROWID(rm-rctd)).
         end.
           
         find first rm-bin
             where rm-bin.company eq rm-rctd.company
               and rm-bin.loc     eq rm-rctd.loc
               and rm-bin.i-no    eq rm-rctd.i-no
               and rm-bin.loc-bin eq rm-rctd.loc-bin
               and rm-bin.tag     eq rm-rctd.tag
             no-error.
           
         assign
          rm-bin.qty     = rm-bin.qty - ld-cvt-qty
          item.q-onh     = item.q-onh - ld-cvt-qty
          item.qlast-iss = rm-rctd.qty
          item.dlast-iss = rm-rctd.rct-date
          item.q-ytd     = item.q-ytd + rm-rctd.qty
          item.q-ptd     = item.q-ptd + rm-rctd.qty
          item.u-ptd     = item.u-ptd + (rm-rctd.cost * rm-rctd.qty)
          item.u-ytd     = item.u-ytd + (rm-rctd.cost * rm-rctd.qty)
          item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      end.  /* I */

      else
      if rm-rctd.rita-code eq "A" then do:  /** ADJUSTMENTS **/
        if rm-rctd.cost ne 0 then do:
          {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}
        end.

        assign
         rm-bin.qty     = rm-bin.qty + ld-cvt-qty
         item.last-cost = if rm-rctd.cost ne 0 then rm-rctd.cost
                                               else item.last-cost
         item.q-onh     = item.q-onh + ld-cvt-qty
         item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      end. /* A */

      else
      if rm-rctd.rita-code eq "T" then do:  /** TRANSFERS **/
        assign
         rm-bin.qty   = rm-bin.qty - rm-rctd.qty
         rm-rctd.cost = rm-bin.cost.

        /* This code is to handel the Transfer to quantity to increase the BIN
           using a buffer record so current rm-bin record is not updated. */

        find first xrm-bin
             where xrm-bin.company eq rm-rctd.company
               and xrm-bin.loc     eq rm-rctd.loc2
               and xrm-bin.i-no    eq rm-rctd.i-no
               and xrm-bin.loc-bin eq rm-rctd.loc-bin2
               and xrm-bin.tag     eq rm-rctd.tag2
             no-error.
        if not avail xrm-bin then do:
          create xrm-bin.
          assign
           xrm-bin.company = rm-rctd.company
           xrm-bin.loc     = rm-rctd.loc2
           xrm-bin.loc-bin = rm-rctd.loc-bin2
           xrm-bin.tag     = rm-rctd.tag2
           xrm-bin.i-no    = rm-rctd.i-no.
        end.

        {rm/rm-post.i "xrm-bin.qty" "xrm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        xrm-bin.qty = xrm-bin.qty + rm-rctd.qty.
      end. /* T */

/*       /** Delete Bins With Zero Quantities. **/ */
/*       IF rm-bin.qty EQ 0 THEN DELETE rm-bin.    */

      RELEASE loadtag.
      IF TRIM(rm-rctd.tag) NE "" THEN
      FIND FIRST loadtag EXCLUSIVE-LOCK 
          WHERE loadtag.company     EQ rm-rctd.company
            AND loadtag.item-type   EQ YES
            AND loadtag.tag-no      EQ rm-rctd.tag
            AND loadtag.i-no        EQ rm-rctd.i-no
            AND loadtag.is-case-tag EQ NO
          NO-ERROR.

      IF AVAIL loadtag THEN DO:
        IF rm-rctd.rita-code EQ "T" THEN 
          ASSIGN
           loadtag.loc     = rm-rctd.loc2
           loadtag.loc-bin = rm-rctd.loc-bin2.
        ELSE
          ASSIGN
           loadtag.loc     = rm-rctd.loc
           loadtag.loc-bin = rm-rctd.loc-bin.

        li = INDEX("RI",rm-rctd.rita-code).

        IF li EQ 1 AND (NOT AVAIL rm-bin OR rm-bin.qty LT 0) THEN li = 3.

        IF li GT 0 THEN loadtag.sts = ENTRY(li,"Received,Issued,Deleted").
      END.

      if last-of(tt-rctd.i-no) then             /* Calculate average cost */
      for each rm-bin
          where rm-bin.company eq rm-rctd.company
            and rm-bin.i-no    eq rm-rctd.i-no
          no-lock use-index i-no
          break by rm-bin.i-no:

        if first(rm-bin.i-no) then
          assign
           v-i-qty = 0
           cost    = 0.

        v-r-qty = rm-bin.qty.

        if v-r-qty lt 0 then v-r-qty = v-r-qty * -1.

        assign
         v-i-qty = v-i-qty + v-r-qty
         cost    = cost    + (v-r-qty * rm-bin.cost).

        IF cost EQ ? THEN cost = 0.

        if last(rm-bin.i-no) and v-i-qty ne 0 AND cost NE 0 THEN item.avg-cost = cost / v-i-qty.

        /* gdm - 10280903 - Assign prep code received date */
        RUN assign-prep-info. 

      end. /* each rm-bin */      

      RUN final-steps.

      FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
      FIND CURRENT item NO-LOCK NO-ERROR.
      FIND CURRENT loadtag NO-LOCK NO-ERROR.
      FIND CURRENT rm-rcpth NO-LOCK NO-ERROR.
      FIND CURRENT rm-rdtlh NO-LOCK NO-ERROR.
      FIND CURRENT mat-act NO-LOCK NO-ERROR.
      FIND CURRENT job NO-LOCK NO-ERROR.
      FIND CURRENT job-mat NO-LOCK NO-ERROR.
    end. /* for each rm-rctd */

    v-dunne = yes.
    for each rm-rctd
        where rowid(rm-rctd) = v-rctd-rowid
          and rm-rctd.rita-code eq "ADDER"
        transaction:
      rm-rctd.rita-code = "I".
    end.     

    IF rmpostgl THEN DO TRANSACTION:
      /* gdm - 11050906 */
      REPEAT:

        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
          WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN DO:
          ASSIGN v-trnum       = gl-ctrl.trnum + 1
                 gl-ctrl.trnum = v-trnum.

          FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.

          RUN gl-from-work (1, v-trnum).
          RUN gl-from-work (2, v-trnum).
          LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */
    END. /* IF rmpostgl */
    
   IF can-find(FIRST tt-email) THEN 
      RUN send-rmemail (v-rmemail-file).
   
    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

PROCEDURE assign-prep-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-tt-rctd FOR tt-rctd.

FOR EACH bf-tt-rctd   
    WHERE bf-tt-rctd.seq-no EQ tt-rctd.seq-no 
      AND bf-tt-rctd.i-no   EQ tt-rctd.i-no:
/*         BREAK BY bf-tt-rctd.seq-no   */
/*               BY bf-tt-rctd.i-no     */
/*               BY bf-tt-rctd.r-no     */
/*               BY bf-tt-rctd.job-no   */
/*               BY bf-tt-rctd.job-no2: */
/*   IF FIRST-OF(bf-tt-rctd.job-no) THEN DO:                                      */
/*       FOR EACH job-hdr NO-LOCK                                                 */
/*       WHERE job-hdr.company EQ bf-tt-rctd.company                              */
/*         AND job-hdr.job-no  EQ bf-tt-rctd.job-no                               */
/*         AND job-hdr.job-no2 EQ bf-tt-rctd.job-no2:                             */
/*       FIND FIRST itemfg                                                        */
/*         WHERE itemfg.company EQ job-hdr.company                                */
/*           AND itemfg.i-no    EQ job-hdr.i-no NO-LOCK NO-ERROR.                 */
/*       IF AVAIL itemfg THEN DO:                                                 */
/*                                                                                */
/*         IF itemfg.plate-no NE "" THEN DO:                                      */
/*           FIND FIRST prep                                                      */
/*              WHERE prep.company EQ cocode                                      */
/*                AND prep.code    EQ itemfg.plate-no NO-ERROR.                   */
/*            IF AVAIL prep THEN ASSIGN prep.received-date = bf-tt-rctd.rct-date. */
/*         END.                                                                   */
/*                                                                                */
/*         IF itemfg.die-no NE "" THEN DO:                                        */
/*           FIND FIRST prep                                                      */
/*             WHERE prep.company EQ cocode                                       */
/*               AND prep.code    EQ itemfg.die-no NO-ERROR.                      */
/*           IF AVAIL prep THEN ASSIGN prep.received-date = bf-tt-rctd.rct-date.  */
/*                                                                                */
/*         END.                                                                   */
/*         RELEASE prep.                                                          */
/*                                                                                */
/*       END. /* avail itemfg */                                                  */
/*     END. /* EACH job-hdr */                                                    */
/*     END. /*bf-tt-rctd.job ne ""*/                                              */
/*   END. /* FIRST-OF */                                                          */
/*Note (BPV) - code above was commented after discussing the simplicity of matching 
the RM item number between receipt record and prep record via the i-no
NOTE: this is not an indexed search but records in prep should not be a large # 
wfk - 9/6 - Changing to join on prep.code so that customers who use the same rm
in multiple prep codes are not affected. Also, should be indexed, so faster.
*/
    FOR EACH prep WHERE prep.company EQ cocode                          
             AND prep.CODE = bf-tt-rctd.i-no:                           
        ASSIGN
            prep.loc            = bf-tt-rctd.loc
            prep.loc-bin        = bf-tt-rctd.loc-bin
            prep.received-date  = bf-tt-rctd.rct-date.
        IF bf-tt-rctd.job-no NE "" THEN
            ASSIGN
                prep.last-job-no    = bf-tt-rctd.job-no
                prep.last-job-no2   = bf-tt-rctd.job-no2.
    END. /* each prep */
END. /* each tt-rctd   */

END PROCEDURE.

PROCEDURE final-steps :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-tt-rctd FOR tt-rctd.
  DEF BUFFER rec-rm-rdtlh FOR rm-rdtlh.
  DEF BUFFER rec-rm-rcpth FOR rm-rcpth.

  DEF VAR v-int AS INT NO-UNDO.
  DEF VAR v-qty-received AS DEC NO-UNDO.
  
  IF rm-rctd.rita-code EQ "I" AND TRIM(rm-rctd.tag) NE "" THEN
     FOR EACH rec-rm-rdtlh NO-LOCK
         WHERE rec-rm-rdtlh.company   EQ rm-rctd.company
           AND rec-rm-rdtlh.tag       EQ rm-rctd.tag
           AND rec-rm-rdtlh.rita-code EQ "R"
         USE-INDEX tag,
         FIRST rec-rm-rcpth
         WHERE rec-rm-rcpth.r-no      EQ rec-rm-rdtlh.r-no
           AND rec-rm-rdtlh.rita-code EQ rec-rm-rdtlh.rita-code
         NO-LOCK:
           
       IF rm-rctd.po-no EQ "" THEN rm-rctd.po-no = rec-rm-rcpth.po-no.
    
       IF rm-rctd.job-no EQ "" THEN
         ASSIGN
          rm-rctd.job-no = rec-rm-rcpth.job-no
          rm-rctd.job-no2 = rec-rm-rcpth.job-no2.
    
       LEAVE.
     END.

    IF v-rmtags-log AND TRIM(rm-rctd.tag) NE "" THEN do:
       FOR EACH wiptag WHERE wiptag.company = rm-rctd.company 
                         AND wiptag.rm-tag-no = rm-rctd.tag EXCLUSIVE-LOCK:
          ASSIGN
             wiptag.sts = "On Hand" .
       END.
    END.
  
  {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */

  IF rm-rctd.rita-code eq "R" then
  DO:
    {rm/rmemails.i}
  END.

  DELETE rm-rctd.

  FOR EACH b-tt-rctd WHERE b-tt-rctd.tt-row-id EQ ROWID(tt-rctd):
    v-int = 0.
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT v-int) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


    CREATE rm-rctd.
    BUFFER-COPY b-tt-rctd TO rm-rctd
    ASSIGN
     rm-rctd.r-no        = v-int
     b-tt-rctd.r-no      = rm-rctd.r-no
     b-tt-rctd.has-rec   = YES
     b-tt-rctd.rm-row-id = ROWID(rm-rctd).    

  END.

  DELETE tt-rctd.

END PROCEDURE.

PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:
      
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "RMPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no NE "" then "RM Issue to Job"
                                                 else "RM Receipt"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    end.
  end.

END PROCEDURE.

PROCEDURE send-rmemail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rmemail-file AS CHAR .

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS CHAR NO-UNDO.
  DEF VAR ls-to-cust AS CHAR NO-UNDO.
  DEF VAR lv-mailto AS CHAR NO-UNDO.
  DEF VAR lv-mailsubject AS CHAR NO-UNDO.
  DEF VAR lv-mailbody AS CHAR NO-UNDO.
  DEF VAR lv-mailattach AS CHAR NO-UNDO.
  DEF VAR v-overrun-found AS LOG NO-UNDO.
  DEF VAR v-underrun-found AS LOG NO-UNDO.

  DEF VAR v-first AS LOG INIT YES NO-UNDO.
  DEF VAR v-cust-rec-key AS CHAR NO-UNDO.

  FIND FIRST cust WHERE
       cust.company = g_company AND
       cust.active = "X"
       NO-LOCK NO-ERROR.

  IF AVAIL cust THEN
  DO:
     v-cust-rec-key = cust.rec_key.
     RELEASE cust.
  END.

  FOR EACH tt-email:

      IF tt-email.total-recvd-qty < tt-email.under-qty THEN
         tt-email.undovr = "U".
      ELSE IF tt-email.total-recvd-qty > tt-email.allow-qty THEN
         tt-email.undovr = "O".
      ELSE IF rmemails = "overrun" THEN
         DELETE tt-email.
      ELSE /*rmemails = "receipts"*/
         tt-email.undovr = "R".
  END.

  FOR EACH tt-email, 
      FIRST vend WHERE
            vend.company = g_company AND
            vend.vend-no = tt-email.vend-no AND
            vend.active = "A"
            NO-LOCK
      BREAK BY tt-email.po-no: 

      IF FIRST-OF(tt-email.po-no) THEN
         ASSIGN
            lv-mailbody = ""
            v-overrun-found = NO
            v-underrun-found = NO.

      IF NOT v-overrun-found AND
         tt-email.undovr = "O" THEN
         v-overrun-found = YES.

      IF tt-email.undovr = "R" THEN  
         lv-mailbody = lv-mailbody +  CHR(10) + "Raw Material Receipt From " 
                     + "Vendor# "  + STRING (tt-email.vend-no) + "  "    
                     + "PO# " + STRING (tt-email.po-no) + "  "
                     + "Item# " + tt-email.item-no + "  "
                     + "Item Name - "  + tt-email.item-name + "  "
                     + "The Purchse Order Quantity was " + TRIM(STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99")) + " "
                     + tt-email.cons-uom + ". " 
                     + "We just received " + trim(string(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom  + " out of a total receipt quantity of "
                     + trim(string(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99")) + " " + tt-email.cons-uom
                     + ".".
      
      ELSE IF tt-email.undovr = "U" THEN 
         lv-mailbody = lv-mailbody + CHR(10) + "UNDERRUN WARNING - "  
                     + "Raw Material Receipt From "   
                     + "Vendor# "  + STRING (tt-email.vend-no)  + "  "     
                     + "PO# " + STRING (tt-email.po-no) + "  "
                     + "For Item# "    + tt-email.item-no       + "  "
                     + "With Item Name "  + tt-email.item-name  + "  "
                     + "The Purchse Order Quantity of " + STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99")
                     + " " + tt-email.cons-uom + " has Vendor Underrun % " + string(tt-email.underrun-pct) + ". " 
                     + "We just received " + string(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99")
                     + " " + tt-email.cons-uom + " out of a total receipt quantity of "
                     + trim(string(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom + ".".
       
      ELSE IF tt-email.undovr = "O" THEN 
         lv-mailbody = lv-mailbody + CHR(10) + "OVERRUN WARNING - "   
                     + "Raw Material Receipt From "     
                     + "Vendor# "  + STRING (tt-email.vend-no) + "  "     
                     + "PO# " + STRING (tt-email.po-no) + "  "
                     + "For Item# "    + tt-email.item-no      + "  " 
                     + "With Item Name " + tt-email.item-name  + "  " 
                     + "The Purchase Order Quantity of " + TRIM(STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom 
                     + " has Maximum Vendor Overrun % " + string(tt-email.overrun-pct) + ". "
                     + "We just received " + trim(string(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom + " out of a total receipt quantity of "
                     + trim(string(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99")) + " " + tt-email.cons-uom
                     + ". " 
                     + "Allowed Qty " + trim(string(tt-email.allow-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom + ". "
                     + "QTY IS OVERRUN NOT AUTHORIZED.  "  + " "
                     + "NEGATIVE RECEIPT SHOULD BE ISSUED FOR OVERRUN. ".
      
      IF LAST-OF(tt-email.po-no) THEN do:
          
          {custom/emailList.i &recKey=vend.rec_key &emailList=ls-to-list}
          {custom/emailList.i &recKey=v-cust-rec-key &emailList=ls-to-cust}

          IF ls-to-list + ls-to-cust NE '' THEN DO:

            ASSIGN lv-mailbody = LEFT-TRIM(lv-mailbody)
                   lv-mailto = "To:" + ls-to-list + "," + ls-to-cust
                   lv-mailsubject = "".

            IF v-overrun-found THEN
               lv-mailsubject = "OVERRUN WARNING ".
            IF v-underrun-found THEN
               lv-mailsubject = "UNDERRUN WARNING ".

            lv-mailsubject = lv-mailsubject + "Raw Goods Receipts have been posted".

            RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,"",
                     INT(rmemail-dlg-box),OUTPUT retcode).
          END.
      END. /* last-of(tt-email.vend-no) */
  END.

  EMPTY TEMP-TABLE tt-email.

END PROCEDURE.
