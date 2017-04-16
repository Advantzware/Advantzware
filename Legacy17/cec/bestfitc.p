/* ------------------------------------------------- cec/bestfitc.p 01/98 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-m-code AS CHAR NO-UNDO.
DEF INPUT PARAM ip-qty AS DEC NO-UNDO.
DEF INPUT PARAM ip-uom AS CHAR NO-UNDO.
DEF INPUT PARAM ipxINo LIKE job-mat.rm-i-no NO-UNDO.
DEF INPUT PARAM ipxINo2 LIKE job-mat.rm-i-no NO-UNDO.


{sys/inc/var.i shared}

def var call_id as recid no-undo.
def var fil_id as recid no-undo.
def var row_id as rowid no-undo.
def var save_id as recid no-undo.
def var k_frac as dec init 6.25 no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def var v-net-sh    as   DEC NO-UNDO.
def var v-onh-sh    as   DEC NO-UNDO.
def var v-avl-sh    as   DEC NO-UNDO.
def var v-tot-msf   as   DEC NO-UNDO.
def var v-wst-msf   as   DEC NO-UNDO.
def var v-nwid      like xef.gsh-wid no-undo.
def var v-nlen      like xef.gsh-len no-undo.
def var v-ndep      like xef.gsh-dep no-undo.
def var v-gwid      like xef.gsh-wid no-undo.
def var v-glen      like xef.gsh-len no-undo.
def var v-gdep      like xef.gsh-dep no-undo.
def var v-n-out     as   int no-undo.
def var v-n-outl    as   int no-undo.
def var v-n-outd    as   int no-undo.
def var v-up        as   INT NO-UNDO.
def var v-use-code  as   LOG NO-UNDO.
def var v-code      as   char no-undo.
def var v-show-all  like sys-ctrl.char-fld no-undo.
def var v-i-no      like xef.board NO-UNDO.
def var v-dim       as   dec extent 4 no-undo.
def var v-nsh       as   dec extent 4 no-undo.
def var v-gsh       as   dec extent 4 no-undo.
def var v           as   int no-undo.
def var v-max-yld   as   log INIT YES no-undo.
DEF VAR useiCodeOriName AS cha NO-UNDO.

{cec/msfcalc.i}  /* for v-corr */

{cec/bestfitc.i SHARED}

{sys/inc/f16to32.i}

do transaction:
  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "SHT CALC"
      no-error.
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "SHT CALC".
  end.

  sys-ctrl.descrip = "Prompt to select by 1st 10 characters" +
                     " of item name on Pg2 ShtCalc?".
  if new sys-ctrl then
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.

  find current sys-ctrl no-lock no-error.
end.
assign
 v-show-all = sys-ctrl.char-fld
 v-use-code = sys-ctrl.log-fld
 v-code     = "".

FOR EACH tt-ef:
  DELETE tt-ef.
END.
FOR EACH tt-eb:
  DELETE tt-eb.
END.
FOR EACH tt-report:
  DELETE tt-report.
END.

IF ip-m-code EQ "" THEN ip-m-code = xef.m-code.

IF ipxINo EQ "" THEN 
    RUN cec/d-bestft.w (v-use-code,
                    OUTPUT v-code,
                    OUTPUT v-max-yld,
                    INPUT-OUTPUT ip-m-code,
                    OUTPUT useiCodeOriName).
ELSE 
    ASSIGN 
        ip-m-code = ""
        v-code = ipxINo
        useicodeoriName = "C".

IF v-max-yld EQ ? THEN RETURN.

CREATE tt-ef.
BUFFER-COPY xef EXCEPT rec_key TO tt-ef.
CREATE tt-eb.
BUFFER-COPY xeb EXCEPT rec_key TO tt-eb.

IF tt-ef.xgrain EQ "S" THEN
  ASSIGN
   v-gwid        = tt-ef.nsh-wid
   tt-ef.nsh-wid = tt-ef.nsh-len
   tt-ef.nsh-len = v-gwid.

run sys/inc/numup.p (tt-ef.company, tt-ef.est-no, tt-ef.form-no, output v-up).

for each mach
    where mach.company   eq cocode
      and ((mach.m-code  eq ip-m-code and ip-m-code ne "") or
           (mach.dept[1] eq "RC"      and ip-m-code eq ""))
    no-lock
    
    by mach.d-seq
    by mach.m-seq:

  assign
   tt-ef.m-code   = caps(mach.m-code)
   tt-ef.m-dscr   = mach.m-dscr
   tt-ef.roll     = (mach.p-type eq "R")
   tt-ef.lam-dscr = "S"
   tt-ef.lsh-len  = mach.max-wid
   tt-ef.lsh-wid  = mach.max-len.
    
  find first style
      where style.company eq cocode
        and style.style   eq tt-eb.style
      no-lock no-error.

  for each item
      {sys/look/itemW.i}
        and ((item.mat-type eq "B"             and style.type ne "F") or
             (index("1234",item.mat-type) gt 0 and style.type eq "F"))
        and item.i-code     eq "R"        
        and ((item.indus eq "1"  and
              xest.est-type ge 1 and xest.est-type le 4) or
             (item.indus eq "2"  and
              xest.est-type ge 5 and xest.est-type le 8))     
        and ((style.type  eq "F"          and      
              (v-max-yld or
               (item.r-wid eq 0             and
                item.s-wid ge tt-ef.nsh-wid and
                item.s-len ge tt-ef.nsh-len and
                item.s-dep ge tt-ef.nsh-dep) or
               (item.r-wid ne 0             and
                item.r-wid ge tt-ef.nsh-wid and
                item.s-dep ge tt-ef.nsh-dep)))    or
             (item.r-wid  eq 0            and
              item.s-wid  ge tt-ef.nsh-wid  and
              item.s-wid  ge mach.min-len and
              item.s-wid  le mach.max-len and
              item.s-len  ge tt-ef.nsh-len  and
              item.s-len  ge mach.min-wid and
              item.s-len  le mach.max-wid)      or
             (item.r-wid  ne 0            and
              item.r-wid  ge tt-ef.nsh-wid  and
              item.r-wid  ge mach.min-len and
              item.r-wid  le mach.max-len))
        and ((not v-use-code)                   or
             ((useiCodeoriName = "N" AND item.i-name   begins v-code) OR
              (useiCodeOriName = "C" AND ITEM.i-no BEGINS v-code)
              )
             )
      use-index mat-type no-lock:

    if style.type eq "F" and v-max-yld then do:
       repeat:
           assign tt-ef.board   = item.i-no
                  tt-eb.wid     = tt-ef.nsh-wid
                  tt-eb.len     = tt-ef.nsh-len
                  tt-eb.dep     = tt-ef.nsh-dep.
       
           run cec/foam.p.
      
           FIND CURRENT tt-ef.
           FIND CURRENT tt-eb.
       
           assign v-dim[1] = tt-ef.gsh-wid
                  v-dim[2] = tt-ef.gsh-len
                  v-dim[3] = tt-ef.gsh-dep.

           undo, leave.
       end.
      
       assign
        v-gsh[1] = if item.r-wid gt 0 then item.r-wid else item.s-wid
        v-gsh[2] = item.s-len
        v-gsh[3] = item.s-dep.
      
       IF v-cecscrn-char NE "Decimal" THEN
       DO i = 1 TO 3:
          ASSIGN
             v-dim[i] = ROUND(v-dim[i] * li-16-32,0) / li-16-32
             v-gsh[i] = ROUND(v-gsh[i] * li-16-32,0) / li-16-32.
       END.
      
       dims:
       do i = 1 to 3.
          if v-dim[1] eq v-gsh[1] then do:
             if i eq 3 then do:
               v-gdep = v-gsh[1].
            
               if v-dim[2] eq v-gsh[2] then
                 assign
                  v-gwid = v-gsh[2]
                  v-glen = v-gsh[3].
              
               else
                 assign
                  v-gwid = v-gsh[3]
                  v-glen = v-gsh[2].
             end.
            
             else
             if i eq 2 then do:
               v-glen = v-gsh[1].
            
               if v-dim[2] eq v-gsh[2] then
                 assign
                  v-gdep = v-gsh[2]
                  v-gwid = v-gsh[3].
            
               else
                 assign
                  v-gdep = v-gsh[3]
                  v-gwid = v-gsh[2].
             end.
            
             else do:
               v-gwid = v-gsh[1].
            
               if v-dim[2] eq v-gsh[2] then
                 assign
                  v-glen = v-gsh[2]
                  v-gdep = v-gsh[3].
              
               else
                 assign
                  v-glen = v-gsh[3]
                  v-gdep = v-gsh[2].
             end.
            
             leave dims.
          end.
         
          assign
           v-dim[4] = v-dim[1]
           v-dim[1] = v-dim[2]
           v-dim[2] = v-dim[3]
           v-dim[3] = v-dim[4]
           v-nsh[4] = v-nsh[1]
           v-nsh[1] = v-nsh[2]
           v-nsh[2] = v-nsh[3]
           v-nsh[3] = v-nsh[4].
       end.
    end.

    else
      assign
       v-gwid   = if item.r-wid gt 0 then item.r-wid    else item.s-wid
       v-glen   = if item.r-wid gt 0 then tt-ef.nsh-len else item.s-len
       v-gdep   = if item.s-dep gt 0 then item.s-dep    else 1.

    if tt-ef.nsh-dep gt 0 then
      assign
       v-n-outd = trunc(v-gdep / tt-ef.nsh-dep,0)
       v-ndep   = tt-ef.nsh-dep * v-n-outd.
    else
      assign
       v-n-outd = 1
       v-ndep   = 1.
     
    assign v-n-out  = trunc(v-gwid / tt-ef.nsh-wid,0)
           v-nwid   = tt-ef.nsh-wid * v-n-out
           v-n-outl = trunc(v-glen / tt-ef.nsh-len,0)
           v-nlen   = tt-ef.nsh-len * v-n-outl
           v-net-sh = IF ip-qty EQ 0 THEN xest.est-qty[1] ELSE ip-qty.

    IF ip-uom EQ "" THEN ip-uom = "EA".

    IF ip-uom NE "EA" THEN
       RUN sys/ref/convquom.p (ip-uom, "EA",
                               item.basis-w, v-glen, v-gwid, 0,
                               v-net-sh, OUTPUT v-net-sh).

    v-net-sh = v-net-sh / (v-up * v-n-out * v-n-outl * v-n-outd).

    {sys/inc/roundup.i v-net-sh}

    ASSIGN
     v-onh-sh = item.q-onh
     v-avl-sh = item.q-avail.

    IF item.cons-uom NE "EA" THEN DO:
       RUN sys/ref/convquom.p (item.cons-uom, "EA",
                               item.basis-w, v-glen, v-gwid, 0,
                               v-onh-sh,
                               OUTPUT v-onh-sh).

       RUN sys/ref/convquom.p (item.cons-uom, "EA",
                               item.basis-w, v-glen, v-gwid, 0,
                               v-avl-sh,
                               OUTPUT v-avl-sh).
    END.

    IF v-n-out NE 0 AND v-n-outl NE 0 AND v-n-outd NE 0 THEN DO:
      assign
       v-tot-msf = (if v-corr then (v-gwid * v-glen * .007)
                              else (v-gwid * v-glen / 144)) * v-net-sh / 1000
       v-wst-msf = (if v-corr then (v-nwid * v-nlen * .007)
                              else (v-nwid * v-nlen / 144)) * v-net-sh / 1000
       v-wst-msf = v-tot-msf - v-wst-msf.
       
      if v-wst-msf lt 0 then v-wst-msf = 0.

      IF not(style.type eq "F" and v-max-yld) THEN
         v-gdep = item.s-dep.

      create tt-report.
      assign
       tt-report.term-id = ""
       tt-report.key-01  = string(v-wst-msf,"99999999.99999999")
       tt-report.key-02  = item.i-no
       tt-report.key-03  = string(v-net-sh,"99999999")
       tt-report.key-04  = string(v-onh-sh,"-9999999")
       tt-report.key-05  = string(v-n-out,"99999999")
       tt-report.key-06  = string(v-n-outl,"99999999")
       tt-report.key-07  = string(v-n-outd,"99999999")
       tt-report.key-08  = string(v-gwid) /*,"99999999.99999999") */
       tt-report.key-09  = string(v-glen) /*,"99999999.99999999") */
       tt-report.key-10  = string(v-gdep) /*,"99999999.99999999") */
       tt-report.rec-id  = recid(item)
       tt-report.tt-reqs = v-net-sh
       tt-report.tt-onhs = v-onh-sh
       tt-report.tt-onhl = item.q-onh
       tt-report.tt-avls = v-avl-sh
       tt-report.tt-avll = item.q-avail.

      IF item.cons-uom NE "LF" THEN DO:
        RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                item.basis-w, v-glen, v-gwid, 0,
                                tt-report.tt-onhl,
                                OUTPUT tt-report.tt-onhl).

        RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                item.basis-w, v-glen, v-gwid, 0,
                                tt-report.tt-avll,
                                OUTPUT tt-report.tt-avll).
      END.
    END.
  END.

  find first tt-report where tt-report.term-id eq "" no-lock no-error.

  IF AVAIL tt-report THEN DO:
    save_id = fil_id.
    IF ipxINo EQ "" THEN
        RUN cec/bestfit.p (ipxINo2,
                           xest.est-type, v-show-all).
    ELSE DO:
        FIND FIRST tt-report
          WHERE tt-report.key-02 EQ ipxINo NO-ERROR.
        IF AVAIL tt-report THEN tt-report.tt-sel = YES.
        
    END.


    DELETE tt-ef.

    FOR EACH tt-report WHERE tt-sel,
        FIRST item WHERE RECID(item) EQ tt-report.rec-id NO-LOCK
        TRANSACTION:

      CREATE tt-ef.
      BUFFER-COPY xef EXCEPT rec_key TO tt-ef.

      {cec/board.i tt-ef tt-eb}
      ASSIGN        
       tt-ef.n-out   = INT(tt-report.key-05)
       tt-ef.n-out-l = INT(tt-report.key-06)
       tt-ef.n-out-d = INT(tt-report.key-07)
         
       tt-ef.gsh-wid = DEC(tt-report.key-08)
       tt-ef.gsh-len = DEC(tt-report.key-09)
       tt-ef.gsh-dep = DEC(tt-report.key-10)
         
       tt-ef.n-cuts  = (tt-ef.n-out - 1) + (tt-ef.n-out-l - 1) + (tt-ef.n-out-d - 1).
     
      IF tt-ef.n-cuts LT 0 THEN tt-ef.n-cuts = 0.
    END.

    fil_id = save_id.
  END.
  else do :
      IF ipxINo EQ "" THEN
        message "No board found with required dimensions. " view-as alert-box error.
  end.             
  leave.
end.

IF NOT CAN-FIND(FIRST tt-report WHERE tt-sel) THEN DELETE tt-eb.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
