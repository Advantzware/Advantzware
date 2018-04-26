/* --------------------------------------------- cec/rep/jobtick5.p           */
/* factory ticket                                                             */
/* -------------------------------------------------------------------------- */

def input parameter v-recid  as recid.
def input parameter v-format as char.
def input parameter v-loc-loop as  int no-undo.
def input parameter v-loc-copies as int no-undo.
DEF OUTPUT PARAMETER v-pack-date AS CHAR FORMAT "X(8)" NO-UNDO.

DEF BUFFER xjob-hdr FOR job-hdr.
DEF BUFFER b-job FOR job.

DEF VAR li AS INT NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-rowid AS ROWID.
DEF VAR v-bold AS CHAR NO-UNDO.
DEF VAR v-bold2 AS CHAR NO-UNDO.

DEF SHARED VAR qty AS INT NO-UNDO.
DEF SHARED VAR v-shared-rel AS INT NO-UNDO.

{sys/inc/VAR.i SHARED}

{jcrep/r-ticket.i "shared"}
{cecrep/jobtick.i "shared"}

{ce/print4.i "new shared"}
{ce/print42.i "new shared"}

DEF BUFFER b-w-i FOR w-i.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "JOB QTY"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name = "JOB QTY"
   sys-ctrl.descrip = "Create Job Quantity with overrun % from OE?"
   sys-ctrl.log-fld = no.
  message "Sys-ctrl record NOT found. " sys-ctrl.descrip
          update sys-ctrl.char-fld.
end.
v-net-shts = sys-ctrl.char-fld eq "Net Shts".
/*
help-id = program-name(1).
*/
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

find job-hdr where recid(job-hdr) eq v-recid NO-LOCK.

find first job
    where job.company eq cocode
      and job.job     eq job-hdr.job
      and job.job-no  eq job-hdr.job-no
      and job.job-no2 eq job-hdr.job-no2
    no-lock.

for each w-ef:
  delete w-ef.
end.

find first cust
    where cust.company eq cocode
      and cust.cust-no eq job-hdr.cust-no
    no-lock.

find first xest where xest.compan = job-hdr.company
                  AND xest.est-no eq job-hdr.est-no no-lock no-error.

if avail xest then do:
  find first xef
      where xef.company = xest.company 
        AND xef.est-no   eq xest.est-no
      no-lock no-error.
      
  find first xeb
      where xeb.company = xest.company 
        AND xeb.est-no   eq xest.est-no
      no-lock no-error.
end.

find first xoe-ordl
    where xoe-ordl.company eq job-hdr.company
      and xoe-ordl.ord-no  eq job-hdr.ord-no
      and xoe-ordl.job-no  eq job-hdr.job-no
      and xoe-ordl.job-no2 eq job-hdr.job-no2
      and xoe-ordl.i-no    eq job-hdr.i-no
    no-error.

IF job-hdr.ord-no NE 0 AND NOT AVAIL xoe-ordl THEN
find first xoe-ordl
    where xoe-ordl.company eq cocode
      and xoe-ordl.ord-no  eq job-hdr.ord-no
      and xoe-ordl.i-no    eq job-hdr.i-no
    no-error.

IF job-hdr.ord-no NE 0 AND NOT AVAIL xoe-ordl THEN
find first xoe-ordl
    where xoe-ordl.company eq cocode
      and xoe-ordl.ord-no  eq job-hdr.ord-no
    no-error.
  
if avail xoe-ordl then do:
  find first xoe-ord
      where xoe-ord.company eq cocode
        and xoe-ord.ord-no  eq xoe-ordl.ord-no
      no-lock no-error.

  if avail xoe-ord then do:
    if (not oe-ctrl.p-fact) and xoe-ord.stat eq "h" then return.
    
    assign
     i     = 0
     v-shp = "".
    if xoe-ord.sold-name ne "" then
      assign
       i        = i + 1
       v-shp[i] = xoe-ord.sold-name.
    if xoe-ord.sold-addr[1] ne "" then
      assign
       i        = i + 1
       v-shp[i] = xoe-ord.sold-addr[1].
    if xoe-ord.sold-addr[2] ne "" then
      assign
       i        = i + 1
       v-shp[i] = xoe-ord.sold-addr[2].
    assign
     i        = i + 1
     v-shp[i] = trim(xoe-ord.sold-city) + ", " +
                xoe-ord.sold-state + "  " + xoe-ord.sold-zip.
  end.
end.

assign
 i      = 0
 v-ship = ""
 v-cus  = "".

if cust.name ne "" then
  assign
   i        = i + 1
   v-cus[i] = cust.name.
   
if cust.addr[1] ne "" then
  assign
   i        = i + 1
   v-cus[i] = cust.addr[1].
   
if cust.addr[2] ne "" then
  assign
   i        = i + 1
   v-cus[i] = cust.addr[2].
   
assign
 i        = i + 1
 v-cus[i] = trim(cust.city) + ", " + cust.state + "  " + cust.zip.

if avail xoe-ordl then do:
  find first xoe-ord
      where xoe-ord.company eq cocode
        and xoe-ord.ord-no  eq xoe-ordl.ord-no
      no-lock.

  assign
   xoe-ordl.ftick-prnt = yes
   i                  = 0
   v-cus              = "".
   
  if xoe-ord.cust-name ne "" then
    assign
     i        = i + 1
     v-cus[i] = xoe-ord.cust-name.
     
  if xoe-ord.addr[1] ne "" then
    assign
     i        = i + 1
     v-cus[i] = xoe-ord.addr[1].
     
  if xoe-ord.addr[2] ne "" then
    assign
     i        = i + 1
     v-cus[i] = xoe-ord.addr[2].
     
  assign
   i        = i + 1
   v-cus[i] = trim(xoe-ord.city) + ", " +
              xoe-ord.state + "  " + xoe-ord.zip.

  IF CAN-DO("Triad,Brick,Corrugat,RFC,ASI,Xprint,Pacific,Hughes,P&P,MWBox,ARTIOS,Suthrlnd,TriLakes,TriLakes2,CapCity",v-format) THEN
  FIND FIRST xoe-rel
      WHERE xoe-rel.company EQ xoe-ordl.company
        AND xoe-rel.ord-no  EQ xoe-ordl.ord-no
        AND xoe-rel.i-no    EQ xoe-ordl.i-no
        AND xoe-rel.line    EQ xoe-ordl.line
      NO-LOCK NO-ERROR.
  ELSE IF v-format EQ "Spectrum" THEN
  DO:
     FOR EACH xoe-rel
         WHERE xoe-rel.company EQ xoe-ordl.company
           AND xoe-rel.ord-no  EQ xoe-ordl.ord-no
           AND xoe-rel.i-no    EQ xoe-ordl.i-no
           AND xoe-rel.line    EQ xoe-ordl.LINE
           NO-LOCK BY xoe-rel.rel-date:
           /*if stat is a or b, xoe-rel.rel-date does not reflect actual
             date shown in order release tab*/

           RUN oe/rel-stat.p (ROWID(xoe-rel), OUTPUT lv-stat).
           IF LOOKUP(lv-stat,"C,Z") = 0 THEN
           DO: 
              lv-rowid = ROWID(xoe-rel).
              LEAVE.
           END.
     END. /*for each xoe-rel*/

     IF lv-rowid NE ? THEN
        FIND FIRST xoe-rel WHERE ROWID(xoe-rel) EQ lv-rowid NO-LOCK NO-ERROR.
  END.

  if avail xoe-rel then do:
    find first shipto
        where shipto.company eq cocode
          and shipto.cust-no eq xoe-rel.cust-no
          and shipto.ship-id eq xoe-rel.ship-id
        no-lock no-error.
    assign
     i     = 0
     v-shp = "".
       
    if avail shipto and shipto.ship-name ne "" then
      assign
       i        = i + 1
       v-shp[i] = shipto.ship-name.
         
    if xoe-rel.ship-addr[1] ne "" then
      assign
       i        = i + 1
       v-shp[i] = xoe-rel.ship-addr[1].
         
    if xoe-rel.ship-addr[2] ne "" then
      assign
       i        = i + 1
       v-shp[i] = xoe-rel.ship-addr[2].
         
    assign
     i        = i + 1
     v-shp[i] = trim(xoe-rel.ship-city) + ", " +
                xoe-rel.ship-state + "  " + xoe-rel.ship-zip.

    do i = 1 to 4:
      if xoe-rel.ship-i[i] ne "" then do:
        if v-ship eq "" then v-ship = "Shipping:".
        v-ship = trim(v-ship) + " " + xoe-rel.ship-i[i].
      end.
    end.
  end.
end.

IF production AND
   job.cs-trans-date NE ? THEN DO:
  li = 0.
  DO WHILE li LT 1000:
    li = li + 1.
    FIND b-job EXCLUSIVE-LOCK
        WHERE ROWID(b-job) EQ ROWID(job)
        NO-ERROR NO-WAIT.
    IF AVAIL b-job THEN
      ASSIGN
       b-job.pr-printed    = YES
       b-job.pr-user-id-p  = USERID("nosweat")
       b-job.pr-print-date = TODAY
       b-job.pr-print-time = TIME
       li                  = 1000.
  END.
END.

ELSE DO:
  li = 0.
  IF NOT job-hdr.ftick-prnt THEN DO WHILE li LT 1000:
    li = li + 1.
    FIND xjob-hdr EXCLUSIVE-LOCK
        WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
        NO-ERROR NO-WAIT.
    IF AVAIL xjob-hdr THEN
      ASSIGN
       xjob-hdr.ftick-prnt = YES
       li                  = 1000.
  END.

  li = 0.
  DO WHILE li LT 1000:
    li = li + 1.
    FIND b-job EXCLUSIVE-LOCK
        WHERE ROWID(b-job) EQ ROWID(job)
        NO-ERROR NO-WAIT.
    IF AVAIL b-job THEN DO:
      li = 1000.

      IF NOT b-job.cs-printed THEN
        ASSIGN
         b-job.cs-printed    = YES
         b-job.cs-user-id-p  = USERID("nosweat")
         b-job.cs-print-date = TODAY
         b-job.cs-print-time = TIME.

      IF approve THEN
        ASSIGN
         b-job.cs-to-pr      = YES
         b-job.cs-user-id-t  = USERID("nosweat")
         b-job.cs-trans-date = TODAY
         b-job.cs-trans-time = TIME.
    END.
  END.
END.

if avail xest then do:
  for each xef
      where xef.company = xest.company AND xef.est-no eq xest.est-no
      no-lock.
      
    create w-ef.
    w-ef.frm = xef.form-no.
  end.

  find first xeb where xeb.company = xest.company AND xeb.est-no eq xest.est-no no-lock no-error.
  if avail xeb and not avail xoe-rel then do:
    assign
     i     = 0
     v-shp = "".
     
    if xeb.ship-name ne "" then
      assign
       i        = i + 1
       v-shp[i] = xeb.ship-name.
         
    if xeb.ship-addr[1] ne "" then
      assign
       i        = i + 1
       v-shp[i] = xeb.ship-addr[1].
         
    if xeb.ship-addr[2] ne "" then
      assign
       i        = i + 1
       v-shp[i] = xeb.ship-addr[2].
         
    assign
     i        = i + 1
     v-shp[i] = trim(xeb.ship-city) + ", " +
                xeb.ship-state + "  " + xeb.ship-zip.

    if v-format eq "Triad" or v-format eq "Brick" OR v-format = "ASI" 
       AND v-format = "xprint" 
    then do:
      find first shipto
          where shipto.company eq cocode
            and shipto.cust-no eq job-hdr.cust-no
            and shipto.ship-id eq xeb.ship-id
          no-lock no-error.
      if not avail shipto then
      find first shipto
          where shipto.company eq cocode
            and shipto.cust-no eq job-hdr.cust-no
            and shipto.ship-id eq job-hdr.cust-no
          no-lock no-error.
      if avail shipto then do:
        do i = 1 to 4:
          if shipto.notes[i] ne "" then do:
            if v-ship eq "" then v-ship = "Shipping:".
            v-ship = trim(v-ship) + " " + shipto.notes[i].
          end.
        end.
      end.
    end.
  end.
end.

else do:
  create w-ef.
  w-ef.frm = job-hdr.frm.
end.
if v-break then do:
  find first job-mch
      where job-mch.company eq cocode
        and job-mch.job     eq job-hdr.job
        and job-mch.job-no  eq job-hdr.job-no
        and job-mch.job-no2 eq job-hdr.job-no2
      no-lock no-error.

  assign
   v-standards = (avail job-mch)
   v-job-prt   = trim(job.job-no) + "-" + string(job.job-no2,"99")
   v-ord-date  = if avail xoe-ord then
                   string(xoe-ord.ord-date,"99/99/99")
                 else
                 if v-format eq "Triad" then
                   string(job.start-date,"99/99/99")
                 else ""
   v-est-no    = trim(job-hdr.est-no)
   v-fg        = trim(job-hdr.i-no)
   v-pack-date = IF AVAIL xoe-ord AND xoe-ord.last-date NE ? THEN
                    STRING(xoe-ord.last-date,"99/99/99")
                 ELSE ""
   v-due-date  = if avail xoe-ordl and
                    CAN-DO("Triad,Brick,ASI,Xprint,RFC,TriLakes",v-format) then
                   xoe-ordl.req-code + " " +
                   string(xoe-ordl.req-date,"99/99/99") /*prom-date*/
                 ELSE IF AVAIL xoe-ordl  THEN string(xoe-ordl.req-date,"99/99/99") 
                 ELSE if avail xoe-ord THEN string(xoe-ord.due-date,"99/99/99")
                 else IF job.due-date = ? THEN string(job.start-date,"99/99/99")
                 ELSE string(job.due-date,"99/99/99").

  IF v-format EQ "CapCity" THEN
  DO:
     IF AVAIL xoe-ordl AND TRIM(xoe-ordl.req-code) NE "ON" THEN
        ASSIGN
          v-bold = "<B>"
          v-bold2 = "</B>".

     v-due-date = v-bold
                + (IF AVAIL xoe-ordl THEN
                   TRIM(xoe-ordl.req-code) ELSE "ON")
                + v-bold2
                + " " + v-due-date.
  END.
  ELSE IF v-format EQ "Artios" THEN
     v-due-date = (IF AVAIL xoe-ordl THEN
                   TRIM(xoe-ordl.req-code) ELSE "ON") + " " + v-due-date.

  if (not v-standards) and avail xest then do:
    qty = job-hdr.qty.
    
    find first xef no-lock
      where xef.company = xest.company 
        AND xef.est-no   eq xest.est-no
       no-error.
    
    if xest.est-type eq 5 then
      run cec/print4.p.
    else
    if xest.est-type eq 5 then
      run cec/box/print42.p.
    else
      run cec/com/print4.p.
  end.
end.

v-ord-no = trim(string(job-hdr.ord-no)).

li = 0.

for each ink:
  li = li + 1.
      
  create w-i.
  assign
   w-i.i-code = ink.i-code
   w-i.i-dscr = ink.i-dscr
   w-i.i-qty  = ink.i-qty.

  IF li GT 4 AND v-format EQ "TriLakes" THEN DO:
    FOR EACH b-w-i
        WHERE b-w-i.i-code2 EQ ""
          AND ROWID(b-w-i)  NE ROWID(w-i):
      LEAVE.
    END.
    IF AVAIL b-w-i THEN DO:
      ASSIGN
       b-w-i.i-code2 = w-i.i-code
       b-w-i.i-dscr2 = w-i.i-dscr
       b-w-i.i-qty2  = w-i.i-qty.
      DELETE w-i.
    END.
  END.
end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
