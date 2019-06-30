




/*------------------------------------------------------------------------
    File        : PoprhstryLook.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPoPriceHistoryLook NO-UNDO 
    FIELD pono         AS INTEGER
    FIELD ino          AS CHAR
    FIELD iname        AS CHAR
    FIELD swid         AS DEC
    FIELD slen         AS DEC
    FIELD ordqty       AS DEC
    FIELD trecqty      AS DEC
    FIELD tinvqty      AS DEC
    FIELD podate       AS DATE
    FIELD potype       AS CHARACTER
    FIELD stat         AS CHAR
    FIELD job          AS CHARACTER
    FIELD job2         AS INT
    FIELD unit-pr      AS DEC
    FIELD prqtyuom     AS CHAR
    FIELD consuom      AS CHAR
    FIELD tax          AS CHAR
    FIELD actno        AS CHAR
    FIELD actname      AS CHAR
    FIELD invqty       AS DEC
    FIELD amt          AS INT
    FIELD srft         AS DEC
    FIELD amtmsf       AS DEC
    FIELD ext          AS CHAR .
    
                                           
    
DEFINE DATASET dsPoPriceHistoryLook FOR ttPoPriceHistoryLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPono      AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmext       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPoPriceHistoryLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.
def var v-term as cha no-undo.
DEF VAR lv-first-recid AS RECID NO-UNDO.
DEF VAR lv-ordl-recid AS RECID NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF VAR lv-pol-rowid AS ROWID NO-UNDO.
DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR lv-po-glnum AS LOG NO-UNDO.
DEF VAR v-vend-actnum AS cha NO-UNDO.
DEF VAR v-ap-pur AS CHAR NO-UNDO.
DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmPono      = ? THEN ASSIGN prmPono      = 0.
IF prmext       = ? THEN ASSIGN prmext       = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN cocode = prmComp.

{sys/inc/ap-gl#.i}
lv-po-glnum = ap-gl#-log.

DEF TEMP-TABLE tt-report NO-UNDO
    FIELD key-01 AS CHAR
    FIELD term-id AS CHAR
    FIELD rec-id AS RECID
    FIELD key-05 AS CHAR
    INDEX term-id term-id key-01
    INDEX key-01 key-01.

run reset-tt-report.



if prmAction <> "search" then do:
     FOR EACH tt-report WHERE tt-report.term-id eq v-term NO-LOCK,
         first po-ordl WHERE RECID(po-ordl) = tt-report.rec-id NO-LOCK,
         FIRST po-ord WHERE po-ord.company = po-ordl.company AND 
             po-ord.po-no = po-ordl.po-no NO-LOCK.

       
       create ttPoPriceHistoryLook.
                 assign                                     
                    ttPoPriceHistoryLook.pono      = po-ord.po-no
                    ttPoPriceHistoryLook.ino       = po-ordl.i-no
                    ttPoPriceHistoryLook.iname     = po-ordl.i-name
                    ttPoPriceHistoryLook.swid      = po-ordl.s-wid
                    ttPoPriceHistoryLook.slen      = po-ordl.s-len 
                    ttPoPriceHistoryLook.ordqty    = po-ordl.ord-qty
                    ttPoPriceHistoryLook.trecqty   = po-ordl.t-rec-qty
                    ttPoPriceHistoryLook.tinvqty   = po-ordl.t-inv-qty
                    ttPoPriceHistoryLook.podate    = po-ord.po-date
                    ttPoPriceHistoryLook.potype    = po-ord.TYPE
                    ttPoPriceHistoryLook.stat      = po-ord.stat
                    ttPoPriceHistoryLook.job       = po-ordl.job-no
                    ttPoPriceHistoryLook.job2      = po-ordl.job-no2 .
             
               RUN display-po.


     END.  /*FOR EACH tt-report*/
        
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        if prmField = "pono"  then do:
         if prmCondition = "EQUAL" then do:
          FOR EACH tt-report WHERE tt-report.term-id eq v-term NO-LOCK,
         first po-ordl WHERE RECID(po-ordl) = tt-report.rec-id 
                         AND (po-ordl.po-no = INT(prmtext) OR prmtext = "" OR prmtext = "0") NO-LOCK,
         FIRST po-ord WHERE po-ord.company = po-ordl.company AND 
             po-ord.po-no = po-ordl.po-no NO-LOCK.

       
       create ttPoPriceHistoryLook.
                 assign                                     
                    ttPoPriceHistoryLook.pono      = po-ord.po-no
                    ttPoPriceHistoryLook.ino       = po-ordl.i-no
                    ttPoPriceHistoryLook.iname     = po-ordl.i-name
                    ttPoPriceHistoryLook.swid      = po-ordl.s-wid
                    ttPoPriceHistoryLook.slen      = po-ordl.s-len 
                    ttPoPriceHistoryLook.ordqty    = po-ordl.ord-qty
                    ttPoPriceHistoryLook.trecqty   = po-ordl.t-rec-qty
                    ttPoPriceHistoryLook.tinvqty   = po-ordl.t-inv-qty
                    ttPoPriceHistoryLook.podate    = po-ord.po-date
                    ttPoPriceHistoryLook.potype    = po-ord.TYPE
                    ttPoPriceHistoryLook.stat      = po-ord.stat
                    ttPoPriceHistoryLook.job       = po-ordl.job-no
                    ttPoPriceHistoryLook.job2      = po-ordl.job-no2 .
             
               RUN display-po.


            END.  /*FOR EACH tt-report*/
         END.
          IF prmCondition = "BEGIN" then do:
              FOR EACH tt-report WHERE tt-report.term-id eq v-term NO-LOCK,
                  first po-ordl WHERE RECID(po-ordl) = tt-report.rec-id 
                                  AND (po-ordl.po-no = INT(prmtext) OR prmtext = "" OR prmtext = "0") NO-LOCK,
                  FIRST po-ord WHERE po-ord.company = po-ordl.company AND 
                  po-ord.po-no = po-ordl.po-no NO-LOCK .

       
           create ttPoPriceHistoryLook.
                 assign                                     
                    ttPoPriceHistoryLook.pono      = po-ord.po-no
                    ttPoPriceHistoryLook.ino       = po-ordl.i-no
                    ttPoPriceHistoryLook.iname     = po-ordl.i-name
                    ttPoPriceHistoryLook.swid      = po-ordl.s-wid
                    ttPoPriceHistoryLook.slen      = po-ordl.s-len 
                    ttPoPriceHistoryLook.ordqty    = po-ordl.ord-qty
                    ttPoPriceHistoryLook.trecqty   = po-ordl.t-rec-qty
                    ttPoPriceHistoryLook.tinvqty   = po-ordl.t-inv-qty
                    ttPoPriceHistoryLook.podate    = po-ord.po-date
                    ttPoPriceHistoryLook.potype    = po-ord.TYPE
                    ttPoPriceHistoryLook.stat      = po-ord.stat
                    ttPoPriceHistoryLook.job       = po-ordl.job-no
                    ttPoPriceHistoryLook.job2      = po-ordl.job-no2 .
             
               RUN display-po .


              END.  /*FOR EACH tt-report*/    
         end.  /* if prmField = state  */
        END.
END.  /* IF prmAction = search then do: */



PROCEDURE reset-tt-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def var i as int no-undo.
 def var a as INT NO-UNDO.

/*{sa/sa-sls01.i}           */

   FOR EACH tt-report WHERE tt-report.TERM-id = v-term :
       DELETE tt-report.
   END.

    DEF VAR v-po LIKE po-ord.po-no NO-UNDO.
    DEF VAR v-qty AS DEC NO-UNDO.
    
    FIND FIRST vend
        WHERE vend.company = cocode
        AND vend.vend-no = prmext
        NO-LOCK NO-ERROR.
   
    

    v-po = prmPono.

    if avail vend then 
    for each po-ord NO-LOCK
        where (po-ord.company eq prmComp
               and  po-ord.vend-no eq vend.vend-no)
          AND po-ord.opened EQ YES
          USE-INDEX opened,
        each po-ordl WHERE
             po-ordl.company EQ po-ord.company AND
             po-ordl.po-no   EQ po-ord.po-no
             no-lock:

      RUN ap/valid-po.p (BUFFER po-ordl, BUFFER ap-invl).
      IF NOT AVAIL po-ordl THEN NEXT.

      RUN po/rec-inv.p (ROWID(po-ordl), OUTPUT v-qty).

      if v-qty NE 0 then do:
        create tt-report.
        assign
         tt-report.term-id = v-term
         tt-report.key-01  = string(po-ord.po-no,">>>>>>")
         tt-report.rec-id  = recid(po-ordl)
         tt-report.key-05 = STRING(RECID(po-ordl))
         lv-first-recid = IF lv-first-recid = ? THEN RECID(tt-report) ELSE lv-first-recid
           .
          
        if po-ord.po-no ge v-po and v-po ne 0 then
          assign
           lv-ordl-recid = recid(tt-report)
           v-po   = 0.          
      end.
    end.
  
END PROCEDURE.

PROCEDURE display-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR v-temp-pr LIKE ap-invl.unit-pr NO-UNDO.
  {ce/msfcalc.i}

     
  
  IF AVAIL po-ordl THEN DO:
      
     lv-pol-rowid = ROWID(po-ordl).
     FIND FIRST po-ord WHERE po-ord.company = po-ordl.company 
                         AND po-ord.po-no = po-ordl.po-no NO-LOCK NO-ERROR.
     ASSIGN ttPoPriceHistoryLook.pono = po-ord.po-no
            /*ap-invl.LINE:SCREEN-VALUE = string(po-ordl.LINE + (po-ord.po-no * 1000) ) /* ap/invline.i 1 */ */
            ttPoPriceHistoryLook.iname = po-ordl.i-name
            ttPoPriceHistoryLook.unit-pr  = po-ordl.cost
            ttPoPriceHistoryLook.prqtyuom  = po-ordl.pr-uom
            ttPoPriceHistoryLook.consuom  = po-ordl.pr-qty-uom
            v-wid = po-ordl.s-wid
            v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
            v-dep = 0
            v-bwt = 0.

     
        FIND ITEM WHERE ITEM.company = cocode
                 AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN ttPoPriceHistoryLook.tax = IF ITEM.tax-rcpt THEN "Y"                                   
                          ELSE "N".                                                                               
                                                                                                           
     /*IF ap-invl.cons-uom EQ "ROLL" THEN ap-invl.cons-uom = "LF".                                                  */
                                                                                                                  
     IF lv-po-glnum THEN ttPoPriceHistoryLook.actno = po-ordl.actnum.                                             
     ELSE DO:                                                                                                     
        if v-vend-actnum eq "" then do:                                                                           
            find first vend where vend.company eq cocode                                                           
                  and vend.vend-no eq po-ord.vend-no                                                              
                no-lock no-error.                                                                                 
            if avail vend then v-vend-actnum = vend.actnum.                                                       
        end.
        ttPoPriceHistoryLook.actno = v-vend-actnum.
     end.
     

     IF v-vend-actnum EQ "" THEN
        v-vend-actnum = v-ap-pur.
     
     if ttPoPriceHistoryLook.actno  eq "" then ttPoPriceHistoryLook.actno = v-vend-actnum.
     
     find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
     if avail item then do:
          v-dep = item.s-dep.          
          {po/pol-dims.i}
     end.
        
     if po-ordl.item-type then do:
          v-qty = if avail item           and
                     item.i-code eq "R"   and
                     INDEX("MOXY789",ITEM.mat-type) GT 0 and
                     item.stocked eq no   then po-ordl.cons-qty
                  else po-ordl.t-rec-qty.

          if po-ordl.cons-uom eq ttPoPriceHistoryLook.consuom then
                 ttPoPriceHistoryLook.invqty = v-qty - po-ordl.t-inv-qty.
          
          else do:
            run sys/ref/convquom.p (po-ordl.cons-uom, ttPoPriceHistoryLook.consuom ,
                                   v-bwt, v-len, v-wid, v-dep,
                                   v-qty, output v-qty).
                                   
            ttPoPriceHistoryLook.invqty = v-qty - po-ordl.t-inv-qty.
          end.
     end.          
     ELSE do:
          if ttPoPriceHistoryLook.consuom eq "EA" then
            ttPoPriceHistoryLook.invqty = po-ordl.t-rec-qty - po-ordl.t-inv-qty.
            
          else do:
            run sys/ref/convquom.p("EA", ttPoPriceHistoryLook.consuom,
                                   0, 0, 0, 0,
                                   po-ordl.t-rec-qty, output v-qty).
                                   
            ttPoPriceHistoryLook.invqty = v-qty - po-ordl.t-inv-qty.
          end.  
      end.
      /*RUN ap/d-selrec.p (RECID(ap-invl), OUTPUT lv-invl-qty).*/

     /* gdm - 05290903  
      RUN ap/d-selrec.w (ROWID(ap-inv), lv-pol-rowid, OUTPUT lv-invl-qty).
      gdm - 05290903 end */
    
  /*    FIND CURRENT ap-invl NO-LOCK NO-ERROR.  */
   
      if ttPoPriceHistoryLook.prqtyuom eq ttPoPriceHistoryLook.consuom
          THEN v-temp-pr = dec(ttPoPriceHistoryLook.unit-pr).          
      else
          run sys/ref/convcuom.p (ttPoPriceHistoryLook.prqtyuom, ttPoPriceHistoryLook.consuom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  ttPoPriceHistoryLook.unit-pr, output v-temp-pr).

        assign
         ttPoPriceHistoryLook.invqty = lv-invl-qty
         ttPoPriceHistoryLook.amt = lv-invl-qty * v-temp-pr
         /*ap-inv.net  = ap-inv.net + dec(ap-invl.amt:SCREEN-VALUE)*/.

        if v-len eq 0 then v-len = 12.
        if v-wid eq 0 then v-wid = 12.
        
        ttPoPriceHistoryLook.srft = if v-corr then (v-len * v-wid * .007)
                                      else (v-len * v-wid / 144).

        if not avail item             and
           (v-len eq 0 or v-wid eq 0) then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
                and NOT po-ordl.item-type
              no-lock no-error.
          if avail itemfg then ttPoPriceHistoryLook.srft = (itemfg.t-sqft).
        end.

        if ttPoPriceHistoryLook.consuom eq "EA" THEN v-qty = dec(ttPoPriceHistoryLook.invqty).          
        else
          run sys/ref/convquom.p(ttPoPriceHistoryLook.consuom, "EA",
                                 v-bwt, v-len, v-wid, v-dep,
                                 int(ttPoPriceHistoryLook.invqty), output v-qty).

        ttPoPriceHistoryLook.amtmsf = (dec(ttPoPriceHistoryLook.srft) * v-qty / 1000).
        
  END.  /* avail po-ordl */

  

END PROCEDURE.
