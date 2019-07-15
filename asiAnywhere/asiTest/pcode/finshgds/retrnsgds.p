

/*------------------------------------------------------------------------
    File        : retrnsgds.p
    Purpose     : Finished goods
    Syntax      :

    Description : 
    Author(s)   : 
    Created     : 19 sept 2012 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttReturnsFinishGds NO-UNDO
        FIELD vRno          AS INT
        FIELD vDate           AS CHAR  
        FIELD vTransTime      AS CHAR   
        FIELD vTag            AS CHAR FORMAT "x(20)"
        FIELD vPo_no          AS CHAR FORMAT "x(9)"
        FIELD vJob_no         AS CHAR FORMAT "x(6)"  
        FIELD vJob_no2        AS INT
        FIELD vItem           AS CHAR FORMAT "x(15)"
        FIELD vItemName       AS CHAR FORMAT "x(30)"
        FIELD vLoc            AS CHAR  FORMAT "x(5)" 
        FIELD vLocBin         AS CHAR  FORMAT "x(8)"  
        FIELD vCases          AS INT 
        FIELD vQtyCas         AS INT 
        FIELD vCasUnit        AS INT    
        FIELD vPartial        AS INT    
        FIELD vStdCost        AS DEC
        FIELD vCostUom        AS CHAR FORMAT "x(3)"
        FIELD vT_Qty          AS DEC
        FIELD vExtCost        AS DEC
        FIELD vTot_Wt         AS DEC
        FIELD extra           AS CHAR
        FIELD vinvno          AS INT
        FIELD vRecKey         AS CHAR 
        
        .


DEFINE DATASET dsReturnsFinishGds FOR ttReturnsFinishGds .

DEFINE INPUT PARAMETER prmUser           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJobno          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPono           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqno          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTagno          AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmTransTime      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJob_no2        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmName           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLoc            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLocBin         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCases          AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmQty_Cas        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCasUnit        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmPartial        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmStdCost        AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmCost_Uom       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTQty           AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmExtCost        AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmInvNo          AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey         AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError            AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReturnsFinishGds.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
/*DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.*/
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF VAR v-auto-add-tag AS LOG NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR v-next-tag AS cha NO-UNDO.
{sys/inc/var.i "new shared"}



DEF VAR ll-help-run AS LOG NO-UNDO.

DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR v-post-date AS DATE INITIAL TODAY.
def var fg-uom-list  as char NO-UNDO.
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
DEF VAR v-prgmname AS CHAR INIT "b-rcptd." NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
def var lv-recid as recid no-undo.

DEF BUFFER bf-tmp FOR fg-rctd.  /* for tag validation */
DEF BUFFER xfg-rdtlh FOR fg-rdtlh. /* for tag validation */


{fg/fg-post3.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/invrecpt.i NEW}

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd 
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOG INIT NO
    FIELD invoiced AS LOG INIT NO.

DEF TEMP-TABLE tt-email FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEF STREAM logFile.
DEF STREAM st-email.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.

{sys/inc/ssmovefg.i}


IF prmUser          = ? THEN ASSIGN prmUser        = "".
IF prmAction        = ? THEN ASSIGN prmAction      = "Select".
IF prmFgItem        = ? THEN ASSIGN prmFgItem      = "".
IF prmJobno         = ? THEN ASSIGN prmJobno       = "".  
IF prmPono          = ? THEN ASSIGN prmPono        = "".
IF prmSeqno         = ? THEN ASSIGN prmSeqno       = "".
IF prmRcptDate      = ? THEN ASSIGN prmRcptDate    = "".
IF prmTagno         = ? THEN ASSIGN prmTagno       = "".
IF prmTransTime     = ? THEN ASSIGN prmTransTime   = "".
IF prmJob_no2       = ? THEN ASSIGN prmJob_no2     = 0.
IF prmName          = ? THEN ASSIGN prmName        = "".
IF prmLoc           = ? THEN ASSIGN prmLoc         = "".
IF prmLocBin        = ? THEN ASSIGN prmLocBin      = "".
IF prmCases         = ? THEN ASSIGN prmCases       = 0.
IF prmQty_Cas       = ? THEN ASSIGN prmQty_Cas     = 0.
IF prmCasUnit       = ? THEN ASSIGN prmCasUnit     = 0.
IF prmPartial       = ? THEN ASSIGN prmPartial     = 0.
IF prmStdCost       = ? THEN ASSIGN prmStdCost     = 0.
IF prmCost_Uom      = ? THEN ASSIGN prmCost_Uom    = "".
IF prmTQty          = ? THEN ASSIGN prmTQty        = 0.
IF prmExtCost       = ? THEN ASSIGN prmExtCost     = 0.
IF prmInvNo         = ? THEN ASSIGN prmInvNo       = 0.
IF prmRecKey        = ? THEN ASSIGN prmRecKey      = "".
IF cError           = ? THEN ASSIGN cError         = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp
    g_company = prmComp
    vuser     = prmUser  .

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DO TRANSACTION:
   {sys/inc/closejob.i FGPost}
   {sys/inc/fgpostgl.i}
   {sys/inc/fgemails.i}
   {sys/inc/adjustgl.i}
   
END.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPOTAG#"
    no-lock no-error.
v-auto-add-tag = NO.
IF AVAIL sys-ctrl THEN
  v-auto-add-tag = sys-ctrl.log-fld.

DEF TEMP-TABLE tt-fg-rctd LIKE fg-rctd
    FIELD tt-rowid AS ROWID
    FIELD po-rowid AS ROWID.


   FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

 IF prmAction = "GridSelect" THEN DO:

     FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
          (fg-rctd.rita-code eq "I" ) NO-LOCK :

         create ttReturnsFinishGds.
           assign
               ttReturnsFinishGds.vRno             = fg-rctd.r-no
               ttReturnsFinishGds.vDate            = string(fg-rctd.rct-date)
               ttReturnsFinishGds.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
               ttReturnsFinishGds.vTag             = fg-rctd.tag
               ttReturnsFinishGds.vPo_no           = fg-rctd.po-no
               ttReturnsFinishGds.vJob_no          = fg-rctd.job-no
               ttReturnsFinishGds.vJob_no2         = fg-rctd.job-no2
               ttReturnsFinishGds.vItem            = fg-rctd.i-no
               ttReturnsFinishGds.vItemName        = fg-rctd.i-name
               ttReturnsFinishGds.vLoc             = fg-rctd.loc 
               ttReturnsFinishGds.vLocBin          = fg-rctd.loc-bin
               ttReturnsFinishGds.vCases           = fg-rctd.cases 
               ttReturnsFinishGds.vQtyCas          = fg-rctd.qty-case 
               ttReturnsFinishGds.vCasUnit         = fg-rctd.cases-unit  
               ttReturnsFinishGds.vPartial         = fg-rctd.partial
               ttReturnsFinishGds.vStdCost         = fg-rctd.std-cost
               ttReturnsFinishGds.vCostUom         = fg-rctd.cost-uom
               ttReturnsFinishGds.vT_Qty           = fg-rctd.t-qty
              /* ttReturnsFinishGds.vFrtCost         = fg-rctd.frt-cost*/
               ttReturnsFinishGds.vExtCost         = fg-rctd.ext-cost
               /*ttReturnsFinishGds.vStackCode       = fg-rctd.stack-code */
               ttReturnsFinishGds.vinvno           = fg-rctd.inv-no  
               ttReturnsFinishGds.vRecKey          = fg-rctd.rec_key .



     END.
 
     
 END.  /* end of grid select */



 IF prmAction = "GridSearch" THEN DO:

      FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and 
            (fg-rctd.rita-code eq "I" ) AND
            (fg-rctd.tag  BEGINS prmTagno OR  prmTagno = "") NO-LOCK :

        create ttReturnsFinishGds.
          assign
              ttReturnsFinishGds.vRno             = fg-rctd.r-no
               ttReturnsFinishGds.vDate            = string(fg-rctd.rct-date)
               ttReturnsFinishGds.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
               ttReturnsFinishGds.vTag             = fg-rctd.tag
               ttReturnsFinishGds.vPo_no           = fg-rctd.po-no
               ttReturnsFinishGds.vJob_no          = fg-rctd.job-no
               ttReturnsFinishGds.vJob_no2         = fg-rctd.job-no2
               ttReturnsFinishGds.vItem            = fg-rctd.i-no
               ttReturnsFinishGds.vItemName        = fg-rctd.i-name
               ttReturnsFinishGds.vLoc             = fg-rctd.loc 
               ttReturnsFinishGds.vLocBin          = fg-rctd.loc-bin
               ttReturnsFinishGds.vCases           = fg-rctd.cases 
               ttReturnsFinishGds.vQtyCas          = fg-rctd.qty-case 
               ttReturnsFinishGds.vCasUnit         = fg-rctd.cases-unit  
               ttReturnsFinishGds.vPartial         = fg-rctd.partial
               ttReturnsFinishGds.vStdCost         = fg-rctd.std-cost
               ttReturnsFinishGds.vCostUom         = fg-rctd.cost-uom
               ttReturnsFinishGds.vT_Qty           = fg-rctd.t-qty
              /* ttReturnsFinishGds.vFrtCost         = fg-rctd.frt-cost*/
               ttReturnsFinishGds.vExtCost         = fg-rctd.ext-cost
               /*ttReturnsFinishGds.vStackCode       = fg-rctd.stack-code */
               ttReturnsFinishGds.vinvno           = fg-rctd.inv-no  
               ttReturnsFinishGds.vRecKey          = fg-rctd.rec_key .


    END.


END.  /* end of grid select */







   

IF prmAction = "Update" THEN DO: 
   
  FIND FIRST fg-rctd WHERE fg-rctd.company = cocode AND 
                           fg-rctd.rita-code eq "I" AND
                           fg-rctd.r-no EQ int(prmSeqno)  NO-LOCK NO-ERROR.

  IF prmTagno = "" THEN do:
      ASSIGN cError = "Tag No Can not Blank ".
      RETURN.
  END.

  FIND FIRST bf-tmp WHERE bf-tmp.company = cocode AND
      bf-tmp.tag = prmTagno AND
      RECID(bf-tmp) <> RECID(fg-rctd) AND
      fg-rctd.rita-code eq "I" NO-LOCK NO-ERROR.
  IF AVAIL bf-tmp THEN do:
      
      cError = "This Tag Number Has Already Been Used. Please Enter A Unique Tag Number." .
      RETURN .
  END.

  FIND first xfg-rdtlh
              where xfg-rdtlh.company   eq g_company
                and xfg-rdtlh.loc       eq prmLoc
                and xfg-rdtlh.tag       eq prmTagno
                and xfg-rdtlh.qty       gt 0
                and xfg-rdtlh.rita-code ne "S" use-index tag NO-LOCK NO-ERROR.
  IF AVAIL xfg-rdtlh THEN do:
      cError = "This Tag Number Has Already Been Used." .
      RETURN .
  END.

  FIND FIRST loadtag WHERE loadtag.company = cocode
      AND loadtag.ITEM-type = NO
      AND loadtag.tag-no = prmTagno NO-LOCK NO-ERROR.
  IF NOT AVAIL loadtag THEN DO:
      cError = "Invalid Loadtag#. ".
      RETURN .
  END.

  /*FIND FIRST fg-bin WHERE fg-bin.company = cocode
      AND fg-bin.i-no = loadtag.i-no
      AND fg-bin.tag = loadtag.tag-no
      /*AND fg-bin.job-no = loadtag.job-no
      AND fg-bin.job-no2 = loadtag.job-no2*/
      AND fg-bin.qty > 0
      NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
      cError =  "Invalid Inventory for the tag." .
      RETURN .
  END.*/


  IF NOT CAN-FIND(FIRST itemfg
       where itemfg.company  EQ cocode AND
       itemfg.i-no EQ prmFgItem) THEN DO:
       cError = "Invalid Item#, try help...".
       RETURN.
    END.

     FIND FIRST loc WHERE loc.company = cocode
         AND loc.loc = prmLoc NO-LOCK NO-ERROR.
     IF NOT AVAIL loc THEN DO:
         cError =  "Invalid Warehouse. Try Help. " .
         RETURN .
     END.
     
  FIND FIRST fg-bin WHERE fg-bin.company = cocode
      AND fg-bin.i-no = ""
      AND fg-bin.loc = prmLoc
      AND fg-bin.loc-bin = prmLocBin
      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
      cError = "Invalid Bin#. Try Help. " .
      RETURN .
  END.

  

  
            
END.  /*** update*/  

IF prmAction = "Update" THEN DO:
  
   FIND FIRST fg-rctd WHERE 
        fg-rctd.company eq cocode and
           fg-rctd.r-no EQ int(prmSeqno) AND
            (fg-rctd.rita-code eq "I" )  EXCLUSIVE-LOCK NO-ERROR.
      
          ASSIGN
            fg-rctd.tag         = prmTagno
            fg-rctd.loc         = prmLoc           
            fg-rctd.loc-bin     = prmLocBin    
            fg-rctd.t-qty       = DEC(prmTQty )
            fg-rctd.inv-no      = prmInvNo       .

          ASSIGN                                   
              fg-rctd.tag        = prmTagno                      
              fg-rctd.job-no     = prmJobno                         
              fg-rctd.job-no2    = prmJob_no2                         
              fg-rctd.i-no       = prmFgItem                     
              fg-rctd.i-name     = prmName                
              fg-rctd.loc        = prmLoc                              
              fg-rctd.loc-bin    = prmLocBin                 
              fg-rctd.cases      = prmCases                       
              fg-rctd.qty-case   = prmQty_Cas                         
              fg-rctd.cases-unit = prmCasUnit                   
              fg-rctd.partial    = prmPartial                   
              fg-rctd.std-cost   = prmStdCost                   
              fg-rctd.cost-uom   = prmCost_Uom                  
              fg-rctd.t-qty      = prmTQty  
              fg-rctd.ext-cost   = prmExtCost                  
              fg-rctd.inv-no     = prmInvNo     .  

      RUN get-fg-bin-cost .
      RUN get-matrix (NO).

      FIND FIRST reftable
          WHERE reftable.reftable EQ "fg-rctd.user-id"
          AND reftable.company  EQ fg-rctd.company
          AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
          EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
              reftable.reftable = "fg-rctd.user-id"
              reftable.company  = fg-rctd.company
              reftable.loc      = STRING(fg-rctd.r-no,"9999999999")
              reftable.code     = prmUser.
          END.
          ASSIGN
              reftable.code2        = prmUser
              fg-rctd.upd-date = TODAY
              fg-rctd.upd-time = TIME.
      
   
          ASSIGN
          prmAction = "Select" . 
           


     

END.  /**** update  ***/ 




     /*ASSIGN
           prmAction = "Select" .*/

IF prmAction = "Addnew" THEN DO:
    
  IF prmTagno = "" THEN do:
      ASSIGN cError = "Tag No Can not Blank ".
      RETURN.
  END.

  IF CAN-FIND (FIRST bf-tmp WHERE bf-tmp.company = cocode AND
      bf-tmp.tag = prmTagno AND
      fg-rctd.rita-code eq "I") THEN DO:
      cError = "This Tag Number Has Already Been Used. Please Enter A Unique Tag Number." .
      RETURN .
  END.

  FIND first xfg-rdtlh
              where xfg-rdtlh.company   eq g_company
                and xfg-rdtlh.loc       eq prmLoc
                and xfg-rdtlh.tag       eq prmTagno
                and xfg-rdtlh.qty       gt 0
                and xfg-rdtlh.rita-code ne "S" use-index tag NO-LOCK NO-ERROR.
  IF AVAIL xfg-rdtlh THEN do:
      cError = "This Tag Number Has Already Been Used." .
      RETURN .
  END.

  FIND FIRST loadtag WHERE loadtag.company = cocode
      AND loadtag.ITEM-type = NO
      AND loadtag.tag-no = prmTagno NO-LOCK NO-ERROR.
  IF NOT AVAIL loadtag THEN DO:
      cError = "Invalid Loadtag#. ".
      RETURN .
  END.

 
  IF NOT CAN-FIND(FIRST itemfg
       where (itemfg.company  = cocode ) AND
       itemfg.i-no EQ prmFgItem) THEN
    DO:
       cError = "Invalid Item#, try help...".
       RETURN.
    END.

     FIND FIRST loc WHERE loc.company = cocode
         AND loc.loc = prmLoc NO-LOCK NO-ERROR.
     IF NOT AVAIL loc THEN DO:
         cError =  "Invalid Warehouse. Try Help. " .
         RETURN .
     END.
     
  FIND FIRST fg-bin WHERE fg-bin.company = cocode
      AND fg-bin.i-no = ""
      AND fg-bin.loc = prmLoc
      AND fg-bin.loc-bin = prmLocBin
      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
      cError = "Invalid Bin#. Try Help. " .
      RETURN .
  END.



END. 

IF prmAction = "Addnew" THEN DO:
   
    DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
    DEF BUFFER b-fg-rctd FOR fg-rctd.

  /* Code placed here will execute PRIOR to standard behavior. */

  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

   /* Dispatch standard ADM method.                             */
  

  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  CREATE fg-rctd .
  /* Code placed here will execute AFTER standard behavior.    */
  assign fg-rctd.company = cocode
         fg-rctd.r-no    = lv-rno
         fg-rctd.rita-code = "I"
         fg-rctd.s-num  = 0
         fg-rctd.rct-date = today
         fg-rctd.trans-time = TIME
         lv-recid = recid(fg-rctd).

     ASSIGN                                   
         fg-rctd.tag        = prmTagno                      
         fg-rctd.job-no     = prmJobno                         
         fg-rctd.job-no2    = prmJob_no2                         
         fg-rctd.i-no       = prmFgItem                     
         fg-rctd.i-name     = prmName                
         fg-rctd.loc        = prmLoc                              
         fg-rctd.loc-bin    = prmLocBin                 
         fg-rctd.cases      = prmCases                       
         fg-rctd.qty-case   = prmQty_Cas                         
         fg-rctd.cases-unit = prmCasUnit                   
         fg-rctd.partial    = prmPartial                   
         fg-rctd.std-cost   = prmStdCost                   
         fg-rctd.cost-uom   = prmCost_Uom                  
         fg-rctd.t-qty      = prmTQty  
         fg-rctd.ext-cost   = prmExtCost                  
         fg-rctd.inv-no     = prmInvNo     .  

     RUN get-fg-bin-cost .
     RUN get-matrix (NO).
                                          
                                            
        FIND FIRST reftable
          WHERE reftable.reftable EQ "fg-rctd.user-id"
          AND reftable.company  EQ fg-rctd.company
          AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
          NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
              reftable.reftable = "fg-rctd.user-id"
              reftable.company  = fg-rctd.company
              reftable.loc      = STRING(fg-rctd.r-no,"9999999999")
              reftable.code     = prmUser.
               END.
          ASSIGN
              reftable.code2        = prmUser
              fg-rctd.upd-date = TODAY
              fg-rctd.upd-time = TIME.


  ASSIGN
        prmAction = "Select" 
        prmSeqno  =  string(fg-rctd.r-no).



 
END.




IF prmAction = "Deletercpt" THEN DO:
    
    
    FIND FIRST fg-rctd WHERE 
        fg-rctd.company eq cocode and
           fg-rctd.r-no EQ int(prmSeqno) AND
            (fg-rctd.rita-code eq "I" ) EXCLUSIVE-LOCK NO-ERROR.
      
      IF AVAIL fg-rctd THEN 
          DELETE fg-rctd .

FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and
           fg-rctd.r-no EQ int(prmSeqno) AND
            (fg-rctd.rita-code eq "I" )  NO-LOCK :
      
           ASSIGN
           prmAction = "Select" 
           prmSeqno  = string(fg-rctd.r-no).
           LEAVE.

END.
     ASSIGN
           prmAction = "Select" .


END.






 IF prmAction = "Select" THEN DO:
  
  FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and
          fg-rctd.rita-code eq "I"  AND
      fg-rctd.r-no EQ int(prmSeqno) NO-LOCK :

    
            create ttReturnsFinishGds.
            assign
                ttReturnsFinishGds.vRno            = fg-rctd.r-no
               ttReturnsFinishGds.vDate            = string(fg-rctd.rct-date)
               ttReturnsFinishGds.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
               ttReturnsFinishGds.vTag             = fg-rctd.tag
               ttReturnsFinishGds.vPo_no           = fg-rctd.po-no
               ttReturnsFinishGds.vJob_no          = fg-rctd.job-no
               ttReturnsFinishGds.vJob_no2         = fg-rctd.job-no2
               ttReturnsFinishGds.vItem            = fg-rctd.i-no
               ttReturnsFinishGds.vItemName        = fg-rctd.i-name
               ttReturnsFinishGds.vLoc             = fg-rctd.loc 
               ttReturnsFinishGds.vLocBin          = fg-rctd.loc-bin
               ttReturnsFinishGds.vCases           = fg-rctd.cases 
               ttReturnsFinishGds.vQtyCas          = fg-rctd.qty-case 
               ttReturnsFinishGds.vCasUnit         = fg-rctd.cases-unit  
               ttReturnsFinishGds.vPartial         = fg-rctd.partial
               ttReturnsFinishGds.vStdCost         = fg-rctd.std-cost
               ttReturnsFinishGds.vCostUom         = fg-rctd.cost-uom
               ttReturnsFinishGds.vT_Qty           = fg-rctd.t-qty
              /* ttReturnsFinishGds.vFrtCost         = fg-rctd.frt-cost*/
               ttReturnsFinishGds.vExtCost         = fg-rctd.ext-cost
               /*ttReturnsFinishGds.vStackCode       = fg-rctd.stack-code */
               ttReturnsFinishGds.vinvno           = fg-rctd.inv-no  
               ttReturnsFinishGds.vRecKey          = fg-rctd.rec_key .


               

                      /* v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.*/
                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/



PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-first-disp as log no-undo.

  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
   DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  

  if not avail fg-rctd then return.


find itemfg where itemfg.company eq cocode
              and itemfg.i-no  eq fg-rctd.i-no
            use-index i-no no-lock no-error.

ASSIGN
 lv-cost-uom = itemfg.prod-uom
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0
 .

/* Always find just to get quantity */
if ip-first-disp  and avail fg-rctd and fg-rctd.i-no <> "" then do: /* for row-display */  
find first po-ordl where po-ordl.company = cocode
                     and po-ordl.po-no = int(fg-rctd.po-no)
                     and po-ordl.i-no  = fg-rctd.i-no
                     and po-ordl.job-no = fg-rctd.job-no
                     and po-ordl.job-no2 = fg-rctd.job-no2
                     and po-ordl.item-type = no
                     no-lock no-error.



  IF AVAIL po-ordl THEN
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.

  ASSIGN
   lv-out-qty  = fg-rctd.t-qty
   lv-out-cost = fg-rctd.std-cost.

  IF fg-rctd.cost-uom NE lv-cost-uom THEN
    RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost, OUTPUT lv-out-cost).

END. /* avail fg-rctd */
/* ======================================================================= */
ELSE 
if avail fg-rctd and fg-rctd.i-no <> "" then do: /* in update mode - use screen-value */
  /*find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = INT(fg-rctd.po-no) 
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = (fg-rctd.job-no)
                       and po-ordl.job-no2 = integer(fg-rctd.job-no2)
                       and po-ordl.item-type = no
                       no-lock no-error.
  v-rec-qty = INT(fg-rctd.t-qty).

  FOR EACH b-fg-rctd
      WHERE b-fg-rctd.company    EQ fg-rctd.company
        AND b-fg-rctd.rita-code  EQ "R"
        AND b-fg-rctd.i-no       EQ fg-rctd.i-no
        AND INT(b-fg-rctd.po-no) EQ INT(fg-rctd.po-no)
        AND b-fg-rctd.job-no     EQ fg-rctd.job-no
        AND b-fg-rctd.job-no2    EQ INT(fg-rctd.job-no2)
        AND ROWID(b-fg-rctd)     NE ROWID(fg-rctd)
      NO-LOCK:
    v-rec-qty = v-rec-qty + b-fg-rctd.t-qty.
  END.

  /* If available PO orderline */
  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-rec-qty = v-rec-qty + po-ordl.t-rec-qty.

    IF LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN DO:
       RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                              v-rec-qty, OUTPUT v-rec-qty).

    END.
    
  END. /* IF AVAIL po-ordl */
  /* Else if not available PO orderline and job number is entered... */
  ELSE IF fg-rctd.job-no <> "" THEN DO:
       find first job-hdr where job-hdr.company = fg-rctd.company                       
                       and job-hdr.i-no  = fg-rctd.i-no
                       and job-hdr.job-no = (fg-rctd.job-no)
                       and job-hdr.job-no2 = integer(fg-rctd.job-no2)
                       no-lock no-error.
       IF AVAIL job-hdr THEN DO:

/*           FOR EACH fg-act                                                                         */
/*               WHERE fg-act.company EQ cocode                                                      */
/*                 AND fg-act.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}        */
/*                 AND fg-act.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})  */
/*                 AND fg-act.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}          */
/*               NO-LOCK:                                                                            */
/*             v-rec-qty = v-rec-qty + fg-act.qty.                                                   */
/*           END.                                                                                    */

          FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-job-qty = job-hdr.qty                          .
          ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-job-qty = (job-hdr.qty * (1 + ((IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                                IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0) / 100))).
          END.


          {sys/inc/roundup.i v-job-qty}

          
       END.
  END.*/


  ASSIGN
   lv-out-qty  = DEC(fg-rctd.t-qty)
   lv-out-cost = DEC(fg-rctd.std-cost).

  IF prmCost_Uom NE lv-cost-uom THEN
    RUN rm/convcuom.p(prmCost_Uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      prmStdCost, OUTPUT lv-out-cost).
END.

IF lv-cost-uom NE "EA" THEN
  RUN rm/convquom.p("EA", lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-qty).


ASSIGN
 fg-rctd.cost-uom  = lv-cost-uom
 fg-rctd.std-cost  = lv-out-cost
 fg-rctd.ext-cost  = (lv-out-qty * lv-out-cost) /*+
           dec(fg-rctd.frt-cost:screen-value in browse {&browse-name}))*/ .
END PROCEDURE.


PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    IF fg-rctd.job-no <> "" THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no
          AND fg-bin.job-no  EQ fg-rctd.job-no
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2)
          AND fg-bin.loc     EQ fg-rctd.loc
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin
          AND fg-bin.tag     EQ fg-rctd.tag
        NO-LOCK NO-ERROR.
    ELSE DO:
        FIND FIRST loadtag WHERE loadtag.company = cocode
                             AND loadtag.item-type = NO
                             AND loadtag.tag-no = fg-rctd.tag NO-LOCK NO-ERROR.
        IF AVAIL loadtag THEN
           FIND FIRST fg-bin WHERE
                fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ fg-rctd.i-no
                  AND fg-bin.job-no  EQ loadtag.job-no
                  AND fg-bin.job-no2 EQ loadtag.job-no2
                  AND fg-bin.loc     EQ fg-rctd.loc
                  AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                  AND fg-bin.tag     EQ fg-rctd.tag
                  NO-LOCK NO-ERROR.
                  
    END.
    IF AVAIL fg-bin THEN
      ASSIGN
       fg-rctd.std-cost = fg-bin.std-tot-cost
       fg-rctd.cost-uom = fg-bin.pur-uom.

  FIND FIRST job-hdr WHERE
       job-hdr.company = g_company
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   /*IF AVAIL job-hdr THEN 
      ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost) . */

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ fg-rctd.job-no
            AND job.job-no2 EQ int(fg-rctd.job-no2)
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ fg-rctd.i-no          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      fg-rctd.std-cost = (job-hdr.std-tot-cost).
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      fg-rctd.std-cost = (reftable.val[5]).
    else do:
     /* find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            and po-ordl.i-no      eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost).
      END.
     
      else */
      if avail itemfg then
        assign
         fg-rctd.cost-uom = itemfg.prod-uom
         fg-rctd.std-cost = (itemfg.total-std-cost).
    end.
   
   IF NOT AVAIL job-hdr OR dec(fg-rctd.std-cost) = 0 THEN DO:
      IF fg-rctd.job-no <> ""  THEN
         FIND FIRST oe-ordl WHERE oe-ordl.company = g_company
                           AND oe-ordl.i-no = fg-rctd.i-no
                           AND oe-ordl.job-no = fg-rctd.job-no
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      ELSE IF AVAIL loadtag THEN
         FIND FIRST oe-ordl WHERE oe-ordl.company = g_company
                           AND oe-ordl.i-no = fg-rctd.i-no
                           AND oe-ordl.ord-no = loadtag.ord-no NO-LOCK NO-ERROR.

      IF AVAIL oe-ordl THEN
           ASSIGN fg-rctd.std-cost = (oe-ordl.cost)
                  fg-rctd.cost-uom = "M".
   END.

END PROCEDURE.
