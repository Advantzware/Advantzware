

/*------------------------------------------------------------------------
    File        : RfqSize.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     :  Feb 18 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRfqSize NO-UNDO
    FIELD part_no LIKE rfqitem.part-no
     FIELD i_name LIKE rfqitem.i-name
     FIELD style LIKE rfqitem.style
     FIELD tab_in AS CHAR
     FIELD len LIKE rfqitem.len
     FIELD wid  LIKE rfqitem.wid
     FIELD dep LIKE rfqitem.dep
     FIELD dust LIKE rfqitem.dust
     FIELD fpanel LIKE rfqitem.fpanel
     FIELD tuck LIKE rfqitem.tuck 
     FIELD adhesive LIKE rfqitem.adhesive 
     FIELD k_wid LIKE rfqitem.k-wid
     FIELD k_len LIKE rfqitem.k-len
     FIELD lock LIKE rfqitem.lock
     FIELD gluelap LIKE rfqitem.gluelap
     FIELD lin_in LIKE rfqitem.lin-in
     FIELD t_wid LIKE rfqitem.t-wid
     FIELD t_len LIKE rfqitem.t-len 
     FIELD vStyleDscr LIKE style.dscr
     FIELD lv_sqin AS DEC
     
        .
  

DEFINE DATASET dsRfqSize FOR ttRfqSize.
DEFINE INPUT PARAMETER prmAction AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER PrmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER PrmPartNo     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmName      AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmStyle     AS CHAR  NO-UNDO. 
DEFINE INPUT PARAMETER prmTab     AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER PrmLen      AS DECIMAL FORMAT ">9.99999" NO-UNDO. 
DEFINE INPUT PARAMETER prmWid      AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmDep      AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmDust     AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmPanel    AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmTuck     AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmAdhesive AS CHARACTER FORMAT "X(10)"  NO-UNDO.
DEFINE INPUT PARAMETER prmKWid     AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmKLen     AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmLock     AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmGluela   AS DECIMAL FORMAT ">9.99999" NO-UNDO.   
DEFINE INPUT PARAMETER prmLin      AS DECIMAL FORMAT ">9.9999"   NO-UNDO.
DEFINE INPUT PARAMETER prmTWid     AS DECIMAL FORMAT ">>>9.999<<" NO-UNDO.   
DEFINE INPUT PARAMETER prmTLen     AS DECIMAL FORMAT ">>>9.999<<" NO-UNDO.   

DEFINE INPUT PARAMETER prmdscr      AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmSqin     AS DECIMAL NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqSize.
    DEFINE OUTPUT PARAMETER cError     AS CHAR NO-UNDO.

IF  prmAction   = ? THEN ASSIGN  prmAction = "".
IF  PrmUser     = ? THEN ASSIGN  PrmUser   = "".
IF  PrmRfqNo    = ? THEN ASSIGN  PrmRfqNo  = 0 .   
IF  PrmPartNo   = ? THEN ASSIGN  PrmPartNo = "".   
IF  prmName     = ? THEN ASSIGN  prmName   = "".  
IF  prmStyle    = ? THEN ASSIGN  prmStyle  = "".  
IF  prmTab      = ? THEN ASSIGN  prmTab    = "".  
IF  PrmLen      = ? THEN ASSIGN  PrmLen    = 0.  
IF  prmWid      = ? THEN ASSIGN  prmWid = 0 .  
IF  prmDep      = ? THEN ASSIGN  prmDep = 0.  
IF  prmDust     = ? THEN ASSIGN  prmDust = 0.  
IF  prmPanel    = ? THEN ASSIGN  prmPanel = 0.  
IF  prmTuck     = ? THEN ASSIGN  prmTuck = 0.  
IF  prmAdhesive = ? THEN ASSIGN  prmAdhesive = "".  
IF  prmKWid     = ? THEN ASSIGN  prmKWid = 0.  
IF  prmKLen     = ? THEN ASSIGN  prmKLen = 0.  
IF  prmLock     = ? THEN ASSIGN  prmLock = 0.  
IF  prmGluela   = ? THEN ASSIGN  prmGluela = 0.  
IF  prmLin      = ? THEN ASSIGN  prmLin = 0.  
IF  prmTWid     = ? THEN ASSIGN  prmTWid = 0.  
IF  prmTLen     = ? THEN ASSIGN  prmTLen = 0. 
             
IF  prmdscr     = ? THEN ASSIGN  prmdscr = "".  
IF  prmSqin     = ? THEN ASSIGN  prmSqin = 0.  
DEFINE BUFFER bf-rfqitem FOR rfqitem.
  DEF VAR prmComp AS CHAR NO-UNDO.
  DEF VAR prmLoc AS CHAR NO-UNDO.
  DEF VAR ld AS DEC EXTENT 3 NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
  def var lv-is-corr as log no-undo.

 FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".
ASSIGN
    cocode = prmComp .

{rfq/msfcalc.i}   /* v-corr */
    
                    /************validation***************/
IF prmAction = "UpdateSize" THEN DO:
    FIND FIRST ITEM WHERE ITEM.company = cocode AND (ITEM.i-no = prmAdhesive) NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN ASSIGN cError = "Invalid Adhesive!!!!".
END.
/*******************************************/
/* IF prmAction = "update" THEN DO: */
IF prmAction = "UpdateSize" THEN DO:
    
    FIND bf-rfqitem WHERE
        bf-rfqitem.company EQ prmComp AND
        bf-rfqitem.loc EQ prmLoc AND
        bf-rfqitem.rfq-no = PrmRfqNo AND
        bf-rfqitem.seq = int(PrmPartNo) 
        EXCLUSIVE-LOCK NO-ERROR.
    find first eb where
       eb.company = bf-rfqitem.company AND
       eb.est-no = bf-rfqitem.est-no AND
       eb.form-no = bf-rfqitem.form-no AND
       eb.blank-no = bf-rfqitem.blank-no
       EXCLUSIVE-LOCK NO-ERROR.

  /*IF AVAIL eb THEN
  DO:
     ASSIGN
        eb.t-wid = decimal(rfqitem.t-wid)
        eb.t-len = decimal(rfqitem.t-len).
    
  END.*/

 /* find item where item.company = bf-rfqitem.company and
                  item.i-no = bf-rfqitem.adhesive no-lock no-error.*/

    IF AVAIL bf-rfqitem THEN DO:
        assign
             bf-rfqitem.part-no  = prmPartNo
             bf-rfqitem.i-name    = prmName
             bf-rfqitem.style     = prmStyle
             bf-rfqitem.tab-in    = IF prmTab = "in" THEN TRUE ELSE FALSE
             bf-rfqitem.len       = PrmLen
             bf-rfqitem.wid       = prmWid
             bf-rfqitem.dep       = prmDep
             bf-rfqitem.dust      = prmDust   
             bf-rfqitem.fpanel    = prmPanel 
             bf-rfqitem.tuck      = prmTuck 
             bf-rfqitem.adhesive  = prmAdhesive
             bf-rfqitem.k-wid     = prmKWid 
             bf-rfqitem.k-len     = prmKLen
             bf-rfqitem.lock      = prmLock 
             bf-rfqitem.gluela    = prmGluela   
             bf-rfqitem.lin-in    = prmLin             
             bf-rfqitem.t-wid     = prmTWid   
             bf-rfqitem.t-len     = prmTLen
            /* bf-rfqitem.t-sqin    = /*prmSqin  */ prmTWid * prmTLen */
            .

             DEF VAR ld1 AS DEC EXTENT 3 NO-UNDO.
             DEF VAR li1 AS INT NO-UNDO.
             find style where style.company = cocode AND style.style = prmStyle no-lock no-error.
            
             if avail style and style.industry = "2" /* Corrugate Box */ 
                 then do:
                 ASSIGN lv-is-corr = YES.
                 END.
             ELSE ASSIGN lv-is-corr = NO.
             ASSIGN
                 ld1[1] = DEC(bf-rfqitem.t-wid)
                 ld1[2] = DEC(bf-rfqitem.t-len).

             IF lv-is-corr  THEN DO li1 = 1 TO EXTENT(ld1):
                 ld1[li1] = TRUNC(ld1[li1],0) + ((ld1[li1] - TRUNC(ld1[li1],0)) * 6.25).
             END.
             
             ld1[3] = ld1[1] * ld1[2].
             
             bf-rfqitem.t-sqin    = ld1[3] .
        
                RELEASE bf-rfqitem.                                                                                    
    END.   /*if avail bf-rfqitem*/                                                                               
 ASSIGN prmAction = "Select". 
END.  /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "Select" THEN DO:
    FOR EACH rfqitem WHERE
        rfqitem.company EQ prmComp AND
        rfqitem.loc EQ prmLoc AND
        rfqitem.rfq-no = PrmRfqNo AND
        rfqitem.seq = int(PrmPartNo) NO-LOCK:
        RUN CreateRfqSize.
    END.  /*FOR EACH rfqitem*/
END.  /* IF prmAction = "Select" THEN DO:*/

PROCEDURE CreateRfqSize:
    CREATE ttRfqSize.
    ASSIGN
        ttRfqSize.part_no = rfqitem.part-no
        ttRfqSize.i_name = rfqitem.i-name
        ttRfqSize.style = rfqitem.style
        ttRfqSize.len = rfqitem.len
        ttRfqSize.wid = rfqitem.wid
        ttRfqSize.dep = rfqitem.dep
        ttRfqSize.dust = rfqitem.dust
        ttRfqSize.fpanel = rfqitem.fpanel
        ttRfqSize.tuck = rfqitem.tuck
        ttRfqSize.adhesive = rfqitem.adhesive
        ttRfqSize.k_wid = rfqitem.k-wid
        ttRfqSize.k_len = rfqitem.k-len
        ttRfqSize.lock = rfqitem.lock
        ttRfqSize.gluelap = rfqitem.gluelap
        ttRfqSize.lin_in = rfqitem.lin-in
        ttRfqSize.t_wid = rfqitem.t-wid
        ttRfqSize.t_len = rfqitem.t-len
            .
        
        DEF VAR ld AS DEC EXTENT 3 NO-UNDO.
        DEF VAR li AS INT NO-UNDO.
        find style where style.company = rfqitem.company AND style.style = rfqitem.style no-lock no-error.
        if avail style then
            ttRfqSize.vStyleDscr = style.dscr. 
        if avail style and style.industry = "2" /* Corrugate Box */ 
            then do:
            ASSIGN lv-is-corr = YES.
        END.
        ELSE ASSIGN lv-is-corr = NO.
        ASSIGN
            ld[1] = DEC(rfqitem.t-wid)
            ld[2] = DEC(rfqitem.t-len).
       
      /* IF lv-is-corr  THEN DO li = 1 TO EXTENT(ld):
           ld[li] = TRUNC(ld[li],0) + ((ld[li] - TRUNC(ld[li],0)) * 6.25).
          
       END.
MESSAGE " length  "  ld[1]  ld[2] . 
       ld[3] = ld[1] * ld[2].

       IF lv-is-corr THEN
           IF  v-corr THEN ld[3] = ld[3] * .007.
           ELSE ld[3] = ld[3] / 144.*/

         /* ttRfqSize.lv_sqin = (ld[3]) .*/

    
   
          IF lv-is-corr THEN
             ttRfqSize.lv_sqin =  if v-corr then rfqitem.t-sqin * 0.007 else rfqitem.t-sqin / 144.
         ELSE
             ttRfqSize.lv_sqin =  /*DEC(rfqitem.t-wid) * DEC(rfqitem.t-len)*/  rfqitem.t-sqin .

          
           IF rfqitem.tab-in THEN DO:
                ttRfqSize.tab_in = "In".
           END.
           IF NOT rfqitem.tab-in THEN DO:
                ttRfqSize.tab_in = "Out".
           END.
END PROCEDURE.

  


    
    



