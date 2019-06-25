/*------------------------------------------------------------------------
    File        : CorrPrep.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 02  march 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttCorrPrep NO-UNDO
    FIELD  vEstNum     AS CHAR 
    FIELD  vCustPart   AS CHAR
    FIELD  vEstDate    AS DATETIME    
    FIELD  vFormNo     AS INT     
    FIELD  vFormQty    AS INT     
    FIELD  vBlankNo    AS INT      
    FIELD  vBlankQty   AS INT

    FIELD vSnum       AS INT
    FIELD vBnum       AS INT
    FIELD vCode       AS CHARACTER
    FIELD vQty        AS DECIMAL
    FIELD vDesc       AS CHAR
    FIELD vCost       AS DECIMAL
    FIELD vMl         AS CHAR
    FIELD vSimon      AS CHAR
    FIELD vMark       AS DECIMAL
    FIELD vAmort      AS DECIMAL
    FIELD vLine       AS INT
    FIELD vOrder      AS INT
    FIELD Jobno      AS CHAR 
    FIELD Jobno2     AS INT 
        .
DEFINE DATASET dsCorrPrep FOR ttCorrPrep.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmActSelect   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmSnum        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBnum        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCode        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmQty         AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmDesc        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmCost        AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmMl          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSimon       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMark        AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmAmort       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLine        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlank       AS INT NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrPrep.
DEFINE  OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
DEFINE  OUTPUT PARAMETER cLogicError AS CHARACTER NO-UNDO.

IF prmUser       = ?  THEN ASSIGN    prmUser        = "".
IF prmAction     = ?  THEN ASSIGN    prmAction      = "".
IF prmComp       = ?  THEN ASSIGN    prmComp        = "".
IF prmEstNum     = ?  THEN ASSIGN    prmEstNum      = "".
IF prmFormNo     = ?  THEN ASSIGN    prmFormNo      = 0.

IF prmSnum       = ?  THEN ASSIGN    prmSnum        = 0.
IF prmBnum       = ?  THEN ASSIGN    prmBnum        = 0.
IF prmCode       = ?  THEN ASSIGN    prmCode        = "".
IF prmQty        = ?  THEN ASSIGN    prmQty         = 0.
IF prmDesc       = ?  THEN ASSIGN    prmDesc        = "".
IF prmCost       = ?  THEN ASSIGN    prmCost        = 0.
IF prmMl         = ?  THEN ASSIGN    prmMl          = "".
IF prmSimon      = ?  THEN ASSIGN    prmSimon       = "".
IF prmMark       = ?  THEN ASSIGN    prmMark        = 0.
IF prmAmort      = ?  THEN ASSIGN    prmAmort       = 0.
IF prmLine       = ?  THEN ASSIGN    prmLine        = 0.
IF prmActSelect  = ?  THEN ASSIGN    prmActSelect   = "".

{est/d-machex.i NEW}

DEF BUFFER b-ef FOR ef.
 DEF BUFFER b-eb FOR eb.
 DEF VAR li AS INT NO-UNDO.
 DEFINE VAR bi AS INT NO-UNDO.
 DEFINE VAR vEstimate AS CHAR NO-UNDO.
 def buffer bf-prep for est-prep.
 def var li-next as int no-undo.
DEFINE NEW SHARED VARIABLE vError AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE vLogicError AS CHARACTER NO-UNDO.
DEF NEW shared var fil_id as recid no-undo.
DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.

{sys/inc/VAR.i "new shared" }
 
    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
 
ASSIGN
    cocode = prmComp
    locode = "Main"
    g_company = cocode
    g_loc     = locode 
     .

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).


/*************************************prmAction***************************************************/  


IF prmActSelect = "Select" THEN DO:
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST eb WHERE eb.est-no =  vEstimate AND eb.company = prmComp AND eb.form-no = prmFormNo NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ vEstimate
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND CURRENT est EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.
   
    /*DISABLE TRIGGERS FOR LOAD OF ef.*/
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ prmFormNo
        NO-LOCK NO-ERROR.
    IF AVAIL ef THEN DO:
      FOR EACH b-eb
          WHERE b-eb.company EQ ef.company
            AND b-eb.est-no  EQ ef.est-no
            AND b-eb.form-no EQ ef.form-no
          NO-LOCK:
        bi = bi + 1.
      END.
      IF li NE ef.blank-qty THEN DO:
        FIND CURRENT ef EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ef THEN ef.blank-qty = bi.
        FIND CURRENT ef NO-LOCK.
      END.
    END.
  
  FOR EACH est-prep WHERE est-prep.company = est.company AND est-prep.est-no = est.est-no NO-LOCK :
      IF AVAIL est-prep THEN DO:
           CREATE ttCorrPrep.
           ASSIGN
              ttCorrPrep.vSnum         = est-prep.s-num
              ttCorrPrep.vBnum         = est-prep.b-num
              ttCorrPrep.vCode         = est-prep.CODE
              ttCorrPrep.vQty          = est-prep.qty
              ttCorrPrep.vDesc         = est-prep.dscr
              ttCorrPrep.vCost         = est-prep.cost
              ttCorrPrep.vMl           = IF  est-prep.ml = TRUE THEN "M" ELSE "L"
              ttCorrPrep.vSimon        = est-prep.simon
              ttCorrPrep.vMark         = est-prep.mkup
              ttCorrPrep.vAmort        = est-prep.amtz 
              ttCorrPrep.vLine         = est-prep.LINE .
             
                END. 

  END. /* end of for each*/
 END. /* end of select*/

IF prmAction = "jobstd" THEN DO:
 FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    IF AVAIL est THEN
  FOR EACH job-hdr
      WHERE job-hdr.company EQ est.company
        AND job-hdr.est-no  EQ est.est-no
      NO-LOCK,
      FIRST job
      where job.company EQ job-hdr.company
        and job.job     EQ job-hdr.job
        and job.job-no  EQ job-hdr.job-no
        and job.job-no2 EQ job-hdr.job-no2
        and job.est-no  EQ job-hdr.est-no
        AND job.opened  EQ YES
      NO-LOCK
      BREAK BY job.job:
      
    IF LAST(job.job) OR job-hdr.ord-no EQ est.ord-no THEN DO:
      RUN jc/jobstds.p (ROWID(job)).
       fil_id = RECID(job).
       LEAVE.
    END.
    END.
    
    IF vError <> "" THEN DO:
        ASSIGN cError = vError .
        RETURN.
    END.
   

    RUN CorrugatedBox/Corrdo-po.p   .

    IF vLogicError <> "" THEN DO:
        ASSIGN
            cLogicError = vLogicError .
            /*RETURN.*/
    END.    
   
 
  ASSIGN
      prmAction = "View".

END.  /*** jobstd**/


IF prmAction = "PrepAdd"  THEN DO:
    
  FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
  FOR EACH bf-prep
      WHERE bf-prep.company EQ est.company  AND bf-prep.est-no  EQ est.est-no                      
        USE-INDEX est-qty NO-LOCK  BY bf-prep.line DESC:
    LEAVE.
  END.
  li-next = (IF AVAIL bf-prep THEN bf-prep.line ELSE 0) + 1.

    CREATE est-prep .
    ASSIGN
        est-prep.e-num   = est.e-num
        est-prep.company = est.company
        est-prep.est-no  = est.est-no
        est-prep.line    = li-next
        est-prep.s-num   = prmSnum
        est-prep.b-num   = prmBnum
        est-prep.CODE    = prmCode
        est-prep.qty     =  prmQty
        est-prep.dscr    = prmDesc
        est-prep.cost    = prmCost
        est-prep.ml      = IF  prmMl = "M" THEN TRUE ELSE FALSE
        est-prep.simon   = prmSimon
        est-prep.mkup    = prmMark
        est-prep.amtz    = prmAmort .


     find first prep where prep.company eq est.company 
         and prep.loc     eq est.loc
         and prep.code    eq est-prep.code
         no-lock no-error.
     IF AVAIL prep THEN est-prep.mat-type = prep.mat-type.
     
     IF est-prep.mat-type = "P" AND est.ord-no <> 0 AND est-prep.simon   EQ "S"
         AND est-prep.amtz    EQ 100 
         THEN  RUN create-reft4plate.

     ASSIGN
        prmLine   = li-next
        prmAction = "View".
     
END.





IF prmAction = "PrepUpdate" THEN DO:
   
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST est-prep WHERE est-prep.company = est.company AND est-prep.est-no = est.est-no AND est-prep.LINE = prmLine EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL est-prep  THEN DO:
       ASSIGN
           est-prep.s-num   = prmSnum
           est-prep.b-num   = prmBnum
           est-prep.CODE    = prmCode
           est-prep.qty     =  prmQty
           est-prep.dscr    = prmDesc
           est-prep.cost    = prmCost
           est-prep.ml      = IF  prmMl = "M" THEN TRUE ELSE FALSE
           est-prep.simon   = prmSimon
           est-prep.mkup    = prmMark
           est-prep.amtz    = prmAmort
           .

   END.
              ASSIGN prmAction = "View".
 
 END.
          

 IF prmAction = "PrepDelete" THEN DO:
     FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
     FIND FIRST  est-prep WHERE est-prep.company = est.company AND est-prep.est-no = est.est-no AND est-prep.LINE = prmLine EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL est-prep  THEN DO:
         DELETE est-prep.
     END.
     FIND LAST est-prep WHERE est-prep.company = est.company AND est-prep.est-no = est.est-no NO-LOCK NO-ERROR.
     IF AVAIL est-prep THEN do: 
         ASSIGN
         prmLine = est-prep.LINE 
         prmAction = "View"   .
     END.
 END.


 
 IF prmAction = "View" THEN DO:
     
 FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
  FOR EACH est-prep WHERE est-prep.company = est.company AND est-prep.est-no = est.est-no AND est-prep.LINE = prmLine NO-LOCK :
      IF AVAIL est-prep THEN DO:
          CREATE ttCorrPrep.
          ASSIGN
              
              ttCorrPrep.vSnum         = est-prep.s-num
              ttCorrPrep.vBnum         = est-prep.b-num
              ttCorrPrep.vCode         = est-prep.CODE
              ttCorrPrep.vQty          = est-prep.qty
              ttCorrPrep.vDesc         = est-prep.dscr
              ttCorrPrep.vCost         = est-prep.cost
              ttCorrPrep.vMl           = IF  est-prep.ml = TRUE THEN "M" ELSE "L"
              ttCorrPrep.vSimon        = est-prep.simon
              ttCorrPrep.vMark         = est-prep.mkup
              ttCorrPrep.vAmort        = est-prep.amtz 
              ttCorrPrep.vLine        = est-prep.LINE.
             
                END. /* end ttCorrPrepof est-prep*/
  END. /* end of for each*/
 END. /* end of view*/
IF prmActSelect = "EstSelect" THEN DO:
    
 FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST eb WHERE eb.est-no =  vEstimate AND eb.company = prmComp AND eb.form-no = prmFormNo AND eb.blank-no = prmBlank NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ vEstimate
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND CURRENT est EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.
   
    /*DISABLE TRIGGERS FOR LOAD OF ef.*/
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ prmFormNo
        NO-LOCK NO-ERROR.
    IF AVAIL ef THEN DO:
      FOR EACH b-eb
          WHERE b-eb.company EQ ef.company
            AND b-eb.est-no  EQ ef.est-no
            AND b-eb.form-no EQ ef.form-no
          NO-LOCK:
        bi = bi + 1.
      END.
      IF li NE ef.blank-qty THEN DO:
        FIND CURRENT ef EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ef THEN ef.blank-qty = bi.
        FIND CURRENT ef NO-LOCK.
      END.
    END.
  
    CREATE ttCorrPrep.
             ASSIGN
                 ttCorrPrep.vEstNum        = est.est-no
                 ttCorrPrep.vCustPart      = eb.part-no
                 ttCorrPrep.vEstDate       = est.est-date
                 ttCorrPrep.vFormNo        = eb.form-no
                 ttCorrPrep.vFormQty       = est.form-qty
                 ttCorrPrep.vBlankNo       = eb.blank-no
                 ttCorrPrep.vBlankQty      = bi  
                 ttCorrPrep.vOrder         = est.ord-no  
                 .
            
END.


/**********************procedure*******************************/


PROCEDURE create-reft4plate :

  DEF VAR lv-prep-cnt AS INT NO-UNDO.
  DEF VAR lv-returnc AS cha NO-UNDO.
  DEF VAR lv-form# AS INT NO-UNDO.
  DEF VAR lv-line# AS INT NO-UNDO.
  DEF VAR lv-eqty AS INT NO-UNDO.

  FIND FIRST oe-ordm WHERE oe-ordm.company = est.company
                 AND oe-ordm.ord-no = est.ord-no
                 AND oe-ordm.charge = est-prep.CODE NO-LOCK NO-ERROR.
  IF AVAIL oe-ordm AND 
     NOT can-find(FIRST reftable
                  WHERE reftable.reftable EQ "oe/ordlmisc.p"
                    AND reftable.company  EQ oe-ordm.company
                    AND reftable.loc      EQ STRING(oe-ordm.ord-no,"9999999999")
                    AND reftable.code     EQ STRING(oe-ordm.line,"9999999999")
                    AND reftable.code2    EQ oe-ordm.charge
                    AND reftable.val[1] = 1)
  THEN DO:      
      CREATE reftable.
      ASSIGN reftable.reftable = "oe/ordlmisc.p"
             reftable.company  = oe-ordm.company
             reftable.loc      = STRING(oe-ordm.ord-no,"9999999999")
             reftable.code     = STRING(oe-ordm.line,"9999999999")
             reftable.code2    = oe-ordm.charge
             reftable.val[1] = 1
             reftable.val[2]   = est-prep.eqty
             reftable.val[3]   = est-prep.line
             reftable.dscr     = est-prep.est-no.    

  END.
END PROCEDURE.


PROCEDURE valid-s-num :

END PROCEDURE.


PROCEDURE valid-b-num :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.
  
END PROCEDURE.


PROCEDURE valid-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  
    
END PROCEDURE.


PROCEDURE valid-simon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                              
  

END PROCEDURE.
