/*------------------------------------------------------------------------
    File        : CorrQty.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 10  march 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttCorrQty NO-UNDO
    FIELD  vCorQty     AS INT 
    FIELD  vCorid      AS CHAR 
    FIELD  vType       AS INT
    
        .
DEFINE DATASET dsCorrQty FOR ttCorrQty.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmActSelect   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmQty         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmReckey      AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrQty.


IF prmUser       = ?  THEN ASSIGN    prmUser        = "".
IF prmAction     = ?  THEN ASSIGN    prmAction      = "".
IF prmComp       = ?  THEN ASSIGN    prmComp        = "".
IF prmEstNum     = ?  THEN ASSIGN    prmEstNum      = "".
IF prmFormNo     = ?  THEN ASSIGN    prmFormNo      = 0.
IF prmQty       = ?  THEN ASSIGN    prmQty        = 0.
IF prmActSelect  = ?  THEN ASSIGN    prmActSelect   = "".


 DEFINE VAR vEstimate AS CHAR NO-UNDO.
  DEF BUFFER b-eb FOR eb.          
  DEF BUFFER b-est-qty FOR est-qty.
  DEF BUFFER b-est-op FOR est-op.
  DEF VAR lv-eqty LIKE est-qty.eqty NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  def new shared var fil_id as recid no-undo.
  def new shared buffer xest for est.
  def new shared buffer xef for ef.
  def new shared buffer xeb for eb.
  DEF NEW   SHARED VAR cocode  AS CHAR NO-UNDO.
  DEF NEW   SHARED VAR locode  AS CHAR NO-UNDO.
  def  NEW   SHARED var  x  as   int no-undo.
  def  NEW   SHARED var  y  as   int no-undo.
  DEF  NEW   SHARED VAR  k  as   int no-undo.

def var i          as   int no-undo.
def var j          as   int no-undo.

def var z          as   int no-undo.
def var xxx        as   dec no-undo.
def var yyy        as   dec no-undo.
def var zzz        as   dec no-undo.
def var tmpstore   as   cha no-undo.

  def new shared var xcal    as de no-undo.
def new shared var sh-wid  as de no-undo.
def new shared var sh-len  as de no-undo.
def shared var head as ch format "x(78)" extent 20.
def shared var chosen as logical format "y/n".
 DEFINE VAR adm-adding-record AS CHAR NO-UNDO.
    DEFINE VAR adm-new-record   AS CHAR NO-UNDO.
    DEFINE VAR vEstType AS LOGICAL NO-UNDO.
 

    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
cocode = prmComp.
locode = "Main".
vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).


/*************************************prmAction***************************************************/  

IF prmAction = "QtyAdd"  THEN DO:
   
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est-qty WHERE est-qty.company = est.company AND est-qty.est-no = est.est-no  AND est-qty.eqty = prmQty  NO-LOCK NO-ERROR.
   
    FIND FIRST b-est-qty
                WHERE b-est-qty.company EQ est-qty.company
                  AND b-est-qty.est-no  EQ est-qty.est-no
                  AND b-est-qty.eqty    EQ prmQty  AND ROWID(b-est-qty)  NE ROWID(est-qty) NO-LOCK NO-ERROR.
    
   IF AVAIL b-est-qty THEN DO:
       ASSIGN
           cError = "Sorry, this quantity already exists..." .
         
              RETURN .
    END.
   

   IF  prmQty <> 0 THEN DO:
    FIND FIRST b-eb
        WHERE b-eb.company EQ est-qty.company
          AND b-eb.est-no  EQ est-qty.est-no
          AND (b-eb.eqty   EQ est-qty.eqty OR
               b-eb.eqty   EQ prmQty)  NO-LOCK NO-ERROR.
    IF AVAIL b-eb THEN DO:
        
        ASSIGN
            cError =  "Sorry, you may not change this quantity entered on the estimate..." .
      RETURN .
    END.
   END.
  


    
      END.  /* end of addvalidation */

IF prmAction = "QtyAdd"  THEN DO:
    adm-adding-record = "Yes".
    adm-new-record    = "Yes".
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    CREATE est-qty.
    ASSIGN
        est-qty.company = est.company
        est-qty.est-no  = est.est-no.
        est-qty.eqty    = prmQty .

   
    lv-eqty = est-qty.eqty.
        IF adm-new-record = "Yes" THEN DO:
    FOR EACH ef
        WHERE ef.company EQ est-qty.company
          AND ef.est-no  EQ est-qty.est-no
        NO-LOCK:
      
      RUN set-lock (ef.form-no, NO).
    END.

    FIND xest WHERE RECID(xest) EQ RECID(est).   

    FIND FIRST xef
        WHERE xef.company EQ est-qty.company 
          AND xef.est-no  EQ est-qty.est-no
        NO-LOCK NO-ERROR.
    FIND FIRST xeb
        WHERE xeb.company EQ est-qty.company 
          AND xeb.est-no  EQ est-qty.est-no
        NO-LOCK NO-ERROR.
    vEstType = IF xest.est-type = 8 THEN TRUE  ELSE FALSE .
    IF adm-adding-record = "Yes" THEN DO:
       
        IF xest.est-type EQ 1 THEN
        RUN ce/mach-seq.p (est-qty.eqty).
      ELSE
        RUN cec/mach-seq.p (0, est-qty.eqty, vEstType).
    END.

    ELSE DO:
      fil_id = ?.

      FOR EACH est-op
          WHERE est-op.company EQ est-qty.company
            AND est-op.est-no  EQ est-qty.est-no
            AND est-op.qty     EQ lv-eqty
          NO-LOCK:

        CREATE b-est-op.
        BUFFER-COPY est-op TO b-est-op
        ASSIGN
         b-est-op.qty = est-qty.eqty.

        fil_id = RECID(b-est-op).
      END.
      
      
                UPDATE ll = FALSE .
        IF ll THEN
          IF est.est-type EQ 1 THEN
            RUN ce/mach-rek.p (?).
          ELSE
            RUN cec/mach-rek.p (?).
      END.
    END.

    FIND xest WHERE RECID(xest) EQ RECID(est) NO-LOCK.

    FOR EACH ef
        WHERE ef.company EQ est-qty.company
          AND ef.est-no  EQ est-qty.est-no
        NO-LOCK:
      
      RUN set-lock (ef.form-no, YES).
    END.
  

/*  ELSE
  IF est-qty.eqty NE lv-eqty THEN
  FOR EACH est-op
      WHERE est-op.company EQ est-qty.company
        AND est-op.est-no  EQ est-qty.est-no
        AND est-op.qty     EQ lv-eqty:

    est-op.qty = est-qty.eqty.
  END.*/                     

      ASSIGN
          prmAction = "View"  .
      

END.  /****add a record****/

/*
IF prmAction = "QtyUpdate" THEN DO:
    MESSAGE "qty" vEstimate prmQty .
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FOR EACH est-qty WHERE est-qty.company = est.company AND est-qty.est-no = est.est-no NO-LOCK:
   
    FIND FIRST b-est-qty
                WHERE b-est-qty.company EQ est-qty.company
                  AND b-est-qty.est-no  EQ est-qty.est-no
                  AND b-est-qty.eqty    EQ prmQty  AND ROWID(b-est-qty)  NE ROWID(est-qty) NO-LOCK NO-ERROR.
   IF AVAIL b-est-qty THEN DO:
       ASSIGN
           cError = "Sorry, this quantity already exists..." .
              RETURN .
    END.
    END.

   IF  prmQty <> 0 THEN DO:
    FIND FIRST b-eb
        WHERE b-eb.company EQ est-qty.company
          AND b-eb.est-no  EQ est-qty.est-no
          AND (b-eb.eqty   EQ est-qty.eqty OR
               b-eb.eqty   EQ dec(prmQty))  NO-LOCK NO-ERROR.
    IF AVAIL b-eb THEN DO:
        
        ASSIGN
            cError =  "Sorry, you may not change this quantity entered on the estimate..." .
      RETURN .
    END.
   END.
  

END. /*end of validation*/

*/

IF prmAction = "QtyUpdate" THEN DO:
    
    adm-adding-record = "No".
    adm-new-record    = "Yes".
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = est.company AND est-qty.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
    
   ASSIGN
       lv-eqty = est-qty.eqty.

    ASSIGN
        est-qty.eqty    = prmQty .

    
   
    FOR EACH est-op
          WHERE est-op.company EQ est-qty.company
            AND est-op.est-no  EQ est-qty.est-no
            AND est-op.qty     EQ lv-eqty
          NO-LOCK:

        CREATE b-est-op.
        BUFFER-COPY est-op TO b-est-op
        ASSIGN
         b-est-op.qty = est-qty.eqty.

        fil_id = RECID(b-est-op).
      END.
    
 END.  /* update a record*/ 
          

 IF prmAction = "QtyDelete" THEN DO:
     
     FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
     FIND FIRST  est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = est.company AND est-qty.eqty = prmQty EXCLUSIVE-LOCK NO-ERROR.

     IF prmQty GT 0 THEN DO:
        
         FIND FIRST b-eb
             WHERE b-eb.company EQ est-qty.company
             AND b-eb.est-no  EQ est-qty.est-no
             AND b-eb.eqty    EQ est-qty.eqty  NO-LOCK NO-ERROR.
 
         IF AVAIL b-eb THEN DO:
             
             ASSIGN
             cError = "Sorry, you may not delete the quantity entered on the estimate..."  .
             
             RETURN .
           END.
         END.

       IF NOT AVAIL b-eb THEN DO:
          FOR EACH est-op 
           WHERE est-op.company EQ est-qty.company
           AND est-op.est-no  EQ est-qty.est-no
           AND est-op.qty     EQ prmQty EXCLUSIVE-LOCK:

           DELETE est-op.
        END.
        DELETE est-qty.
       END.
    FIND LAST  est-qty WHERE est-qty.est-no = vEstimate AND est-qty.company = prmComp AND est-qty.eqty = prmQty NO-LOCK NO-ERROR.
    IF AVAIL est-qty THEN DO:
        ASSIGN
            prmQty = est-qty.eqty
            prmAction = "View"   .

    END.


 END.


 IF prmActSelect = "Select" THEN DO:
     
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FOR EACH est-qty WHERE est-qty.company = est.company AND est-qty.est-no = est.est-no NO-LOCK:

      IF AVAIL est-qty THEN DO:
          CREATE ttCorrQty.
          ASSIGN
              ttCorrQty.vCorQty        = est-qty.eqty 
              ttCorrQty.vType        = est.est-type
              .
             
                END. /* end ttCorrQtyof est-qty*/
              
  END. /* end of for each*/
 END. /* end of select*/


 IF prmAction = "View" THEN DO:
     
 FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
 FOR EACH est-qty WHERE est-qty.company = est.company AND est-qty.est-no = est.est-no AND est-qty.eqty = prmQty NO-LOCK:
 
          CREATE ttCorrQty.
          ASSIGN
              
              ttCorrQty.vCorQty         = est-qty.eqty
              ttCorrQty.vCorid          = est-qty.rec_key
              ttCorrQty.vType           = est.est-type
              .
             
               
  END. /* end of for each*/
 END. /* end of view*/

/**********************procedure*******************************/


PROCEDURE set-lock :

  def input parameter ip-form-no like ef.form-no no-undo.
  def input parameter ip-op-lock like ef.op-lock no-undo.
  

  find first ef
      where ef.company eq est-qty.company
        and ef.est-no  eq est-qty.est-no
        and ef.form-no eq ip-form-no
      no-error.
  if avail ef then do:
    ef.op-lock = ip-op-lock.
    release ef.
  end.
  
END PROCEDURE.
