/*------------------------------------------------------------------------
    File        : FgViewAdjustment.p
    Purpose     : View Adjustment

    Syntax      :

    Description : Return a Dataset to view adjustment

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttFgViewAdjustment 
    FIELD abc              AS CHAR
    FIELD vSeq             AS INTEGER FORMAT ">>>>>>>>"
    FIELD vAdjustmentDate  AS DATE FORMAT "99/99/9999"
    FIELD vitemno          AS CHAR FORMAT "X(15)"
    FIELD vitemname        AS CHAR FORMAT "X(30)"
    FIELD vjob1            AS CHAR FORMAT "X(6)"
    FIELD vjob2            AS INTEGER FORMAT "99"
    FIELD vwhse            AS CHAR FORMAT "X(5)"
    FIELD vbin             AS CHAR FORMAT "X(8)"
    FIELD vtag             AS CHAR FORMAT "X(20)"
    FIELD vcustomer        AS CHAR FORMAT "X(8)"
    FIELD vunits           AS INTEGER FORMAT "->>>,>>9"
    FIELD vqtyunit         AS INTEGER FORMAT ">>>,>>9"
    FIELD vpartial         AS INTEGER FORMAT "->>>,>>9"
    FIELD vtotalqty        AS INTEGER FORMAT "->,>>>,>>>,>>9"
    FIELD vcost            AS DECIMAL FORMAT "->>>,>>9.99<<"
    FIELD vcreatedby       AS CHAR FORMAT "x(8)"
    FIELD vlastupdated     AS CHAR FORMAT "x(8)"
    .
DEFINE DATASET dsFgViewAdjustment FOR ttFgViewAdjustment.


    DEFINE INPUT PARAMETER prmUser            AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmFgViewAct       AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmSeq             AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmAdjustmentDate  AS DATE    NO-UNDO.
    DEFINE INPUT PARAMETER prmitemno          AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmitemname        AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmjob1            AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmjob2            AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmwhse            AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmbin             AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmtag             AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmcustomer        AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmunits           AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmqtyunit         AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmpartial         AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmtotalqty        AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmcost            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmcreatedby       AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER prmlastupdated     AS CHAR    NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET  FOR dsFgViewAdjustment.   
    
    IF prmUser      = ?     THEN ASSIGN   prmUser          = "".
    IF prmFgViewAct = ?     THEN ASSIGN   prmFgViewAct     = "".
    IF prmSeq = ?           THEN ASSIGN   prmSeq           = 0.
    IF prmitemno    = ?     THEN ASSIGN   prmitemno        = "".
    IF prmitemname  = ?     THEN ASSIGN   prmitemname      = "".
    IF prmjob1      = ?     THEN ASSIGN   prmjob1          = "".
    IF prmjob2      = ?     THEN ASSIGN   prmjob2          = 0.
    IF prmwhse      = ?     THEN ASSIGN   prmwhse          = "".
    IF prmbin       = ?     THEN ASSIGN   prmbin           = "".
    IF prmtag       = ?     THEN ASSIGN   prmtag           = "".
    IF prmcustomer  = ?     THEN ASSIGN   prmcustomer      = "".
    IF prmunits     = ?     THEN ASSIGN   prmunits         = 0.
    IF prmqtyunit   = ?     THEN ASSIGN   prmqtyunit       = 0.
    IF prmpartial   = ?     THEN ASSIGN   prmpartial       = 0.
    IF prmtotalqty  = ?     THEN ASSIGN   prmtotalqty      = 0.
    IF prmcost      = ?     THEN ASSIGN   prmcost          = 0.
    IF prmcreatedby    = ?  THEN ASSIGN   prmcreatedby     = "".
    IF prmlastupdated  = ?  THEN ASSIGN   prmlastupdated   = "".
 DEFINE VAR cocode AS CHAR NO-UNDO.
 DEFINE VAR locode AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.   
DEF VAR lv-fgrecpt-val AS INT NO-UNDO.
def var ld-cost as decimal no-undo.
def var lv-uom as char no-undo.
 DEF VAR totqty  AS CHAR NO-UNDO.
 DEF VAR totcost  AS CHAR NO-UNDO.
DEF VAR vjob AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

 DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.



prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
    cocode = prmComp
    locode = usercomp.loc 
    vjob =  FILL(" ",6 - LENGTH(TRIM(prmJob1))) + TRIM(prmJob1).
DO TRANSACTION:
  {sys/inc/fgrecpt.i}
  lv-fgrecpt-val = sys-ctrl.int-fld.
END.
ASSIGN
   ld-cost = 0
   lv-uom  = "EA".
/***********************************************************************************************/
IF prmFgViewAct = "Add" OR  prmFgViewAct  = "Update" THEN DO:
FIND FIRST itemfg WHERE itemfg.company = prmComp  
                      AND itemfg.i-no = prmitemno NO-LOCK NO-ERROR.
IF NOT AVAIL itemfg THEN  DO:
      cError = "Invalid Item Num entry, try help..." .
      RETURN.
      
END.   /*valid i-no*/

/*---------------------valid job-no-----------------------------------------------*/
FIND FIRST fg-bin WHERE fg-bin.company  = prmComp
                      /*AND fg-bin.i-no     EQ prmItemno*/
                      AND fg-bin.job-no  EQ vjob NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN  DO:
      cError = "Invalid Job Num entry, try help..." .
      RETURN.
      
END.   /*valid job-no*/
/*-------------------------valid job2-----------------------------------------------*/
FIND FIRST fg-bin WHERE fg-bin.company  = prmComp
                      /*AND fg-bin.i-no    = prmItemno*/
                      AND fg-bin.job-no  = vjob  
                      AND fg-bin.job-no2 = prmJob2  NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN  DO:
      cError = "Invalid job-no2 entry, try help..." .
      RETURN.
     
END.   /*valid job-no*/
/*------------------------------------------------------------------------------*/

FIND FIRST loc WHERE loc.company = prmComp
                        AND loc.loc = prmwhse
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          cError = "Invalid Warehouse. Try Help. ".
          RETURN .
          
  END.
  
  FIND FIRST fg-bin WHERE fg-bin.company = prmComp 
                      AND fg-bin.i-no = ""
                      AND fg-bin.loc = prmwhse
                      AND fg-bin.loc-bin = prmbin
                      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
          cError =  "Invalid Bin#. Try Help. " .
         RETURN.
         
  END.
  /****------------------------------------------------------------------------****/

/*------------------------------------------------------------------------------*/
FIND FIRST fg-bin WHERE fg-bin.company   = prmComp
                      /*AND fg-bin.i-no    = prmItemno*/
                      AND fg-bin.tag     = prmtag
                      NO-LOCK NO-ERROR.

    IF NOT AVAIL fg-bin THEN  DO:
      cError = "Invalid Tag entry, try help..." .
      RETURN.
      
END.   /*valid job-no*/
RUN valid-tag.
/*------------------------------------------------------------------------------*/
FIND FIRST fg-bin WHERE fg-bin.company   = prmComp
                      /*AND fg-bin.i-no    = prmItemno*/
                      AND fg-bin.cust-no  = prmcustomer
                      NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN  DO:
      cError = "Invalid Customer entry, try help..." .
      RETURN.
     
END.   /*valid job-no*/
/*------------------------------------------------------------------------------*/



END.   /*if prmFgViewAct = "ADD"**/
     
/*************************************Delete**********************************/

IF prmFgViewAct = "Delete" THEN DO:

    MESSAGE "deletecheck" prmComp.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.company    = prmComp AND b-fg-rctd.r-no = prmSeq EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN DO:
 /*FIND FIRST reftable WHERE reftable.company = prmComp AND reftable.loc EQ STRING(b-fg-rctd.r-no,"9999999999") EXCLUSIVE-LOCK NO-ERROR.
 IF AVAIL reftable  THEN
     DELETE reftable.*/
        DELETE b-fg-rctd.
    END.

    FIND LAST fg-rctd WHERE fg-rctd.company = prmComp   NO-LOCK NO-ERROR.
    ASSIGN 
            prmSeq = fg-rctd.r-no
            prmFgViewAct = "View".
END.
/********************************End Delete*************************************/

/*************************************Add**********************************/

IF prmFgViewAct = "Add" THEN DO:
    MESSAGE "add1"  prmFgViewAct.
  FIND LAST b-fg-rctd USE-INDEX fg-rctd WHERE b-fg-rctd.company = prmComp NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd THEN DO:
      ASSIGN lv-rno = b-fg-rctd.r-no + 1.
  END.
  ELSE DO:
      ASSIGN lv-rno = 1.
  END.
   FIND FIRST fg-bin WHERE fg-bin.company = prmComp AND fg-bin.i-no = prmitemno NO-LOCK NO-ERROR.
   ASSIGN
       ld-cost    = fg-bin.std-tot-cost
       lv-uom     = fg-bin.pur-uom.
  ASSIGN totqty = STRING(INT(prmunits) * INT(prmqtyunit) + INT(prmpartial))
  totcost =
       STRING(INT(totqty) /
              (IF lv-uom EQ "M" THEN 1000 ELSE 1) * ld-cost) 
          prmtotalqty  = INT(totqty)  
          prmcost      = DEC(totcost)        
               .
  CREATE fg-rctd NO-ERROR.
  assign fg-rctd.company    = prmComp
         fg-rctd.r-no       = lv-rno
         fg-rctd.rita-code  = "A"
         fg-rctd.i-no       = prmitemno
         fg-rctd.i-name     = prmitemname
         fg-rctd.job-no     = prmjob1
         fg-rctd.job-no2    = prmjob2 
         fg-rctd.loc        = prmwhse
         fg-rctd.loc-bin    = prmbin
         fg-rctd.tag        = prmtag
         fg-rctd.cust-no    = prmcustomer 
         fg-rctd.cases      = prmunits
         fg-rctd.qty-case   = prmqtyunit 
         fg-rctd.partial    = prmpartial
         fg-rctd.t-qty      = prmtotalqty
         fg-rctd.ext-cost   = prmcost
         fg-rctd.rct-date     = TODAY
         fg-rctd.s-num        = 0
         fg-rctd.units-pallet = 1
         fg-rctd.cases-unit   = 1
       .
  MESSAGE "add2" prmComp lv-rno prmitemno.
  FIND FIRST fg-rctd WHERE fg-rctd.company  = prmComp AND  fg-rctd.r-no   = lv-rno NO-LOCK NO-ERROR.
  MESSAGE "Add3"  fg-rctd.company fg-rctd.r-no     fg-rctd.rita-code fg-rctd.i-no     fg-rctd.i-name   fg-rctd.job-no   fg-rctd.job-no2  
                  fg-rctd.loc      
                  fg-rctd.loc-bin  
                  fg-rctd.tag   .     

     /*CREATE reftable.
     ASSIGN
         reftable.reftable = "fg-rctd.user-id"
         reftable.company = prmComp
         reftable.loc = string(lv-rno)
         reftable.CODE = USERID("nosweat")
         reftable.code2 = USERID("nosweat").
         reftable.rec_key = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999").*/
  
ASSIGN 
    prmSeq = lv-rno
    prmFgViewAct= "View"
    .
END.
/*******************************End Add***************************************/

/*************************************Update**********************************/

IF prmFgViewAct = "Update" THEN DO:
MESSAGE "updatecheck" prmFgViewAct prmComp prmitemno prmitemname prmjob1 prmwhse prmbin prmtag prmcustomer prmunits prmqtyunit prmpartial.
    FIND FIRST fg-rctd WHERE fg-rctd.company    = prmComp AND fg-rctd.r-no = prmSeq EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST fg-bin WHERE fg-bin.company = prmComp AND fg-bin.i-no = prmitemno NO-LOCK NO-ERROR.
   ASSIGN
       ld-cost    = fg-bin.std-tot-cost
       lv-uom     = fg-bin.pur-uom.
  ASSIGN totqty = STRING(INT(prmunits) * INT(prmqtyunit) + INT(prmpartial))
  totcost =
       STRING(INT(totqty) /
              (IF lv-uom EQ "M" THEN 1000 ELSE 1) * ld-cost) 
          prmtotalqty  = INT(totqty)  
          prmcost      = DEC(totcost)        
               .  
         assign fg-rctd.company    = prmComp    
            
            fg-rctd.rita-code  = "A"        
            fg-rctd.i-no       = prmitemno  
            fg-rctd.i-name     = prmitemname
            fg-rctd.job-no     = prmjob1    
            fg-rctd.job-no2    = prmjob2    
            fg-rctd.loc        = prmwhse    
            fg-rctd.loc-bin    = prmbin     
            fg-rctd.tag        = prmtag     
            fg-rctd.cust-no    = prmcustomer
            fg-rctd.cases      = prmunits   
            fg-rctd.qty-case   = prmqtyunit 
            fg-rctd.partial    = prmpartial
                    
            .
        /* ASSIGN
   reftable.code2        = USERID("nosweat")
   {&TABLENAME}.upd-date = TODAY
   {&TABLENAME}.upd-time = TIME.
          */
ASSIGN prmFgViewAct = "View".

END.
/******************************End Update*************************************/

/*************************************Select**********************************/

IF prmFgViewAct = "View" THEN DO:
    FIND FIRST fg-rctd WHERE fg-rctd.company = prmComp AND fg-rctd.r-no = prmSeq NO-LOCK NO-ERROR.
     FIND FIRST  reftable WHERE reftable.reftable EQ "fg-rctd.user-id" 
        AND reftable.company  EQ fg-rctd.company 
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") NO-LOCK NO-ERROR.
    CREATE ttFgViewAdjustment.
    ASSIGN
         ttFgViewAdjustment.vSeq              = fg-rctd.r-no     
         ttFgViewAdjustment.vAdjustmentDate   = fg-rctd.rct-date 
         ttFgViewAdjustment.vitemno           = fg-rctd.i-no     
         ttFgViewAdjustment.vitemname         = fg-rctd.i-name   
         ttFgViewAdjustment.vjob1             = fg-rctd.job-no   
         ttFgViewAdjustment.vjob2             = fg-rctd.job-no2  
         ttFgViewAdjustment.vwhse             = fg-rctd.loc      
         ttFgViewAdjustment.vbin              = fg-rctd.loc-bin  
         ttFgViewAdjustment.vtag              = fg-rctd.tag      
         ttFgViewAdjustment.vcustomer         = fg-rctd.cust-no  
         ttFgViewAdjustment.vunits            = fg-rctd.cases    
         ttFgViewAdjustment.vqtyunit          = fg-rctd.qty-case 
         ttFgViewAdjustment.vpartial          = fg-rctd.partial  
         ttFgViewAdjustment.vtotalqty         = fg-rctd.t-qty    
         ttFgViewAdjustment.vcost             = fg-rctd.ext-cost 
         ttFgViewAdjustment.vcreatedby        = reftable.CODE
         ttFgViewAdjustment.vlastupdated      = reftable.code2  .
END.
/*****************************End Select******************************************/
PROCEDURE valid-tag :

  IF lv-fgrecpt-val = 1 THEN DO:
     FIND FIRST loadtag WHERE loadtag.company = prmComp
                          AND loadtag.item-type = NO
                          AND loadtag.i-no = prmitemno
                          AND loadtag.tag-no = prmTag NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag THEN DO:
        cError =  "Invalid Tag#. Try help or Scan valid tag#..." .
        RETURN .
ASSIGN prmFgViewAct = "View".
     END.
  END.
END PROCEDURE.

/**********************************************************************************************/


