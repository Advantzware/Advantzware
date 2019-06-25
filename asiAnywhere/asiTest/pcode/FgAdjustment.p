/*------------------------------------------------------------------------
    File         : FgAdjustment
    Purpose      : Fg Adjustment
    Syntax       :

    Description : Return a Dataset of all Fg Adjustment

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttFgAdjustment NO-UNDO
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
DEFINE DATASET dsFgAdjustment FOR ttFgAdjustment.

    DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmActFgAdj  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSeq          AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmTag          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmRecDate      AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmPo           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmitemno       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmjob          AS CHAR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFgAdjustment.
DEFINE VAR prmComp AS CHAR NO-UNDO.

    IF   prmActFgAdj = ? THEN ASSIGN prmActFgAdj = "".
    IF   prmSeq         = ? THEN ASSIGN     prmSeq     = 0.   
    IF   prmTag         = ? THEN ASSIGN     prmTag     = "".   
    IF   prmPo          = ? THEN ASSIGN     prmPo      = "".   
    IF   prmitemno      = ? THEN ASSIGN     prmitemno  = "".   
    IF   prmjob         = ? THEN ASSIGN     prmjob     = "".

FIND FIRST usercomp WHERE 
    usercomp.user_id = prmUser AND
    usercomp.loc = '' AND
    usercomp.company_default = YES NO-LOCK NO-ERROR.
prmComp =  IF AVAIL usercomp THEN usercomp.company ELSE "001".
DEFINE VAR vjob AS CHAR NO-UNDO.
FOR EACH ttFgAdjustment:
    DELETE ttFgAdjustment.
END.

/* ********************  Preprocessor Definitions  ******************** */

IF prmActFgAdj = "SearchFgAdjust"  THEN DO:
MESSAGE "check" prmUser prmActFgAdj prmSeq prmTag prmRecDate prmPo prmitemno vjob.
  ASSIGN vjob =  FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).
FOR EACH fg-rctd WHERE fg-rctd.company = prmComp  /*AND  fg-rctd.rita-code = "A" */
     AND(fg-rctd.r-no = prmSeq OR prmSeq = 0)
     AND(fg-rctd.tag BEGINS prmTag OR fg-rctd.tag = prmTag OR prmTag = "")
     AND(fg-rctd.rct-date  >= prmRecDate OR prmRecDate = ?)
     AND(fg-rctd.cust-no BEGINS prmPo OR fg-rctd.cust-no = prmPo OR prmPo = "")
     AND(fg-rctd.i-no BEGINS prmitemno OR fg-rctd.i-no = prmitemno OR prmitemno = "")
     /*AND(fg-rctd.job-no = vjob OR prmjob = "") */       NO-LOCK:  /**/

   FIND FIRST  reftable WHERE reftable.reftable EQ "fg-rctd.user-id" 
        AND reftable.company  EQ fg-rctd.company 
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") NO-LOCK NO-ERROR.

             CREATE ttFgAdjustment.
                 ASSIGN 
                 ttFgAdjustment.vSeq              = fg-rctd.r-no
                 ttFgAdjustment.vAdjustmentDate   = fg-rctd.rct-date
                 ttFgAdjustment.vitemno           = fg-rctd.i-no
                 ttFgAdjustment.vitemname         = fg-rctd.i-name
                 ttFgAdjustment.vjob1             = fg-rctd.job-no
                 ttFgAdjustment.vjob2             = fg-rctd.job-no2
                 ttFgAdjustment.vwhse             = fg-rctd.loc
                 ttFgAdjustment.vbin              = fg-rctd.loc-bin
                 ttFgAdjustment.vtag              = fg-rctd.tag
                 ttFgAdjustment.vcustomer         = fg-rctd.cust-no
                 ttFgAdjustment.vunits            = fg-rctd.cases
                 ttFgAdjustment.vqtyunit          = fg-rctd.qty-case
                 ttFgAdjustment.vpartial          = fg-rctd.partial
                 ttFgAdjustment.vtotalqty         = fg-rctd.t-qty
                 ttFgAdjustment.vcost             = fg-rctd.ext-cost
                 ttFgAdjustment.vcreatedby        = reftable.code
                 ttFgAdjustment.vlastupdated      = reftable.code2
                    .
END.
END.

/************************************************************************************/

IF prmActFgAdj = "SelectFgAdjust"  THEN DO:
    FOR EACH usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id NO-LOCK :
        FOR EACH fg-rctd WHERE fg-rctd.company = prmComp   AND fg-rctd.rita-code = "A" NO-LOCK:
         FIND FIRST  reftable WHERE reftable.reftable EQ "fg-rctd.user-id" 
        AND reftable.company  EQ fg-rctd.company 
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") NO-LOCK NO-ERROR.
         FIND FIRST ttFgAdjustment WHERE ttFgAdjustment.vSeq   = fg-rctd.r-no NO-LOCK NO-ERROR.
         IF AVAIL ttFgAdjustment THEN NEXT.
CREATE ttFgAdjustment.
                 ASSIGN 
                 ttFgAdjustment.vSeq              = fg-rctd.r-no
                 ttFgAdjustment.vAdjustmentDate   = fg-rctd.rct-date
                 ttFgAdjustment.vitemno           = fg-rctd.i-no
                 ttFgAdjustment.vitemname         = fg-rctd.i-name
                 ttFgAdjustment.vjob1             = fg-rctd.job-no
                 ttFgAdjustment.vjob2             = fg-rctd.job-no2
                 ttFgAdjustment.vwhse             = fg-rctd.loc
                 ttFgAdjustment.vbin              = fg-rctd.loc-bin
                 ttFgAdjustment.vtag              = fg-rctd.tag
                 ttFgAdjustment.vcustomer         = fg-rctd.cust-no
                 ttFgAdjustment.vunits            = fg-rctd.cases
                 ttFgAdjustment.vqtyunit          = fg-rctd.qty-case
                 ttFgAdjustment.vpartial          = fg-rctd.partial
                 ttFgAdjustment.vtotalqty         = fg-rctd.t-qty
                 ttFgAdjustment.vcost             = fg-rctd.ext-cost
                 ttFgAdjustment.vcreatedby        = reftable.code
                 ttFgAdjustment.vlastupdated      = reftable.code2
                    .
END.     /*FOR EACH fg-rctd*/
    END.  /* for each usercust*/
END.    /*IF prmActFgAdj = "SelectFgAdjust"  THEN DO:*/
