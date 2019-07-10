

/*------------------------------------------------------------------------
    File        : RfqItemSpec.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     :  Feb 18 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
DEFINE TEMP-TABLE ttRfqItemSpec NO-UNDO
    FIELD est_no LIKE rfqitem.est-no
    FIELD part_no LIKE rfqitem.part-no
     FIELD i_name LIKE rfqitem.i-name
     
     FIELD part_dscr1 LIKE rfqitem.part-dscr1
    FIELD part_dscr2 LIKE rfqitem.part-dscr2
    FIELD part_dscr3 LIKE rfqitem.part-dscr3
     FIELD procat LIKE rfqitem.procat
     FIELD stock_no  LIKE rfqitem.stock-no
     FIELD plate_no LIKE rfqitem.plate-no
     FIELD die_no LIKE rfqitem.die-no
     FIELD cad_no LIKE rfqitem.cad-no
     FIELD upc_no LIKE rfqitem.upc-no 
     FIELD spc_no LIKE rfqitem.spc-no 
     FIELD ProcatDscr LIKE fgcat.dscr
    . 


DEFINE DATASET dsRfqItemSpec FOR ttRfqItemSpec.
DEFINE INPUT PARAMETER prmAction       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER PrmPartNo     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmItemName   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmDscr       AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmDscr2      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmDscr3      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmStock      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPlate      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmDie        AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmSample     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUpc        AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmSpc        AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmCat        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCatdscr    AS CHAR  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqItemSpec.

IF  prmAction   = ? THEN ASSIGN  prmAction    = "".
IF  PrmUser     = ? THEN ASSIGN  PrmUser      = "".
IF  PrmRfqNo    = ? THEN ASSIGN  PrmRfqNo     = 0 .   
IF  PrmPartNo   = ? THEN ASSIGN  PrmPartNo    = "".   
IF  prmItemName = ? THEN ASSIGN  prmItemName  = "".  
IF  prmDscr     = ? THEN ASSIGN  prmDscr      = "".  
IF  prmDscr2    = ? THEN ASSIGN  prmDscr2     = "".  
IF  prmDscr3    = ? THEN ASSIGN  prmDscr3     = "".  
IF  prmEstimate = ? THEN ASSIGN  prmEstimate  = "" .  
IF  prmStock    = ? THEN ASSIGN  prmStock     = "".  
IF  prmPlate    = ? THEN ASSIGN  prmPlate     = "".  
IF  prmDie      = ? THEN ASSIGN  prmDie       = "".  
IF  prmSample   = ? THEN ASSIGN  prmSample    = "".  
IF  prmUpc      = ? THEN ASSIGN  prmUpc       = "".  
IF  prmSpc      = ? THEN ASSIGN  prmSpc       = ""  .  
IF  prmCat      = ? THEN ASSIGN  prmCat       = "".  
IF  prmCatdscr  = ? THEN ASSIGN  prmCatdscr   = "Select".

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.
DEFINE BUFFER bf-rfqitem FOR rfqitem.
    DEFINE BUFFER bf-fgcat FOR fgcat.
IF prmUser = ? THEN prmUser = "".

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
MESSAGE "hello" prmComp PrmRfqNo PrmPartNo prmEstimate prmItemName prmDscr prmDscr2 prmDscr3 prmCat prmStock prmPlate prmDie prmSample prmUpc prmSpc.
prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".
IF prmAction = "Update" THEN DO:
   FOR EACH bf-rfqitem WHERE
       bf-rfqitem.company EQ prmComp AND
       bf-rfqitem.loc EQ prmLoc AND
       bf-rfqitem.rfq-no = PrmRfqNo AND
       bf-rfqitem.seq = int(PrmPartNo)  EXCLUSIVE-LOCK:
       IF AVAIL bf-rfqitem THEN DO:
        assign
           bf-rfqitem.est-no        = prmEstimate   
           bf-rfqitem.part-no       = PrmPartNo   
           bf-rfqitem.i-name        = prmItemName     
           bf-rfqitem.part-dscr1    = prmDscr     
           bf-rfqitem.part-dscr2    = prmDscr2    
           bf-rfqitem.part-dscr3    = prmDscr3
           bf-rfqitem.procat        = prmCat
           bf-rfqitem.stock-no      = prmStock    
           bf-rfqitem.plate-no      = prmPlate    
           bf-rfqitem.die-no        = prmDie      
           bf-rfqitem.cad-no        = prmSample   
           bf-rfqitem.upc-no        = prmUpc      
           bf-rfqitem.spc-no        = prmSpc .         
       END.
        FIND bf-fgcat where bf-fgcat.company = bf-rfqitem.company and
                   bf-fgcat.procat = bf-rfqitem.procat EXCLUSIVE-LOCK no-error.
        ASSIGN 
            bf-fgcat.dscr = prmCatdscr .
RELEASE bf-rfqitem.
RELEASE bf-fgcat.

   END.   /*FOR EACHbf-rfqitem WHERE*/
   ASSIGN prmAction = "Select".
END.    /*IF prmAction = "Update" THEN DO:*/
IF prmAction = "Select" THEN DO:
   FOR EACH rfqitem WHERE
       rfqitem.company EQ prmComp AND
       rfqitem.loc EQ prmLoc AND
       rfqitem.rfq-no = PrmRfqNo AND
       rfqitem.seq = int(PrmPartNo)  NO-LOCK:
       CREATE ttRfqItemSpec.
       ASSIGN
           ttRfqItemSpec.est_no       = rfqitem.est-no
           ttRfqItemSpec.part_no      = rfqitem.part-no
           ttRfqItemSpec.i_name       = rfqitem.i-name
           ttRfqItemSpec.part_dscr1   = rfqitem.part-dscr1
           ttRfqItemSpec.part_dscr2   = rfqitem.part-dscr2
           ttRfqItemSpec.part_dscr3   = rfqitem.part-dscr3
           ttRfqItemSpec.procat       = rfqitem.procat
           ttRfqItemSpec.stock_no     = rfqitem.stock-no
           ttRfqItemSpec.plate_no     = rfqitem.plate-no
           ttRfqItemSpec.die_no       = rfqitem.die-no
           ttRfqItemSpec.cad_no       = rfqitem.cad-no
           ttRfqItemSpec.upc_no       = rfqitem.upc-no
           ttRfqItemSpec.spc_no       = rfqitem.spc-no
           .
       FIND fgcat where fgcat.company = rfqitem.company and
                    fgcat.procat = rfqitem.procat no-lock no-error.      
       ttRfqItemSpec.ProcatDscr = IF NOT AVAILABLE fgcat THEN "" ELSE fgcat.dscr.


   END.  /*FOR EACH rfqitem WHERE*/
    
END. /*IF prmAction = "Select" THEN DO:*/

