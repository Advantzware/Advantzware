

/*------------------------------------------------------------------------
    File        : BrwsCEstimate.p
    Purpose     : Corrugated Box
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTrnsFGrcpt NO-UNDO
        
        FIELD vDate           AS CHAR  
        FIELD vTransTime      AS CHAR   
        FIELD vTag            AS CHAR FORMAT "x(20)"
        FIELD vJobno          AS CHAR FORMAT "x(6)"  
        FIELD vJobno2         AS INT
        FIELD vItem           AS CHAR FORMAT "x(15)"
        FIELD vItemName       AS CHAR FORMAT "x(30)"
        FIELD vLoc            AS CHAR  FORMAT "x(5)" 
        FIELD vLocBin         AS CHAR  FORMAT "x(8)"  
        FIELD vCases          AS INT 
        FIELD vQtyCas         AS INT 
        FIELD vCasUnit        AS INT    
        FIELD vPartial        AS INT
        FIELD vcust           AS CHAR
        FIELD vLoc2           AS CHAR  FORMAT "x(5)" 
        FIELD vLocBin2        AS CHAR  FORMAT "x(8)"
        FIELD vTag2           AS CHAR FORMAT "x(20)"
        FIELD blan            AS INT
        FIELD usrcrt          AS CHAR
        FIELD usrupdt         AS CHAR
        FIELD vRecKey         AS CHAR 
        .


DEFINE DATASET dstrnsFGrcpt FOR ttTrnsFGrcpt .

DEFINE INPUT PARAMETER prmUser        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJobno       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPono        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqno       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTagno       AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmRecKey      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmOut            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmllSetParts   AS CHAR        NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dstrnsFGrcpt.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

IF prmUser        = ? THEN ASSIGN prmUser     = "".
IF prmAction      = ? THEN ASSIGN prmAction   = "Select".
IF prmFgItem      = ? THEN ASSIGN prmFgItem   = "".
IF prmJobno       = ? THEN ASSIGN prmJobno    = "".  
IF prmPono        = ? THEN ASSIGN prmPono     = "".
IF prmSeqno       = ? THEN ASSIGN prmSeqno    = "".
IF prmRcptDate    = ? THEN ASSIGN prmRcptDate = "".
IF prmTagno       = ? THEN ASSIGN prmTagno    = "".
IF prmRecKey      = ? THEN ASSIGN prmRecKey   = "".
IF prmllSetParts  = ? THEN ASSIGN prmllSetParts = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp .



   
   FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

                                                  

 IF prmAction = "Select" THEN DO:
  MESSAGE "test 1 " .
 
    FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.rita-code eq "T" NO-LOCK,
        EACH reftable WHERE reftable.company EQ fg-rctd.company 
        AND reftable.reftable EQ "fg-rctd.user-id"  
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") 
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") 
        AND reftable.code EQ prmUser
        AND reftable.reftable EQ "fg-rctd.user-id" 
        AND reftable.company  EQ fg-rctd.company 
        AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
        AND ( NOT reftable.dscr begins "fg-rctd: ") NO-LOCK :
       
       MESSAGE "test view ".      

            create ttTrnsFGrcpt.
            assign
                
                ttTrnsFGrcpt.vDate           = string(fg-rctd.rct-date)
                ttTrnsFGrcpt.vTransTime      = STRING(fg-rctd.trans-time,'HH:MM')
                ttTrnsFGrcpt.vTag            = fg-rctd.tag
                ttTrnsFGrcpt.vJobno          = fg-rctd.job-no
                ttTrnsFGrcpt.vJobno2         = fg-rctd.job-no2
                ttTrnsFGrcpt.vItem           = fg-rctd.i-no
                ttTrnsFGrcpt.vItemName       = fg-rctd.i-name
                ttTrnsFGrcpt.vLoc            = fg-rctd.loc 
                ttTrnsFGrcpt.vLocBin         = fg-rctd.loc-bin
                ttTrnsFGrcpt.vCases          = fg-rctd.cases 
                ttTrnsFGrcpt.vQtyCas         = fg-rctd.qty-case 
                ttTrnsFGrcpt.vCasUnit        = fg-rctd.cases-unit  
                ttTrnsFGrcpt.vPartial        = fg-rctd.partial
                ttTrnsFGrcpt.vcust           = fg-rctd.cust-no
                ttTrnsFGrcpt.vLoc2           = fg-rctd.loc2
                ttTrnsFGrcpt.vLocBin2        = fg-rctd.loc-bin2
                ttTrnsFGrcpt.vTag2           = fg-rctd.tag2
                ttTrnsFGrcpt.blan            = fg-rctd.b-num
                ttTrnsFGrcpt.usrcrt          = reftable.code
                ttTrnsFGrcpt.usrupdt         = reftable.code2
                ttTrnsFGrcpt.vRecKey         = fg-rctd.rec_key .

                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/
  
 IF prmAction = "Search" THEN DO:

 MESSAGE "search " prmTagno .
    FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.rita-code eq "T" AND
           (fg-rctd.i-no BEGINS prmFgItem OR prmFgItem = "" ) AND
           (fg-rctd.job-no = prmJobno OR prmJobno = "" ) AND
           (fg-rctd.rct-date = DATE(prmRcptDate) OR prmRcptDate = "" ) AND
           (fg-rctd.tag BEGINS prmTagno OR prmTagno = "" ) AND
           (fg-rctd.r-no = int(prmSeqno) OR prmSeqno = "" )   AND
           (fg-rctd.po-no BEGINS prmPono OR prmPono = "" ) NO-LOCK, 
        EACH reftable WHERE reftable.company EQ fg-rctd.company 
        AND reftable.reftable EQ "fg-rctd.user-id"  
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") 
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") 
        AND reftable.code EQ prmUser 
        AND reftable.reftable EQ "fg-rctd.user-id" 
        AND reftable.company  EQ fg-rctd.company 
        AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
        AND ( NOT reftable.dscr begins "fg-rctd: ") NO-LOCK :
       
             

            create ttTrnsFGrcpt.
            assign
                ttTrnsFGrcpt.vDate           = string(fg-rctd.rct-date)
                ttTrnsFGrcpt.vTransTime      = STRING(fg-rctd.trans-time,'HH:MM')
                ttTrnsFGrcpt.vTag            = fg-rctd.tag
                ttTrnsFGrcpt.vJobno          = fg-rctd.job-no
                ttTrnsFGrcpt.vJobno2         = fg-rctd.job-no2
                ttTrnsFGrcpt.vItem           = fg-rctd.i-no
                ttTrnsFGrcpt.vItemName       = fg-rctd.i-name
                ttTrnsFGrcpt.vLoc            = fg-rctd.loc 
                ttTrnsFGrcpt.vLocBin         = fg-rctd.loc-bin
                ttTrnsFGrcpt.vCases          = fg-rctd.cases 
                ttTrnsFGrcpt.vQtyCas         = fg-rctd.qty-case 
                ttTrnsFGrcpt.vCasUnit        = fg-rctd.cases-unit  
                ttTrnsFGrcpt.vPartial        = fg-rctd.partial
                ttTrnsFGrcpt.vcust           = fg-rctd.cust-no
                ttTrnsFGrcpt.vLoc2           = fg-rctd.loc2
                ttTrnsFGrcpt.vLocBin2        = fg-rctd.loc-bin2
                ttTrnsFGrcpt.vTag2           = fg-rctd.tag2
                ttTrnsFGrcpt.blan            = fg-rctd.b-num
                ttTrnsFGrcpt.usrcrt          = reftable.code
                ttTrnsFGrcpt.usrupdt         = reftable.code2
                ttTrnsFGrcpt.vRecKey         = fg-rctd.rec_key  .
                  
                END.   /* end of for loop*/
            
            
 END. /* end search */


