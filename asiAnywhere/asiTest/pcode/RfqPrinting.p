                                 
/*------------------------------------------------------------------------
    File        : RfqPrinting.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     : mon Feb 18 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


{RfqPrinting.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER RfqSeq        AS integer   NO-UNDO.   
DEFINE INPUT PARAMETER prmPcol       AS integer   NO-UNDO.   
DEFINE INPUT PARAMETER prmPass       AS integer   NO-UNDO.      
DEFINE INPUT PARAMETER prmCoat       AS integer   NO-UNDO.      
DEFINE INPUT PARAMETER prmColdscr    AS CHAR      NO-UNDO.
DEFINE INPUT PARAMETER prmIps1       AS integer   NO-UNDO.         
DEFINE INPUT PARAMETER prmIps2       AS integer   NO-UNDO.          
DEFINE INPUT PARAMETER prmIps3       AS integer   NO-UNDO.          
DEFINE INPUT PARAMETER prmIps4       AS integer   NO-UNDO.         
DEFINE INPUT PARAMETER prmIps5       AS integer   NO-UNDO.                      
DEFINE INPUT PARAMETER prmIps6       AS integer   NO-UNDO.                      
DEFINE INPUT PARAMETER prmIps7       AS integer   NO-UNDO.          
DEFINE INPUT PARAMETER prmvIps8       AS integer   NO-UNDO.         
DEFINE INPUT PARAMETER prmIps9       AS integer   NO-UNDO.                      
DEFINE INPUT PARAMETER prmIps10      AS integer   NO-UNDO.                      
DEFINE INPUT PARAMETER prmIcode1     AS CHAR  NO-UNDO.          
DEFINE INPUT PARAMETER prmIcode2     AS CHAR   NO-UNDO.         
DEFINE INPUT PARAMETER prmIcode3     AS CHAR    NO-UNDO.      
DEFINE INPUT PARAMETER prmIcode4     AS CHAR    NO-UNDO.      
DEFINE INPUT PARAMETER prmIcode5     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmIcode6     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmIcode7     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmIcode8     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmIcode9     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmIcode10    AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr1     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr2     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr3     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr4     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr5     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr6     AS CHAR    NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr7     AS CHAR     NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr8     AS CHAR     NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr9     AS CHAR     NO-UNDO. 
DEFINE INPUT PARAMETER prmCdscr10    AS CHAR     NO-UNDO. 
DEFINE INPUT PARAMETER prmIper1      AS integer  NO-UNDO.        
DEFINE INPUT PARAMETER prmIper2      AS integer  NO-UNDO.        
DEFINE INPUT PARAMETER prmIper3      AS integer  NO-UNDO.        
DEFINE INPUT PARAMETER prmIper4      AS integer  NO-UNDO.        
DEFINE INPUT PARAMETER prmIper5      AS integer  NO-UNDO.        
DEFINE INPUT PARAMETER prmIper6      AS integer  NO-UNDO.        
DEFINE INPUT PARAMETER prmIper7      AS integer  NO-UNDO.        
DEFINE INPUT PARAMETER prmIper8      AS integer  NO-UNDO. 
DEFINE INPUT PARAMETER prmIper9      AS integer  NO-UNDO. 
DEFINE INPUT PARAMETER prmIper10     AS integer  NO-UNDO. 
DEFINE INPUT PARAMETER RfqPRowid     AS RECID  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqPrinting.
    DEFINE OUTPUT PARAMETER cError     AS CHAR  NO-UNDO.

DEFINE BUFFER buff-rfqitem FOR rfqitem.

DEF VAR prmLoc AS CHAR NO-UNDO.


IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF PrmRfqNo  = ?  THEN ASSIGN prmRfqNo = 0.

IF  prmPcol    = ?  THEN ASSIGN        prmPcol  = 0.      
IF  prmPass    = ?   THEN ASSIGN       prmPass  = 0.   
IF  prmCoat     = ?  THEN ASSIGN       prmCoat  = 0.    
IF  prmColdscr  = ?   THEN ASSIGN      prmColdscr = "". 
IF  prmIps1     = ?  THEN ASSIGN       prmIps1  =  0.   
IF  prmIps2     = ?  THEN ASSIGN       prmIps2  =  0.   
IF  prmIps3     = ?   THEN ASSIGN      prmIps3  =  0.   
IF  prmIps4     = ?  THEN ASSIGN       prmIps4  =  0.   
IF  prmIps5     = ?   THEN ASSIGN      prmIps5   = 0.    
IF  prmIps6     = ?   THEN ASSIGN      prmIps6   = 0.    
IF  prmIps7     = ?  THEN ASSIGN       prmIps7   = 0.    
IF  prmvIps8    = ?  THEN ASSIGN       prmvIps8  = 0.    
IF  prmIps9     = ?   THEN ASSIGN      prmIps9    = 0.   
IF  prmIps10    = ?  THEN ASSIGN       prmIps10   = 0.   
IF  prmIcode1   = ?   THEN ASSIGN      prmIcode1  = "".   
IF  prmIcode2   = ?  THEN ASSIGN       prmIcode2  = "".   
IF  prmIcode3   = ?   THEN ASSIGN      prmIcode3  = "".   
IF  prmIcode4   = ?  THEN ASSIGN       prmIcode4  = "".   
IF  prmIcode5   = ?  THEN ASSIGN       prmIcode5  = "".   
IF  prmIcode6   = ?   THEN ASSIGN      prmIcode6  = "".   
IF  prmIcode7   = ?  THEN ASSIGN       prmIcode7  = "".   
IF  prmIcode8   = ?   THEN ASSIGN      prmIcode8  = "".   
IF  prmIcode9   = ?   THEN ASSIGN      prmIcode9  = "".   
IF  prmIcode10  = ?  THEN ASSIGN       prmIcode10 = "".    
IF  prmCdscr1   = ?  THEN ASSIGN       prmCdscr1  = "".  
IF  prmCdscr2   = ?   THEN ASSIGN      prmCdscr2  = "".  
IF  prmCdscr3   = ?  THEN ASSIGN       prmCdscr3  = "".  
IF  prmCdscr4   = ?   THEN ASSIGN      prmCdscr4  = "".  
IF  prmCdscr5   = ?  THEN ASSIGN       prmCdscr5  = "".                     
IF  prmCdscr6   = ?   THEN ASSIGN      prmCdscr6  = "".  
IF  prmCdscr7   = ?  THEN ASSIGN       prmCdscr7  = "".  
IF  prmCdscr8    = ?  THEN ASSIGN      prmCdscr8  = "".   
IF  prmCdscr9  = ?   THEN ASSIGN       prmCdscr9  = "".  
IF  prmCdscr10 = ?  THEN ASSIGN        prmCdscr10 = "".  
IF  prmIper1   = ?   THEN ASSIGN       prmIper1   = 0.   
IF  prmIper2   = ?   THEN ASSIGN       prmIper2   = 0.   
IF  prmIper3   = ?  THEN ASSIGN        prmIper3   = 0.   
IF  prmIper4   = ?  THEN ASSIGN        prmIper4   = 0.   
IF  prmIper5   = ?   THEN ASSIGN       prmIper5   = 0.  
IF  prmIper6   = ?  THEN ASSIGN        prmIper6   = 0.  
IF  prmIper7   = ?  THEN ASSIGN        prmIper7   = 0.  
IF  prmIper8   = ?   THEN ASSIGN       prmIper8   = 0.                  
IF  prmIper9   = ?   THEN ASSIGN       prmIper9   = 0. 
IF  prmIper10  = ?   THEN ASSIGN       prmIper10  = 0.   
                                      
  

IF RfqSeq  = ?  THEN ASSIGN RfqSeq = 0.

IF prmAction = ""  THEN ASSIGN prmAction = "Select".

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

/*************************************validation************************************************/
IF prmAction = "UpdateRfqPrinting" THEN DO:
    FIND FIRST ITEM WHERE 
                    (item.i-no = prmIcode1 OR prmIcode1 = "" ) NO-LOCK NO-ERROR.
    /*
         (item.mat-type = "V" OR ITEM.mat-type = "I") AND OR item.i-no = prmIcode2 OR item.i-no = prmIcode3 OR
                    item.i-no = prmIcode4 OR item.i-no = prmIcode5 OR item.i-no = prmIcode6 OR
                    item.i-no = prmIcode7 OR item.i-no = prmIcode8 OR item.i-no = prmIcode9 OR
                    item.i-no = prmIcode10 ) */
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code1!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode2 OR prmIcode2 = "" ) NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code2!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode3 OR prmIcode3 = "") NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code3!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode4 OR prmIcode4 = "" ) NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code4!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode5 OR prmIcode5 = "") NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code5!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode6 OR prmIcode6 = "") NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code6!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode7 OR prmIcode7 = "") NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code7!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode8 OR prmIcode8 = "") NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code8!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode9 OR prmIcode9 = "" ) NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code9!!!!!".
                        RETURN.
                    END.
                    FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                    (item.i-no = prmIcode10 OR prmIcode10 = "" ) NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ITEM  THEN DO:
                        ASSIGN
                        cError = "Invalid Code10!!!!!".
                        RETURN.
                    END.
END.

IF prmAction = "UpdateRfqPrinting" THEN DO:

IF prmIcode1 <> "" THEN DO:
        FIND FIRST ITEM WHERE  (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode1 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr1    =  ITEM.i-name. 
    END.
    IF prmIcode2 <> "" THEN DO:
    FIND FIRST ITEM WHERE  (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode2 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr2    =  ITEM.i-name. 
    END.
     IF prmIcode3 <> "" THEN DO:
           FIND FIRST ITEM WHERE  (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode3 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
            ASSIGN prmCdscr3    =  ITEM.i-name. 
     END.
     IF prmIcode4 <> "" THEN DO:
           FIND FIRST ITEM WHERE  (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode4 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr4    =  ITEM.i-name. 
     END.
     IF prmIcode5 <> "" THEN DO:
           FIND FIRST ITEM WHERE  (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode5 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr5    =  ITEM.i-name. 
     END.
     IF prmIcode6 <> "" THEN DO:
           FIND FIRST ITEM WHERE  (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode6 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr6    =  ITEM.i-name. 
     END.
     IF prmIcode7 <> "" THEN DO:
          FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode7 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr7    =  ITEM.i-name. 
     END.
     IF prmIcode8 <> "" THEN DO:
           FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode8 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr8    =  ITEM.i-name. 
     END.
     IF prmIcode9 <> "" THEN DO:
           FIND FIRST ITEM WHERE (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode9 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr9    =  ITEM.i-name. 
     END.
     IF prmIcode10 <> "" THEN DO:
         FIND FIRST ITEM WHERE  (item.mat-type = "V" OR ITEM.mat-type = "I") AND ITEM.i-no = prmIcode10 NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN prmCdscr10    =  ITEM.i-name.    
     END.



END.






/*************************************update ink***********************************************/
IF prmAction = "UpdateRfqPrinting" THEN DO:
MESSAGE "jyoti" prmRfqNo prmAction RfqSeq.

    FIND buff-rfqitem WHERE
         buff-rfqitem.company EQ prmComp AND
         buff-rfqitem.loc EQ prmLoc AND
         buff-rfqitem.rfq-no = prmRfqNo AND
         buff-rfqitem.seq = RfqSeq
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL buff-rfqitem THEN DO:

        assign
             buff-rfqitem.i-col        =  prmPcol   
             buff-rfqitem.i-pass       =  prmPass             
             buff-rfqitem.i-coat       =  prmCoat   
             buff-rfqitem.i-coldscr    =  prmColdscr
             buff-rfqitem.i-ps[1]      =  prmIps1   
             buff-rfqitem.i-ps[2]      =  prmIps2   
             buff-rfqitem.i-ps[3]      =  prmIps3   
             buff-rfqitem.i-ps[4]      =  prmIps4   
             buff-rfqitem.i-ps[5]      =  prmIps5   
             buff-rfqitem.i-ps[6]      =  prmIps6   
             buff-rfqitem.i-ps[7]      =  prmIps7   
             buff-rfqitem.i-ps[8]      =  prmvIps8  
             buff-rfqitem.i-ps[9]      =  prmIps9   
             buff-rfqitem.i-ps[10]     =  prmIps10  
             buff-rfqitem.i-code[1]    =  prmIcode1 
             buff-rfqitem.i-code[2]    =  prmIcode2 
             buff-rfqitem.i-code[3]    =  prmIcode3 
             buff-rfqitem.i-code[4]    =  prmIcode4 
             buff-rfqitem.i-code[5]    =  prmIcode5 
             buff-rfqitem.i-code[6]    =  prmIcode6 
             buff-rfqitem.i-code[7]    =  prmIcode7 
             buff-rfqitem.i-code[8]    =  prmIcode8 
             buff-rfqitem.i-code[9]    =  prmIcode9 
             buff-rfqitem.i-code[10]   =  prmIcode10
             buff-rfqitem.i-dscr[1]    =  prmCdscr1 
             buff-rfqitem.i-dscr[2]    =  prmCdscr2 
             buff-rfqitem.i-dscr[3]    =  prmCdscr3 
             buff-rfqitem.i-dscr[4]    =  prmCdscr4 
             buff-rfqitem.i-dscr[5]    =  prmCdscr5 
             buff-rfqitem.i-dscr[6]    =  prmCdscr6 
             buff-rfqitem.i-dscr[7]    =  prmCdscr7 
             buff-rfqitem.i-dscr[8]    =  prmCdscr8 
             buff-rfqitem.i-dscr[9]    =  prmCdscr9 
             buff-rfqitem.i-dscr[10]   =  prmCdscr10
             buff-rfqitem.i-%[1]       =  prmIper1  
             buff-rfqitem.i-%[2]       =  prmIper2  
             buff-rfqitem.i-%[3]       =  prmIper3  
             buff-rfqitem.i-%[4]       =  prmIper4  
             buff-rfqitem.i-%[5]       =  prmIper5  
             buff-rfqitem.i-%[6]       =  prmIper6  
             buff-rfqitem.i-%[7]       =  prmIper7  
             buff-rfqitem.i-%[8]       =  prmIper8  
             buff-rfqitem.i-%[9]       =  prmIper9  
             buff-rfqitem.i-%[10]      =  prmIper10 .
         
            RELEASE buff-rfqitem.                                                                                    
        END.   /*if avail buff-rfqitem*/                                                                               
        ASSIGN prmAction = "Select". 
                

    END.  /*IF prmAction = "UpdateRfqPrinting" THEN DO:*/ 

IF prmAction = "Select" THEN DO:
   FIND FIRST rfqitem WHERE rfqitem.company = prmComp
                        AND rfqitem.loc = prmLoc
                        AND rfqitem.rfq-no = PrmRfqNo 
                        AND rfqitem.seq = RfqSeq 
                        AND rfqitem.seq < 999  NO-LOCK NO-ERROR.
       RUN Createprinting.
   
END. /*IF prmAction = "Select" */

PROCEDURE Createprinting:
    CREATE ttRfqPrinting.
        ASSIGN 
            ttRfqPrinting.vPcol    = rfqitem.i-col
            ttRfqPrinting.vPass    = rfqitem.i-pass
            ttRfqPrinting.vCoat    = rfqitem.i-coat
            ttRfqPrinting.vColdscr = rfqitem.i-coldscr
            ttRfqPrinting.vIps1    = rfqitem.i-ps[1]
            ttRfqPrinting.vIps2    = rfqitem.i-ps[2]
            ttRfqPrinting.vIps3    = rfqitem.i-ps[3]
            ttRfqPrinting.vIps4    = rfqitem.i-ps[4] 
            ttRfqPrinting.vIps5    = rfqitem.i-ps[5] 
            ttRfqPrinting.vIps6    = rfqitem.i-ps[6] 
            ttRfqPrinting.vIps7    = rfqitem.i-ps[7] 
            ttRfqPrinting.vIps8    = rfqitem.i-ps[8] 
            ttRfqPrinting.vIps9    = rfqitem.i-ps[9] 
            ttRfqPrinting.vIps10   = rfqitem.i-ps[10]  
            ttRfqPrinting.vIcode1  = rfqitem.i-code[1]   
            ttRfqPrinting.vIcode2  = rfqitem.i-code[2]
            ttRfqPrinting.vIcode3  = rfqitem.i-code[3] 
            ttRfqPrinting.vIcode4  = rfqitem.i-code[4]  
            ttRfqPrinting.vIcode5  = rfqitem.i-code[5]  
            ttRfqPrinting.vIcode6  = rfqitem.i-code[6]  
            ttRfqPrinting.vIcode7  = rfqitem.i-code[7]  
            ttRfqPrinting.vIcode8  = rfqitem.i-code[8] 
            ttRfqPrinting.vIcode9  = rfqitem.i-code[9]    
            ttRfqPrinting.vIcode10 = rfqitem.i-code[10]   
            ttRfqPrinting.vCdscr1  = rfqitem.i-dscr[1]
            ttRfqPrinting.vCdscr2  = rfqitem.i-dscr[2]
            ttRfqPrinting.vCdscr3  = rfqitem.i-dscr[3]
            ttRfqPrinting.vCdscr4  = rfqitem.i-dscr[4]   
            ttRfqPrinting.vCdscr5  = rfqitem.i-dscr[5]   
            ttRfqPrinting.vCdscr6  = rfqitem.i-dscr[6]   
            ttRfqPrinting.vCdscr7  = rfqitem.i-dscr[7]   
            ttRfqPrinting.vCdscr8  = rfqitem.i-dscr[8]  
            ttRfqPrinting.vCdscr9  = rfqitem.i-dscr[9]  
            ttRfqPrinting.vCdscr10 = rfqitem.i-dscr[10]    
            ttRfqPrinting.vIper1   = rfqitem.i-%[1]     
            ttRfqPrinting.vIper2   = rfqitem.i-%[2]     
            ttRfqPrinting.vIper3   = rfqitem.i-%[3]    
            ttRfqPrinting.vIper4   = rfqitem.i-%[4]    
            ttRfqPrinting.vIper5   = rfqitem.i-%[5]    
            ttRfqPrinting.vIper6   = rfqitem.i-%[6]    
            ttRfqPrinting.vIper7   = rfqitem.i-%[7]    
            ttRfqPrinting.vIper8   = rfqitem.i-%[8]   
            ttRfqPrinting.vIper9   = rfqitem.i-%[9]   
            ttRfqPrinting.vIper10  = rfqitem.i-%[10]  
             
            .
                              
END PROCEDURE.  /*PROCEDURE createrecordRfq:*/

/*****************************************PROCEDURE assign-RfqPrinting******************************/


IF prmAction = "ResetInk" THEN DO:
    run calc-pass.
END.


PROCEDURE calc-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*message "Are you sure to reset all print codes?" view-as alert-box question
         button yes-no update ll-ans as log.
  if not ll-ans then return no-apply.       
  */

/*********** copied from uest3.p ***********/

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.

     FIND FIRST rfqitem WHERE rfqitem.company EQ prmComp AND rfqitem.loc EQ prmLoc AND rfqitem.rfq-no = prmRfqNo AND
         rfqitem.seq = RfqSeq NO-LOCK NO-ERROR.
           
      find first style where style.company = rfqitem.company and
                 style.style = rfqitem.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).
         find first item where item.company = rfqitem.company and
                    item.i-no = style.material[2] no-lock no-error.
         if avail item then k = integer(style.material[3]).
         find first alt-item where alt-item.company  = rfqitem.company  and
                                   alt-item.mat-type = "V"     and
                                   alt-item.i-no     = style.material[6]
                                   no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first rfq-ctrl where rfq-ctrl.company = rfqitem.company and
                                   rfq-ctrl.loc = rfqitem.loc
                                   no-lock no-error.
         if k = 0 then k = rfq-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = rfqitem.company and
                       item.i-no = rfq-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = rfqitem.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = rfq-ctrl.def-coat
                                      no-lock no-error.
      end.
   
      save_id = recid(item). save_id2 = recid(alt-item).
      j = (integer(prmPcol)
          + integer(prmCoat)  ) 
          / integer(prmPass).
      {sys/inc/roundup.i j}
      counter = 1.
      choice = true.
/*    do i = 1 to 10:
       if rfqitem.i-code[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */
 
/*    find bf-rfqitem of rfqitem exclusive-lock.    */

      CREATE ttRfqPrinting.

      if choice then do i = 1 to 10:
         if i le integer(prmPcol) then do:
              find item where recid(item) = save_id no-lock no-error.            
             case string(i) :
                when "1" then assign ttRfqPrinting.vIps1    = INT(counter)
                                     ttRfqPrinting.vIcode1  = item.i-no
                                     ttRfqPrinting.vCdscr1  = item.est-dscr
                                     ttRfqPrinting.vIper1   = k.
                when "2" then assign ttRfqPrinting.vIps2    = INT(counter)
                                     ttRfqPrinting.vIcode2  = item.i-no
                                     ttRfqPrinting.vCdscr2  = item.est-dscr
                                     ttRfqPrinting.vIper2   = k.
                when "3" then assign ttRfqPrinting.vIps3    = INT(counter)
                                     ttRfqPrinting.vIcode3  = item.i-no
                                     ttRfqPrinting.vCdscr3  = item.est-dscr
                                     ttRfqPrinting.vIper3   = k.
                when "4" then assign ttRfqPrinting.vIps4    = INT(counter)
                                     ttRfqPrinting.vIcode4  = item.i-no
                                     ttRfqPrinting.vCdscr4  = item.est-dscr
                                     ttRfqPrinting.vIper4   = k.
                when "5" then assign ttRfqPrinting.vIps5    = INT(counter)
                                     ttRfqPrinting.vIcode5  = item.i-no
                                     ttRfqPrinting.vCdscr5  = item.est-dscr
                                     ttRfqPrinting.vIper5   = k.
                when "6" then assign ttRfqPrinting.vIps6    = INT(counter)
                                     ttRfqPrinting.vIcode6  = item.i-no
                                     ttRfqPrinting.vCdscr6  = item.est-dscr
                                     ttRfqPrinting.vIper6   = k.
                when "7" then assign ttRfqPrinting.vIps7    = INT(counter)
                                     ttRfqPrinting.vIcode7  = item.i-no
                                     ttRfqPrinting.vCdscr7  = item.est-dscr
                                     ttRfqPrinting.vIper7   = k.
                when "8" then assign ttRfqPrinting.vIps8    = INT(counter)
                                     ttRfqPrinting.vIcode8  = item.i-no
                                     ttRfqPrinting.vCdscr8  = item.est-dscr
                                     ttRfqPrinting.vIper8   = k.
                when "9" then assign ttRfqPrinting.vIps9    = INT(counter)
                                     ttRfqPrinting.vIcode9  = item.i-no
                                     ttRfqPrinting.vCdscr9  = item.est-dscr
                                     ttRfqPrinting.vIper9   = k.
                when "10" then assign ttRfqPrinting.vIps10  = INT(counter)
                                     ttRfqPrinting.vIcode10 = item.i-no
                                     ttRfqPrinting.vCdscr10 = item.est-dscr
                                     ttRfqPrinting.vIper10  = k.                      
             end case.
         end.
         else if (i > integer(prmPcol)) and
                 (i <= (integer(prmPcol) + 
                       integer(prmCoat)))
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
         
              case string(i) :
                when "1" then assign ttRfqPrinting.vIps1    = INT(counter)
                                     ttRfqPrinting.vIcode1  = alt-item.i-no
                                     ttRfqPrinting.vCdscr1  = alt-item.est-dscr
                                     ttRfqPrinting.vIper1   = 100.
                when "2" then assign ttRfqPrinting.vIps2    = INT(counter)
                                     ttRfqPrinting.vIcode2  = alt-item.i-no
                                     ttRfqPrinting.vCdscr2  = alt-item.est-dscr
                                     ttRfqPrinting.vIper2   = 100.
                when "3" then assign ttRfqPrinting.vIps3    = INT(counter)
                                     ttRfqPrinting.vIcode3  = alt-item.i-no
                                     ttRfqPrinting.vCdscr3  = alt-item.est-dscr
                                     ttRfqPrinting.vIper3   = 100.
                when "4" then assign ttRfqPrinting.vIps4    = INT(counter)
                                     ttRfqPrinting.vIcode4  = alt-item.i-no
                                     ttRfqPrinting.vCdscr4  = alt-item.est-dscr
                                     ttRfqPrinting.vIper4   = 100.
                when "5" then assign ttRfqPrinting.vIps5    = INT(counter)
                                     ttRfqPrinting.vIcode5  = alt-item.i-no
                                     ttRfqPrinting.vCdscr5  = alt-item.est-dscr
                                     ttRfqPrinting.vIper5   = 100.
                when "6" then assign ttRfqPrinting.vIps6    = INT(counter)
                                     ttRfqPrinting.vIcode6  = alt-item.i-no
                                     ttRfqPrinting.vCdscr6  = alt-item.est-dscr
                                     ttRfqPrinting.vIper6   = 100.
                when "7" then assign ttRfqPrinting.vIps7    = INT(counter)
                                     ttRfqPrinting.vIcode7  = alt-item.i-no
                                     ttRfqPrinting.vCdscr7  = alt-item.est-dscr
                                     ttRfqPrinting.vIper7   = 100.
                when "8" then assign ttRfqPrinting.vIps8    = INT(counter)
                                     ttRfqPrinting.vIcode8  = alt-item.i-no
                                     ttRfqPrinting.vCdscr8  = alt-item.est-dscr
                                     ttRfqPrinting.vIper8   = 100.
                when "9" then assign ttRfqPrinting.vIps9    = INT(counter)
                                     ttRfqPrinting.vIcode9  = alt-item.i-no
                                     ttRfqPrinting.vCdscr9  = alt-item.est-dscr
                                     ttRfqPrinting.vIper9   = 100.
                when "10" then assign ttRfqPrinting.vIps10  = INT(counter)
                                     ttRfqPrinting.vIcode10 = alt-item.i-no
                                     ttRfqPrinting.vCdscr10 = alt-item.est-dscr
                                     ttRfqPrinting.vIper10  = 100.    
                    
             end.                   
         end.
         else if (i >  integer(prmPcol) + 
                       integer(prmCoat) )
         then do:       
              case string(i) :
                   when "1" then assign ttRfqPrinting.vIps1    = 0
                                        ttRfqPrinting.vIcode1  = ""
                                        ttRfqPrinting.vCdscr1  = ""
                                        ttRfqPrinting.vIper1   = 0.
                   when "2" then assign ttRfqPrinting.vIps2    = 0
                                        ttRfqPrinting.vIcode2  = ""
                                        ttRfqPrinting.vCdscr2  = ""
                                        ttRfqPrinting.vIper2   = 0.
                   when "3" then assign ttRfqPrinting.vIps3    = 0
                                        ttRfqPrinting.vIcode3  = ""
                                        ttRfqPrinting.vCdscr3  = ""
                                        ttRfqPrinting.vIper3   = 0.
                   when "4" then assign ttRfqPrinting.vIps4    = 0
                                        ttRfqPrinting.vIcode4  = ""
                                        ttRfqPrinting.vCdscr4  = ""
                                        ttRfqPrinting.vIper4   = 0.
                   when "5" then assign ttRfqPrinting.vIps5    = 0
                                        ttRfqPrinting.vIcode5  = ""
                                        ttRfqPrinting.vCdscr5  = ""
                                        ttRfqPrinting.vIper5   = 0.
                   when "6" then assign ttRfqPrinting.vIps6    = 0
                                        ttRfqPrinting.vIcode6  = ""
                                        ttRfqPrinting.vCdscr6  = ""
                                        ttRfqPrinting.vIper6   = 0.
                   when "7" then assign ttRfqPrinting.vIps7    = 0
                                        ttRfqPrinting.vIcode7  = ""
                                        ttRfqPrinting.vCdscr7  = ""
                                        ttRfqPrinting.vIper7   = 0.
                   when "8" then assign ttRfqPrinting.vIps8    = 0
                                        ttRfqPrinting.vIcode8  = ""
                                        ttRfqPrinting.vCdscr8  = ""
                                        ttRfqPrinting.vIper8   = 0.
                   when "9" then assign ttRfqPrinting.vIps9    = 0
                                        ttRfqPrinting.vIcode9  = ""
                                        ttRfqPrinting.vCdscr9  = ""
                                        ttRfqPrinting.vIper9   = 0.
                   when "10" then assign ttRfqPrinting.vIps10  = 0
                                        ttRfqPrinting.vIcode10 = ""
                                        ttRfqPrinting.vCdscr10 = ""
                                        ttRfqPrinting.vIper10  = 0.                   

              end case.       
                     
         end.
         if i modulo j = 0 then counter = counter + 1.
         if counter > integer(prmPass) then counter = integer(prmPass).
      
      end.
   
END PROCEDURE.

