
/*------------------------------------------------------------------------
    File        : vendtotl.p
    Purpose     : 

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE TEMP-TABLE ttVendorTotal NO-UNDO
    FIELD vend          AS CHAR
    FIELD vendname      AS CHAR
    FIELD purch         AS DEC
    FIELD lst_yr        AS DEC
    FIELD ytd_msf       AS DEC
    FIELD lyytd_msf     AS DEC
    FIELD hibal         AS DEC
    FIELD hibal_date    AS CHAR
    FIELD num_inv       AS INT
    FIELD lpay          AS DEC
    FIELD lpay_date     AS CHAR
    FIELD AVG_pay       AS INT
    FIELD acc_bal       AS DEC
    FIELD purchase      AS DEC
    FIELD tot_msf       AS DEC
    FIELD ordbal        AS DEC
    FIELD extra         AS CHAR
      . 

DEFINE DATASET dsVendorTotal FOR ttVendorTotal.

DEFINE INPUT PARAMETER prmAction            AS CHAR    NO-UNDO.    
DEFINE INPUT PARAMETER prmUser              AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmReckey            AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmvend              AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmvendname          AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmpurch             AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmlst_yr            AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmytd_msf           AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmlyytd_msf         AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmhibal             AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmhibal_date        AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmnum_inv           AS INT     NO-UNDO.
DEFINE INPUT PARAMETER prmlpay              AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmlpay_date         AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmAVG_pay           AS INT     NO-UNDO.
DEFINE INPUT PARAMETER prmacc_bal           AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmpurchase          AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmtot_msf           AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmordbal            AS DEC     NO-UNDO.
DEFINE INPUT PARAMETER prmext               AS CHAR    NO-UNDO.



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorTotal .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
 DEF VAR prmComp AS CHAR NO-UNDO.

 
     IF prmAction              = ? THEN ASSIGN prmAction            = "Select".
     IF prmUser                = ? THEN ASSIGN prmUser              = "".
     IF prmReckey              = ? THEN ASSIGN prmReckey            = "".
     IF prmvend                = ? THEN ASSIGN prmvend              = "".
     IF prmvendname            = ? THEN ASSIGN prmvendname          = "".
     IF prmpurch               = ? THEN ASSIGN prmpurch             = 0.
     IF prmlst_yr              = ? THEN ASSIGN prmlst_yr            = 0.
     IF prmytd_msf             = ? THEN ASSIGN prmytd_msf           = 0.
     IF prmlyytd_msf           = ? THEN ASSIGN prmlyytd_msf         = 0.
     IF prmhibal               = ? THEN ASSIGN prmhibal             = 0.
     IF prmhibal_date          = ? THEN ASSIGN prmhibal_date        = "".
     IF prmnum_inv             = ? THEN ASSIGN prmnum_inv           = 0.
     IF prmlpay                = ? THEN ASSIGN prmlpay              = 0.
     IF prmlpay_date           = ? THEN ASSIGN prmlpay_date         = "".
     IF prmAVG_pay             = ? THEN ASSIGN prmAVG_pay           = 0.
     IF prmacc_bal             = ? THEN ASSIGN prmacc_bal           = 0.
     IF prmpurchase            = ? THEN ASSIGN prmpurchase          = 0.
     IF prmtot_msf             = ? THEN ASSIGN prmtot_msf           = 0.
     IF prmordbal              = ? THEN ASSIGN prmordbal            = 0.
     IF prmext                 = ? THEN ASSIGN prmext               = "".
     
     
DEFINE VARIABLE ptd-purch AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 NO-UNDO.
DEFINE VARIABLE total-msf AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 NO-UNDO.

     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  g_company = prmComp.



  MESSAGE "testg " .

    
IF prmAction = "Update" THEN DO:
    FIND FIRST vend WHERE vend.company = prmComp AND vend.rec_key = prmReckey
        EXCLUSIVE-LOCK NO-ERROR.
MESSAGE "hello " .
        ASSIGN 
            vend.Purch[13]      = prmpurch      
            vend.last-year      = prmlst_yr     
            vend.ytd-msf        = prmytd_msf    
            vend.lyytd-msf      = prmlyytd_msf  
            vend.hibal          = prmhibal      
            vend.hibal-date     = date(prmhibal_date)
            vend.num-inv        = prmnum_inv    
            vend.lpay           = prmlpay       
            vend.lpay-date      = date(prmlpay_date)  
            vend.avg-pay        = prmAVG_pay    
            vend.acc-bal        = prmacc_bal  
            ptd-purch           = prmpurchase
            total-msf           = prmtot_msf
            .
        ASSIGN
            prmAction = "View" .
                       
  
END. /*IF prmAction = "Update" THEN DO:*/





IF prmAction = "View"  THEN DO:
    FIND FIRST vend WHERE vend.company = prmComp AND vend.rec_key = prmReckey
        NO-LOCK NO-ERROR.

    CREATE  ttVendorTotal.
            ASSIGN 
                 ttVendorTotal.vend              = vend.vend  
                 ttVendorTotal.vendname          = vend.name  
                 ttVendorTotal.purch             = vend.Purch[13]   
                 ttVendorTotal.lst_yr            = vend.last-year   
                 ttVendorTotal.ytd_msf           = vend.ytd-msf   
                 ttVendorTotal.lyytd_msf         = vend.lyytd-msf   
                 ttVendorTotal.hibal             = vend.hibal   
                 ttVendorTotal.hibal_date        = STRING(vend.hibal-date)   
                 ttVendorTotal.num_inv           = vend.num-inv   
                 ttVendorTotal.lpay              = vend.lpay   
                 ttVendorTotal.lpay_date         = string(vend.lpay-date)   
                 ttVendorTotal.AVG_pay           = vend.avg-pay   
                 ttVendorTotal.acc_bal           = vend.acc-bal 
                 ttVendorTotal.ordbal            = vend.ord-bal 
                 ttVendorTotal.extra             = vend.rec_key
                 ttVendorTotal.purchase          = ptd-purch 
                 ttVendorTotal.tot_msf           = total-msf .
                                  
               
END. /*IF prmAction = "View" THEN DO:*/
