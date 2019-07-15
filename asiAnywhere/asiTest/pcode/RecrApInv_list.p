
/*------------------------------------------------------------------------
    File        : RecrApInv_list.p
    Purpose     : Vendor
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttRecurVendorinvlist NO-UNDO
    FIELD invno       AS CHAR     
    FIELD vendno      AS CHAR         
    FIELD vendname    AS CHAR     
    FIELD net         AS DECIMAL
    FIELD freqcode    AS CHAR
    FIELD reckey      AS CHAR
    FIELD extra       AS CHAR 
    .

DEFINE DATASET dsRecurVendorinvlist FOR ttRecurVendorinvlist.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvend     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInv      AS CHAR  NO-UNDO.

DEFINE INPUT PARAMETER prmPosted   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmunPosted AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmvendname AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmnet      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmFreqcode AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey   AS CHAR NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRecurVendorinvlist .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttRecurVendorinvlist:
        DELETE ttRecurVendorinvlist .
    END.

IF prmAction         = ?  THEN ASSIGN prmAction     = "Search".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmvend           = ?  THEN ASSIGN prmvend       = "".
IF prmInv            = ?  THEN ASSIGN prmInv        = "".
IF prmPosted         = ?  THEN ASSIGN prmPosted     = "". 
IF prmunPosted       = ?  THEN ASSIGN prmunPosted   = "". 
IF prmvendname       = ?  THEN ASSIGN prmvendname   = "".
IF prmnet            = ?  THEN ASSIGN prmnet        = 0.
IF prmFreqcode       = ?  THEN ASSIGN prmFreqcode   = "".



DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
  DEF BUFFER bARInvl FOR ap-invl. 
   DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.

  
         DEF BUFFER b-ap-inv FOR ap-inv.
         DEF BUFFER b-ap-invl FOR ap-invl.
         DEF BUFFER bf-inv FOR ap-inv.
         DEF VAR lv-msg AS CHAR NO-UNDO.
         DEFINE VAR custcount AS CHAR NO-UNDO.
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_user    = prmUser  .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .

  {sys/inc/apsecure.i}


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.


FUNCTION get-vend-name RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  FIND FIRST vend
      WHERE vend.company EQ cocode
        AND vend.vend-no EQ (IF AVAIL ap-inv THEN ap-inv.vend-no
                                             ELSE prmvend)
      NO-LOCK NO-ERROR.

  RETURN IF AVAIL vend THEN vend.name ELSE "Not on file".
                     /* Function return value. */

END FUNCTION.





IF prmAction = "Search" THEN DO:
    
     FOR EACH ap-inv                                      
        WHERE  ap-inv.company = cocode 
         and ap-inv.posted = no 
         and ap-inv.recur = YES and ap-inv.vend-no BEGINS prmvend
         and ap-inv.freq-code BEGINS prmFreqcode  NO-LOCK:

        CREATE ttRecurVendorinvlist.
           ASSIGN 
                 
                 ttRecurVendorinvlist.vendno      = ap-inv.vend-no
                 ttRecurVendorinvlist.vendname    = get-vend-name()
                 ttRecurVendorinvlist.freqcode    = ap-inv.freq-code
                 ttRecurVendorinvlist.net         = ap-inv.net 
                 ttRecurVendorinvlist.reckey      = ap-inv.rec_key .
            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/




IF prmAction = "LoadRecur" THEN DO:

    DEF BUFFER inp-ap-inv  FOR ap-inv.
  DEF BUFFER inp-ap-invl FOR ap-invl.
  DEF BUFFER out-ap-inv  FOR ap-inv.
  DEF BUFFER out-ap-invl FOR ap-invl.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

  FIND FIRST ap-inv WHERE  ap-inv.company = cocode 
            and ap-inv.rec_key = prmReckey  NO-LOCK NO-ERROR.
        
        IF AVAIL ap-inv THEN DO:
          FIND inp-ap-inv WHERE ROWID(inp-ap-inv) EQ ROWID(ap-inv) NO-LOCK.

          CREATE out-ap-inv.
          BUFFER-COPY inp-ap-inv EXCEPT i-no inv-no rec_key TO out-ap-inv
          ASSIGN
           out-ap-inv.recur    = NO
           out-ap-inv.inv-date = TODAY
           out-ap-inv.inv-no   = STRING(out-ap-inv.i-no,"9999999999").

          FIND FIRST vend
              WHERE vend.company EQ out-ap-inv.company
                AND vend.vend-no EQ out-ap-inv.vend-no
              NO-LOCK NO-ERROR.

          IF AVAIL vend THEN DO:
            FIND FIRST terms WHERE terms.t-code EQ vend.terms NO-LOCK NO-ERROR.
            out-ap-inv.due-date = ap-inv.inv-date +
                                  (IF AVAIL terms THEN terms.net-days
                                   ELSE 0).
          END.

          FOR EACH inp-ap-invl WHERE inp-ap-invl.i-no EQ inp-ap-inv.i-no NO-LOCK:
            CREATE out-ap-invl.
            BUFFER-COPY inp-ap-invl EXCEPT rec_key TO out-ap-invl
            ASSIGN
             out-ap-invl.i-no = out-ap-inv.i-no.
          END.
        END.
      
    /*  MESSAGE "Load of Recurring AP Invoices Complete..."
          VIEW-AS ALERT-BOX. */
     

END.

/*****************************procedure**********************************/



