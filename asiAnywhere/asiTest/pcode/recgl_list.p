
/*------------------------------------------------------------------------
    File        : recgl_list.p
    Purpose     : General/Ledger
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttRecrGeneralLedgerlist NO-UNDO
    FIELD jrn_no        AS INT          
    FIELD tr_date       AS CHAR
    FIELD period        AS INT
    FIELD t_deb         AS DEC
    FIELD t_crd         AS DEC
    FIELD t_amt         AS DEC
    FIELD reverse       AS CHAR
    FIELD from_revr     AS CHAR
    FIELD cb_freq       AS CHAR

    
    FIELD dscr          AS CHAR
    FIELD reckey        AS CHAR


    FIELD rc_extra      AS CHAR  .

DEFINE DATASET dsRecrGeneralLedgerlist FOR ttRecrGeneralLedgerlist.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmjrn_no       AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmtr_date      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmperiod       AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmt_deb        AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmt_crd        AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmt_amt        AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmreverse      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmfrom_revr    AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmcb_freq      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmdscr         AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHAR       NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRecrGeneralLedgerlist .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     FOR EACH ttRecrGeneralLedgerlist:
        DELETE ttRecrGeneralLedgerlist .
    END.

IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmjrn_no         = ?  THEN ASSIGN prmjrn_no     = 0.
IF prmtr_date        = ?  THEN ASSIGN prmtr_date    = "".
IF prmperiod         = ?  THEN ASSIGN prmperiod     = 0. 
IF prmt_deb          = ?  THEN ASSIGN prmt_deb      = 0. 
IF prmt_crd          = ?  THEN ASSIGN prmt_crd      = 0.
IF prmt_amt          = ?  THEN ASSIGN prmt_amt      = 0.
IF prmreverse        = ?  THEN ASSIGN prmreverse    = "".
IF prmfrom_revr      = ?  THEN ASSIGN prmfrom_revr  = "".
IF prmcb_freq        = ?  THEN ASSIGN prmcb_freq    = "".
IF prmReckey         = ?  THEN ASSIGN prmReckey     = "".





DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
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

DEF TEMP-TABLE tt-gl-jrn NO-UNDO
    FIELD DATE AS DATE
    FIELD reverse AS LOG
    FIELD id AS CHAR
    FIELD j-no AS INT
    FIELD period AS INT.

DEF TEMP-TABLE tt-gl-jrnl NO-UNDO
    FIELD LINE AS INT
    FIELD actnum AS CHAR
    FIELD dscr AS CHAR
    FIELD tr-amt AS DEC
    FIELD id AS CHAR.

DEF VAR ll-recur AS LOG INIT YES NO-UNDO .

DEF VAR lv-dscr LIKE gl-jrnl.dscr NO-UNDO.
DEF BUFFER bf-jrn FOR gl-jrn.


FUNCTION get-dscr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER b-gl-jrn FOR gl-jrn.


  RELEASE gl-jrnl.

  IF AVAIL gl-jrn THEN
  FIND b-gl-jrn WHERE ROWID(b-gl-jrn) EQ ROWID(gl-jrn) NO-LOCK NO-ERROR.

  IF AVAIL b-gl-jrn THEN
  FIND FIRST gl-jrnl
      WHERE gl-jrnl.j-no EQ b-gl-jrn.j-no
        AND gl-jrnl.dscr NE ""
      NO-LOCK NO-ERROR.

  RETURN IF AVAIL gl-jrnl THEN gl-jrnl.dscr ELSE "".   /* Function return value. */

END FUNCTION.

IF prmAction = "Search" THEN DO:
    
     FOR EACH gl-jrn WHERE gl-jrn.company = g_company 
         and gl-jrn.journal > 0 
         and gl-jrn.posted eq no 
         and gl-jrn.recur eq yes 
         AND ((gl-jrn.journal EQ prmjrn_no) OR (prmjrn_no EQ 0))
         AND (STRING(gl-jrn.freq-code) BEGINS string(prmcb_freq) OR string(prmcb_freq) EQ "")  NO-LOCK :

        CREATE ttRecrGeneralLedgerlist.
           ASSIGN 
                 ttRecrGeneralLedgerlist.jrn_no        = gl-jrn.journal
                 ttRecrGeneralLedgerlist.cb_freq       = gl-jrn.freq
                 ttRecrGeneralLedgerlist.dscr          = get-dscr ()
                 ttRecrGeneralLedgerlist.t_deb         = gl-jrn.tdeb
                 ttRecrGeneralLedgerlist.t_crd         = gl-jrn.tcred
                 ttRecrGeneralLedgerlist.t_amt         = gl-jrn.tr-amt
                 ttRecrGeneralLedgerlist.reckey        = gl-jrn.rec_key   
                 ttRecrGeneralLedgerlist.tr_date       = string(gl-jrn.tr-date)  .
            
    END. /*FOR EACH gl-jrn  */
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "ValidateAdd" THEN DO:
    DEF VAR choice AS LOG NO-UNDO.

 /* IF NOT ll-recur THEN
         find first period where period.company  =  g_company  and
                                period.pst  <= date(prmtr_date) and
                                period.pend >= date(prmtr_date)
                                no-lock no-error.
         IF AVAIL period AND period.pstat then do:
            assign prmperiod  = (period.pnum).          
         end.
         else do:
            cError = "Period is not OPEN !".
            RETURN.
         end.
         choice = not (FOCUS:NAME NE "tr-date" AND avail gl-jrn and period.pnum ne gl-jrn.period).
         if not choice then DO:            
            cError = "Reset Journal Entries' period to new Period ?" .
                   
         end.
         if not choice then do:
           ASSIGN
            prmtr_date = STRING(gl-jrn.tr-date)
            prmperiod   = (gl-jrn.period).
           RETURN.
         END.*/
 END.

/********************************************************************/

IF prmAction = "AddNew" THEN DO:
     
    CREATE gl-jrn .

    ASSIGN 
        gl-jrn.company      = g_company
        gl-jrn.period       = prmperiod
        gl-jrn.recur        = YES
        gl-jrn.reverse      = NO
        gl-jrn.from-reverse = NO .

    ASSIGN prmAction = "View"
                prmReckey = gl-jrn.rec_key 
        .
        
END.



 IF prmAction = "AddUpdate" THEN DO:

     
        FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
    
        IF AVAIL gl-jrn then
            ASSIGN
            gl-jrn.tr-date         =  date(prmtr_date)  
            gl-jrn.period          =  prmperiod   
            gl-jrn.freq            =  prmcb_freq
            gl-jrn.reverse         =  IF prmreverse = "True" THEN TRUE ELSE FALSE 
            gl-jrn.from-reverse    =  IF prmfrom_revr = "True" THEN TRUE ELSE FALSE   .

    ASSIGN prmAction = "View" 
           prmReckey = gl-jrn.rec_key.

END.


/**************Update *************************************************/

IF prmAction = "Update" THEN DO:
  

  /*IF NOT ll-recur THEN
         find first period where period.company  =  g_company  and
                                period.pst  <= date(prmtr_date) and
                                period.pend >= date(prmtr_date)
                                no-lock no-error.
         IF AVAIL period AND period.pstat then do:
            assign prmperiod  = (period.pnum).          
         end.
         else do:
            cError = "Period is not OPEN !".
            RETURN.
         end.*/
        /* choice = not (FOCUS:NAME NE "tr-date" AND avail gl-jrn and period.pnum ne gl-jrn.period).
         if not choice then DO:            
            cError = "Reset Journal Entries' period to new Period ?" .
                   
         end.
         if not choice then do:
           ASSIGN
            prmtr_date = STRING(gl-jrn.tr-date)
            prmperiod   = (gl-jrn.period).
           RETURN.
         END.*/
     

END.

IF prmAction = "Update" THEN DO:
     FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

                    
     ASSIGN 
         gl-jrn.tr-date         =  date(prmtr_date)  
         gl-jrn.period          =  prmperiod   
         gl-jrn.freq            =  prmcb_freq
         gl-jrn.reverse         =  IF prmreverse = "True" THEN TRUE ELSE FALSE 
         gl-jrn.from-reverse    =  IF prmfrom_revr = "True" THEN TRUE ELSE FALSE   .

        
        ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/


IF prmAction = "DataDelete"  THEN DO:
    
    FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
   
    IF AVAIL gl-jrn THEN DELETE gl-jrn .

    FIND LAST gl-jrn WHERE gl-jrn.company = cocode  and gl-jrn.journal > 0 
         and gl-jrn.posted eq no 
         AND gl-jrn.recur EQ YES NO-LOCK NO-ERROR.

        IF AVAIL gl-jrn THEN
        ASSIGN
            prmjrn_no = (gl-jrn.journal)
            prmReckey = gl-jrn.rec_key 
            prmAction = "View" .
        

END.



/*******************************View************************************/


IF prmAction = "View" THEN DO:
     
     FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  NO-LOCK NO-ERROR.
        CREATE ttRecrGeneralLedgerlist.
           ASSIGN 
                 ttRecrGeneralLedgerlist.jrn_no        = gl-jrn.journal
                 ttRecrGeneralLedgerlist.tr_date       = string(gl-jrn.tr-date)
                 ttRecrGeneralLedgerlist.cb_freq       = gl-jrn.freq
                 ttRecrGeneralLedgerlist.period        = gl-jrn.period
                 ttRecrGeneralLedgerlist.t_deb         = gl-jrn.tdeb
                 ttRecrGeneralLedgerlist.t_crd         = gl-jrn.tcred
                 ttRecrGeneralLedgerlist.t_amt         = gl-jrn.tr-amt
                 ttRecrGeneralLedgerlist.reverse       = string(gl-jrn.reverse)
                 ttRecrGeneralLedgerlist.from_revr     = string(gl-jrn.from-reverse)
                 ttRecrGeneralLedgerlist.reckey        = gl-jrn.rec_key    .

            
END. /*IF prmAction = "View" THEN DO:*/


IF prmAction = "LoadRecr" THEN DO:
 DEF BUFFER inp-gl-jrn  FOR gl-jrn.
  DEF BUFFER inp-gl-jrnl FOR gl-jrnl.
  DEF BUFFER out-gl-jrn  FOR gl-jrn.
  DEF BUFFER out-gl-jrnl FOR gl-jrnl.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR post-date AS DATE NO-UNDO INIT TODAY.


  FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  NO-LOCK NO-ERROR.

 
     FIND FIRST period NO-LOCK WHERE period.company = cocode 
                                  AND period.pst     LE date(prmtr_date)
                                  AND period.pend    GE date(prmtr_date)
                                  AND period.pstat   = YES NO-ERROR.
      IF NOT AVAIL period THEN DO:
          cError = "Posting Date must be within an open period" .
          RETURN .
      END.


        IF AVAIL gl-jrn THEN DO:
          FIND inp-gl-jrn WHERE ROWID(inp-gl-jrn) EQ ROWID(gl-jrn) NO-LOCK.
          
          CREATE out-gl-jrn.
          BUFFER-COPY inp-gl-jrn EXCEPT j-no journal rec_key TO out-gl-jrn     .
          ASSIGN
           out-gl-jrn.recur   = NO
           out-gl-jrn.tr-date = date(prmtr_date)
           out-gl-jrn.period  = IF date(prmtr_date) <> TODAY THEN period.pnum ELSE "" /*g_period*/ .

          FOR EACH inp-gl-jrnl OF inp-gl-jrn NO-LOCK:
            CREATE out-gl-jrnl.
            BUFFER-COPY inp-gl-jrnl EXCEPT rec_key TO out-gl-jrnl
            ASSIGN
             out-gl-jrnl.j-no = out-gl-jrn.j-no.
          END.
           cError = "Load of Recurring Journal Complete..." .
        END.
     
      
      

   
   /* END.*/
 
END. /*IF prmAction = "LoadRecr" */
