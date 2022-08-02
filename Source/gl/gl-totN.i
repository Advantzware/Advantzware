/* ---------------------------------------------------- gl/gl-tot.i 12/00 JLF */
/* g/l report -  totaling                                                     */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-acct AS RECID.
DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

DEFINE BUFFER b-acc FOR account.

{gl/gl-fs.i}

DEFINE VARIABLE v-pyear              LIKE period.yr INIT 0 NO-UNDO.
DEFINE VARIABLE v-fyear              LIKE period.yr INIT 0 NO-UNDO.
DEFINE VARIABLE v-sperq              LIKE period.pnum INIT 0 NO-UNDO.
DEFINE VARIABLE v-eperq              LIKE period.pnum INIT 0 NO-UNDO.
DEFINE VARIABLE v-datep              LIKE period.pst INIT TODAY NO-UNDO.
DEFINE VARIABLE v-dateq              LIKE period.pst INIT TODAY NO-UNDO.
DEFINE VARIABLE v-datey              LIKE period.pst INIT TODAY NO-UNDO.

DEFINE VARIABLE dOpenBalance         AS DECIMAL NO-UNDO.
DEFINE VARIABLE dPeriodTotal         AS DECIMAL NO-UNDO.
DEFINE VARIABLE dYtdAmount           AS DECIMAL NO-UNDO.
DEFINE VARIABLE dQTD                 AS DECIMAL NO-UNDO.
DEFINE VARIABLE dLastYearPeriodTotal AS DECIMAL NO-UNDO.
DEFINE VARIABLE dQTDLastYear         AS DECIMAL NO-UNDO.

FIND FIRST account WHERE RECID(account) EQ v-acct NO-LOCK NO-ERROR.

FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ account.company NO-LOCK.

FIND FIRST period
    WHERE period.company EQ account.company
    AND period.pst     LE udate
    AND period.pend    GE udate
    NO-LOCK NO-ERROR.
   
IF AVAILABLE period THEN
    ASSIGN
        v-pyear = period.yr
        v-datep = period.pst.
   
IF "{1}" EQ "trans" AND index("ALCT",account.type) GT 0 THEN
    FIND FIRST period
        WHERE period.company EQ account.company
        AND period.pst     LE udate
        AND period.pstat   EQ YES
        NO-LOCK NO-ERROR.

IF AVAILABLE period THEN v-fyear = period.yr.

ASSIGN
    v-eperq = (trunc(uperiod / 3,0) + int(uperiod MODULO 3 GT 0)) * 3
    v-sperq = v-eperq - 2.
 
IF v-sperq GT 9 THEN
    ASSIGN
        v-sperq = 10
        v-eperq = 13.

FOR EACH period
    WHERE period.company EQ account.company
    AND period.yr      EQ v-pyear
    AND period.pnum    GE v-sperq
    AND period.pnum    LE v-eperq
    NO-LOCK
    BY period.pnum:
    
    v-dateq = period.pst.
    LEAVE.
END.

FOR EACH period
    WHERE period.company EQ account.company
    AND period.yr      GE v-fyear
    NO-LOCK
    BY period.yr
    BY period.pnum:
    
    v-datey = period.pst.
    LEAVE.
END. 

RUN GL_GetAccountOpenBal(ROWID(account), v-datep , OUTPUT dOpenBalance).
 
dYtdAmount = dOpenBalance.
dPeriodTotal = 0.

FOR EACH glhist NO-LOCK 
    WHERE glhist.company EQ account.company
    AND glhist.actnum  EQ account.actnum
    AND glhist.tr-date GE v-datep 
    AND glhist.tr-date LE udate BY glhist.tr-date  :
             
    dYtdAmount = dYtdAmount +  glhist.tr-amt.   
    dPeriodTotal = dPeriodTotal + glhist.tr-amt.             
END.
FOR EACH glhist NO-LOCK 
    WHERE glhist.company EQ account.company
    AND glhist.actnum  EQ account.actnum
    AND glhist.tr-date GE v-dateq 
    AND glhist.tr-date LE udate BY glhist.tr-date  : 
              
    dQTD = dQTD +  glhist.tr-amt.
END.  
  
IF ip-int EQ 3 THEN
    ASSIGN
        tot3[1] = dPeriodTotal  
        tot3[2] = dQTD 
        tot3[3] = dYtdAmount .
IF ip-int EQ 0 THEN
    ASSIGN
        tot[1] = dPeriodTotal 
        tot[2] = dQTD 
        tot[3] = dYtdAmount.

FIND FIRST period NO-LOCK                  
    WHERE period.company EQ account.company
    AND period.yr     EQ (YEAR(udate) - 1) 
    AND period.pnum   EQ MONTH(udate) NO-ERROR .

FOR EACH glhist NO-LOCK 
    WHERE glhist.company EQ account.company
    AND glhist.actnum  EQ account.actnum
    AND glhist.tr-date GE period.pst
    AND glhist.tr-date LE period.pend BY glhist.tr-date  :         

    dLastYearPeriodTotal = dLastYearPeriodTotal + glhist.tr-amt.             
END.
  
RUN GL_GetAccountOpenBal(ROWID(account), (v-datey - 1) , OUTPUT dOpenBalance).
tot[9]  =  dOpenBalance.
tot[7]  =  dLastYearPeriodTotal.
   
FOR EACH period NO-LOCK                  
    WHERE period.company EQ account.company
    AND period.yr     EQ (YEAR(udate) - 1) 
    AND period.pnum   GE v-sperq
    AND period.pnum   LE (v-sperq + 3) :
           
    FOR EACH glhist NO-LOCK 
        WHERE glhist.company EQ account.company
        AND glhist.actnum  EQ account.actnum
        AND glhist.tr-date GE period.pst
        AND glhist.tr-date LE period.pend BY glhist.tr-date  :         

        dQTDLastYear = dQTDLastYear + glhist.tr-amt.             
    END.
END.
tot[8]  =  dQTDLastYear.
  
RETURN.

/* end ---------------------------------- Copr. 2000  Advanced Software, Inc. */
