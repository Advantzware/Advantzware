
/*------------------------------------------------------------------------
    File        : company_list.p
    Purpose     : Customer

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttperiodlistview NO-UNDO
    FIELD   company     AS CHAR
    FIELD   cname       AS CHAR
    FIELD   peryr       AS CHAR 
    FIELD   perpnum     AS CHAR
    FIELD   perpst      AS CHAR 
    FIELD   perpend     AS CHAR
    FIELD   perpstat    AS CHAR
    FIELD   preckey     AS CHAR .
                     
  

DEFINE DATASET dsperiodlistview FOR ttperiodlistview.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcompany  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmyr       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmpnum     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmpst      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmpend     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmpstat    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmreckey   AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsperiodlistview.
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

     FOR EACH ttperiodlistview:
        DELETE ttperiodlistview .
    END.

     IF prmAction     = ?  THEN ASSIGN prmAction      = "Select".
     IF prmComp       = ?  THEN ASSIGN prmComp        = "".
     IF prmUser       = ?  THEN ASSIGN prmUser        = "".
     IF prmcompany    = ?  THEN ASSIGN prmcompany     = "".
     IF prmyr         = ?  THEN ASSIGN prmyr          = "0".
     IF prmpnum       = ?  THEN ASSIGN prmpnum        = "0".
     IF prmpst        = ?  THEN ASSIGN prmpst         = "".
     IF prmpend       = ?  THEN ASSIGN prmpend        = "".
     IF prmpstat      = ?  THEN ASSIGN prmpstat       = "".
     IF prmreckey     = ?  THEN ASSIGN prmreckey      = "".
     
{sys/inc/var.i NEW SHARED}

DEF BUFFER b-period FOR period.

DEF VAR op-company AS CHAR NO-UNDO.
DEF VAR ll-secure AS LOG INIT NO NO-UNDO.
DEF NEW SHARED VAR vupdate AS LOG INIT NO.

    
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

       



     IF prmAction = "Search" THEN DO:
         FIND FIRST  company WHERE company.company = prmCompany NO-LOCK NO-ERROR.
         FOR EACH period OF company WHERE (period.yr = int(prmyr) OR prmyr = "0") NO-LOCK:
             CREATE ttperiodlistview.
             ASSIGN 
                 ttperiodlistview.company    = company.company 
                 ttperiodlistview.cname    = company.NAME 
                 ttperiodlistview.peryr      = string(period.yr)
                 ttperiodlistview.perpnum    = string(period.pnum)
                 ttperiodlistview.perpst     = string(period.pst)
                 ttperiodlistview.perpend    = string(period.pend)
                 ttperiodlistview.preckey    = period.rec_key .

             IF period.pstat  THEN
                 ttperiodlistview.perpstat   = "Open" .
             ELSE 
                 ttperiodlistview.perpstat   = "Close" .
            

      END. /*FOR EACH buff-cust  */
 END. /*IF prmAction = "Search" THEN DO:*/


 IF prmAction = "AddNewRec" THEN DO:
    
     DEF VAR period_year AS INT NO-UNDO.
     DEF VAR old-yr LIKE period_year NO-UNDO.
     DEF VAR lv-rowid AS ROWID NO-UNDO.

     DEF BUFFER old-period FOR period.
     DEF BUFFER new-period FOR period.
     DEF BUFFER b-company FOR company.

     ASSIGN op-company = prmCompany .

     FIND FIRST company WHERE company.company = prmCompany NO-LOCK NO-ERROR.
    
     /*{methods/viewers/create/period.i}*/

        FIND FIRST b-company WHERE b-company.company EQ op-company NO-LOCK NO-ERROR.

       
            ASSIGN
                period_year    = INT(prmyr)
                /*period.company = op-company
                period.pstat   = YES*/   .

            RELEASE old-period.

            IF NOT CAN-FIND(FIRST old-period
                            WHERE old-period.company EQ op-company
                            AND old-period.yr      EQ period_year) THEN

                FIND LAST old-period
                WHERE old-period.company EQ op-company
                AND old-period.yr      LT period_year
                NO-LOCK NO-ERROR.

            IF AVAIL old-period THEN DO:
                old-yr = old-period.yr.

                FOR EACH old-period
                    WHERE old-period.company EQ op-company
                    AND old-period.yr      EQ old-yr
                    NO-LOCK:

                    CREATE new-period.
                    BUFFER-COPY old-period to new-period
                        ASSIGN
                        new-period.yr    = period_year
                        new-period.pstat = YES
                        lv-rowid         = ROWID(new-period).
                    DO i = 1 TO 31:
                        new-period.pst   = DATE(MONTH(old-period.pst),
                                                DAY(old-period.pst) + (i - 1),
                                                period_year + (YEAR(old-period.pst) - old-period.yr)) NO-ERROR.
                        IF NOT ERROR-STATUS:ERROR THEN LEAVE.
                    END.

                    DO i = 1 TO 31:
                        new-period.pend  = DATE(MONTH(old-period.pend),
                                                DAY(old-period.pend) - (i - 1),
                                                period_year + (YEAR(old-period.pend) - old-period.yr)) NO-ERROR.
                        IF NOT ERROR-STATUS:ERROR THEN LEAVE.
                    END.
                 END.
             END.

            ELSE
                DO i = 1 TO b-company.num-per:
                FIND FIRST new-period
                    WHERE new-period.company EQ op-company
                    AND new-period.yr      EQ period_year
                    AND new-period.pnum    EQ i
                    AND ROWID(new-period)  NE ROWID(period)
                    NO-LOCK NO-ERROR.

                IF NOT AVAIL new-period THEN DO:
                    CREATE new-period.
                    ASSIGN
                        new-period.company = op-company
                        new-period.yr      = period_year
                        new-period.pnum    = i
                        new-period.pstat   = TRUE
                        lv-rowid           = ROWID(new-period).

                    IF (b-company.yend-off + i) GE 13 AND b-company.yend-off NE 12 THEN
                        period_year = period_year + 1.
                    IF i LE 12 THEN DO:
                        IF b-company.yend-off LT 12 THEN DO:
                            IF (b-company.yend-off + i) MODULO 12 EQ 0 THEN
                                ASSIGN
                                new-period.pst  = DATE(12,1,period_year - 1)
                                new-period.pend = DATE(12,31,period_year - 1).
                            ELSE
                                ASSIGN
                                    new-period.pst  =
                                    DATE(((b-company.yend-off + i) MODULO 12),1,period_year - 1)
                                    new-period.pend =
                                    DATE(((b-company.yend-off + i) MODULO 12) + 1,1,period_year - 1) - 1.
                            END.

                   ELSE DO:
                       IF (company.yend-off + i) MODULO 12 EQ 0 THEN
                           ASSIGN
                           new-period.pst  = DATE(12,1,period_year)
                           new-period.pend = DATE(12,31,period_year).
                       ELSE
                           ASSIGN
                               new-period.pst  =
                               DATE(((company.yend-off + i) modulo 12),1,period_year)
                               new-period.pend =
                               DATE(((company.yend-off + i) modulo 12) + 1,1,period_year) - 1.
                   END.
           END.

           ELSE
               IF i GT 12 THEN
                   ASSIGN
                   new-period.pst = ?
                   new-period.pend = ?.

               IF (b-company.yend-off + i) GE 13 AND b-company.yend-off NE 12 THEN
                   period_year = period_year - 1.
           END.
   END.

   FIND new-period WHERE ROWID(new-period) EQ lv-rowid NO-ERROR.
   /*WHERE new-period.company EQ op-company
        AND new-period.yr      EQ period_year
        AND ROWID(new-period)  NE ROWID(period)
      NO-ERROR.*/

  IF AVAIL new-period THEN DO:
   
    cocode = new-period.company.
    vupdate = IF prmpst = "Yes" THEN TRUE ELSE FALSE .
    RUN gl/reopenpr.p (RECID(new-period)).

    DO TRANSACTION:
      FIND CURRENT company.    
      IF new-period.pnum EQ company.num-per THEN company.yend-per = YES.
      FIND CURRENT company NO-LOCK.
    END.

    ASSIGN
        prmAction = "View"
        prmReckey = new-period.rec_key .

   
  END.

  /*ELSE DO:
    RUN dispatch ("cancel-record").
    RETURN "ADM-ERROR".
  END.*/
 /*END.*/
         
         
   

 END.  /* add new rec*/

 IF prmAction = "UpdateRec" THEN DO:

     FIND FIRST company WHERE company.company = prmCompany NO-LOCK NO-ERROR.
         FIND FIRST period WHERE period.rec_key = prmReckey AND (period.yr = int(prmyr)) EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL period AND NOT period.pstat THEN DO:
       ASSIGN cError = "Period already Close..." .
       RETURN.
     END.
 END.

 IF prmAction = "UpdateRec"  THEN DO:
     DEF VAR hld-pstat LIKE period.pstat NO-UNDO.

     FIND FIRST company WHERE company.company = prmCompany NO-LOCK NO-ERROR.
     FIND FIRST period WHERE period.rec_key = prmReckey AND (period.yr = int(prmyr)) EXCLUSIVE-LOCK NO-ERROR.

     hld-pstat = period.pstat. 

    ASSIGN
         
         period.pst       =  date(prmpst)
         period.pend      =  date(prmpend)
         
        .
   IF NOT hld-pstat AND period.pstat THEN DO:
       cocode = period.company.
       RUN gl/reopenpr.p (RECID(period)).
       
       DO TRANSACTION:
           FIND CURRENT company.    
           IF period.pnum EQ company.num-per THEN company.yend-per = YES.
           FIND CURRENT company NO-LOCK.
       END.
   END.

   ASSIGN
       prmAction = "View".

 END. /* end of update */


 IF prmAction = "Delete" THEN DO:

   FIND FIRST company WHERE company.company = prmCompany NO-LOCK NO-ERROR.
   FIND FIRST period WHERE period.rec_key = prmReckey AND period.company EQ company.company
        AND (period.yr = int(prmyr)) EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL period AND NOT period.pstat THEN DO:
       ASSIGN cError = "Period already Close..." .
       RETURN.
   END.

     IF AVAIL period THEN
         DELETE period .

     FIND LAST period WHERE period.company EQ company.company NO-LOCK NO-ERROR.
      IF AVAIL period THEN
           ASSIGN 
          prmAction = "View"
          prmReckey =  period.rec_key
          prmyr     =   STRING(period.yr) .

 END.
    
IF prmAction = "View" THEN DO:
    FIND FIRST  company WHERE company.company = prmCompany NO-LOCK NO-ERROR.
           FIND FIRST period WHERE period.rec_key = prmReckey AND (period.yr = int(prmyr)) NO-LOCK NO-ERROR.
           IF NOT AVAIL period  THEN DO:
               FIND FIRST period WHERE period.company = company.company NO-LOCK NO-ERROR.
           END.
           CREATE ttperiodlistview.
             IF AVAIL period THEN
                 ASSIGN 
                 ttperiodlistview.company    = company.company 
                 ttperiodlistview.cname    = company.NAME 
                 ttperiodlistview.peryr      = string(period.yr)
                 ttperiodlistview.perpnum    = string(period.pnum)
                 ttperiodlistview.perpst     = string(period.pst)
                 ttperiodlistview.perpend    = string(period.pend)
                 ttperiodlistview.perpstat   = string(period.pstat)
                 ttperiodlistview.preckey    = period.rec_key .
            
 END. /*IF prmAction = "View" THEN DO:*/


 IF prmAction = "CompanyDetail" THEN DO:
     FIND FIRST  company WHERE company.company = prmCompany NO-LOCK NO-ERROR.
           CREATE ttperiodlistview.
              IF AVAIL company THEN
                  ASSIGN 
                  ttperiodlistview.company    = company.company 
                  ttperiodlistview.cname    = company.NAME 
                 .

  END. /*IF prmAction = "View" THEN DO:*/


