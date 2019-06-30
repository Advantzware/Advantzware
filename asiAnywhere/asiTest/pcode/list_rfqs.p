
/*------------------------------------------------------------------------
    File        : list_rfqs.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     : tue Feb 12 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cocode LIKE oe-ordl.company.

{list_rfqs.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vRfqNo         AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmPartno    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vPartDscr  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vStyle  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLength      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER vWidth      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vDepth      AS CHAR  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsListRfqs.
DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ll-is-corr-style   AS LOG  NO-UNDO.
def var K_frac as dec init 6.25 no-undo.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
                 
IF prmComp = ?   THEN ASSIGN prmComp = "".
IF prmCust = ?   THEN ASSIGN prmCust = "".
IF prmUser = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF vRfqNo = ?  THEN ASSIGN vRfqNo = 0.
IF prmPartno = ? THEN ASSIGN prmPartno = "".
IF vPartDscr = ? THEN ASSIGN vPartDscr = "".
IF vStyle = ? THEN ASSIGN vStyle = "".
IF vLength = ?   THEN ASSIGN vLength = "".
IF prmEst = ?   THEN ASSIGN prmEst = "".
IF vWidth = ?   THEN ASSIGN vWidth = "".
IF vDepth = ?   THEN ASSIGN vDepth = "".

IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.
    
   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

IF prmAction = "Search" THEN DO:
    IF prmCust NE "" THEN DO:
        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp 
            AND usercust.cust-no = prmCust NO-LOCK NO-ERROR.
        IF NOT AVAIL usercust THEN DO: 
            ASSIGN cError = "Invalid Customer".
             RETURN.
        END.
       
    END.

    IF vRfqNo NE 0 THEN DO:
        FIND FIRST rfq WHERE rfq.company = prmComp                           
                   AND rfq.rfq-no = vRfqNo NO-LOCK NO-ERROR.
        IF NOT AVAIL rfq THEN DO: 
            ASSIGN cError = "Rfq Not Available".
             RETURN.
        END.
       
    END.
END.

/*make dynamic query*/
IF prmAction = "Select" THEN DO:
    v-count = 0.
 MAIN-LOOP:  
    FOR EACH  usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp NO-LOCK,
        EACH rfq WHERE rfq.company = prmComp AND
            (rfq.cust-no = usercust.cust-no) NO-LOCK,
             EACH rfqitem of rfq WHERE rfqitem.seq < 999  NO-LOCK:   
                CREATE ttListRfqs.
                ASSIGN 
                    ttListRfqs.vRfqNo = rfq.rfq-no 
                    ttListRfqs.prmPartno = rfqitem.part-no 
                    ttListRfqs.prmCust = rfq.cust-no 
                    ttListRfqs.ReqDt = rfq.req-date 
                    ttListRfqs.vStyle = rfqitem.style 
                    ttListRfqs.Procat = rfqitem.procat 
                    ttListRfqs.Qty = rfqitem.qty[1] 
                    ttListRfqs.iCol = rfqitem.i-col 
                    ttListRfqs.iCoat = rfqitem.i-coat 
                    ttListRfqs.Board = rfqitem.board 
                    ttListRfqs.vCal = rfqitem.cal 
                    ttListRfqs.prmEst =  rfqitem.est-no 
                    ttListRfqs.vPartDscr = rfqitem.part-dscr1 
                    ttListRfqs.SeqNo     = rfqitem.seq
                    .
                find style where style.company = rfq.company AND style.style = rfqitem.style
                    no-lock no-error.
                if avail style and style.industry = "2" then   ll-is-corr-style = yes.
                else ll-is-corr-style = no.
                     
                     if ll-is-corr-style then 
                         ASSIGN ttListRfqs.vLength = round(trunc(rfqitem.len,0) + ((rfqitem.len - trunc(rfqitem.len,0)) / K_FRAC),2).
                     else ASSIGN ttListRfqs.vLength = rfqitem.len.
                     if ll-is-corr-style then 
                         ASSIGN ttListRfqs.vWidth = round(trunc(rfqitem.wid,0) + ((rfqitem.wid - trunc(rfqitem.wid,0)) / K_FRAC),2).
                     else ASSIGN ttListRfqs.vWidth = rfqitem.wid.
                     if ll-is-corr-style then 
                         ASSIGN ttListRfqs.vDepth = round(trunc(rfqitem.dep,0) + ((rfqitem.dep - trunc(rfqitem.dep,0)) / K_FRAC),2).
                     else ASSIGN ttListRfqs.vDepth = rfqitem.dep.
             
             v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.
        
    END. /*for each usercust*/
END. /*IF prmAction = "Select" THEN DO:*/
IF prmEst NE "" THEN vEst = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst).

/*needs to be dynamic query*/

IF prmAction = "Search" THEN DO:
    v-count = 0.
 MAIN-LOOP:
    FOR EACH  usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp NO-LOCK,
         EACH rfq WHERE rfq.company = prmComp AND
                           (rfq.cust-no = usercust.cust-no) 
                   AND (rfq.rfq-no = vRfqNo OR vRfqNo = 0 )
                   AND (rfq.cust-no = prmCust OR prmCust = "") NO-LOCK,
               EACH rfqitem  OF rfq WHERE  rfqitem.seq < 999  NO-LOCK:
                           IF NOT ((rfqitem.part-no BEGINS prmPartno OR prmPartno = "")
                              AND (rfqitem.part-dscr1 BEGINS vPartDscr OR vPartDscr = "")                                 
                              AND (rfqitem.style BEGINS vStyle OR vStyle = "")                 
                              AND (rfqitem.len = DEC( vLength) OR vLength = "" OR vLength = "0.0000" OR vLength = ".0000")
                              AND ( rfqitem.est-no = vEst OR vEst = "")
                              AND (rfqitem.wid = DEC( vWidth) OR vWidth = "" OR vWidth = "0.0000" OR vWidth = ".0000")
                              AND (rfqitem.dep = DEC( vDepth) OR vDepth = "" OR vDepth = "0.0000" OR vDepth = ".0000") )
                              THEN NEXT.

                        CREATE ttListRfqs.
                        ASSIGN 
                            ttListRfqs.vRfqNo = rfq.rfq-no 
                            ttListRfqs.prmPartno = rfqitem.part-no 
                            ttListRfqs.prmCust = rfq.cust-no 
                            ttListRfqs.ReqDt = rfq.req-date 
                            ttListRfqs.vStyle = rfqitem.style 
                            ttListRfqs.Procat = rfqitem.procat 
                            ttListRfqs.Qty = rfqitem.qty[1] 
                            ttListRfqs.iCol = rfqitem.i-col 
                            ttListRfqs.iCoat = rfqitem.i-coat 
                            ttListRfqs.Board = rfqitem.board 
                            ttListRfqs.vCal = rfqitem.cal 
                            ttListRfqs.prmEst =  rfqitem.est-no 
                            ttListRfqs.vPartDscr = rfqitem.part-dscr1 
                            ttListRfqs.SeqNo     = rfqitem.seq
                            .
            
                        find style where style.company = rfq.company AND style.style = rfqitem.style
                            no-lock no-error.
                        if avail style and style.industry = "2" then   ll-is-corr-style = yes.
                        else ll-is-corr-style = no.

                        if ll-is-corr-style then 
                            ASSIGN ttListRfqs.vLength = round(trunc(rfqitem.len,0) + ((rfqitem.len - trunc(rfqitem.len,0)) / K_FRAC),2).
                        else ASSIGN ttListRfqs.vLength = rfqitem.len.
                        if ll-is-corr-style then 
                            ASSIGN ttListRfqs.vWidth = round(trunc(rfqitem.wid,0) + ((rfqitem.wid - trunc(rfqitem.wid,0)) / K_FRAC),2).
                        else ASSIGN ttListRfqs.vWidth = rfqitem.wid.
                        if ll-is-corr-style then 
                            ASSIGN ttListRfqs.vDepth = round(trunc(rfqitem.dep,0) + ((rfqitem.dep - trunc(rfqitem.dep,0)) / K_FRAC),2).
                        else ASSIGN ttListRfqs.vDepth = rfqitem.dep.
           
             v-count = v-count + 1.
                      IF v-count = 200 THEN LEAVE MAIN-LOOP.

END. /*IF prmAction = "Search" THEN DO:*/
    
END.

