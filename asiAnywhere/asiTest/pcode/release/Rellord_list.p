
/*------------------------------------------------------------------------
    File        : Rellord_list.p
    Purpose     : Release
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttReleaseOrderlist NO-UNDO
    FIELD rellno    AS INT     
    FIELD ordno     AS INT         
    FIELD pono      AS CHAR     
    FIELD custno    AS CHAR
    FIELD partno    AS CHAR
    FIELD shipid    AS CHAR
    FIELD ino       AS CHAR
    FIELD reldate   AS CHAR
    FIELD jobno     AS CHAR
    FIELD jobno2    AS INT
    FIELD printed   AS CHAR
    FIELD qty       AS INT
    FIELD posted    AS CHAR
    FIELD carrier   AS CHAR
    FIELD dtchng    AS CHAR
    FIELD usr       AS CHAR
    FIELD trailer   AS CHAR
    FIELD hold      AS CHAR
    FIELD custname  AS CHAR
    FIELD custadd1  AS CHAR
    FIELD custadd2  AS CHAR
    FIELD custcty   AS CHAR
    FIELD custstat  AS CHAR
    FIELD custzip   AS CHAR
    FIELD shipname  AS CHAR
    FIELD shipadd1  AS CHAR
    FIELD shipadd2  AS CHAR
    FIELD shipcty   AS CHAR
    FIELD shipstat  AS CHAR
    FIELD shipzip   AS CHAR
    FIELD lin_ino   AS CHAR
    FIELD qtyord    AS INT
    FIELD qtyrel    AS INT
    FIELD qtyship   AS INT
    FIELD qtyoh     AS INT
    FIELD ship1     AS CHAR
    FIELD ship2     AS CHAR
    FIELD ship3     AS CHAR
    FIELD ship4     AS CHAR
    FIELD reckey    AS CHAR
    FIELD extra     AS CHAR 
    .

DEFINE DATASET dsReleaseOrderlist FOR ttReleaseOrderlist.
    
DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmrellno  AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmordno   AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmpono    AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmcustno  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmpartno  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmshipid  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmino     AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmreldate AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmjobno   AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmjobno2  AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmcarrier AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmtrailer AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmposted  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmship1  AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmship2  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmship3  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmship4  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmreckey  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmextra   AS CHAR   NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReleaseOrderlist .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttReleaseOrderlist:
        DELETE ttReleaseOrderlist .
    END.

IF prmAction        = ?  THEN ASSIGN prmAction    = "Search".
IF prmComp          = ?  THEN ASSIGN prmComp      = "".
IF prmUser          = ?  THEN ASSIGN prmUser      = "".
IF prmrellno        = ?  THEN ASSIGN prmrellno    = 0.
IF prmordno         = ?  THEN ASSIGN prmordno     = 0.
IF prmpono          = ?  THEN ASSIGN prmpono      = "".
IF prmcustno        = ?  THEN ASSIGN prmcustno    = "".
IF prmpartno        = ?  THEN ASSIGN prmpartno    = "".
IF prmshipid        = ?  THEN ASSIGN prmshipid    = "". 
IF prmino           = ?  THEN ASSIGN prmino       = "". 
IF prmreldate       = ?  THEN ASSIGN prmreldate   = "".
IF prmjobno         = ?  THEN ASSIGN prmjobno     = "".
IF prmjobno2        = ?  THEN ASSIGN prmjobno2    = 0.
IF prmcarrier       = ?  THEN ASSIGN prmcarrier   = "".
IF prmtrailer       = ?  THEN ASSIGN prmtrailer   = "".
IF prmposted        = ?  THEN ASSIGN prmposted    = "".
IF prmship1         = ?  THEN ASSIGN prmship1     = "".
IF prmship2         = ?  THEN ASSIGN prmship2     = "".
IF prmship3         = ?  THEN ASSIGN prmship3     = "". 
IF prmship4         = ?  THEN ASSIGN prmship4     = "". 
IF prmextra         = ?  THEN ASSIGN prmextra     = "0" .



DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
def var li-next-r-no as int no-undo.
def var char-hdl as cha no-undo.
DEF VAR li-next-release AS INT NO-UNDO.  
DEF VAR v-date-change-reason  AS CHAR      NO-UNDO.
DEF VAR v-date-reason-note-id AS ROWID     NO-UNDO.
DEF VAR v-save-rec-key LIKE oe-rel.rec_key NO-UNDO.
DEF VAR sdate AS CHAR NO-UNDO.
DEF VAR v-orig-shipid AS CHAR NO-UNDO.
DEF BUFFER bfNotes FOR notes.
def var lv-ship-no like shipto.ship-no no-undo.
def var ll-got-ship-id as log no-undo.
DEF VAR lv-cust-x LIKE oe-bolh.cust-no NO-UNDO.
DEF VAR l-update-reason-perms AS LOG NO-UNDO.
DEF VAR oeDateChange-log AS LOG NO-UNDO.
DEF VAR v-rtn-char AS CHAR NO-UNDO.
DEF VAR v-rec-found AS LOG NO-UNDO.
DEF VAR lv-date LIKE oe-relh.rel-date NO-UNDO.
  
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

RUN sys/ref/nk1look.p (g_company, "oeDateChange", "L", no, no, "", "", 
                          OUTPUT v-rtn-char, OUTPUT v-rec-found).                      
IF v-rec-found THEN
    oeDateChange-log = LOGICAL(v-rtn-char) NO-ERROR.


DEF BUFFER io-shipto FOR shipto.
DEF BUFFER b-oe-ordl-2 FOR oe-ordl.
DEF BUFFER b-oe-rell-2 FOR oe-rell.




IF prmAction = "Search" THEN DO:
     FOR EACH oe-relh WHERE oe-relh.company = prmComp 
         AND (oe-relh.release# eq prmrellno OR prmrellno EQ 0) 
         /*AND oe-relh.stat NE "W"*/
         AND oe-relh.deleted EQ NO             
          AND ((oe-relh.posted  EQ NO  AND prmposted = "No")  OR (oe-relh.posted  EQ YES  AND prmposted = "Yes"))
          AND (oe-relh.cust-no BEGINS prmcustno OR prmcustno EQ "")
         use-index release# NO-LOCK, 
         EACH oe-rell WHERE oe-rell.company = oe-relh.company 
         AND oe-rell.r-no = oe-relh.r-no
         AND (oe-rell.i-no BEGINS prmino OR prmino EQ "")     
          AND (oe-rell.ord-no   EQ prmordno OR prmordno EQ 0) 
          AND (oe-rell.po-no     BEGINS prmpono OR prmpono EQ "")    
          AND (oe-rell.job-no    BEGINS prmjobno OR prmjobno EQ "")    
          AND (oe-rell.job-no2  EQ prmjobno2 OR prmjobno2 EQ 0 OR prmjobno EQ "") 
         use-index r-no NO-LOCK, 
         EACH itemfg OF oe-rell NO-LOCK BY oe-relh.release# DESC:

        CREATE ttReleaseOrderlist.
           ASSIGN 
                 
                 ttReleaseOrderlist.rellno     = oe-relh.release#
                 ttReleaseOrderlist.ordno      = oe-rell.ord-no
                 ttReleaseOrderlist.pono       = oe-rell.po-no
                 ttReleaseOrderlist.custno     = oe-relh.cust-no 
                 ttReleaseOrderlist.partno     = itemfg.part-no 
                 ttReleaseOrderlist.shipid     = oe-relh.ship-id
                 ttReleaseOrderlist.ino        = oe-rell.i-no
                 ttReleaseOrderlist.reldate    = STRING(oe-relh.rel-date)
                 ttReleaseOrderlist.jobno      = oe-rell.job-no 
                 ttReleaseOrderlist.jobno2     = oe-rell.job-no2          
                 ttReleaseOrderlist.printed    = string(oe-relh.printed)
                 ttReleaseOrderlist.qty        = oe-rell.qty
                 ttReleaseOrderlist.trailer    = oe-relh.trailer    
                 ttReleaseOrderlist.dtchng     = oe-relh.spare-char-1   
                 ttReleaseOrderlist.usr        = oe-relh.spare-char-2  
                 ttReleaseOrderlist.carrier    = oe-relh.carrier
                 ttReleaseOrderlist.ship1      = oe-relh.ship-i[1]    
                 ttReleaseOrderlist.ship2      = oe-relh.ship-i[2]  
                 ttReleaseOrderlist.ship3      = oe-relh.ship-i[3]  
                 ttReleaseOrderlist.ship4      = oe-relh.ship-i[4]  
                 ttReleaseOrderlist.reckey     = oe-rell.rec_key 
                 ttReleaseOrderlist.extra      = string(oe-relh.r-no) .
           

            
    END. /*FOR EACH oe-relh  */
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "ValidateAdd" THEN DO:

    IF prmcustno EQ "" OR
       NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ cocode
                      AND cust.cust-no EQ prmcustno
                      AND INDEX("AXSE",cust.active) GT 0) THEN do:
      cError = "Invalid Customer, try help...." .
      RETURN .
    END.

    FIND FIRST io-shipto NO-LOCK
        WHERE io-shipto.company EQ prmComp
        AND io-shipto.cust-no EQ prmcustno
        AND io-shipto.ship-id EQ prmshipid
        USE-INDEX ship-id NO-ERROR.
    IF NOT AVAIL io-shipto THEN
        FOR EACH cust NO-LOCK
            WHERE cust.company EQ prmComp
            AND cust.active  EQ "X",
            EACH io-shipto
            WHERE io-shipto.company EQ cust.company
            AND io-shipto.cust-no EQ cust.cust-no
            AND io-shipto.ship-id EQ prmshipid:
            LEAVE.
        END.
        IF NOT AVAIL io-shipto OR prmshipid EQ "" THEN DO:
            cError = "ShipID doesn't exist for this customer, try help... " .
            RETURN.
        END.

END.

IF prmAction = "CreateAdd" THEN DO:

  /* Code placed here will execute PRIOR to standard behavior. */
/*   10051225 */
/*   find last oe-relh use-index r-no no-lock no-error.            */
/*   li-next-r-no = if avail oe-relh then oe-relh.r-no + 1 else 1. */
  RUN oe/get-r-no.p (INPUT "oe-relh", OUTPUT li-next-r-no).
  RUN oe/release#.p (cocode, OUTPUT li-next-release).

  CREATE oe-relh .
  /* Code placed here will execute AFTER standard behavior.    */
  assign oe-relh.company = cocode
         oe-relh.r-no = li-next-r-no
         oe-relh.rel-date = today
         oe-relh.release# = li-next-release  .
       ASSIGN prmAction = "ViewNew" 
              prmextra  = string(oe-relh.r-no)  .


END.  /* prmActioncreate add */


IF prmAction = "Add" THEN DO:

     FIND FIRST oe-relh WHERE oe-relh.company = prmComp 
         AND oe-relh.r-no eq int(prmextra) EXCLUSIVE-LOCK NO-ERROR.

     sdate = STRING(prmreldate).

  IF prmreldate NE 
      string(oe-relh.rel-date, "99/99/9999")
      AND oeDateChange-log THEN DO:
    FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK,
      FIRST oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.link-no  EQ oe-rell.r-no
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.po-no    EQ oe-rell.po-no        
        USE-INDEX link-ord NO-LOCK .
        LEAVE.
    END.
    IF NOT AVAIL oe-rel THEN DO:
      FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK,
        FIRST oe-rel
        WHERE oe-rel.company  EQ oe-rell.company
          AND oe-rel.ord-no   EQ oe-rell.ord-no
          AND oe-rel.i-no     EQ oe-rell.i-no
          AND oe-rel.line     EQ oe-rell.line
          AND oe-rel.rel-no   EQ oe-rell.rel-no
          AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
          AND oe-rel.po-no    EQ oe-rell.po-no
          AND INDEX("SIL", oe-rel.stat) EQ 0
        USE-INDEX ord-item NO-LOCK .
        LEAVE.
      END.
    END.

    /* prompt user for reason for date change */
    IF AVAIL(oe-rel) THEN DO:
      v-save-rec-key = oe-rel.rec_key.
      /*RUN oe/d-relnot.w 
        (INPUT oe-rel.rec_key, INPUT "R", INPUT "", INPUT "", INPUT 0, INPUT "", INPUT "",
         OUTPUT v-date-change-reason, OUTPUT v-date-reason-note-id).*/
    END.
    IF AVAIL oe-relh AND v-date-change-reason GT "" THEN DO:
   
        FIND CURRENT oe-relh EXCLUSIVE-LOCK.
        IF v-date-change-reason GT "" THEN
            ASSIGN oe-relh.spare-char-1 = v-date-change-reason
                   oe-relh.spare-char-2 = prmUser .
        FIND CURRENT oe-relh NO-LOCK.

        FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK:
          FIND FIRST oe-rel
              WHERE oe-rel.company  EQ oe-rell.company
                AND oe-rel.link-no  EQ oe-rell.r-no
                AND oe-rel.ord-no   EQ oe-rell.ord-no
                AND oe-rel.rel-no   EQ oe-rell.rel-no
                AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
                AND oe-rel.i-no     EQ oe-rell.i-no
                AND oe-rel.line     EQ oe-rell.line
                AND oe-rel.po-no    EQ oe-rell.po-no        
                USE-INDEX link-ord EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAIL oe-rel THEN
            FIND FIRST oe-rel
              WHERE oe-rel.company  EQ oe-rell.company
                AND oe-rel.ord-no   EQ oe-rell.ord-no
                AND oe-rel.i-no     EQ oe-rell.i-no
                AND oe-rel.line     EQ oe-rell.line
                AND oe-rel.rel-no   EQ oe-rell.rel-no
                AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
                AND oe-rel.po-no    EQ oe-rell.po-no
                AND INDEX("SIL", oe-rel.stat) EQ 0
              USE-INDEX ord-item EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL(oe-rel) THEN DO:
             /* Update all related oe-rel records with new update reason
                and copy the note to that rec_key */
             ASSIGN oe-rel.spare-char-2 = v-date-change-reason
                    oe-rel.spare-char-3 = prmUser.

             /* Was already created */
             IF oe-rel.rec_key = v-save-rec-key THEN
                 NEXT.
             FIND notes WHERE ROWID(notes) EQ v-date-reason-note-id NO-LOCK NO-ERROR.
             IF AVAIL notes THEN DO:
                 CREATE bfNotes.
                 BUFFER-COPY notes EXCEPT rec_key TO bfNotes.
                 bfNotes.rec_key = oe-rel.rec_key.
             END.
           END.
        END.
    END.
  END.
ASSIGN
  v-orig-shipid = oe-relh.ship-id
  lv-date       = oe-relh.rel-date.

            ASSIGN 
               /*oe-relh.release#          = prmrellno */
               oe-relh.cust-no           = prmcustno
               oe-relh.ship-id           = prmshipid 
               oe-relh.rel-date          = date(prmreldate)
               oe-relh.carrier           = prmcarrier
               oe-relh.trailer           = prmtrailer
               oe-relh.spare-char-2 = prmUser
                 .

            

            RUN oe/custxship.p (oe-relh.company,
                        prmcustno,
                        prmshipid,
                        BUFFER shipto).

            IF AVAIL shipto THEN 
                ASSIGN
                lv-ship-no                   = shipto.ship-no.

  /* Code placed here will execute AFTER standard behavior.    */
   IF oe-relh.rel-date NE lv-date THEN
      oe-relh.printed = NO. /* task 05211304 */
  
  if lv-ship-no <> 0 then do:
     oe-relh.ship-no = lv-ship-no.
  
     find shipto where shipto.company = oe-relh.company 
                   and shipto.cust-no = oe-relh.cust-no
                   and shipto.ship-no = lv-ship-no
                   no-lock no-error.
     if avail shipto then 
        assign oe-relh.ship-i[1] = shipto.notes[1]
               oe-relh.ship-i[2] = shipto.notes[2]
               oe-relh.ship-i[3] = shipto.notes[3]
               oe-relh.ship-i[4] = shipto.notes[4].
  END.
  

  IF oe-relh.ship-id NE v-orig-shipid THEN
      RUN changed-shipid.

        
        ASSIGN
      prmAction = "View"
       .
                                         
    

END.   /* end of add*/

IF prmAction = "ValidateUpdate" THEN DO:

    FIND FIRST oe-relh WHERE oe-relh.company = prmComp
        AND oe-relh.r-no eq int(prmextra) NO-LOCK NO-ERROR.
    
    IF AVAIL oe-relh AND oe-relh.posted THEN DO:
      cError =  "This release has already been posted, no update allowed." .
      RETURN .
     END.

    IF prmcustno EQ "" OR
       NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ cocode
                      AND cust.cust-no EQ prmcustno
                      AND INDEX("AXSE",cust.active) GT 0) THEN do:
      cError = "Invalid Customer, try help...." .
      RETURN .
    END.

    FIND FIRST io-shipto NO-LOCK
        WHERE io-shipto.company EQ prmComp
        AND io-shipto.cust-no EQ prmcustno
        AND io-shipto.ship-id EQ prmshipid
        USE-INDEX ship-id NO-ERROR.
    IF NOT AVAIL io-shipto THEN
        FOR EACH cust NO-LOCK
            WHERE cust.company EQ prmComp
            AND cust.active  EQ "X",
            EACH io-shipto
            WHERE io-shipto.company EQ cust.company
            AND io-shipto.cust-no EQ cust.cust-no
            AND io-shipto.ship-id EQ prmshipid:
            LEAVE.
        END.
        IF NOT AVAIL io-shipto OR prmshipid EQ "" THEN DO:
            cError = "ShipID doesn't exist for this customer, try help... " .
            RETURN.
        END.

END.  /* end  od validateupdate */ 


IF prmAction = "Update" THEN DO:

    FIND FIRST oe-relh WHERE oe-relh.company = prmComp
        AND oe-relh.r-no eq int(prmextra) EXCLUSIVE-LOCK NO-ERROR.

    sdate = STRING(prmreldate).

IF prmreldate NE 
  string(oe-relh.rel-date, "99/99/9999")
  AND oeDateChange-log  THEN DO:
FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK,
  FIRST oe-rel
  WHERE oe-rel.company  EQ oe-rell.company
    AND oe-rel.link-no  EQ oe-rell.r-no
    AND oe-rel.ord-no   EQ oe-rell.ord-no
    AND oe-rel.rel-no   EQ oe-rell.rel-no
    AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
    AND oe-rel.i-no     EQ oe-rell.i-no
    AND oe-rel.line     EQ oe-rell.line
    AND oe-rel.po-no    EQ oe-rell.po-no        
    USE-INDEX link-ord NO-LOCK .
    LEAVE.
END.
IF NOT AVAIL oe-rel THEN DO:
  FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK,
    FIRST oe-rel
    WHERE oe-rel.company  EQ oe-rell.company
      AND oe-rel.ord-no   EQ oe-rell.ord-no
      AND oe-rel.i-no     EQ oe-rell.i-no
      AND oe-rel.line     EQ oe-rell.line
      AND oe-rel.rel-no   EQ oe-rell.rel-no
      AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
      AND oe-rel.po-no    EQ oe-rell.po-no
      AND INDEX("SIL", oe-rel.stat) EQ 0
    USE-INDEX ord-item NO-LOCK .
    LEAVE.
  END.
END.

/* prompt user for reason for date change */
IF AVAIL(oe-rel) THEN DO:
  v-save-rec-key = oe-rel.rec_key.
  /*RUN oe/d-relnot.w 
    (INPUT oe-rel.rec_key, INPUT "R", INPUT "", INPUT "", INPUT 0, INPUT "", INPUT "",
     OUTPUT v-date-change-reason, OUTPUT v-date-reason-note-id).*/
END.
IF AVAIL oe-relh AND v-date-change-reason GT "" THEN DO:

    FIND CURRENT oe-relh EXCLUSIVE-LOCK.
    IF v-date-change-reason GT "" THEN
        ASSIGN oe-relh.spare-char-1 = v-date-change-reason
               oe-relh.spare-char-2 = prmUser
               oe-relh.spare-char-2 = prmUser .
    FIND CURRENT oe-relh NO-LOCK.

    FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK:
      FIND FIRST oe-rel
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.link-no  EQ oe-rell.r-no
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.po-no    EQ oe-rell.po-no        
            USE-INDEX link-ord EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
            AND INDEX("SIL", oe-rel.stat) EQ 0
          USE-INDEX ord-item EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL(oe-rel) THEN DO:
         /* Update all related oe-rel records with new update reason
            and copy the note to that rec_key */
         ASSIGN oe-rel.spare-char-2 = v-date-change-reason
                oe-rel.spare-char-3 = prmUser.

         /* Was already created */
         IF oe-rel.rec_key = v-save-rec-key THEN
             NEXT.
         FIND notes WHERE ROWID(notes) EQ v-date-reason-note-id NO-LOCK NO-ERROR.
         IF AVAIL notes THEN DO:
             CREATE bfNotes.
             BUFFER-COPY notes EXCEPT rec_key TO bfNotes.
             bfNotes.rec_key = oe-rel.rec_key.
         END.
       END.
    END.
  END.
END.
   assign
       v-orig-shipid = oe-relh.ship-id
       lv-date       = oe-relh.rel-date.

        ASSIGN oe-relh.ship-id           = prmshipid 
               oe-relh.rel-date          = date(prmreldate)
               oe-relh.carrier           = prmcarrier
               oe-relh.trailer           = prmtrailer
                 .

        RUN oe/custxship.p (oe-relh.company,
                        prmcustno,
                        prmshipid,
                        BUFFER shipto).

            IF AVAIL shipto THEN 
                ASSIGN
                lv-ship-no                   = shipto.ship-no.

  /* Code placed here will execute AFTER standard behavior.    */
  IF oe-relh.rel-date NE lv-date THEN
      oe-relh.printed = NO. /* task 05211304 */

  if lv-ship-no <> 0 then do:
     oe-relh.ship-no = lv-ship-no.
  
     find shipto where shipto.company = oe-relh.company 
                   and shipto.cust-no = oe-relh.cust-no
                   and shipto.ship-no = lv-ship-no
                   no-lock no-error.
     /*if avail shipto then 
        assign oe-relh.ship-i[1] = shipto.notes[1]
               oe-relh.ship-i[2] = shipto.notes[2]
               oe-relh.ship-i[3] = shipto.notes[3]
               oe-relh.ship-i[4] = shipto.notes[4].*/
  END.

  IF oe-relh.ship-id NE v-orig-shipid THEN
      RUN changed-shipid.

        ASSIGN
      prmAction = "View" .

END.     /* end of update */

IF prmAction = "UpdateNotes" THEN DO:

    FIND FIRST oe-relh WHERE oe-relh.company = prmComp
        AND oe-relh.r-no eq int(prmextra) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL oe-relh THEN
    assign oe-relh.ship-i[1] = prmship1
               oe-relh.ship-i[2] = prmship2
               oe-relh.ship-i[3] = prmship3

               oe-relh.ship-i[4] = prmship4 .
    ASSIGN
        prmAction = "View" .

END. /* update notes  */


IF prmAction = "DeleteValidate" THEN DO:

     FIND FIRST  oe-relh WHERE oe-relh.company = prmComp
         AND oe-relh.r-no eq int(prmextra) NO-LOCK NO-ERROR.

     IF AVAIL oe-relh AND oe-relh.posted THEN DO:
      cError =  "This release has already been posted, no delete allowed." .
      RETURN .
     END.
END. /* DeleteValidate  */

IF prmAction = "Delete" THEN DO:

     FIND FIRST  oe-relh WHERE oe-relh.company = prmComp
         AND oe-relh.r-no eq int(prmextra) EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL oe-relh  THEN
         DELETE oe-relh .

     FIND FIRST oe-relh WHERE oe-relh.company = prmComp NO-LOCK NO-ERROR.
     IF AVAIL oe-relh THEN
         ASSIGN
         prmextra = STRING(oe-relh.r-no)
         prmAction = "View" .

END.  /* action delete */ 

IF prmAction = "View" THEN DO:
     FIND FIRST oe-relh WHERE oe-relh.company = prmComp
         AND oe-relh.r-no eq int(prmextra)
         AND oe-relh.stat NE "W"
         AND oe-relh.deleted EQ NO   NO-LOCK NO-ERROR.     
        /* AND oe-relh.posted  EQ (IF prmposted EQ "Yes" THEN YES ELSE NO)*/
         IF AVAIL oe-relh THEN
         FIND FIRST oe-rell WHERE oe-rell.company = oe-relh.company 
         AND oe-rell.rec_key EQ prmReckey
         AND oe-rell.r-no = oe-relh.r-no NO-LOCK NO-ERROR.
         IF AVAIL oe-rell  THEN
         FIND FIRST itemfg OF oe-rell NO-LOCK NO-ERROR.
 
       IF AVAIL oe-relh THEN DO:
       
        CREATE ttReleaseOrderlist.
           ASSIGN 
                 
                 ttReleaseOrderlist.rellno     = oe-relh.release#
                 ttReleaseOrderlist.custno     = oe-relh.cust-no 
                 ttReleaseOrderlist.shipid     = oe-relh.ship-id 
                 ttReleaseOrderlist.reldate    = STRING(oe-relh.rel-date)
                 ttReleaseOrderlist.printed    = string(oe-relh.printed) 
                 ttReleaseOrderlist.trailer    = oe-relh.trailer    
                 ttReleaseOrderlist.dtchng     = oe-relh.spare-char-1   
                 ttReleaseOrderlist.usr        = oe-relh.spare-char-2  
                 ttReleaseOrderlist.carrier    = oe-relh.carrier
                 ttReleaseOrderlist.ship1      = oe-relh.ship-i[1]    
                 ttReleaseOrderlist.ship2      = oe-relh.ship-i[2]  
                 ttReleaseOrderlist.ship3      = oe-relh.ship-i[3]  
                 ttReleaseOrderlist.ship4      = oe-relh.ship-i[4]       
                 ttReleaseOrderlist.extra      = string(oe-relh.r-no) 
                 ttReleaseOrderlist.posted     = STRING(oe-relh.posted) .
           IF AVAIL itemfg THEN
            ttReleaseOrderlist.partno     = itemfg.part-no .
                IF AVAIL oe-rell THEN
                    ASSIGN
                    ttReleaseOrderlist.ordno      = oe-rell.ord-no
                    ttReleaseOrderlist.pono       = oe-rell.po-no
                    ttReleaseOrderlist.ino        = oe-rell.i-no
                    ttReleaseOrderlist.jobno      = oe-rell.job-no 
                    ttReleaseOrderlist.jobno2     = oe-rell.job-no2 
                    ttReleaseOrderlist.qty        = oe-rell.qty
                    ttReleaseOrderlist.reckey     = oe-rell.rec_key 
                    .
 

           if avail oe-relh then do:
               
               find cust where cust.company = prmComp and
                   cust.cust-no = oe-relh.cust-no no-lock no-error.
               if avail cust then 
                   assign ttReleaseOrderlist.custname = cust.name
                   ttReleaseOrderlist.custadd1 = cust.addr[1]
                   ttReleaseOrderlist.custadd2 = cust.addr[2]
                   ttReleaseOrderlist.custcty  = cust.city
                   ttReleaseOrderlist.custstat = cust.state
                   ttReleaseOrderlist.custzip  = cust.zip .
                   
                   FIND FIRST io-shipto NO-LOCK
                       WHERE io-shipto.company EQ prmComp
                       AND io-shipto.cust-no EQ oe-relh.cust-no
                       AND io-shipto.ship-id EQ oe-relh.ship-id
                       USE-INDEX ship-id NO-ERROR.
                   IF NOT AVAIL io-shipto THEN
                       FOR EACH cust NO-LOCK
                            WHERE cust.company EQ prmComp
                            AND cust.active  EQ "X",
                        EACH io-shipto
                            WHERE io-shipto.company EQ cust.company
                            AND io-shipto.cust-no EQ cust.cust-no
                            AND io-shipto.ship-id EQ oe-relh.ship-id:
                       LEAVE.
                       END.

                   if avail io-shipto then 
                       assign ttReleaseOrderlist.shipname = io-shipto.ship-name
                       ttReleaseOrderlist.shipadd1  = io-shipto.ship-addr[1]
                       ttReleaseOrderlist.shipadd2  = io-shipto.ship-addr[2]
                       ttReleaseOrderlist.shipcty   = io-shipto.ship-city
                       ttReleaseOrderlist.shipstat  = io-shipto.ship-state
                       ttReleaseOrderlist.shipzip   = io-shipto.ship-zip .
                      

                   IF oe-relh.w-ord THEN
                       ASSIGN 
                            ttReleaseOrderlist.hold = "On Hold".
                   ELSE
                       ASSIGN
                           ttReleaseOrderlist.hold = "Approved".
           END.
           else 
               assign 
                   ttReleaseOrderlist.shipname  = ""
                   ttReleaseOrderlist.shipadd1  = ""
                   ttReleaseOrderlist.shipadd2  = ""
                   ttReleaseOrderlist.shipcty   = ""
                   ttReleaseOrderlist.shipstat  = ""
                   ttReleaseOrderlist.shipzip   = ""
                   ttReleaseOrderlist.custname = ""
                   ttReleaseOrderlist.custadd1 = ""
                   ttReleaseOrderlist.custadd2 = ""
                   ttReleaseOrderlist.custcty  = ""
                   ttReleaseOrderlist.custstat = ""
                   ttReleaseOrderlist.custzip  = "".  

            find first b-oe-rell-2
                WHERE b-oe-rell-2.company EQ oe-relh.company
                AND b-oe-rell-2.r-no    EQ oe-relh.r-no
                USE-INDEX r-no no-lock no-error.
            
            IF AVAIL b-oe-rell-2 THEN DO:
                ttReleaseOrderlist.lin_ino = if avail b-oe-rell-2 then b-oe-rell-2.i-no else "".
            END.
       /*     FIND FIRST itemfg where itemfg.company = prmComp and
                    itemfg.i-no = oe-rell.i-no no-lock no-error.
            IF AVAIL itemfg THEN
                ASSIGN
                    ttReleaseOrderlist.qtyoh = itemfg.q-onh. */

            find first b-oe-ordl-2 where
                b-oe-ordl-2.company = prmComp AND
                b-oe-ordl-2.ord-no = b-oe-rell-2.ord-no AND
                b-oe-ordl-2.i-no = b-oe-rell-2.i-no
                no-lock no-error.

            if avail b-oe-ordl-2 then
                assign
                ttReleaseOrderlist.qtyord = b-oe-ordl-2.qty
                ttReleaseOrderlist.qtyrel = b-oe-ordl-2.t-rel-qty
                ttReleaseOrderlist.qtyship = b-oe-ordl-2.ship-qty .
            else 
                assign 
                    ttReleaseOrderlist.qtyord  = 0
                    ttReleaseOrderlist.qtyrel  = 0
                    ttReleaseOrderlist.qtyship = 0 .

                
            
    END. /*FOR EACH oe-relh  */
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "ViewNew" THEN DO:
     FOR EACH oe-relh WHERE oe-relh.company = prmComp
         AND oe-relh.r-no eq int(prmextra)
         /*AND oe-relh.stat NE "W"*/ NO-LOCK:
         
        CREATE ttReleaseOrderlist.
           ASSIGN 
                 
                 ttReleaseOrderlist.rellno     = oe-relh.release#
                 ttReleaseOrderlist.custno     = oe-relh.cust-no 
                 ttReleaseOrderlist.shipid     = oe-relh.ship-id
                 ttReleaseOrderlist.reldate    = STRING(oe-relh.rel-date)
                 ttReleaseOrderlist.printed    = string(oe-relh.printed)
                 ttReleaseOrderlist.trailer    = oe-relh.trailer    
                 ttReleaseOrderlist.dtchng     = oe-relh.spare-char-1   
                 ttReleaseOrderlist.usr        = oe-relh.spare-char-2  
                 ttReleaseOrderlist.carrier    = oe-relh.carrier
                 ttReleaseOrderlist.ship1      = oe-relh.ship-i[1]    
                 ttReleaseOrderlist.ship2      = oe-relh.ship-i[2]  
                 ttReleaseOrderlist.ship3      = oe-relh.ship-i[3]  
                 ttReleaseOrderlist.ship4      = oe-relh.ship-i[4]  
                 ttReleaseOrderlist.extra      = string(oe-relh.r-no) .
            
    END. /*FOR EACH oe-relh  */
END. /*IF prmAction = "Select" THEN DO:*/


 


/******************************************** pr *******************************************************/

PROCEDURE changed-shipid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-oe-rell FOR oe-rell.
  DEF BUFFER bf-oe-rel FOR oe-rel.
  DEF BUFFER bf-itemfg-loc FOR itemfg-loc.
  DEF VAR v-row AS ROWID NO-UNDO.    
  DEF VAR v-q-back LIKE itemfg.q-back NO-UNDO.

    /* 10021210 - Per Joe, if they change the shipto must update the 
       locations on oe-rell and oe-rel                                 */
       
    RUN oe/custxship.p (oe-relh.company,
                        oe-relh.cust-no,
                        oe-relh.ship-id,
                        BUFFER shipto).

    FOR EACH bf-oe-rell WHERE  bf-oe-rell.r-no EQ oe-relh.r-no EXCLUSIVE-LOCK:

        bf-oe-rell.loc = shipto.loc.
        RUN get-oe-rel (INPUT ROWID(bf-oe-rell), OUTPUT v-row).
        FIND bf-oe-rel 
            WHERE ROWID(bf-oe-rel) EQ v-row
            EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL bf-oe-rel THEN DO: 
            /* Back out inventory for this location */
           FIND FIRST bf-itemfg-loc
               WHERE bf-itemfg-loc.company EQ bf-oe-rel.company
                 AND bf-itemfg-loc.i-no    EQ bf-oe-rel.i-no
                 AND bf-itemfg-loc.loc     EQ bf-oe-rel.spare-char-1
               EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL bf-itemfg-loc THEN DO:
           
               FIND itemfg WHERE itemfg.company EQ bf-oe-rel.company
                    AND itemfg.i-no EQ bf-oe-rel.i-no
                   NO-LOCK NO-ERROR.
               IF AVAIL itemfg AND AVAIL(bf-itemfg-loc) THEN
                 RUN fg/calcqabl.p (ROWID(itemfg), bf-oe-rel.spare-char-1, OUTPUT bf-itemfg-loc.q-alloc, OUTPUT v-q-back).
    
               bf-itemfg-loc.q-avail = bf-itemfg-loc.q-onh + bf-itemfg-loc.q-ono - bf-itemfg-loc.q-alloc.
    
               ASSIGN  bf-oe-rel.spare-char-1   = shipto.loc
                       bf-oe-rel.ship-zip       = shipto.ship-zip
                       bf-oe-rel.ship-state     = shipto.ship-state           
                       bf-oe-rel.ship-city      = shipto.ship-city
                       bf-oe-rel.ship-addr[1]   = shipto.ship-addr[1]
                       bf-oe-rel.ship-addr[2]   = shipto.ship-addr[2]
                       bf-oe-rel.ship-id        = shipto.ship-id.
    
    
                RUN fg/fgitmloc.p (INPUT bf-oe-rel.i-no, INPUT ROWID(bf-oe-rel)).  
           END. /* avail itemfg-loc */
        END. /* each bf-oe-rell */
            
    END.

END PROCEDURE.
