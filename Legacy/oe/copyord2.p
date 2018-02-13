&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* replacement for copyOrder.i */
/* Attempt to improve problem with mixup in order number with other order */

DEFINE INPUT PARAMETER ipFromCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipFromOrdNo AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ipToOrdNo AS INTEGER NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER ipfil_id AS RECID.
DEFINE INPUT-OUTPUT PARAMETER ipv-qty-mod AS LOG NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ipnufile AS LOG NO-UNDO.
DEFINE INPUT  PARAMETER iplIncrementOrder AS LOGICAL     NO-UNDO.

DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef FOR ef.
DEF NEW SHARED BUFFER xeb FOR eb.

DEF BUFFER xop FOR est-op.

DEF NEW SHARED VAR xcal    AS de NO-UNDO.
DEF NEW SHARED VAR sh-wid  AS de NO-UNDO.
DEF NEW SHARED VAR sh-len  AS de NO-UNDO.
DEF NEW SHARED VAR fil_id  AS RECID NO-UNDO.
DEF NEW SHARED VAR maxco   AS INT NO-UNDO.
DEF NEW SHARED VAR qty     AS INT NO-UNDO.
DEF NEW SHARED VAR v-qty-mod AS LOG NO-UNDO.
DEF NEW SHARED VAR nufile AS LOG INITIAL YES NO-UNDO.
DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.
DEF VAR oeDateAuto-log  AS LOG  NO-UNDO.
DEF VAR oeDateAuto-char AS CHAR NO-UNDO.
DEF VAR v-rec-found AS LOG  NO-UNDO.
DEF VAR v-rtn-char  AS CHAR NO-UNDO.

DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.    /* BUFFER WITH ORDER HEADER */
DEF BUFFER xoe-rel FOR oe-rel.
DEF BUFFER b-oe-ord  FOR oe-ord.
DEF BUFFER b-oe-ordl FOR oe-ordl.

{custom/globdefs.i}

fil_id = ipfil_id.
v-qty-mod = ipv-qty-mod.
nufile = ipnufile.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR locode AS CHAR NO-UNDO.
/*DEF VAR g_company AS CHAR NO-UNDO.*/
cocode = ipToCompany.
g_company = ipToCompany.
locode  = g_loc .

{sys/inc/oeship.i}
{sys/inc/shiptorep.i}
RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateAuto-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateAuto-char = v-rtn-char NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN copyOrder (
    INPUT ipFromCompany,
    INPUT ipToCompany,
    INPUT ipFromOrdNo,
    INPUT ipToOrdNo).
ipfil_id = fil_id.
ipv-qty-mod = v-qty-mod.
ipnufile = nufile.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-copyBuildRoute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyBuildRoute Procedure 
PROCEDURE copyBuildRoute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER inEstNum LIKE est.est-no.
  DEFINE INPUT PARAMETER inComp LIKE est.company.

  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR lv-msg1 AS CHAR NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.

  DEF BUFFER buff-est-qty FOR est-qty.

  {est/checkuse.i}  

  FIND FIRST est WHERE est.est-no = inEstNum
                 AND est.company = inComp NO-LOCK NO-ERROR.

  ASSIGN
      cocode = est.company.  

  FIND xest WHERE RECID(xest) EQ recid(est).  

  FIND FIRST est-qty WHERE est-qty.est-no = xest.est-no 
                     AND est-qty.company = xest.company NO-LOCK NO-ERROR.

  ll = NO.

  IF xest.est-type GE 7 THEN DO:
    FOR EACH ef
        WHERE ef.company EQ est-qty.company
          AND ef.est-no  EQ est-qty.est-no
        NO-LOCK:
      
      RUN set-lock (ef.form-no, NO).
    END.    
    
    FIND FIRST xef WHERE xef.company = est-qty.company 
                     AND xef.est-no = est-qty.est-no
                   NO-LOCK NO-ERROR.
    FIND FIRST xeb WHERE xeb.company = est-qty.company 
                     AND xeb.est-no = est-qty.est-no
                     AND xeb.form-no = xef.form-no
                   NO-LOCK NO-ERROR.

    RUN cec/mach-seq.p (0, 0, xest.est-type EQ 8).
  END.

  ELSE DO:
    FOR EACH buff-est-qty
        WHERE buff-est-qty.company EQ est-qty.company
          AND buff-est-qty.est-no  EQ est-qty.est-no
        NO-LOCK BREAK BY buff-est-qty.eqty:

      IF FIRST(buff-est-qty.eqty)    AND
         NOT LAST(buff-est-qty.eqty) THEN
        MESSAGE "Build routings for all quantities?" SKIP
                "  (Yes=AllQtys    No=" +
                TRIM(STRING(est-qty.eqty,">>>,>>>,>>>")) + " Only)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.

      IF ll OR ROWID(est-qty) EQ ROWID(buff-est-qty) THEN DO:  
        FOR EACH ef
            WHERE ef.company EQ buff-est-qty.company
              AND ef.est-no  EQ buff-est-qty.est-no
            NO-LOCK:
      
          RUN set-lock (ef.form-no, NO).
        END.    
    
        FIND FIRST xef WHERE xef.company = buff-est-qty.company 
                         AND xef.est-no = buff-est-qty.est-no
                       NO-LOCK NO-ERROR.
        FIND FIRST xeb WHERE xeb.company = buff-est-qty.company 
                         AND xeb.est-no = buff-est-qty.est-no
                         AND xeb.form-no = xef.form-no
                       NO-LOCK NO-ERROR.

        RUN cec/mach-seq.p (0, buff-est-qty.eqty, NO).
      END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyCust) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyCust Procedure 
PROCEDURE copyCust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipCustno AS CHARACTER NO-UNDO.

  DEF BUFFER b-cust        FOR cust.
  DEF BUFFER b-cust-markup FOR cust-markup.
  DEF BUFFER b-shipto      FOR shipto.
  DEF BUFFER b-soldto      FOR soldto.
  DEF BUFFER b-notes       FOR notes.

  DEF VAR lv-rec_key LIKE cust.rec_key NO-UNDO.
  

  SESSION:SET-WAIT-STATE("general").

  FIND FIRST b-cust
      WHERE b-cust.company EQ ipToCompany
        AND b-cust.cust-no EQ ipCustno
      NO-LOCK NO-ERROR. 

  IF NOT AVAIL b-cust THEN DO:
      
      FIND FIRST cust
      WHERE cust.company EQ ipFrmCompany
        AND cust.cust-no EQ ipCustno
      NO-LOCK NO-ERROR.

      IF AVAIL cust THEN DO:
      
        lv-rec_key = STRING(TODAY,"99999999") +
               STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
        CREATE rec_key.
         ASSIGN
          rec_key.rec_key    = lv-rec_key
          rec_key.table_name = "CUST".
           
         CREATE b-cust.
         BUFFER-COPY cust EXCEPT rec_key TO b-cust
         ASSIGN
          b-cust.company    = ipToCompany
          b-cust.cust-no    = ipCustno
          b-cust.ytd-sales  = 0
          b-cust.lyr-sales  = 0
          b-cust.cost       = 0
          b-cust.comm       = 0
          b-cust.ytd-msf    = 0
          b-cust.lyytd-msf  = 0
          b-cust.hibal      = 0
          b-cust.hibal-date = ?
          b-cust.num-inv    = 0
          b-cust.lpay       = 0
          b-cust.lpay-date  = ?
          b-cust.avg-pay    = 0
          b-cust.ord-bal    = 0
          b-cust.acc-bal    = 0
          b-cust.on-account = 0.
         
         FOR EACH cust-markup
             WHERE cust-markup.company EQ cust.company
               AND cust-markup.cust-no    EQ cust.cust-no
             NO-LOCK:
         
            CREATE b-cust-markup.
            BUFFER-COPY cust-markup EXCEPT rec_key TO b-cust-markup
            ASSIGN
             b-cust-markup.company = b-cust.company
             b-cust-markup.cust-no = b-cust.cust-no.
         END.
         
         FOR EACH shipto
             WHERE shipto.company EQ cust.company
               AND shipto.cust-no EQ cust.cust-no
             NO-LOCK:

            CREATE b-shipto.
            BUFFER-COPY shipto EXCEPT rec_key TO b-shipto
            ASSIGN
             b-shipto.company = b-cust.company
             b-shipto.cust-no = b-cust.cust-no.
         END.
         
         FOR EACH soldto
             WHERE soldto.company EQ cust.company
               AND soldto.cust-no    EQ cust.cust-no
             NO-LOCK:
           CREATE b-soldto.
           BUFFER-COPY soldto EXCEPT rec_key TO b-soldto
           ASSIGN
            b-soldto.company = b-cust.company
            b-soldto.cust-no    = b-cust.cust-no.
         END.
         
         FOR EACH notes WHERE notes.rec_key EQ cust.rec_key NO-LOCK:
           CREATE b-notes.
           BUFFER-COPY notes TO b-notes
           ASSIGN b-notes.rec_key = lv-rec_key.
         END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyEst) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyEst Procedure 
PROCEDURE copyEst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipIno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipPartno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER n-est-no LIKE est.est-no.
         
    DEF BUFFER boxd-hdr FOR box-design-hdr.
    DEF BUFFER boxd-lin FOR box-design-line.
    DEF BUFFER reftable1 FOR reftable.
    DEF BUFFER b-est FOR est.
    DEF BUFFER b-ef FOR ef.
    DEF BUFFER b-eb FOR eb.   
    DEF BUFFER b-eb1 FOR eb.
    DEF BUFFER b-est-prep FOR est-prep.    
    DEF BUFFER b-est-inst FOR est-inst.
    DEF BUFFER b-est-flm FOR est-flm.
    DEF BUFFER b-est-qty FOR est-qty.
    DEF BUFFER b-notes FOR notes.
    DEF BUFFER reftable2 FOR reftable.    
    DEF BUFFER b-probe FOR probe.

    DEF VAR v-est-no LIKE est.est-no.
    DEF VAR lv-rec_key LIKE est.rec_key NO-UNDO.
  

    SESSION:SET-WAIT-STATE("general").

    FIND FIRST ce-ctrl WHERE ce-ctrl.company = ipToCompany EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN
           ce-ctrl.e-num = ce-ctrl.e-num + 1
           v-est-no      = STRING(ce-ctrl.e-num,">>>>>>>9").    

    FIND FIRST b-eb
      WHERE b-eb.company EQ ipToCompany
        AND b-eb.part-no EQ ipPartno
        AND b-eb.stock-no EQ ipIno
      NO-LOCK NO-ERROR.

    IF NOT AVAIL b-eb THEN DO:    
        FIND FIRST est
          WHERE est.company EQ ipFrmCompany
            AND est.est-no EQ ipEstno
          NO-LOCK NO-ERROR.        
        
        IF AVAIL est THEN DO:

            ASSIGN
                n-est-no   = v-est-no
                lv-rec_key = STRING(TODAY,"99999999") +
                      STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
            CREATE rec_key.
            ASSIGN
                rec_key.rec_key    = lv-rec_key
                rec_key.table_name = "EST".                   
        
            CREATE b-est.
            BUFFER-COPY est EXCEPT rec_key est-no ord-no TO b-est
            ASSIGN
                b-est.company    = ipToCompany
                b-est.est-no     = v-est-no
                b-est.ord-no     = ipOrdno.           
        
            FOR EACH ef
                WHERE ef.company EQ est.company
                AND ef.est-no    EQ est.est-no
                NO-LOCK:
        
               CREATE b-ef.
               BUFFER-COPY ef EXCEPT rec_key est-no TO b-ef
               ASSIGN
                b-ef.company = b-est.company
                b-ef.est-no  = v-est-no.


               FOR EACH reftable
                  WHERE reftable.reftable EQ "EST-MISC"
                  AND reftable.company  EQ ef.company                
                  AND reftable.code     EQ trim(ef.est-no) + string(ef.form-no,"/99")
                  NO-LOCK:
                      CREATE reftable1.
                      BUFFER-COPY reftable EXCEPT rec_key TO reftable1.                      
               END.
            END.
        
            FOR EACH eb
                WHERE eb.company EQ est.company
                AND eb.est-no    EQ est.est-no
                NO-LOCK:                
               CREATE b-eb1.
               BUFFER-COPY eb EXCEPT rec_key est-no ord-no TO b-eb1
               ASSIGN
                b-eb1.company = b-est.company
                b-eb1.est-no  = v-est-no
                b-eb1.ord-no  = ipOrdno.

               FOR EACH box-design-hdr
                   WHERE box-design-hdr.design-no EQ 0
                     AND box-design-hdr.company   EQ eb.company
                     AND box-design-hdr.est-no    EQ eb.est-no
                     AND box-design-hdr.form-no   EQ eb.form-no
                     AND box-design-hdr.blank-no  EQ eb.blank-no
                   USE-INDEX design NO-LOCK:

                  CREATE boxd-hdr.
                  BUFFER-COPY box-design-hdr EXCEPT rec_key est-no TO boxd-hdr
                  ASSIGN
                    boxd-hdr.company = b-est.company
                    boxd-hdr.est-no  = v-est-no.

                  FOR EACH box-design-line OF box-design-hdr:
                    CREATE boxd-lin.
                    BUFFER-COPY box-design-line EXCEPT rec_key est-no TO boxd-lin
                    ASSIGN
                        boxd-lin.company = b-est.company
                        boxd-lin.est-no  = v-est-no.
                  END.
               END.              
            END.                   

            FOR EACH est-prep
                WHERE est-prep.company EQ est.company
                AND est-prep.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-est-prep.
                BUFFER-COPY est-prep EXCEPT rec_key est-no TO b-est-prep
                ASSIGN
                    b-est-prep.company = b-est.company
                    b-est-prep.est-no  = v-est-no.
            END.

            FOR EACH est-inst
                WHERE est-inst.company EQ est.company
                AND est-inst.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-est-inst.
                BUFFER-COPY est-inst EXCEPT rec_key est-no TO b-est-inst
                ASSIGN
                    b-est-inst.company = b-est.company
                    b-est-inst.est-no  = v-est-no.
            END.

            FOR EACH est-flm
                WHERE est-flm.company EQ est.company
                AND est-flm.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-est-flm.
                BUFFER-COPY est-flm EXCEPT rec_key est-no TO b-est-flm
                ASSIGN
                    b-est-flm.company = b-est.company
                    b-est-flm.est-no  = v-est-no.
            END.

            FOR EACH est-qty
                WHERE est-qty.company EQ est.company
                AND est-qty.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-est-qty.
                BUFFER-COPY est-qty EXCEPT rec_key est-no TO b-est-qty
                ASSIGN
                    b-est-qty.company = b-est.company
                    b-est-qty.est-no  = v-est-no.
            END.

            FOR EACH notes WHERE notes.rec_key EQ est.rec_key NO-LOCK:
               CREATE b-notes.
               BUFFER-COPY notes TO b-notes
               ASSIGN b-notes.rec_key = lv-rec_key.
            END.

            FOR EACH reftable
                WHERE reftable.reftable EQ "est/getqty.w"
                AND reftable.company  EQ est.company              
                AND reftable.code     EQ est.est-no
                NO-LOCK:

                CREATE reftable2.
                BUFFER-COPY reftable EXCEPT reftable.rec_key reftable.code TO reftable2
                ASSIGN
                   reftable2.code = v-est-no.                   
            END.

            FOR EACH probe
                WHERE probe.company EQ est.company
                AND probe.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-probe.
                BUFFER-COPY probe EXCEPT rec_key est-no TO b-probe
                ASSIGN
                    b-probe.company = b-est.company
                    b-probe.est-no  = v-est-no.
            END.
        END.

        /*RUN build-route(RECID(b-est)).*/
        RUN copyBuildRoute(b-est.est-no,b-est.company).        

    END. /* if not avail b-eb */
    ELSE DO:    
        ASSIGN n-est-no = b-eb.est-no.
    END. /* if not avail b-eb */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyFG) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyFG Procedure 
PROCEDURE copyFG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipIno AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFEstno AS CHARACTER NO-UNDO.

  DEF BUFFER b-itemfg        FOR itemfg.
  DEF BUFFER b-cust-part     FOR cust-part.
  DEF BUFFER b-fg-set        FOR fg-set.
  DEF BUFFER s-fg-set        FOR fg-set.    
  DEF BUFFER b-itemfg-ink    FOR itemfg-ink.
  DEF BUFFER b-itemfg-bom    FOR itemfg-bom.
  DEF BUFFER b-itemfg-loc    FOR itemfg-loc.
  DEF BUFFER b-e-itemfg      FOR e-itemfg.
  DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
  DEF BUFFER b-itemfgdtl     FOR itemfgdtl.
  DEF BUFFER b-ref           FOR reftable.
  DEF BUFFER b-notes         FOR notes.

  DEF VAR lv-rec_key LIKE itemfg.rec_key NO-UNDO.
  DEF VAR x AS INT NO-UNDO.
  DEF VAR y AS INT NO-UNDO.
  

  SESSION:SET-WAIT-STATE("general").
  /* Changed to compare company to ipToCompany since */
  /* the ipFestNo was equal to the est no in the new company */
  FOR EACH eb WHERE eb.est-no = ipFEstno 
              AND eb.company = ipToCompany NO-LOCK:  
      
  FIND FIRST b-itemfg
      WHERE b-itemfg.company EQ ipToCompany
        AND b-itemfg.i-no    EQ eb.stock-no
      NO-LOCK NO-ERROR.  

  IF NOT AVAIL b-itemfg THEN DO: 
      
    FIND FIRST itemfg
      WHERE itemfg.company EQ ipFrmCompany
        AND itemfg.i-no    EQ eb.stock-no
      NO-LOCK NO-ERROR.
                      
    IF AVAIL itemfg THEN DO:    

        lv-rec_key = STRING(TODAY,"99999999") +
                  STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
        CREATE rec_key.
        ASSIGN
            rec_key.rec_key    = lv-rec_key
            rec_key.table_name = "ITEMFG".
      
        CREATE b-itemfg.
        BUFFER-COPY itemfg EXCEPT rec_key est-no TO b-itemfg
        ASSIGN
         b-itemfg.company    = ipToCompany
         b-itemfg.i-no       = eb.stock-no
         b-itemfg.beg-bal    = 0
         b-itemfg.beg-date   = TODAY
         b-itemfg.q-ptd      = 0
         b-itemfg.q-ord-ytd  = 0
         b-itemfg.u-ord      = 0
         b-itemfg.q-prod-ptd = 0
         b-itemfg.q-prod-ytd = 0
         b-itemfg.u-prod     = 0
         b-itemfg.q-ship-ptd = 0
         b-itemfg.q-ship-ytd = 0
         b-itemfg.u-ship     = 0
         b-itemfg.q-inv-ptd  = 0
         b-itemfg.q-inv-ytd  = 0
         b-itemfg.u-inv      = 0
         b-itemfg.ytd-msf    = 0
         b-itemfg.lyytd-msf  = 0
         b-itemfg.est-no     = ipEstno.

        FOR EACH cust-part
            WHERE cust-part.company EQ itemfg.company
              AND cust-part.i-no    EQ itemfg.i-no
            NO-LOCK:

           CREATE b-cust-part.
           BUFFER-COPY cust-part EXCEPT rec_key TO b-cust-part
           ASSIGN
            b-cust-part.company = b-itemfg.company
            b-cust-part.i-no    = b-itemfg.i-no.
        END.      

        FOR EACH fg-set
            WHERE fg-set.company EQ itemfg.company
              AND fg-set.set-no  EQ itemfg.i-no
            NO-LOCK:     

            FIND LAST s-fg-set WHERE s-fg-set.company = b-itemfg.company USE-INDEX s-no NO-LOCK NO-ERROR.
              x = IF AVAIL s-fg-set THEN s-fg-set.s-no + 1 ELSE 1.
            FIND LAST s-fg-set WHERE s-fg-set.set-no = b-itemfg.i-no AND s-fg-set.company = b-itemfg.company NO-LOCK NO-ERROR.
              y = IF AVAIL s-fg-set THEN s-fg-set.line + 1 ELSE 1. 

           CREATE b-fg-set.                                
           BUFFER-COPY fg-set EXCEPT rec_key s-no line TO b-fg-set 
           ASSIGN
            b-fg-set.company = b-itemfg.company
            b-fg-set.set-no  = b-itemfg.i-no
            b-fg-set.s-no    = x
            b-fg-set.line    = y.
        END.
    
        FOR EACH itemfg-ink
            WHERE itemfg-ink.company EQ itemfg.company
              AND itemfg-ink.i-no    EQ itemfg.i-no
            NO-LOCK:

           CREATE b-itemfg-ink.
           BUFFER-COPY itemfg-ink EXCEPT rec_key TO b-itemfg-ink
           ASSIGN
            b-itemfg-ink.company = b-itemfg.company
            b-itemfg-ink.i-no    = b-itemfg.i-no.


        END.
    
        FOR EACH itemfg-bom
            WHERE itemfg-bom.company  EQ itemfg.company
              AND itemfg-bom.parent-i EQ itemfg.i-no
            NO-LOCK:
           CREATE b-itemfg-bom.
           BUFFER-COPY itemfg-bom EXCEPT rec_key TO b-itemfg-bom
           ASSIGN
            b-itemfg-bom.company  = b-itemfg.company
            b-itemfg-bom.parent-i = b-itemfg.i-no.
        END.

        FOR EACH itemfg-loc
            WHERE itemfg-loc.company EQ itemfg.company
              AND itemfg-loc.i-no    EQ itemfg.i-no
            NO-LOCK:
           CREATE b-itemfg-loc.
           BUFFER-COPY itemfg-loc EXCEPT rec_key TO b-itemfg-loc
           ASSIGN
            b-itemfg-loc.company = b-itemfg.company
            b-itemfg-loc.i-no    = b-itemfg.i-no.
        END.

        FOR EACH e-itemfg
            WHERE e-itemfg.company EQ itemfg.company
              AND e-itemfg.i-no    EQ itemfg.i-no
            NO-LOCK:
           CREATE b-e-itemfg.
           BUFFER-COPY e-itemfg EXCEPT rec_key TO b-e-itemfg
           ASSIGN
            b-e-itemfg.company = b-itemfg.company
            b-e-itemfg.i-no    = b-itemfg.i-no.
        END.
     
        FOR EACH e-itemfg-vend
            WHERE e-itemfg-vend.company EQ itemfg.company
              AND e-itemfg-vend.i-no    EQ itemfg.i-no
            NO-LOCK:
           CREATE b-e-itemfg-vend.
           BUFFER-COPY e-itemfg-vend EXCEPT rec_key est-no TO b-e-itemfg-vend
           ASSIGN
            b-e-itemfg-vend.company = b-itemfg.company
            b-e-itemfg-vend.i-no    = b-itemfg.i-no
            b-e-itemfg-vend.est-no  = ipEstno.
        END.
    
        FOR EACH itemfgdtl
            WHERE itemfgdtl.company EQ itemfg.company
              AND itemfgdtl.i-no    EQ itemfg.i-no
            NO-LOCK:

           CREATE b-itemfgdtl.
           BUFFER-COPY itemfgdtl EXCEPT rec_key est-no TO b-itemfgdtl
           ASSIGN
            b-itemfgdtl.company = b-itemfg.company
            b-itemfgdtl.i-no    = b-itemfg.i-no
            b-itemfgdtl.est-no  = ipEstno.
        END.

        FOR EACH reftable
            WHERE reftable.reftable EQ "FGSTATUS"
              AND reftable.company  EQ itemfg.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ itemfg.i-no
            NO-LOCK:
           CREATE b-ref.
           BUFFER-COPY reftable EXCEPT rec_key TO b-ref
           ASSIGN
            b-ref.company = b-itemfg.company
            b-ref.code    = b-itemfg.i-no.
        END.

        FOR EACH reftable
            WHERE reftable.reftable EQ "itemfg.exempt-disc"
              AND reftable.company  EQ itemfg.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ itemfg.i-no
            NO-LOCK:
           CREATE b-ref.
           BUFFER-COPY reftable EXCEPT rec_key TO b-ref
           ASSIGN
            b-ref.company = b-itemfg.company
            b-ref.code    = b-itemfg.i-no.
        END.

        FOR EACH notes WHERE notes.rec_key EQ itemfg.rec_key NO-LOCK:
          CREATE b-notes.
          BUFFER-COPY notes TO b-notes
          ASSIGN b-notes.rec_key = lv-rec_key.
        END.

        cocode = b-itemfg.company.
        RUN fg/fg-reset.p (RECID(b-itemfg)).
    END.
  END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyJob Procedure 
PROCEDURE copyJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
    DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.        
    
    DEFINE VARIABLE v-job-no       LIKE oe-ord.job-no  NO-UNDO.
    DEFINE VARIABLE v-job-no2      LIKE oe-ord.job-no2 NO-UNDO.
    DEFINE VARIABLE v-prod-cat     AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE lv-job-recid   AS RECID          NO-UNDO.
    DEFINE VARIABLE choice         AS LOGICAL        NO-UNDO.
    DEFINE VARIABLE hld-id         AS RECID          NO-UNDO.
    DEFINE VARIABLE hld-stat       LIKE job.stat     NO-UNDO.
    DEFINE VARIABLE hld-nufile     AS LOGICAL        NO-UNDO.
    DEFINE VARIABLE v-run-schedule AS LOGICAL        NO-UNDO.

    FIND CURRENT oe-ord.
    DEF BUFFER b-oe-ordl1 FOR oe-ordl.
    DEF BUFFER b-oe-ord1 FOR oe-ord.   

    FIND FIRST eb WHERE eb.est-no = ipEstno 
                  AND eb.company = ipToCompany NO-LOCK NO-ERROR.

   IF AVAIL eb THEN ASSIGN
       v-prod-cat = eb.procat.

    cocode     = ipToCompany.

    v-job-no = FILL(" ",6 - length(TRIM(STRING(ipOrdno)))) + string(ipOrdno).
        RUN jc/job-no.p (INPUT-OUTPUT v-job-no, 
                         INPUT-OUTPUT v-job-no2,
                         INPUT v-prod-cat,
                         FILL(" ",6 - length(TRIM(ipEstno))) + trim(ipEstno)).
         
    IF v-job-no EQ "" THEN
      v-job-no = FILL(" ",6 - length(TRIM(ipEstno))) + trim(ipEstno).

    IF v-job-no NE "" THEN DO:
          FIND FIRST job NO-LOCK
              WHERE job.company EQ ipToCompany
                AND job.job-no  EQ v-job-no
                AND job.job-no2 EQ v-job-no2
              NO-ERROR.
         
          IF AVAIL job AND TRIM(job.est-no) NE TRIM(ipEstno) THEN
            IF CAN-FIND(FIRST job-hdr
                        WHERE job-hdr.company EQ job.company
                          AND job-hdr.job     EQ job.job
                          AND job-hdr.job-no  EQ job.job-no
                          AND job-hdr.job-no2 EQ job.job-no2
                          AND job-hdr.ord-no  NE ipOrdno) OR
               CAN-FIND(FIRST b-oe-ord1
                        WHERE b-oe-ord1.company EQ job.company
                          AND b-oe-ord1.job-no  EQ job.job-no
                          AND b-oe-ord1.job-no2 EQ job.job-no2
                          AND b-oe-ord1.est-no  EQ job.est-no)   OR
               CAN-FIND(FIRST b-oe-ordl1
                        WHERE b-oe-ordl1.company EQ job.company
                          AND b-oe-ordl1.job-no  EQ job.job-no
                          AND b-oe-ordl1.job-no2 EQ job.job-no2
                          AND b-oe-ordl1.est-no  EQ job.est-no)  THEN RELEASE job.
            ELSE
            DO TRANSACTION:
              FIND CURRENT job NO-ERROR.
              IF AVAIL job THEN DELETE job.
            END.
         
          IF NOT AVAIL job THEN DO:
            RUN create-ord-job (ipFrmCompany,ipToCompany,ipEstno,ipOrdno,v-job-no,v-job-no2,ipLoc,OUTPUT lv-job-recid).
            FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
          END.        

          ipv-qty-mod = YES.

          IF AVAIL job AND INDEX("HWPRL",job.stat) NE 0 THEN DO:
            /*IF NOT ipv-qty-mod THEN
               RUN oe/job-qty.p (ROWID(oe-ord), OUTPUT ipv-qty-mod).*/
         
            IF ipv-qty-mod OR job.stat EQ "P" THEN DO:
              RUN jc/chkrebld.p (RECID(job), OUTPUT choice).     
              IF NOT choice THEN DO:
                ASSIGN hld-id     = ipFil_id
                       hld-nufile = /*ipNufile */ YES
                       hld-stat   = job.stat
                       ipNufile     = YES
                       nufile  = YES. /* task 08201407 */
         
                RUN jc/jc-calc.p(RECID(job), NO).
                ASSIGN ipFil_id   = hld-id
                       ipNufile   = hld-nufile.
               
                IF hld-stat NE "P" THEN DO TRANSACTION:
                  FIND CURRENT job EXCLUSIVE.
                  job.stat = hld-stat.
                  FIND CURRENT job NO-LOCK.
                END.
              END.
            END.
          END.
                
          FIND FIRST sys-ctrl WHERE
               sys-ctrl.company EQ cocode AND
               sys-ctrl.name    EQ "SCHEDULE"
               NO-LOCK NO-ERROR.

          v-run-schedule = IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                           ELSE IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                           ELSE NO.

          FOR EACH oe-ordl NO-LOCK
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ ipOrdno
                AND oe-ordl.is-a-component EQ NO
         
              BREAK BY oe-ordl.job-no
                    BY oe-ordl.job-no2:
         
            IF LAST-OF(oe-ordl.job-no2) THEN DO:
              ASSIGN
               hld-id     = ipFil_id
               hld-nufile = ipNufile
               ipFil_id     = RECID(oe-ordl).
             
              RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
              /* check oe-ordl.due-date and calc promised date and job's start-date */

              IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.
              
              ASSIGN
               ipFil_id = hld-id
               ipNufile = hld-nufile.
            END. /* last of job-no2 */
          END. /* each oe-ordl */
    END. /* if v-job ne "" */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyOrder Procedure 
PROCEDURE copyOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipFromCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFromOrdNo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipToOrdNo AS INTEGER NO-UNDO.    

  
  DEF BUFFER b-oe-ordm FOR oe-ordm.
  DEF BUFFER b-oe-rel  FOR oe-rel.
  DEF BUFFER b-notes   FOR notes.
  DEF BUFFER b-reftable FOR reftable.
  DEF BUFFER b-cust-c FOR cust.

  DEF VAR lv-rec_key LIKE oe-ord.rec_key NO-UNDO.
  DEF VAR lv-r-no LIKE oe-rel.r-no NO-UNDO.
  DEF VAR n-est-no LIKE est.est-no NO-UNDO.
  DEF VAR li-next-ordno AS INT NO-UNDO.
  DEF VAR liNumTries AS INT NO-UNDO.
  
  DEF VAR v-lead-days LIKE oe-ord.lead-days NO-UNDO.
  DEF VAR v-last-date LIKE oe-ord.last-date NO-UNDO.
  DEF VAR v-due-date  LIKE oe-ord.due-date  NO-UNDO.
  DEF VAR v-ord-date  LIKE oe-ord.ord-date  NO-UNDO.
  DEF VAR li-lead-days AS INT NO-UNDO.
  DEF VAR dCalcDueDate  AS DATE NO-UNDO.
  DEF VAR dCalcPromDate AS DATE NO-UNDO.

   DO :
    {sys/inc/lastship.i} 
  END.
   {sys/inc/oereleas.i}

  FIND FIRST oe-ord
      WHERE oe-ord.company EQ ipFromCompany
        AND oe-ord.ord-no  EQ ipFromOrdNo
      NO-LOCK NO-ERROR.

  lv-rec_key = STRING(TODAY,"99999999") +
               STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
  CREATE rec_key.
  ASSIGN
   rec_key.rec_key    = lv-rec_key
   rec_key.table_name = "oe-ord".
      
/*   FIND b-oe-ord                                         */
/*     WHERE b-oe-ord.company EQ ipToCompany               */
/*       AND b-oe-ord.ord-no  EQ ipToOrdNo                 */
/*     EXCLUSIVE-LOCK NO-ERROR.                            */
/*   IF NOT AVAIL b-oe-ord THEN DO:                        */
/*       MESSAGE "Internal Error - Please notify ASI" SKIP */
/*           "company: " iptocompany SKIP                  */
/*           "order: " ipToOrdNo SKIP                      */
/*           VIEW-AS ALERT-BOX.                            */
/*             RETURN ERROR.                               */
/*   END.                                                  */

/*  Replaced with sequence                 */
/*   RUN nextOrdNo (OUTPUT li-next-ordno). */
/*  IF li-next-ordno EQ ? THEN RETURN ERROR. */
  IF NOT iplIncrementOrder THEN
    li-next-ordno = ipToOrdNo.
  ELSE
    RUN sys/ref/asiseq.p (INPUT ipToCompany, INPUT "order_seq", OUTPUT li-next-ordno) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

  /* Just in case sequence got reset incorrectly */
  DO WHILE CAN-FIND(FIRST b-oe-ord
                    WHERE b-oe-ord.company EQ ipToCompany
                      AND b-oe-ord.ord-no  EQ li-next-ordno):
    
      RUN sys/ref/asiseq.p (INPUT ipToCompany, 
                          INPUT "order_seq", 
                          OUTPUT li-next-ordno) NO-ERROR.    
    
  END.

  CREATE b-oe-ord.                                 
/*   lv-new-row-id = ROWID(b-oe-ord). */

/*   DO WHILE TRUE:                                              */
/*     liNumTries = liNumTries + 1.                              */
/*     FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company     */
/*                          AND oe-ordl.ord-no  EQ li-next-ordno */
/*                        NO-LOCK NO-ERROR.                      */
/*     assign b-oe-ord.company = g_company                       */
/*          b-oe-ord.ord-date = today                            */
/*          b-oe-ord.ord-no = li-next-ordno                      */
/*          b-oe-ord.user-id = userid("nosweat")                 */
/*     NO-ERROR.                                                 */
/*     IF ERROR-STATUS:ERROR OR AVAIL oe-ordl THEN DO:           */
/*         IF liNumTries GT 20  THEN DO:                         */
/*             RETURN ERROR.                                     */
/*         END.                                                  */
/*         li-next-ordno = li-next-ordno + 1.                    */
/*         NEXT.                                                 */
/*     END.                                                      */
/*     LEAVE.                                                    */
/*   END.                                                        */

  b-oe-ord.ord-no = li-next-ordno.
  li-next-ordno = b-oe-ord.ord-no.
  BUFFER-COPY oe-ord EXCEPT rec_key job-no job-no2 ord-no po-no2 company TO b-oe-ord.
  b-oe-ord.company = ipToCompany.
  
  FIND FIRST b-cust-c WHERE b-cust-c.company = b-oe-ord.company
      AND b-cust-c.cust-no = b-oe-ord.cust-no NO-LOCK NO-ERROR .

  ASSIGN
      b-oe-ord.ord-date = TODAY
      b-oe-ord.cc-expiration = TODAY 
      li-lead-days = b-cust-c.ship-days
      b-oe-ord.last-date = DATE(b-oe-ord.ord-date) + 
                                             b-cust-c.ship-days
      b-oe-ord.due-date  = b-oe-ord.last-date .

       IF  lastship-cha = "Stock/Custom" THEN DO:
          /* If order has no estimate. */
          IF b-oe-ord.est-no = "" THEN
              ASSIGN b-oe-ord.due-date = DATE(b-oe-ord.ord-date) + lastship-int.
          ELSE
              ASSIGN b-oe-ord.due-date = DATE(b-oe-ord.ord-date) + INT(lastship-dec).
      END.

       ASSIGN
           v-lead-days = li-lead-days
           v-last-date = DATE(b-oe-ord.last-date)
           v-due-date  = DATE(b-oe-ord.due-date)
           v-ord-date  = DATE(b-oe-ord.ord-date).
       
       {oe/lastship.i "v-" 1}

           ASSIGN
           /*li-lead-days       = v-lead-days*/
           b-oe-ord.last-date = (v-last-date)
           b-oe-ord.due-date  = (v-due-date).


  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no
      EXCLUSIVE-LOCK:
    
    CREATE b-oe-ordl.
    BUFFER-COPY oe-ordl EXCEPT rec_key job-no job-no2 ord-no oe-ordl.t-inv-qty oe-ordl.t-ship-qty oe-ordl.po-no-po TO b-oe-ordl
    ASSIGN
     b-oe-ordl.company   = b-oe-ord.company
     b-oe-ordl.ord-no    = b-oe-ord.ord-no  
     b-oe-ordl.req-date  = b-oe-ord.due-date
     b-oe-ordl.prom-date = b-oe-ord.due-date.
     
    IF oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN 
    DO:

          RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
              INPUT b-oe-ordl.req-date,
              INPUT b-oe-ordl.prom-date,
              INPUT "DueDate",
              INPUT ROWID(b-oe-ordl),
              OUTPUT dCalcDueDate,
              OUTPUT dCalcPromDate).
          
          b-oe-ordl.prom-date = dCalcPromDate.


    END.
    
    IF oe-ordl.est-no GT "" THEN
        FIND est WHERE est.est-no EQ oe-ordl.est-no NO-LOCK NO-ERROR.
     
    IF ipFromCompany NE ipToCompany AND avail(est) THEN
      RUN copyEst (ipFromCompany,ipToCompany,oe-ordl.est-no,b-oe-ordl.i-no,b-oe-ordl.part-no,b-oe-ordl.ord-no, OUTPUT n-est-no).
    ELSE
      n-est-no = oe-ordl.est-no.
   
    ASSIGN
      ipFil_id    = RECID(oe-ordl).

    IF oe-ordl.est-no <> "" THEN DO:
        FIND est WHERE est.est-no EQ oe-ordl.est-no NO-LOCK NO-ERROR.
        IF AVAIL(est) THEN do:
            RUN copyJob (ipFromCompany,ipToCompany,n-est-no,b-oe-ord.ord-no,b-oe-ord.loc).
            ASSIGN b-oe-ord.job-no = STRING(b-oe-ord.ord-no) .
            IF est.ord-no NE 0 THEN ASSIGN
                b-oe-ordl.po-no2 = STRING(est.ord-no)
                b-oe-ord.pord-no = est.ord-no  .
            FIND CURRENT est EXCLUSIVE-LOCK NO-ERROR .
            ASSIGN est.ord-no = b-oe-ord.ord-no .
            FIND CURRENT est NO-LOCK NO-ERROR .
            FIND FIRST eb EXCLUSIVE-LOCK
                 WHERE eb.company EQ b-oe-ord.company 
                   AND eb.est-no  EQ est.est-no 
                   AND ((eb.cust-no EQ oe-ordl.cust-no AND
                         eb.part-no EQ oe-ordl.part-no) OR
                         eb.est-type EQ 2 OR
                         eb.est-type EQ 6) NO-ERROR .
            IF AVAIL eb THEN
                ASSIGN eb.ord-no = b-oe-ord.ord-no .
            FIND CURRENT eb NO-LOCK NO-ERROR .
        END.
        
    END.
    
    IF ipFromCompany NE ipToCompany THEN
      RUN copyFg (ipFromCompany,ipToCompany,b-oe-ordl.i-no, n-est-no, oe-ordl.est-no).
    ASSIGN
      ipFil_id    = RECID(oe-ordl).

  END.
  
  FOR EACH oe-ordm
      WHERE oe-ordm.company EQ oe-ord.company
        AND oe-ordm.ord-no EQ oe-ord.ord-no
      NO-LOCK:

    CREATE b-oe-ordm.
    BUFFER-COPY oe-ordm EXCEPT rec_key TO b-oe-ordm
    ASSIGN
     b-oe-ordm.company = b-oe-ord.company
     b-oe-ordm.ord-no  = b-oe-ord.ord-no.
  END.


   IF oereleas-log THEN  /* n-k-1 oerelease*/
   FOR EACH b-oe-ordl
          WHERE b-oe-ordl.company = b-oe-ord.company
            AND b-oe-ordl.ord-no  = b-oe-ord.ord-no NO-LOCK:
        FIND xoe-ord WHERE RECID(xoe-ord) = recid(b-oe-ord) EXCLUSIVE.
        RUN create-rel .
   END.

  /*IF oereleas-log THEN  /* n-k-1 oerelease*/
  FOR EACH oe-rel
      WHERE oe-rel.company EQ oe-ord.company
        AND oe-rel.ord-no    EQ oe-ord.ord-no
      NO-LOCK:

      /* FIND FIRST b-oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR. */
      /* lv-r-no = (IF AVAIL b-oe-rel THEN b-oe-rel.r-no ELSE 0) + 1. */
      RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT lv-r-no).
      CREATE b-oe-rel.
      BUFFER-COPY oe-rel EXCEPT rec_key TO b-oe-rel

      ASSIGN
       b-oe-rel.company = b-oe-ord.company
       b-oe-rel.r-no    = lv-r-no
       b-oe-rel.ord-no  = b-oe-ord.ord-no
       b-oe-rel.link-no = 0
       b-oe-rel.opened  = YES
       b-oe-rel.rel-no  = 0
       b-oe-rel.ship-date = ?
       b-oe-rel.stat    = "S"
       b-oe-rel.qty     = 0   
       b-oe-rel.rel-date =  b-oe-ord.due-date .

      if oereleas-cha eq "LastShip" then
          b-oe-rel.rel-date = oe-ord.last-date.
      ELSE IF oereleas-cha EQ "Due Date" THEN
          b-oe-rel.rel-date = oe-ordl.req-date.
      ELSE /*DueDate+1Day*/
          DO:
          b-oe-rel.rel-date = oe-ordl.req-date + 1.
          IF WEEKDAY(b-oe-rel.rel-date) EQ 7 THEN
              b-oe-rel.rel-date = b-oe-rel.rel-date + 2.
          ELSE
              IF WEEKDAY(b-oe-rel.rel-date) EQ 1 THEN
                  b-oe-rel.rel-date = b-oe-rel.rel-date + 1.
          END.
          
      FIND FIRST reftable WHERE
           reftable.reftable EQ "oe-rel.lot-no" AND
           reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
           NO-LOCK NO-ERROR.

      IF AVAIL reftable THEN
      DO:
         CREATE b-reftable.
         BUFFER-COPY reftable EXCEPT company TO b-reftable
            ASSIGN b-reftable.company = STRING(b-oe-rel.r-no,"9999999999").
         RELEASE b-reftable.
         RELEASE reftable.
      END.

      FIND FIRST reftable WHERE
           reftable.reftable EQ "oe-rel.sell-price" AND
           reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
           NO-LOCK NO-ERROR.

      IF AVAIL reftable THEN
      DO:
         CREATE b-reftable.
         BUFFER-COPY reftable EXCEPT company TO b-reftable
            ASSIGN b-reftable.company = STRING(b-oe-rel.r-no,"9999999999").
         RELEASE b-reftable.
         RELEASE reftable.
      END.
  END.*/

  FOR EACH notes WHERE notes.rec_key EQ oe-ord.rec_key NO-LOCK:
    CREATE b-notes.
    BUFFER-COPY notes EXCEPT rec_key TO b-notes
    ASSIGN b-notes.rec_key = lv-rec_key.
  END.
  IF ipFromCompany NE ipToCompany THEN
    RUN copyCust (ipFromCompany,ipToCompany,b-oe-ord.cust-no).
  /* This needs to go back to the calling program */
  ipToOrdNo = b-oe-ord.ord-no.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-ord-job) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-ord-job Procedure 
PROCEDURE create-ord-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
  DEFINE INPUT PARAMETER ipJobno LIKE oe-ord.job-no NO-UNDO.
  DEFINE INPUT PARAMETER ipJobno2 LIKE oe-ord.job-no2 NO-UNDO.
  DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.

  DEF VAR v-item-chk AS CHAR NO-UNDO .
  
  FIND CURRENT oe-ord.
  DEF OUTPUT PARAM op-recid AS RECID NO-UNDO.
  ASSIGN  v-item-chk = oe-ordl.i-no .

  DEF BUFFER v-ord-job-hdr FOR job-hdr.

  DEFINE VARIABLE v-job-job LIKE job.job     NO-UNDO.
  DEFINE VARIABLE v-job-no  LIKE job.job-no  NO-UNDO.
  DEFINE VARIABLE v-job-no2 LIKE job.job-no2 NO-UNDO.
  DEFINE VARIABLE li-j-no   AS INTEGER     NO-UNDO.
    DEF BUFFER b-sys-ctrl FOR sys-ctrl.
  /* === from oe/oe-ord1.p  ============= */
  
       FIND FIRST b-sys-ctrl WHERE
               b-sys-ctrl.company EQ cocode AND
               b-sys-ctrl.name    EQ "Job#"
               NO-LOCK NO-ERROR.

  FIND LAST job WHERE job.company EQ ipToCompany NO-LOCK NO-ERROR.
  v-job-job = IF AVAIL job THEN job.job + 1 ELSE 1.

  ASSIGN
   v-job-no  = ipJobno
   v-job-no2 = ipJobno2.

  FOR EACH job
      WHERE job.company EQ ipToCompany
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.

  CREATE job.
  ASSIGN job.job        = v-job-job
         job.company    = ipToCompany
         job.loc        = ipLoc
         job.est-no     = ipEstno
         job.job-no     = v-job-no
         job.job-no2    = v-job-no2
         job.stat       = "P"
         op-recid       = RECID(job).

  FOR EACH oe-ordl WHERE oe-ordl.company EQ ipToCompany
                     AND oe-ordl.ord-no  EQ ipOrdno EXCLUSIVE:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ ipToCompany
            AND job-hdr.job-no  EQ oe-ord.job-no
            AND job-hdr.job-no2 EQ oe-ord.job-no2
            AND job-hdr.ord-no  EQ ipOrdno
            AND job-hdr.i-no    EQ oe-ordl.i-no
          NO-ERROR.

      IF NOT AVAIL job-hdr THEN DO:
         FIND FIRST itemfg WHERE itemfg.company EQ oe-ordl.company
                             AND itemfg.i-no    EQ oe-ordl.i-no
                             NO-LOCK NO-ERROR.   
         
         CREATE job-hdr.
         ASSIGN job-hdr.company      = ipToCompany
                job-hdr.loc          = ipLoc
                job-hdr.est-no       = ipEstno
                job-hdr.i-no         = oe-ordl.i-no
                job-hdr.qty          = oe-ordl.qty 
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no
                job-hdr.blank-no     = oe-ordl.blank-no 
                job-hdr.due-date     = oe-ord.due-date .

         IF AVAIL itemfg THEN
              ASSIGN job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         ASSIGN job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
      END. /* not avail job-hdr */

      ELSE
      DO WHILE TRUE:
        FIND v-ord-job-hdr WHERE ROWID(v-ord-job-hdr) EQ ROWID(job-hdr)
            EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL v-ord-job-hdr THEN DO:
          FIND CURRENT v-ord-job-hdr NO-LOCK NO-ERROR.
          FIND CURRENT job-hdr NO-ERROR.
          LEAVE.
        END.
      END. /* avail job hdr */

      ASSIGN job-hdr.est-no  = ipEstno
             job-hdr.job     = job.job
             job-hdr.job-no  = job.job-no
             job-hdr.job-no2 = job.job-no2             
             .
      IF ipFrmCompany NE ipToCompany THEN
          oe-ordl.est-no  = job-hdr.est-no.
        /* task 07311410 */
          IF AVAIL b-sys-ctrl AND b-sys-ctrl.char-fld = "Order#" AND oe-ordl.i-no  EQ v-item-chk THEN
          ASSIGN
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
             oe-ordl.j-no    = job-hdr.j-no .
          IF AVAIL b-sys-ctrl AND b-sys-ctrl.char-fld NE "Order#"  THEN
          ASSIGN
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
             oe-ordl.j-no    = job-hdr.j-no .
          
      FIND CURRENT job-hdr NO-LOCK.
  END. /* each oe-ordl */

  FIND CURRENT job NO-LOCK.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-rel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-rel Procedure 
PROCEDURE create-rel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE v-qty-sum    AS INTEGER        NO-UNDO.
  DEFINE VARIABLE v-nxt-r-no   AS INTEGER        INIT 1 NO-UNDO.
  DEFINE VARIABLE v-lst-rel    AS DATE           NO-UNDO.
  DEFINE VARIABLE v-pct-chg    AS DECIMAL        NO-UNDO.
  DEFINE VARIABLE v-ship-id    LIKE oe-rel.ship-id NO-UNDO.
  DEFINE VARIABLE v-num-shipto AS INTEGER        NO-UNDO.
  DEFINE VARIABLE v-relType    AS cha            NO-UNDO.
  DEFINE VARIABLE cShipSlsRep  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE gv-ship-from  AS CHARACTER NO-UNDO.
  DEF VAR dCalcDueDate  AS DATE NO-UNDO.
  DEF VAR dCalcPromDate AS DATE NO-UNDO.
    
  DEFINE VARIABLE v-relflg2    AS LOGICAL        INIT YES NO-UNDO.
  DEF BUFFER bf-ordl FOR oe-ordl.
  DEF BUFFER bf-rel FOR oe-rel.
  DEF BUFFER bf-oe-rel FOR oe-rel.

  DEF VAR lcReturn AS CHAR NO-UNDO.
  DEF VAR llRecFound AS LOG NO-UNDO.
  DEF VAR llOeShipFromLog AS LOG NO-UNDO.
  RUN sys/ref/nk1look.p (cocode, "OESHIPFROM", "L", NO, NO, "", "", 
                          OUTPUT lcReturn, OUTPUT llRecFound).
  IF llRecFound THEN
   llOeShipFromLog = LOGICAL(lcReturn) NO-ERROR.

  ASSIGN v-qty-sum  = 0.

FIND FIRST oe-rel WHERE oe-rel.company EQ cocode
     AND oe-rel.ord-no EQ b-oe-ord.ord-no 
   NO-LOCK NO-ERROR.

IF gv-ship-from EQ "" AND AVAIL oe-rel AND oe-rel.spare-char-1 NE "" THEN
  gv-ship-from = oe-rel.spare-char-1.

/* when OESHIPTO then retaining same shipid for next prompt */
IF NOT oeship-cha = "OESHIPTO" THEN 
   v-ship-id = "".

  IF b-oe-ordl.est-no NE "" THEN
      FOR EACH eb
        WHERE eb.company  EQ b-oe-ordl.company
        AND eb.est-no   EQ b-oe-ordl.est-no
        AND eb.cust-no  EQ b-oe-ord.cust-no
        AND eb.ship-id  NE ""
        NO-LOCK
          BREAK BY eb.stock-no DESC:
          IF LAST(eb.stock-no)           OR
          eb.stock-no EQ b-oe-ordl.i-no THEN DO:
          v-ship-id = eb.ship-id.
          LEAVE.
          END.
        END.
        
        ELSE
        FOR EACH shipto
            WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ b-oe-ord.cust-no
            NO-LOCK
            BREAK BY shipto.ship-no DESC:
            IF shipto.ship-id EQ b-oe-ord.cust-no THEN DO:
                v-ship-id = shipto.ship-id.
                LEAVE.
            END.
        END.

        IF v-ship-id EQ "" THEN DO:
            /* In case no default shipto exists for this cust */
            FIND FIRST shipto
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ b-oe-ord.cust-no
                NO-LOCK NO-ERROR.
            IF AVAIL shipto THEN
                v-ship-id = shipto.ship-id.
        END.
        IF v-ship-id NE "" THEN DO:
          FIND FIRST shipto WHERE shipto.company EQ cocode
              AND shipto.ship-id EQ v-ship-id
              NO-LOCK NO-ERROR.
          IF AVAIL shipto AND gv-ship-from EQ "" THEN
              gv-ship-from = shipto.loc.
        END.
        RUN oe/d-shipid.w (INPUT b-oe-ordl.cust-no,
                   INPUT-OUTPUT v-ship-id,
                   INPUT-OUTPUT gv-ship-from).

        v-num-shipto = 0.
        FOR EACH shipto WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ b-oe-ordl.cust-no
            NO-LOCK:
            ASSIGN v-num-shipto = v-num-shipto + 1.
        END.
  

    {oe/oe-rel.a &fil="b-oe-ordl"}.
 
ASSIGN /* v-ship-id = "" */
lv-qty = b-oe-ordl.qty.

FIND FIRST xoe-rel WHERE xoe-rel.company EQ cocode
    AND xoe-rel.ord-no  EQ b-oe-ordl.ord-no
    AND RECID(xoe-rel)  NE RECID(oe-rel)
    AND xoe-rel.link-no EQ 0
  NO-LOCK NO-ERROR.

IF TRUE OR ( NOT AVAIL xoe-rel OR b-oe-ordl.est-no NE "" ) THEN DO:


  /* Calculate number of shipto records for this customer in ask-release-questions */
/*   FOR EACH shipto WHERE shipto.company EQ cocode */
/*     AND shipto.cust-no EQ oe-ordl.cust-no:       */
/*     ASSIGN v-num-shipto = v-num-shipto + 1.      */
/*   END.                                           */
  
  IF v-num-shipto GT 1 THEN
  DO:
    /* More than one ship-to for this customer ... */

    /* gdm - 06220908*/
    IF v-relflg2 OR llOeShipFromLog THEN
    ASSIGN oe-rel.ship-id = TRIM(v-ship-id).
    
    FIND FIRST shipto WHERE shipto.company = cocode AND
    shipto.cust-no = xoe-ord.cust-no  AND
    shipto.ship-id = v-ship-id
    USE-INDEX ship-id NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN DO:

      ASSIGN v-ship-id           = shipto.ship-id.

      IF v-shiptorep-log AND AVAIL shipto THEN DO:  /* task 05301401 */
         IF shipto.spare-char-1 <> "" THEN DO:
             FIND CURRENT b-oe-ordl EXCLUSIVE-LOCK NO-ERROR.
             FIND CURRENT b-oe-ord EXCLUSIVE-LOCK NO-ERROR.
              ASSIGN
              b-oe-ordl.s-man[1] = shipto.spare-char-1
              b-oe-ord.sman[1]   = shipto.spare-char-1 .

              FIND sman WHERE sman.company = b-oe-ord.company
                  AND sman.sman = b-oe-ordl.s-man[1]
                  NO-LOCK NO-ERROR.
              IF AVAIL sman THEN ASSIGN b-oe-ord.sname[1] = sman.sname
                                        b-oe-ord.s-comm[1] = (sman.scomm)
                                        b-oe-ordl.s-comm[1] = (sman.scomm).
              FIND CURRENT b-oe-ordl NO-LOCK NO-ERROR.
              FIND CURRENT b-oe-ord NO-LOCK NO-ERROR.
          END.
      END.
      
      /* gdm - 06220908 */
      IF v-relflg2 THEN
      ASSIGN oe-rel.ship-no      = shipto.ship-no
      oe-rel.ship-id      = shipto.ship-id
      oe-rel.ship-addr[1] = shipto.ship-addr[1]
      oe-rel.ship-addr[2] = shipto.ship-addr[2]
      oe-rel.ship-city    = shipto.ship-city
      oe-rel.ship-state   = shipto.ship-state
      oe-rel.ship-zip     = shipto.ship-zip
      oe-rel.ship-i[1] = shipto.notes[1]
      oe-rel.ship-i[2] = shipto.notes[2]
      oe-rel.ship-i[3] = shipto.notes[3]
      oe-rel.ship-i[4] = shipto.notes[4]
      oe-rel.spare-char-1 = shipto.loc.
      
      FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "OECARIER"
      NO-LOCK NO-ERROR.
      IF NOT AVAIL sys-ctrl THEN DO:
        CREATE sys-ctrl.
        ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.NAME     = "OECARIER"
        sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
        sys-ctrl.char-fld = "ShipTo".
        
        DO WHILE TRUE:
          MESSAGE "Default Shipping Carrier from Header or Shipto?"
          UPDATE sys-ctrl.char-fld.
          IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE.
        END.
      END.
      
      /* gdm - 06220908 */
      IF v-relflg2 THEN
      ASSIGN oe-rel.carrier = IF sys-ctrl.char-fld = "Shipto"
      THEN shipto.carrier
      ELSE xoe-ord.carrier.
      
      IF oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN 
      DO:

        RUN oe/dueDateCalc.p (INPUT b-oe-ordl.cust-no,
            INPUT b-oe-ordl.req-date,
            INPUT b-oe-ordl.prom-date,
            INPUT "DueDate",
            INPUT ROWID(b-oe-ordl),
            OUTPUT dCalcDueDate,
            OUTPUT dCalcPromDate).
        FIND CURRENT b-oe-ordl EXCLUSIVE-LOCK NO-ERROR.
        b-oe-ordl.prom-date = dCalcPromDate.
        FIND CURRENT b-oe-ordl NO-LOCK NO-ERROR.

      END.
      
    END.
    IF gv-ship-from GT "" THEN
      oe-rel.spare-char-1 = gv-ship-from.
    
    /* Run Freight calculation  */
    RUN oe/oe-frtcl.p.
    
  END.  /* multi ship to */
  ELSE DO:
    /* If not multi ship-to */
    FIND FIRST shipto WHERE shipto.company EQ cocode AND
        shipto.cust-no EQ xoe-ord.cust-no AND
        shipto.ship-id EQ v-ship-id
      NO-LOCK NO-ERROR.
    IF NOT AVAIL shipto THEN
      FIND FIRST shipto WHERE shipto.company EQ cocode 
         AND shipto.cust-no EQ xoe-ord.cust-no
      NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN DO:
      
      /* gdm - 06220908 */
      IF v-relflg2 THEN
      ASSIGN  oe-rel.ship-no      = shipto.ship-no
              oe-rel.ship-id      = shipto.ship-id
              oe-rel.ship-addr[1] = shipto.ship-addr[1]
              oe-rel.ship-addr[2] = shipto.ship-addr[2]
              oe-rel.ship-city    = shipto.ship-city
              oe-rel.ship-state   = shipto.ship-state
              oe-rel.ship-zip     = shipto.ship-zip
              oe-rel.ship-i[1] = shipto.notes[1]
              oe-rel.ship-i[2] = shipto.notes[2]
              oe-rel.ship-i[3] = shipto.notes[3]
              oe-rel.ship-i[4] = shipto.notes[4]
              oe-rel.spare-char-1 = shipto.loc.
      
      /* check that itemfg-loc exists */
      IF oe-rel.spare-char-1 GT "" THEN
        RUN fg/chkfgloc.p (INPUT oe-rel.i-no, INPUT oe-rel.spare-char-1).
      
      /* if add mode then use default carrier */
     /* IF ll-new-record /* and NOT oe-rel.carrier ENTERED */ THEN DO:*/
        FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.NAME    EQ "OECARIER"
          NO-LOCK NO-ERROR.
        IF NOT AVAIL sys-ctrl THEN DO:
          CREATE sys-ctrl.
          ASSIGN sys-ctrl.company  = cocode
          sys-ctrl.NAME     = "OECARIER"
          sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
          sys-ctrl.char-fld = "ShipTo".
          
          DO WHILE TRUE:
            MESSAGE "Default Shipping Carrier from Header or Shipto?"
            UPDATE sys-ctrl.char-fld.
            IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE.
          END.
        END.
        /* gdm - 06220908 */
        IF v-relflg2 THEN
        oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto~.carrier
        ELSE xoe-ord.carrier.
        
        /* sman by itemfg overrides that of the shipto */
        RUN itemfg-sman.
        
     /* END.*/
    END. /* avail shipto */
  END. /* not multi */
  IF gv-ship-from GT "" THEN
    oe-rel.spare-char-1 = gv-ship-from.

END. /* if no oe-rel was found */
ELSE DO:
  /* If oe-rel was already available */
  FIND FIRST shipto WHERE shipto.company = cocode AND
  shipto.cust-no = xoe-ord.cust-no  AND
  shipto.ship-id = xoe-rel.ship-id
  USE-INDEX ship-id NO-LOCK NO-ERROR.
  IF AVAIL shipto THEN DO:
    /* gdm - 06220908 */
    IF v-relflg2 THEN
    ASSIGN oe-rel.ship-no      = shipto.ship-no
    oe-rel.ship-id      = shipto.ship-id
    oe-rel.ship-addr[1] = shipto.ship-addr[1]
    oe-rel.ship-addr[2] = shipto.ship-addr[2]
    oe-rel.ship-city    = shipto.ship-city
    oe-rel.ship-state   = shipto.ship-state
    oe-rel.ship-zip     = shipto.ship-zip
    oe-rel.ship-i[1] = shipto.notes[1]
    oe-rel.ship-i[2] = shipto.notes[2]
    oe-rel.ship-i[3] = shipto.notes[3]
    oe-rel.ship-i[4] = shipto.notes[4]
    oe-rel.spare-char-1 = shipto.loc.
    

      FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "OECARIER"
      NO-LOCK NO-ERROR.
      IF NOT AVAIL sys-ctrl THEN DO:
        CREATE sys-ctrl.
        ASSIGN sys-ctrl.company  = cocode
        sys-ctrl.NAME     = "OECARIER"
        sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
        sys-ctrl.char-fld = "ShipTo".
        
        DO WHILE TRUE:
          MESSAGE "Default Shipping Carrier from Header or Shipto?"
          UPDATE sys-ctrl.char-fld.
          IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE.
        END.
      END.
      /* gdm - 06220908 */
      IF v-relflg2 THEN
      oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto~.carrier
      ELSE xoe-ord.carrier.
    
  END. /* if avail shipto */
END. /* ... else (if oe-rel was already available */


/* Update reftable for order type */
IF v-relflg2 THEN DO:
  
  /* task 04011103*/
  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.NAME EQ "RelType" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN
    FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = b-oe-ordl.cust-no
      AND sys-ctrl-ship.ship-id = oe-rel.ship-id NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl-shipto THEN
    FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = b-oe-ordl.cust-no
      AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.
      ELSE IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.
  
  IF v-relType <> "" THEN DO:
    FIND FIRST reftable
    WHERE reftable.reftable EQ "oe-rel.s-code"
        AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999") 
      NO-LOCK NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN reftable.reftable = "oe-rel.s-code"
             reftable.company = STRING(oe-rel.r-no,"9999999999")
             reftable.CODE = IF b-oe-ordl.is-a-component THEN "S"
                                ELSE SUBSTRING(v-relType,1,1).
      IF b-oe-ord.TYPE = "T" THEN
        ASSIGN reftable.CODE = "T".
      FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
        EXCLUSIVE-LOCK.
      bf-oe-rel.s-code = IF b-oe-ordl.is-a-component THEN "S"
                                ELSE SUBSTRING(v-relType,1,1).
      IF b-oe-ord.TYPE = "T" THEN
        ASSIGN bf-oe-rel.s-code = "T".      
      RELEASE bf-oe-rel.
      
    END. /* not avail reftable */
  END. /* v-reltype <> "" */
END. /* if v-relflg2 */

/* Assign qty to itemfg-loc */
RUN fg/fgitmloc.p (INPUT b-oe-ordl.i-no, INPUT ROWID(b-oe-ordl)).

RELEASE oe-rel.
RELEASE reftable.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-itemfg-sman) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemfg-sman Procedure 
PROCEDURE itemfg-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cNewRep AS CHARACTER   NO-UNDO.
  DEF BUFFER bf-oe-ord FOR oe-ord.

  IF NOT AVAIL oe-ord THEN
    FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
      AND oe-ord.ord-no  EQ b-oe-ordl.ord-no
    NO-ERROR.

  /* if order is from an estimate, use the esitmate sales rep # */
  IF b-oe-ordl.est-no GT "" THEN
    RETURN.

   
    FIND FIRST itemfg
        WHERE itemfg.company = g_company
          AND itemfg.i-no = b-oe-ordl.i-no
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN
      RETURN.

    RUN fg/fgSlsRep.p (INPUT cocode,
                   INPUT oe-ord.cust-no,
                   INPUT b-oe-ordl.part-no,
                   INPUT itemfg.i-no,
                   OUTPUT cNewRep).
   
    IF cNewRep GT "" AND cNewRep NE b-oe-ordl.s-man[1] THEN DO:
     b-oe-ordl.s-man[1] = cNewRep. 
     /* RUN new-s-man (1). */
    END.
      
    IF cNewRep GT "" AND cNewRep NE oe-ord.sman[1] THEN DO:

      /* Update the header with the new sales rep */
      FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord)
         EXCLUSIVE-LOCK NO-ERROR.
     
      IF AVAIL bf-oe-ord THEN DO:
        ASSIGN bf-oe-ord.sman[1] = cNewRep.
        FIND sman WHERE sman.company = oe-ord.company
            AND sman.sman = bf-oe-ord.sman[1]
            NO-LOCK NO-ERROR.
       
        IF AVAIL sman THEN ASSIGN bf-oe-ord.sname[1] = sman.sname
                                  bf-oe-ord.s-comm[1] = (sman.scomm).             
      END. /* fi avail header */

      RELEASE bf-oe-ord.
    END. /* if new rep found */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-nextOrdNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextOrdNo Procedure 
PROCEDURE nextOrdNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       No longer in use, replace by order_seq
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-ord-no AS INTEGER NO-UNDO.
  DEF VAR iNextOrd AS INT NO-UNDO.

/*   FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ g_company */
/*        NO-ERROR.                                                */
/*   IF AVAIL oe-ctrl THEN DO:                                     */
/*     op-ord-no = oe-ctrl.n-ord.                                  */
/*                                                                 */
/*   END.                                                          */
/*   ELSE op-ord-no = ?.                                           */
  RUN sys/ref/asiseq.p (INPUT g_company, INPUT "order_seq", OUTPUT iNextOrd) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

  op-ord-no = iNextOrd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-lock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-lock Procedure 
PROCEDURE set-lock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-form-no LIKE ef.form-no NO-UNDO.
  DEF INPUT PARAMETER ip-op-lock LIKE ef.op-lock NO-UNDO.
  

  FIND FIRST ef
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        AND ef.form-no EQ ip-form-no
      NO-ERROR.
  IF AVAIL ef THEN DO:
    ef.op-lock = ip-op-lock.
    RELEASE ef.
  END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-start-date1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-start-date1 Procedure 
PROCEDURE update-start-date1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-update-job-stdate AS LOG  NO-UNDO.
 DEF VAR lv-prom-date AS DATE NO-UNDO.
 DEFINE VARIABLE v-run-schedule AS LOGICAL NO-UNDO.

 IF oe-ordl.job-no = "" THEN RETURN.

   DEF BUFFER bx-ordl FOR oe-ordl.
   DEF VAR lv-first-due-date AS DATE NO-UNDO.
   lv-first-due-date = oe-ordl.req-date.

  FOR EACH bx-ordl WHERE bx-ordl.company = oe-ordl.company
                      AND bx-ordl.job-no = oe-ordl.job-no
                      AND bx-ordl.job-no2 = oe-ordl.job-no2 
                      AND RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
       lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
                           ELSE lv-first-due-date.
  END. /* each bx-ordl */

  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bf-job FOR job.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-maccum-time AS INT NO-UNDO.
  DEF VAR lv-job-hr AS INT NO-UNDO.
  DEF VAR lv-job-day AS INT NO-UNDO.
  DEF VAR lv-wrk-st-time AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  DEF VAR li-num-of-wkend AS INT NO-UNDO.
  DEF VAR lv-start-date-fr AS DATE NO-UNDO.

  /*===  calculate start date from due-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0.

  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no 
                    AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
          ASSIGN
             lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                             TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                           ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
             lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                             TRUNCATE(bf-mch.run-hr,0) * 3600 +
                           ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
             lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
  END. /* each bf-hdr */
  
  ASSIGN
     lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN TRUNCATE(lv-job-time / 3600,0) + 1
                 ELSE TRUNCATE(lv-job-time / 3600,0)
     lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
                  ELSE TRUNCATE(lv-job-hr / 8,0)
     lv-start-date = lv-first-due-date - lv-job-day. /*- 1. */

  /*  get from mach-calendar 
  lv-chk-date = lv-start-date.
  li-num-of-wkend = 0.
  DO i = 1 TO lv-first-due-date - lv-start-date:
     IF WEEKDAY(lv-chk-date) = 1 OR WEEKDAY(lv-chk-date) = 7 THEN li-num-of-wkend = li-num-of-wkend + 1.
     lv-chk-date = lv-chk-date + 1.
  END.
  lv-start-date = lv-start-date - li-num-of-wkend.
  */
  FIND bx-ordl WHERE RECID(bx-ordl) = RECID(oe-ordl).
  lv-prom-date = TODAY + lv-job-day.
  IF lv-start-date < TODAY  /* ip-type = "Update-2" is from v-ord.w*/
  THEN DO:
     lv-update-job-stdate = NO.
     /*MESSAGE "JOB CANNOT BE COMPLETED BEFORE REQUESTED DUE DATE DUE TO TOTAL MACHINE HOURS."
         SKIP
         "PROMISED DATE WILL BE   " lv-prom-date SKIP
         "UPDATE JOB's START DATE & DUE DATE?" UPDATE lv-update-job-stdate
            VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
    */
     MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
             "Due Date is before Calculates Promised Date. Update Due Date?" UPDATE lv-update-job-stdate
             VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
     /*IF lv-update-job-stdate THEN .
     ELSE DO:
         bx-ordl.prom-date = lv-prom-date.           
         return.
     END. */
     lv-start-date = TODAY.
  END. /* lv-start-date < today */
  
  v-run-schedule = NOT CAN-FIND(FIRST sys-ctrl
                                WHERE sys-ctrl.company EQ oe-ord.company
                                  AND sys-ctrl.name EQ 'SCHEDULE'
                                  AND sys-ctrl.char-fld EQ 'NoDate'
                                  AND sys-ctrl.log-fld EQ YES).
  IF v-run-schedule THEN DO: /* run if above does not exist */
  
  /* === reset start-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0
         li-num-of-wkend = 0.
  
      FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                        AND bf-hdr.job-no = oe-ordl.job-no
                        AND bf-hdr.job-no2 = oe-ordl.job-no2,
          EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2
                        AND NOT bf-mch.anchored
                   BREAK BY bf-mch.frm BY bf-mch.blank-no BY bf-mch.pass BY bf-mch.m-code:
    
              FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                                AND mach-calendar.m-code = bf-mch.m-code
                                AND mach-calendar.m-date = lv-start-date
                                NO-LOCK NO-ERROR.
              lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                          ELSE 28800. /* 8 HRs*/
              IF lv-m-time LT 0 THEN lv-m-time = 28800.
              lv-maccum-time = lv-maccum-time + lv-m-time.
              IF FIRST(bf-mch.frm) THEN DO:
                 FIND FIRST bf-job OF bf-hdr.
                 ASSIGN
                    bf-job.start-date = lv-start-date
                    lv-wrk-st-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
              END. /* first (bf-mch.frm) */
              IF FIRST-OF(bf-mch.frm) THEN
                    bf-hdr.start-date = job.start-date.
          
              ASSIGN
              lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                          TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                        ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
              lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                          TRUNCATE(bf-mch.run-hr,0) * 3600 +
                        ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
              bf-mch.seq-no = 0                 
              bf-mch.start-time-su = lv-wrk-st-time
              bf-mch.start-time = lv-wrk-st-time + lv-mr-time
              bf-mch.start-date-su = lv-start-date
              lv-start-date-fr = lv-start-date
              lv-job-time = lv-job-time + lv-mr-time
              lv-start-date = lv-start-date + 
                              IF lv-mr-time > lv-m-time AND
                                 lv-mr-time MOD lv-m-time > 0 THEN TRUNCATE(lv-mr-time / lv-m-time,0) 
                              ELSE IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0) - 1
                              ELSE 0
              lv-start-date-fr = lv-start-date.
              IF lv-m-time <> lv-maccum-time THEN DO:
                 lv-start-date = lv-start-date + 
                              IF lv-job-time > lv-maccum-time AND
                                 lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                              ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                              ELSE 0.
              END. /* lv-m-time <> lv-maccum-time  */
              ASSIGN
              lv-start-date-fr = lv-start-date
              bf-mch.end-date-su = lv-start-date
              bf-mch.start-date = lv-start-date
              lv-job-time = lv-job-time + lv-run-time
              lv-start-date = lv-start-date + 
                              IF lv-run-time > lv-m-time AND
                                 lv-run-time MOD lv-m-time > 0 THEN TRUNCATE(lv-run-time / lv-m-time,0) 
                              ELSE IF lv-run-time > lv-m-time THEN TRUNCATE(lv-run-time / lv-m-time,0) - 1
                              ELSE 0
              lv-start-date-fr = lv-start-date.
    
              IF lv-m-time <> lv-maccum-time THEN
                 lv-start-date = lv-start-date + 
                              IF lv-job-time > lv-maccum-time AND
                                 lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                              ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                              ELSE 0.
              
              ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time
                     bf-mch.end-time-su = bf-mch.start-time-su + lv-mr-time
                     bf-mch.end-date = lv-start-date           
                     lv-wrk-st-time = lv-wrk-st-time + lv-mr-time + lv-run-time.
      END. /* each bf-hdr */
  END. /* if v-run-schedule*/
  
  ASSIGN
     bx-ordl.prom-date = lv-prom-date
     bx-ordl.req-date  = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

