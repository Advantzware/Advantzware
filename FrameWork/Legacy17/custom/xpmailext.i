/* xpmailext.i - copied from xpmail.i and changed to have 2nd parameter to include shipTo or other email list */


DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.

&IF DEFINED(passGroupTitle) NE 0 &THEN
  DEFINE INPUT PARAMETER  ipGroupTitle AS CHARACTER NO-UNDO.
&ELSE
  DEFINE VARIABLE         ipGroupTitle AS CHARACTER NO-UNDO INITIAL 'EMail'.
&ENDIF

DEFINE INPUT PARAMETER ipFileList   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipIdxKey     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipSubject    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipBody       AS CHARACTER NO-UNDO.
define input parameter ip2ndGroup as character no-undo.
define input parameter ip2ndkey as character no-undo.
DEFINE OUTPUT PARAMETER opReturn    AS INTEGER NO-UNDO.

{custom/globdefs.i}

DEFINE VARIABLE retcode             AS INTEGER   NO-UNDO.
DEFINE VARIABLE ls-to-list          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-mailto           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-mailsubject      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-mailbody         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-mailattach       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlSilentMode        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vcRecordID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-to-list2         AS CHARACTER NO-UNDO.
define variable iCount as integer no-undo.

DEFINE BUFFER b2-cust               FOR cust.
    
IF ipType BEGINS 'CUSTOMER' THEN DO:

   FIND FIRST cust NO-LOCK
        WHERE cust.company EQ g_company
          AND cust.cust-no EQ ipIdxKey NO-ERROR.
   
   IF AVAILABLE cust THEN DO:
      vcRecordId = cust.cust-no.
      RUN buildToList (input  cust.rec_key,   /* Rec_Key        */
                       input  cust.email,     /* Email Address  */
                       input  ipGroupTitle,   /* Title          */
                       output ls-to-list).    /* Recepients     */
   END.
   ELSE ls-to-list = ''.
   if ip2ndKey ne "" then do:
      ls-to-list2 = "".
      iCount = 0. 
      case ip2ndGroup:
           when "ShipTo" then do while iCount <= num-entries(ip2ndKey):
               iCount = iCount + 1. 
               FIND FIRST shipto NO-LOCK  WHERE shipto.company EQ g_company
                        AND shipto.rec_key EQ entry(iCount,ip2ndKey) NO-ERROR.

               IF AVAILABLE shipto THEN DO:
                  vcRecordId = shipto.cust-no + ' ' + shipto.ship-id.
                  RUN buildToList (input  shipto.rec_key, /* Rec_Key        */
                       input  '',             /* Email Address  */
                       input  ipGroupTitle,   /* Title          */
                       output ls-to-list2).    /* Recepients     */
               END.
               ELSE ls-to-list2 = ''.
               if ls-to-list2 <> "" then ls-to-list = ls-to-list2 + "," + ls-to-list.                  
           end.
                   
      end case. 
   end.
       
END.

ELSE IF ipType begins 'VENDOR' THEN DO:

   FIND FIRST vend NO-LOCK
        WHERE vend.company EQ g_company
          AND vend.vend-no EQ ipIdxKey NO-ERROR.

   IF AVAILABLE vend THEN DO:
      vcRecordId = vend.vend-no.
      RUN buildToList (input  vend.rec_key,   /* Rec_Key        */
                       input  '',             /* Email Address  */
                       input  ipGroupTitle,   /* Title          */
                       output ls-to-list).    /* Recepients     */
   END.
   ELSE ls-to-list = ''.
END.

ELSE IF ipType BEGINS 'SHIPTO' THEN DO:

  FIND FIRST shipto NO-LOCK
       WHERE shipto.company EQ g_company
         AND shipto.rec_key EQ ipIdxKey NO-ERROR.

   IF AVAILABLE shipto THEN DO:
      vcRecordId = shipto.cust-no + ' ' + shipto.ship-id.
      RUN buildToList (input  shipto.rec_key, /* Rec_Key        */
                       input  '',             /* Email Address  */
                       input  ipGroupTitle,   /* Title          */
                       output ls-to-list).    /* Recepients     */
   END.
   ELSE ls-to-list = ''.   

END.
ELSE IF ipType begins 'SoldTo' THEN DO:
  
   FIND FIRST soldto NO-LOCK
          WHERE soldto.company EQ g_company
            AND soldto.cust-no EQ ENTRY(1,ipIdxKey,"|")
            AND soldto.sold-id EQ (entry(2,ipIdxKey,"|")) NO-ERROR.

   IF AVAILABLE soldto THEN DO:
         vcRecordId = soldto.cust-no + ' ' + soldto.sold-id.
         RUN buildToList (input  soldto.rec_key, /* Rec_Key        */
                          input  '',             /* Email Address  */
                          input  ipGroupTitle,   /* Title          */
                          output ls-to-list).    /* Recepients     */
         FIND cust OF soldto NO-LOCK NO-ERROR.
         IF AVAIL cust THEN DO:
            ls-to-list2 = "".
            RUN buildToList (input  cust.rec_key, /* Rec_Key        */
                             input  '',             /* Email Address  */
                             input  ipGroupTitle,   /* Title          */
                             output ls-to-list2).    /* Recepients     */
            IF ls-to-list2 <> "" THEN
                ls-to-list = ls-to-list2 + "," + ls-to-list.
         END.
   END.
   ELSE ls-to-list = ''.

   IF ls-to-list = '' THEN DO:
     ipType = "Customer".
     FIND FIRST cust NO-LOCK
        WHERE cust.company EQ g_company
          AND cust.cust-no EQ entry(1,ipIdxKey,"|") NO-ERROR.

     IF AVAILABLE cust THEN DO:
      vcRecordId = cust.cust-no.
      RUN buildToList (input  cust.rec_key,   /* Rec_Key        */
                       input  cust.email,     /* Email Address  */
                       input  ipGroupTitle,   /* Title          */
                       output ls-to-list).    /* Recepients     */
         
     END.
     ELSE ls-to-list = ''.
   END.
END.

ELSE IF ipType begins 'SalesRep' THEN DO:

   FIND FIRST sman NO-LOCK
        WHERE sman.company EQ g_company
          AND sman.sman EQ ipIdxKey NO-ERROR.

   IF AVAILABLE sman THEN DO:
      vcRecordId = sman.sman.
      RUN buildToList (input  sman.rec_key,   /* Rec_Key        */
                       input  '',             /* Email Address  */
                       input  ipGroupTitle,   /* Title          */
                       output ls-to-list).    /* Recepients     */
   END.
   ELSE ls-to-list = ''.
END.

ELSE IF ipType EQ 'ALL' THEN
    RUN buildToList (input  ipType,           /* Rec_Key        */
                     input  '',               /* Email Address  */
                     input  ipGroupTitle,     /* Title          */
                     output ls-to-list).      /* Recepients     */
ASSIGN
  lv-mailto       = 'To:' + ls-to-list
  lv-mailsubject  = ipSubject
  lv-mailbody     = ipBody
  lv-mailattach   = ipFileList.

IF ls-to-list EQ '' OR 
   ls-to-list EQ ? OR NOT ls-to-list MATCHES '*@*' THEN DO:

  IF ipType MATCHES '*1' THEN RETURN. /* Quiet Mode. */

END.

IF lv-mailattach MATCHES('*xpr*') AND SEARCH('viewer.exe') NE ? THEN
  ASSIGN
    FILE-INFO:FILE-NAME = 'viewer.exe'
    lv-mailattach       = FILE-INFO:FULL-PATHNAME + ',' + lv-mailattach.

/* Customer, Vendor, ShipTo   = DIALOG BOX        */
IF TRIM(ipType) EQ "" OR SUBSTRING (ipType, LENGTH (ipType)) ne '1' THEN
do:
    
  RUN mail (lv-mailto,        /* Mail Recepients  */
            lv-mailsubject,   /* Subject          */
            lv-mailbody,      /* Body             */
            lv-mailattach,    /* Attachment       */
            1,                /* Mail Dialog Type */
            OUTPUT retcode).  /* Return Code      */
  
end.

/* Customer1, Vendor1, ShipTo1 = SILENT MODE      */ 
else do:
             
  RUN mail (lv-mailto,        /* Mail Recepients  */
            lv-mailsubject,   /* Subject          */
            lv-mailbody,      /* Body             */
            lv-mailattach,    /* Attachment       */
            0,                /* Mail Dialog Type */
            OUTPUT retcode).  /* Return Code      */
end.

opReturn = retcode.
                    
PROCEDURE mail EXTERNAL 'xpMail.dll' :

  DEFINE INPUT  PARAMETER mailTo      AS CHAR.
  DEFINE INPUT  PARAMETER mailsubject AS CHAR.
  DEFINE INPUT  PARAMETER mailText    AS CHAR.
  DEFINE INPUT  PARAMETER mailFiles   AS CHAR.
  DEFINE INPUT  PARAMETER mailDialog  AS LONG.
  DEFINE OUTPUT PARAMETER retCode     AS LONG.
END.

PROCEDURE buildToList:

  DEFINE INPUT  PARAMETER ipRecKey    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipEMail     AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipCode      AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opToList    AS CHARACTER NO-UNDO.

  CASE ipCode:
    WHEN 'PO-ORDL_.' THEN ipCode = 'R-POPRT.'.
  END CASE.

  FOR EACH phone NO-LOCK 
     WHERE phone.table_rec_key EQ ipRecKey
        OR ipRecKey EQ 'ALL':
    
    IF CAN-FIND(FIRST emaildtl
                WHERE emaildtl.emailcod       EQ ipCode
                  AND emaildtl.table_rec_key  EQ phone.rec_key) 
       OR
       phone.titlcode EQ ipCode THEN
    DO:
    
      IF phone.e_mail NE '' AND NOT CAN-DO(opToList,phone.e_mail) then 
      do:
/*         IF ipCode BEGINS 'R-BOLPRT' THEN DO:                               */
/*           IF NOT CAN-FIND (FIRST reftable NO-LOCK                          */
/*                            WHERE reftable.rec_key = STRING (RECID (phone)) */
/*                              AND reftable.CODE    = ipCode) THEN NEXT.     */
/*         END.                                                               */
        
        opToList = opToList + (IF opToList NE '' THEN ',' 
                                                  ELSE '') 
                            + phone.e_mail.
      end.
    END.
        
  END. /* each phone */

  IF opToList EQ '' OR opToList = ? THEN DO: 

    IF ipType BEGINS 'Customer' THEN DO:

      FIND FIRST b2-cust 
           WHERE b2-cust.rec_key = ipRecKey
             AND b2-cust.active  = 'X' 
           NO-LOCK NO-ERROR.

      IF AVAIL b2-cust THEN DO: 

        FOR EACH phone NO-LOCK
           WHERE phone.table_rec_key = b2-cust.rec_key,
            EACH reftable NO-LOCK
           WHERE reftable.rec_key = phone.rec_key
             AND reftable.CODE    = ipCode:
            
          opToList = opToList + (IF opToList NE '' THEN ',' 
                                                   ELSE '') 
                              + phone.e_mail.
        END.

        IF opToList EQ '' OR opToList EQ ? THEN 
          opToList = b2-cust.email.
      END.

      ELSE opToList = ipEMail.
    END.
  END.
  
END PROCEDURE.
