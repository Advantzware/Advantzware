 
/*------------------------------------------------------------------------
    File        : RfqEstimate.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of transfer to Estimate

    Author(s)   : 
    Created     : x
    Notes       :
  ----------------------------------------------------------------------*/
                    
/* ***************************  Definitions  ************************** */
{RfqEstimate.i}
DEFINE INPUT PARAMETER prmComp        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLoc         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo       AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmEstType     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstNew      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSeqList     AS CHAR  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqEstimate.
DEFINE OUTPUT PARAMETER cError        AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vMailto       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vSubject      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vBody         AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vInternalUser AS CHARACTER  NO-UNDO.

def var lv-seq-max as int no-undo.
def var ll-transfer as log no-undo.
def var li-est-type as int no-undo.
def var ls-seq-list as cha no-undo.
def var ll-selected as log no-undo.
def var li-current-row as int no-undo.
def var ls-find-list as cha no-undo.
def var ls-crt-list as cha no-undo.
def var ls-dest-code as cha no-undo.

DEF TEMP-TABLE tt-rfqmail
    FIELD rfqnum AS INT
    FIELD stock AS CHAR
    FIELD qty AS DEC
    FIELD rfqDate AS DATE
    FIELD part AS CHAR
    FIELD cust-no AS CHAR
    .
DEF VAR viCount AS INT NO-UNDO.
def buffer bf-rfqitem for rfqitem.
DEFINE VARIABLE cb-est-type AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE li-num-of-blank AS INTEGER FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE li-num-of-form AS INTEGER FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE rd-trx-type AS INTEGER NO-UNDO.

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF PrmRfqNo  = ?  THEN ASSIGN prmRfqNo = 0.
IF prmAction = ?  THEN ASSIGN prmAction = "Select".
IF prmEstType = ? THEN ASSIGN prmEstType = "".
IF prmEstNew = ? THEN ASSIGN prmEstNew = "".
IF prmSeqList = ? THEN ASSIGN prmSeqList = "".
IF prmEstType = "" THEN ASSIGN li-est-type = 0.
IF prmEstType = "Folding Single Item" THEN ASSIGN li-est-type = 1.
IF prmEstType = "Folding Two Piece Box" THEN ASSIGN li-est-type = 2.
IF prmEstType = "Folding Tandem Runs" THEN ASSIGN li-est-type = 3.
IF prmEstType = "Folding Combination" THEN ASSIGN li-est-type = 4.
IF prmEstType = "Corrugated Single Item" THEN ASSIGN li-est-type = 5.
IF prmEstType = "Corrugated Set" THEN ASSIGN li-est-type = 6.
IF prmEstNew  = "yes" THEN ASSIGN rd-trx-type = 1.
 ELSE ASSIGN rd-trx-type = 2.
ASSIGN ls-seq-list = prmSeqList.

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
/*************************************/
IF prmAction = "TransferEstimate" THEN DO:
    def var li-cnt as int no-undo.
    def var ll-return as INT no-undo.
    def var ll-is-transfered as log no-undo.
    def var ls-est-list as cha no-undo.
    DEF BUFFER bf-item FOR rfqitem.
   /* IF li-est-type = 6 THEN DO:
        FIND FIRST rfq WHERE rfq.rfq-no = prmRfqNo NO-LOCK NO-ERROR.
        FIND FIRST rfqitem OF rfq where rfqitem.seq = 999 NO-LOCK NO-ERROR.
        IF NOT AVAIL rfqitem THEN DO: 
            FIND FIRST bf-item OF rfq NO-LOCK NO-ERROR.
            CREATE rfqitem.
            BUFFER-COPY bf-item EXCEPT bf-item.seq TO rfqitem.
            rfqitem.seq = 999.
            END.
    END.*/
        
        assign  
        li-num-of-form  = 1
        li-num-of-blank = 1
        ls-find-list = ""
        ls-crt-list = "" 
        ll-is-transfered = no.
        if rd-trx-type = 1 then do: /* new */ 
            IF li-est-type = 1 OR li-est-type = 5  then run trx-rfq-to-est.
            IF li-est-type = 2 OR li-est-type = 3 OR li-est-type = 4 then run trx-rfq-to-est-4.
            IF li-est-type = 6  then run trx-rfq-to-est-6.        
        end. 
        else do: /* update */
            ls-find-list = ls-seq-list.
            IF (li-est-type = 1 OR li-est-type = 5 ) then run find-est-update.
            IF (li-est-type = 2 OR li-est-type = 3 OR li-est-type = 4 OR li-est-type = 7 OR li-est-type = 8) then run find-est-update4.
            IF (li-est-type = 6 ) then run find-est-update6.
        END.

       FIND CURRENT est EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL est THEN DO:
           ASSIGN est.mod-date = 01/01/1900 .           
       END.
     /*******************************************mail*****************************/
     
     cError = "".
     FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.      
     for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :  
         IF rfqitem.est-no <> "" THEN 
             ASSIGN  cError = "Successfully Transfered. ".
         ELSE
                 cError = "Not Transfered to Estimate".
                 CREATE tt-rfqmail.
                 ASSIGN
                     tt-rfqmail.rfqnum = rfqitem.rfq-no
                     tt-rfqmail.stock  = rfqitem.stock-no
                     tt-rfqmail.part    = rfqitem.part-no
                     tt-rfqmail.qty     = rfqitem.qty[1]
                     tt-rfqmail.rfqDate = TODAY
                     tt-rfqmail.cust-no = rfq.cust-no.
                 RELEASE tt-rfqmail.
          END.
   RUN send-email-proc.
    ASSIGN
        prmAction = "Select".



END.  /*if prmAction = transfer*/

/*******************************************************************************************************************/

PROCEDURE trx-rfq-to-est :
def var li-next-est as int no-undo.
def var li-next-enum as int no-undo.
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
DEF VAR i AS INT NO-UNDO.
DEF VAR ls-key AS cha NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF est-qty.
DISABLE TRIGGERS FOR LOAD OF ef.
DISABLE TRIGGERS FOR LOAD OF eb.
    
FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.
FIND FIRST rfqitem WHERE
         rfqitem.company = prmComp AND
         rfqitem.loc     = prmLoc AND
         rfqitem.rfq-no = PrmRfqNo
         NO-LOCK NO-ERROR.

find first ce-ctrl where ce-ctrl.company = rfqitem.company and
                         ce-ctrl.loc = rfqitem.loc no-error.
li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0.   
find last est /*use-index e-num no in V9 */ no-lock no-error.
li-next-enum = if avail est then est.e-num else 0.
for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :  /* of rfq*/
    li-next-est = li-next-est + 1.
    li-next-enum = li-next-enum + 1.
    li-form-no = 1.
    li-blank-no = 1.
    create est.
    assign est.company  = rfqitem.company                                     
       est.loc      = rfqitem.loc
       est.est-qty[1] = rfqitem.qty[1]
       est.est-qty[2] = rfqitem.qty[2]
       est.est-qty[3] = rfqitem.qty[3]
       est.est-qty[4] = rfqitem.qty[4]                                             
       est.est-type = li-est-type
       est.e-num    = li-next-enum
       /*est.est-no   = string(li-next-est)   */
        est.est-no   = string(li-next-est,">>>>>>>>")      
       est.form-qty = 1               
       est.mod-date = 01/01/1900  
       est.est-date = TODAY.
    ls-key = string(today,"99999999") +
             string(next-value(rec_key_seq,asi),"99999999").
    est.rec_key = ls-key.      
         
    CREATE est-qty.
    ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no.
          est-qty.eqty = rfqitem.qty[1]
           .
          DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
         END.    

    run create-est-prep (li-next-enum,STRING(li-next-est,">>>>>>>>"),rfqitem.company,rfqitem.qty[1]).    

    if rfqitem.ship-id <> "" then do:
        find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
        if avail shipto then ls-dest-code = shipto.dest-code.
    end.        
    else do:
        find cust where cust.company = rfqitem.company and
                        cust.cust-no = rfq.cust-no 
                        no-lock no-error.
        if avail cust then ls-dest-code = cust.del-zone.
    end.
    create ef.
    FIND FIRST ITEM WHERE
        ITEM.company EQ rfqitem.company AND
        ITEM.i-no EQ rfqitem.board
        NO-LOCK NO-ERROR.
    {rfq/asn-ef.i}  
        RELEASE ITEM.        
    create eb.
    {rfq/asn-eb.i}
   assign rfqitem.est-no = est.est-no
          rfqitem.form-no = li-form-no
          rfqitem.blank-no = li-blank-no
          .
           
    /*{rfq/upd-note.i}*/    

 end.  /* each rfqitem */

 ce-ctrl.e-num = li-next-est.
 
END PROCEDURE.
/************************************************************************************************************/
PROCEDURE trx-rfq-to-est-2 :
   
def var li-next-est as int no-undo.
def var li-next-enum as int no-undo.
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
 FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.
FIND FIRST rfqitem WHERE
         rfqitem.company = prmComp AND
         rfqitem.loc     = prmLoc AND
         rfqitem.rfq-no = PrmRfqNo
         NO-LOCK NO-ERROR.
find first ce-ctrl where ce-ctrl.company = rfqitem.company and
                         ce-ctrl.loc = rfqitem.loc
                         no-error.
li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0.
find last est no-lock no-error.
li-next-enum = if avail est then est.e-num else 0.
for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :  /* of rfq*/
       li-next-est = li-next-est + 1.
       li-next-enum = li-next-enum + 1.
       create est.
       assign est.company  = rfqitem.company                                     
       est.loc        = rfqitem.loc
       est.est-qty[1] = rfqitem.qty[1]
       est.est-qty[2] = rfqitem.qty[2]
       est.est-qty[3] = rfqitem.qty[3]
       est.est-qty[4] = rfqitem.qty[4]                                             
       est.est-type = li-est-type
       est.e-num    = li-next-enum
       est.est-no   = string(li-next-est,">>>>>>>>") .

        FIND FIRST ITEM WHERE
             ITEM.company EQ rfqitem.company AND
             ITEM.i-no EQ rfqitem.board
             NO-LOCK NO-ERROR.

        do li-cnt = 1 to li-num-of-form:
           create ef.
           assign li-form-no = li-cnt
                  li-blank-no = 1.
           {rfq/asn-ef.i}

           create eb.
           {rfq/asn-eb.i}
        end.

        RELEASE ITEM.

        /*{rfq/upd-note.i}*/

    rfqitem.est-no = est.est-no.
   end.  /* each rfqitem */

   ce-ctrl.e-num = li-next-est.
END PROCEDURE.
/*********************************************************************************************/
PROCEDURE trx-rfq-to-est-3 :
def var li-next-est as int no-undo.
def var li-next-enum as int no-undo.
def var li-cnt as int no-undo.
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.
 FIND FIRST rfqitem WHERE
         rfqitem.company = prmComp AND
         rfqitem.loc     = prmLoc AND
         rfqitem.rfq-no = PrmRfqNo
         NO-LOCK NO-ERROR.
find first ce-ctrl where ce-ctrl.company = rfqitem.company and
                         ce-ctrl.loc = rfqitem.loc
                         no-error.
li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0.
find last est no-lock no-error.
li-next-enum = if avail est then est.e-num else 0.
li-form-no = 1.
for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :  /* of rfq*/
       li-next-est = li-next-est + 1.
       li-next-enum = li-next-enum + 1.
       create est.
       assign est.company  = rfqitem.company                                     
              est.loc      = rfqitem.loc
              est.est-qty[1] = rfqitem.qty[1]
              est.est-qty[2] = rfqitem.qty[2]
              est.est-qty[3] = rfqitem.qty[3]
              est.est-qty[4] = rfqitem.qty[4]                                             
              est.est-type = li-est-type
              est.e-num    = li-next-enum
              est.est-no   = string(li-next-est,">>>>>>>>")  .                        
       create ef.
       FIND FIRST ITEM WHERE
            ITEM.company EQ rfqitem.company AND
            ITEM.i-no EQ rfqitem.board
            NO-LOCK NO-ERROR.

       {rfq/asn-ef.i}
       
      RELEASE ITEM.
    do li-cnt = 1 to li-num-of-blank: 
       create eb.
       li-blank-no = li-cnt.
       {rfq/asn-eb.i}  
    end.  /* do */

    rfqitem.est-no = est.est-no.
   end.  /* each rfqitem */

   ce-ctrl.e-num = li-next-est.
   
END PROCEDURE.
/*************************************************************************************************/
PROCEDURE trx-rfq-to-est-4 :
  def var li-next-est as int no-undo.
  def var li-next-enum as int no-undo.
  def var li-cnt as int no-undo.
  def var li-cnt2 as int no-undo.
  def var li-form-no as int no-undo.
  def var li-blank-no as int no-undo.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR ls-key AS cha NO-UNDO.

  
FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.
FIND FIRST rfqitem WHERE
         rfqitem.company = prmComp AND
         rfqitem.loc     = prmLoc AND
         rfqitem.rfq-no = PrmRfqNo
         NO-LOCK NO-ERROR.
find first ce-ctrl where ce-ctrl.company = rfqitem.company and
                         ce-ctrl.loc = rfqitem.loc
                         no-error.
  li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0.
  find last est no-lock no-error.
  li-next-enum = if avail est then est.e-num else 0.
  li-form-no = 1.
  li-cnt = 0.
  li-num-of-blank = num-entries(ls-seq-list) - 1. /* don't count last comma */
  for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :
      /*if FIRST-OF(rfqitem.part-no) then do:*/
         li-next-est = li-next-est + 1.
         li-next-enum = li-next-enum + 1.
         li-cnt2 = 0.
         create est.
       assign est.company  = rfqitem.company                                     
              est.loc      = rfqitem.loc
              est.est-qty[1] = rfqitem.qty[1]
              est.est-qty[2] = rfqitem.qty[2]
              est.est-qty[3] = rfqitem.qty[3]
              est.est-qty[4] = rfqitem.qty[4]                                             
              est.est-type = li-est-type
              est.e-num    = li-next-enum
              est.est-no   = string(li-next-est,">>>>>>>>")  /*,">>>>>>>>"*/                          
              est.form-qty = li-num-of-form
              est.est-date = today 
              est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */.

      ls-key = string(today,"99999999") +
             string(next-value(rec_key_seq,asi),"99999999").
      est.rec_key = ls-key.       
      CREATE est-qty.
      ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no
           est-qty.eqty = rfqitem.qty[1]
           .
      DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
      END.
          run create-est-prep (li-next-enum,STRING(li-next-est,">>>>>>>>"),rfqitem.company,rfqitem.qty[1]).
          li-form-no = li-cnt + 1.
          create ef.
          FIND FIRST ITEM WHERE
               ITEM.company EQ rfqitem.company AND
               ITEM.i-no EQ rfqitem.board
               NO-LOCK NO-ERROR.
          {rfq/asn-ef.i}
          RELEASE ITEM.
  /*END.*/
  if rfqitem.ship-id <> "" then do:
      find shipto where shipto.company = rfqitem.company and
                        shipto.cust-no = rfq.cust-no and
                        shipto.ship-id = rfqitem.ship-id
                        no-lock no-error.
      if avail shipto then ls-dest-code = shipto.dest-code.
      end.    
      else do:
          find cust where cust.company = rfqitem.company and
                          cust.cust-no = rfq.cust-no 
                          no-lock no-error.
          if avail cust then ls-dest-code = cust.del-zone.
      end.
      create eb.         /* 1-eb per rfqitem */
      li-cnt2 = li-cnt2 + 1.
      li-blank-no = li-cnt2.
      {rfq/asn-eb.i}              
          assign rfqitem.est-no = est.est-no
          rfqitem.form-no = li-form-no
          rfqitem.blank-no = li-blank-no
          .
      
  END.
  ASSIGN
ce-ctrl.e-num = li-next-est.   

END PROCEDURE.

/********************************************************************************************************/
PROCEDURE trx-rfq-to-est-6 :
  def var li-next-est as int no-undo.
  def var li-next-enum as int no-undo.
  def var li-cnt as int no-undo.
  def var li-cnt2 as int no-undo.
  def var li-form-no as int no-undo.
  def var li-blank-no as int no-undo.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-est-recid AS RECID NO-UNDO.
  DEF VAR ls-key AS cha NO-UNDO.
  DEF BUFFER bf-ritem FOR rfqitem.
FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.
      FIND FIRST rfqitem WHERE
         rfqitem.company = prmComp AND
         rfqitem.loc     = prmLoc AND
         rfqitem.rfq-no = PrmRfqNo
         NO-LOCK NO-ERROR.
  find first ce-ctrl where ce-ctrl.company = rfqitem.company and
                         ce-ctrl.loc = rfqitem.loc
                         no-error.
  li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0.
  find last est /*use-index e-num */ no-lock no-error.
  li-next-enum = if avail est then est.e-num else 0.

  li-form-no = 0.
  li-cnt = 0.
  li-num-of-blank = num-entries(ls-seq-list) - 1. /* don't count last comma */
  
  for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :  /* of rfq*/
      li-next-est = li-next-est + 1.
      li-next-enum = li-next-enum + 1.
      li-cnt2 = 0.
      create est.
          assign est.company  = rfqitem.company                                     
              est.loc      = rfqitem.loc
              est.est-qty[1] = rfqitem.qty[1]
              est.est-qty[2] = rfqitem.qty[2]
              est.est-qty[3] = rfqitem.qty[3]
              est.est-qty[4] = rfqitem.qty[4]                                             
              est.est-type = li-est-type
              est.e-num    = li-next-enum
              est.est-no   = string(li-next-est,">>>>>>>>")                          /*,,*/
              est.form-qty = li-num-of-form
              est.est-date = today 
              est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */.
          
          ls-key = string(today,"99999999") +
              string(next-value(rec_key_seq,asi),"99999999").
          est.rec_key = ls-key.

        CREATE est-qty.
        ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no
           est-qty.eqty = rfqitem.qty[1]
           .
        DO i = 1 TO 99:
           est-qty.qty[i] = rfqitem.qty[i].
        END.

        run create-est-prep (li-next-enum,STRING(li-next-est,">>>>>>>>"),rfqitem.company,rfqitem.qty[1]).
        /*{rfq/upd-note.i}  */        
      
      IF NOT AVAIL est THEN FIND est WHERE RECID(est) = lv-est-recid .      

      if rfqitem.ship-id <> "" then do:
         find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
         if avail shipto then ls-dest-code = shipto.dest-code.
      end.    
      else do:
         find cust where cust.company = rfqitem.company and
                         cust.cust-no = rfq.cust-no 
                         no-lock no-error.
         if avail cust then ls-dest-code = cust.del-zone.
      end.
      li-form-no = li-form-no + 1.
      create ef.

      FIND FIRST ITEM WHERE
           ITEM.company EQ rfqitem.company AND
           ITEM.i-no EQ rfqitem.board
           NO-LOCK NO-ERROR.
      {rfq/asn-ef.i}
      RELEASE ITEM.         
      li-cnt2 = 0.
      create eb.         /* 1-eb per rfqitem */
      li-cnt2 = li-cnt2 + 1.
      li-blank-no = li-cnt2.
      {rfq/asn-eb.i}             
      
      assign rfqitem.est-no = est.est-no
             rfqitem.form-no = li-form-no
             rfqitem.blank-no = li-blank-no
            .
         est.form-qty = li-form-no.
         FIND FIRST bf-ritem of rfq where bf-ritem.seq = 999  NO-ERROR.
         IF AVAIL bf-ritem THEN DO:
         create eb.         /* 1-eb per rfqitem */
           {rfq/asn-ebs.i}
           bf-ritem.est-no = est.est-no.
         END.
  end.  /* each rfqitem */
  ce-ctrl.e-num = li-next-est.  

END PROCEDURE.
/********************************************************************/
PROCEDURE select-all :
    def var li-count as int no-undo.
      DEF VAR li AS INT NO-UNDO.
      DEF VAR lv-seq AS CHAR NO-UNDO.
      ls-seq-list = "".
      FOR EACH rfqitem WHERE
             rfqitem.company = prmComp AND
             rfqitem.rfq-no = PrmRfqNo  NO-LOCK:

          ASSIGN
           lv-seq      = TRIM(STRING(rfqitem.seq,">>9"))
           ls-seq-list = ls-seq-list + lv-seq + ",".

          IF rfqitem.est-no EQ "" THEN
            ls-crt-list = ls-crt-list + lv-seq + ",".
          ELSE
            ls-find-list = ls-find-list + lv-seq + ",".
     END.

 
END PROCEDURE.
/***********************************************************************/
PROCEDURE find-est-update :
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
DEF VAR i AS INT NO-UNDO.

FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR. 
for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :  /* of rfq*/
    find est where est.company = rfqitem.company and
                   est.est-no = rfqitem.est-no
                   .
    assign est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
    FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.
    IF AVAIL est-qty THEN do:
        est-qty.eqty = rfqitem.qty[1].
        DO i = 1 TO 98:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
    END.
    find first ef where ef.company = est.company
                    AND ef.est-no = est.est-no
                    AND ef.form-no = rfqitem.form-no.
    assign li-form-no = 1
           li-num-of-blank = 1 
           li-blank-no = 1.
    
    if rfqitem.ship-id <> "" then do:
       find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
       if avail shipto then ls-dest-code = shipto.dest-code.
    end.    
    else do:
       find cust where cust.company = rfqitem.company and
                       cust.cust-no = rfq.cust-no 
                       no-lock no-error.
       if avail cust then ls-dest-code = cust.del-zone.
    end.
           
    FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.
{rfq/upd-ef.i}

    RELEASE ITEM.

    find first eb where eb.company = rfqitem.company
                    AND eb.est-no = rfqitem.est-no
                    AND eb.form-no = rfqitem.form-no
                    AND eb.blank-no = rfqitem.blank-no NO-ERROR.
  

    IF AVAIL eb THEN DO:     
        {rfq/upd-eb.i}
    END.
    /*{rfq/upd-note.i}*/
end.  /* each rfqitem */
END PROCEDURE.
/********************************************************************************************************/
PROCEDURE find-est-update4 :
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
DEF VAR i AS INT NO-UNDO.
FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.

for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :  /* of rfq*/
    find est where est.company = rfqitem.company and
                   est.est-no = rfqitem.est-no EXCLUSIVE-LOCK NO-ERROR
                   .
    
    assign est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
    
     FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.
     

     IF AVAIL est-qty THEN do:
        est-qty.eqty = rfqitem.qty[1].
        DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
     END.

     find first ef where ef.company = est.company AND 
                         ef.est-no = est.est-no.
    FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.
    
    {rfq/upd-ef.i}
        RELEASE ITEM.
        if rfqitem.ship-id <> "" then do:
       find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
       if avail shipto then ls-dest-code = shipto.dest-code.
       
    end.    
    else do:
       find cust where cust.company = rfqitem.company and
                       cust.cust-no = rfq.cust-no 
                       no-lock no-error.
       if avail cust then ls-dest-code = cust.del-zone.
    end.

       find first eb of ef where eb.blank-no = rfqitem.blank-no  /*li-cnt*/  no-error.
       if avail eb then do: 
          {rfq/upd-eb.i} 
       end.
       else do:
         leave.
       end.  
  /*  end.*/
    /*{rfq/upd-note.i}*/
end.  /* each rfqitem */


END PROCEDURE.
/*********************************************************************************/
PROCEDURE find-est-update6 :
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
DEF VAR i AS INT NO-UNDO.
DEF BUFFER bf-ritem FOR rfqitem.
    FIND FIRST rfq WHERE rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.
for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq)) > 0 :  /* of rfq*/

    find est where est.company = rfqitem.company and
                   est.est-no = rfqitem.est-no EXCLUSIVE-LOCK NO-ERROR.
                   .
    assign est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
     /*{rfq/upd-note.i}*/
     FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.

     IF AVAIL est-qty THEN do:
        DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
     END.

     find first ef where ef.company = est.company AND 
                         ef.est-no = est.est-no AND
                         ef.form-no = rfqitem.form-no.
     FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.

    {rfq/upd-ef.i}

    RELEASE ITEM.
    if rfqitem.ship-id <> "" then do:
        find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
       if avail shipto then ls-dest-code = shipto.dest-code.
    end.    
    else do:
       find cust where cust.company = rfqitem.company and
                       cust.cust-no = rfq.cust-no 
                       no-lock no-error.
       if avail cust then ls-dest-code = cust.del-zone.
    end.

    find first eb of ef where eb.blank-no = rfqitem.blank-no  /*li-cnt*/  no-error.
    if avail eb then do: 
          {rfq/upd-eb.i} 
    end.
    else do:
         leave.
    end.  
end.  /* each rfqitem */
/* update set header */
  FIND FIRST bf-ritem of rfq where bf-ritem.seq = 999 NO-LOCK NO-ERROR.
  IF AVAIL bf-ritem THEN DO:
    FIND FIRST est WHERE est.company = bf-ritem.company
                     AND est.est-no = bf-ritem.est-no NO-LOCK NO-ERROR.
    FIND FIRST ef WHERE ef.company = est.company
                   AND ef.est-no = bf-ritem.est-no NO-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.company = bf-ritem.company
                    AND eb.est-no = bf-ritem.est-no 
                    AND eb.form-no = 0
                    AND eb.blank-no = 0 NO-ERROR.
    IF NOT AVAIL eb THEN CREATE eb.
    {rfq/asn-ebs.i}
  END.
END PROCEDURE.
/***************************************************************************************************************************/
PROCEDURE create-est-prep :
   def input parameter ip-enum like est.e-num no-undo.
   def input parameter ip-est-no as cha no-undo.
   def input parameter vComp as cha no-undo.
   def input parameter vqty as DECIMAL no-undo.
   def var i as int no-undo.
   i = 1.   

  for each prep where prep.company = rfqitem.company and prep.dfault NO-LOCK:
       create est-prep.
       assign   est-prep.e-num  = ip-enum
                est-prep.est-no = ip-est-no
                est-prep.company = rfqitem.company
                est-prep.eqty = rfqitem.qty[1]
                est-prep.line   = i
                est-prep.s-num  = 1
                est-prep.b-num  = 1
                est-prep.qty    = 1           
                est-prep.code   = prep.code
                est-prep.dscr   = prep.dscr
                est-prep.cost   = prep.cost
                est-prep.ml     = prep.ml
                est-prep.simon  = prep.simon
                est-prep.mkup   = prep.mkup
                est-prep.amtz   = prep.amtz
                est-prep.mat-type = prep.mat-type
                .
                i = i + 1.
  end.  
END PROCEDURE.
/**************************************************************************************************************************************************/

PROCEDURE send-email-proc:
   DEF VAR ls-to-list AS cha NO-UNDO.
   DEF VAR lv-mailto AS cha NO-UNDO.
   DEF VAR lv-mailsubject AS cha NO-UNDO.
   DEF VAR lv-mailbody AS cha NO-UNDO.
   DEF VAR retcode AS INT NO-UNDO.
   DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
   v-prgmname = "R-QuoPrt.".

FOR EACH tt-rfqmail,
       FIRST cust WHERE
       cust.company = prmComp AND
       cust.cust-no = tt-rfqmail.cust-no 
       NO-LOCK
       BREAK BY tt-rfqmail.rfqnum:

    FIND FIRST rfqitem NO-LOCK NO-ERROR.
    for each rfqitem WHERE rfqitem.rfq-no = tt-rfqmail.rfqnum AND index(ls-seq-list,string(rfqitem.seq)) > 0 NO-LOCK :  /* of rfq*/
    
    IF FIRST-OF(tt-rfqmail.rfqnum) THEN
          vBody = "RFQ Number " +  STRING(tt-rfqmail.rfqnum)
                      + " has been Transfered TO Estimate "  +  string(rfqitem.est-no )   + CHR(10).
       vBody = vBody + " <br>"
                   + "Fg Item: " + STRING(tt-rfqmail.stock,"X(15)") + " &nbsp;&nbsp;"
                   + " Qty: " + STRING(tt-rfqmail.qty,"->>,>>>,>>9") + " &nbsp;&nbsp;" + "<br>"
                   + " Date: " + STRING(tt-rfqmail.rfqDate,"99/99/99") + " &nbsp;&nbsp;"
                   + " Part Num: "  + STRING(tt-rfqmail.part,"X(15)") + CHR(10).
      
       IF LAST-OF(tt-rfqmail.rfqnum) THEN do:
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}

                FIND FIRST users WHERE  users.USER_id = prmUser  NO-LOCK NO-ERROR.
                 IF AVAIL users THEN DO:
                         ASSIGN 
                             vInternalUser = STRING(users.internal-user) .
                 END.

           IF ls-to-list NE '' THEN DO:
               ASSIGN vMailto = ls-to-list.
             END. /* last-of(tt-rfqmail.cust-no) */
            IF vMailto = ""  THEN 
                ASSIGN
                    vMailto = vMailto + users.image_filename .
             IF vMailto <> users.image_filename AND vMailto <> "" THEN 
                ASSIGN
                    vMailto = vMailto + "," +  users.image_filename .
            
   END.
END.
END.
END PROCEDURE.
/*******************************************************************************************************************************/
IF prmAction = "Select" THEN DO:

FOR EACH rfqitem WHERE
         rfqitem.company = prmComp AND
         rfqitem.loc     = prmLoc AND
         rfqitem.rfq-no = PrmRfqNo AND
         /*rfqitem.est-no <> "" AND*/
         rfqitem.seq < 999  NO-LOCK:
    CREATE ttRfqEstimate.
        ASSIGN 
            ttRfqEstimate.RfqNo       = rfqitem.rfq-no
            ttRfqEstimate.RfqSeq      = rfqitem.seq
            ttRfqEstimate.RfqFormNo   = rfqitem.form-no
            ttRfqEstimate.RfqBlank    = rfqitem.blank-no
            ttRfqEstimate.RfqSeq      = rfqitem.seq
            ttRfqEstimate.RfqStock    = rfqitem.stock-no
            ttRfqEstimate.RfqName     = rfqitem.i-name
            ttRfqEstimate.RfqPart     = rfqitem.part-no
            ttRfqEstimate.RfqStyle    = rfqitem.style
            ttRfqEstimate.RfqProcat   = rfqitem.procat
            ttRfqEstimate.RfqEst      = rfqitem.est-no
            ttRfqEstimate.RfqERowid   = RECID(rfqitem)
             
           .        
END.

END.   /*IF prmAction = "Select" THEN DO:*/


