/*------------------------------------------------------------------------
    File        : system\quickAttachProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Sachin Chahal
    Created     : Wed. Jun 29 2022
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ipcContext       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKey        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcContextValue  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcDirectory     AS CHARACTER NO-UNDO.

DEFINE VARIABLE ll-ok   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstimateNo AS CHARACTER NO-UNDO.

system-dialog get-file cFileName 
             title "Select Image File to insert"
             filters "All Files    (*.*) " "*.*",
                     "JPG Files    (*.jpg)" "*.jpg",
                     "Bitmap files (*.bmp)" "*.bmp",
                     "JPEG Files   (*.jpeg)" "*.jpeg",
                     "Rich Test Files (*.rtf)" "*.rtf",
                     "MS Word Files  (*.doc)" "*.doc",
                     "MS Word Template Files (*.dot)" "*.dot",
                     "MS Word 2007 Files  (*.docx)" "*.docx",
                     "MS Word 2007 Template Files (*.dotx)" "*.dotx",
                     "MS Excel Files  (*.xls)" "*.xls",
                     "MS Excel 2007 Files  (*.xlsx)" "*.xlsx",
                     "Adobe PDF Files (*.pdf)" "*.pdf"                         
             initial-dir ipcDirectory
             MUST-EXIST
             USE-FILENAME
             UPDATE ll-ok.

IF ll-ok THEN DO:
    IF ipcContext EQ "est" THEN
    DO:
       RUN pIsContextEstimate ( OUTPUT cEstimateNo, OUTPUT cItemNo).
       RUN pAddRecord (cEstimateNo, entry(1,cItemNo)). 
    END.
    ELSE
    DO:
       RUN pAddRecord (ipcContextValue, ""). 
    END.
END.


PROCEDURE pIsContextEstimate :
    
    DEFINE OUTPUT PARAMETER opcEstimateNo    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemNo        AS CHARACTER NO-UNDO.
    
    DEF VAR v-rec-key-list AS CHAR NO-UNDO.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    ASSIGN
        opcEstimateNo = ""
        opcItemNo     = ""
        .
    
    FIND FIRST est WHERE est.rec_key = ipcRecKey NO-LOCK NO-ERROR.
    FIND FIRST job WHERE job.rec_key = ipcRecKey NO-LOCK NO-ERROR.

    IF AVAIL est THEN DO:
       opcEstimateNo = est.est-no.
       FOR EACH eb fields(stock-no) WHERE
           eb.company = est.company AND
           eb.est-no = est.est-no NO-LOCK :
           IF eb.stock-no <> "" THEN opcItemNo = opcItemNo + eb.stock-no + ",". 
       END.

    END.
    ELSE IF AVAIL job THEN DO:
        opcEstimateNo = job.est-no.
       FOR EACH job-hdr fields(i-no) WHERE
           job-hdr.company EQ job.company AND
           job-hdr.job EQ job.job AND
           job-hdr.job-no EQ job.job-no NO-LOCK :
           IF job-hdr.i-no <> "" THEN opcItemNo = opcItemNo + job-hdr.i-no + ",". 
       END.
    END.
    ELSE DO:
        FIND FIRST oe-ord WHERE
             oe-ord.rec_key EQ ipcRecKey
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ord THEN
        DO:
            FIND FIRST est WHERE
                 est.company EQ oe-ord.company AND
                 est.est-no EQ oe-ord.est-no
                 NO-LOCK NO-ERROR.

            IF AVAIL est THEN DO:
               opcEstimateNo = est.est-no.
               FOR EACH eb FIELDS(stock-no) WHERE
                   eb.company = est.company AND
                   eb.est-no = est.est-no AND
                   eb.stock-no NE "" NO-LOCK :
                   opcItemNo = opcItemNo + eb.stock-no + ",". 
               END.
            END.

        END.
        ELSE
        DO:
          FIND FIRST itemfg WHERE itemfg.rec_key = ipcRecKey NO-LOCK NO-ERROR.    
          IF AVAIL itemfg THEN DO:          
              ASSIGN
                 opcItemNo = itemfg.i-no
                 opcEstimateNo = itemfg.est-no.
          END.
          ELSE IF CONNECTED("emptrack") THEN
              RUN browsers/addonatt.p(INPUT ipcRecKey,
                                         OUTPUT opcEstimateNo,
                                         OUTPUT opcItemNo,
                                         OUTPUT v-rec-key-list).             

          IF opcItemNo = "" THEN DO:

              FIND FIRST oe-ordl WHERE
                 oe-ordl.rec_key EQ ipcRecKey
                 NO-LOCK NO-ERROR.

              IF AVAIL oe-ordl THEN DO:

                 FOR EACH bf-oe-ordl WHERE
                     bf-oe-ordl.company EQ oe-ordl.company AND
                     bf-oe-ordl.ord-no EQ oe-ordl.ord-no
                     NO-LOCK:

                     opcItemNo = opcItemNo + bf-oe-ordl.i-no + ",".
                 END.

              END.
          END. /* opcItemNo = "" */
        END. /* not avail oe-ord */
    END. /* not avail est */
    
END PROCEDURE.

PROCEDURE pAddRecord:

DEFINE BUFFER bf-attach FOR ATTACH.

DEFINE INPUT PARAMETER ipcEstimateNo    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemNo        AS CHARACTER NO-UNDO.

    CREATE bf-attach. 
        ASSIGN 
            bf-attach.rec_key          = ipcRecKey
            bf-attach.company          = ipcCompany
            bf-attach.attach-file      = cFileName
            bf-attach.run-application  = "Windows Default"
            bf-attach.run-program      = ""
            bf-attach.creat-date       = TODAY
            bf-attach.est-no           = ipcEstimateNo
            bf-attach.i-no             = ipcItemNo
            .
    
    RELEASE bf-attach.

END PROCEDURE.
