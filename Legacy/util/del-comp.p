DEF VAR iCnt AS INT NO-UNDO.
DEF VAR jCnt AS INT NO-UNDO.
DEF VAR llCancel AS LOG NO-UNDO.

SUBSCRIBE TO "CancelIt" ANYWHERE.



DEF STREAM sSave.
DEF STREAM sNotes.


PROCEDURE Delete-Table:

DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcSaveDir AS CHAR NO-UNDO.


OUTPUT STREAM sSave TO VALUE(ipcSaveDir + "\" + ipcTable + ".d") APPEND.
OUTPUT STREAM sNotes TO VALUE(ipcSaveDir + "\" + "NOTES" + ".d") APPEND.

CASE ipcTable:

    /* Generated Code Section */
    
        WHEN "ar-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable ar-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "acctcost" THEN DO: 
          &SCOPED-DEFINE deltable acctcost 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     

         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-buy" THEN DO: 
          &SCOPED-DEFINE deltable ap-buy 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-chk" THEN DO: 
          &SCOPED-DEFINE deltable ap-chk 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable ap-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-dis" THEN DO: 
          &SCOPED-DEFINE deltable ap-dis 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-disl" THEN DO: 
          &SCOPED-DEFINE deltable ap-disl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-inv" THEN DO: 
          &SCOPED-DEFINE deltable ap-inv 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-invl" THEN DO: 
          &SCOPED-DEFINE deltable ap-invl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-invlr" THEN DO: 
          &SCOPED-DEFINE deltable ap-invlr 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-ledger" THEN DO: 
          &SCOPED-DEFINE deltable ap-ledger 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
   
        /* Custom Code */
        
        WHEN "ap-payl" THEN DO: 
          &SCOPED-DEFINE deltable ap-payl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 

          FOR EACH ap-pay
              WHERE ap-pay.company EQ ipcCompany 
              NO-lock,
              each ap-payl
                WHERE ap-payl.c-no = ap-pay.c-no
                use-index c-no EXCLUSIVE-LOCK:

              IF llCancel THEN 
                  LEAVE. 

              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 

              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 

              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 

        END. 



        WHEN "ap-pay" THEN DO: 
          &SCOPED-DEFINE deltable ap-pay 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ap-sel" THEN DO: 
          &SCOPED-DEFINE deltable ap-sel 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "aphist" THEN DO: 
          &SCOPED-DEFINE deltable aphist 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ar-invm" THEN DO: 
          &SCOPED-DEFINE deltable ar-invm 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ar-cash" THEN DO: 
          &SCOPED-DEFINE deltable ar-cash 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ar-cashl" THEN DO: 
          &SCOPED-DEFINE deltable ar-cashl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ar-inv" THEN DO: 
          &SCOPED-DEFINE deltable ar-inv 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ar-invl" THEN DO: 
          &SCOPED-DEFINE deltable ar-invl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ar-ledger" THEN DO: 
          &SCOPED-DEFINE deltable ar-ledger 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ar-mcash" THEN DO: 
          &SCOPED-DEFINE deltable ar-mcash 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
        
        WHEN "asinotes" THEN DO: 
          &SCOPED-DEFINE deltable asinotes 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "attach" THEN DO: 
          &SCOPED-DEFINE deltable attach 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "bank" THEN DO: 
          &SCOPED-DEFINE deltable bank 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
    
        WHEN "box-design-line" THEN DO: 
          &SCOPED-DEFINE deltable box-design-line 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
      WHEN "box-design-hdr" THEN DO: 
      &SCOPED-DEFINE deltable box-design-hdr 
      DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
      IF llCancel THEN 
          RETURN. 
      PUBLISH "NUMDEL" (ipcTable, jCnt). 

      FOR EACH {&deltable}  
          WHERE {&deltable}.company EQ ipcCompany 
          EXCLUSIVE-LOCK: 

          IF llCancel THEN 
              LEAVE. 

          FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
              EXCLUSIVE-LOCK: 
              EXPORT STREAM sNotes notes. 
              DELETE notes. 
          END. 

          EXPORT STREAM sSave {&deltable}. 
          DELETE {&deltable}. 

          iCnt = iCnt + 1. 
          IF iCnt GT 999 THEN DO:  
              iCnt = 0. PROCESS EVENTS. 
              jCnt = jCnt + 1000. 
              PUBLISH "NUMDEL" (ipcTable, jCnt). 
          END. 
      END. 

    END. 

        WHEN "buyer" THEN DO: 
          &SCOPED-DEFINE deltable buyer 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "company" THEN DO: 
          &SCOPED-DEFINE deltable company 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "carr-mtx" THEN DO: 
          &SCOPED-DEFINE deltable carr-mtx 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ce-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable ce-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "carrier" THEN DO: 
          &SCOPED-DEFINE deltable carrier 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
        
        WHEN "credit-hold-type" THEN DO: 
          &SCOPED-DEFINE deltable credit-hold-type 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "crew" THEN DO: 
          &SCOPED-DEFINE deltable crew 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "currency" THEN DO: 
          &SCOPED-DEFINE deltable currency 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "cust-markup" THEN DO: 
          &SCOPED-DEFINE deltable cust-markup 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "cust-part" THEN DO: 
          &SCOPED-DEFINE deltable cust-part 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "cust-prod-sales" THEN DO: 
          &SCOPED-DEFINE deltable cust-prod-sales 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "custype" THEN DO: 
          &SCOPED-DEFINE deltable custype 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "cust" THEN DO: 
          &SCOPED-DEFINE deltable cust 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "db-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable db-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "dept" THEN DO: 
          &SCOPED-DEFINE deltable dept 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "down-type" THEN DO: 
          &SCOPED-DEFINE deltable down-type 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "e-item" THEN DO: 
          &SCOPED-DEFINE deltable e-item 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "e-item-cust" THEN DO: 
          &SCOPED-DEFINE deltable e-item-cust 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "e-item-vend" THEN DO: 
          &SCOPED-DEFINE deltable e-item-vend 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "e-itemfg" THEN DO: 
          &SCOPED-DEFINE deltable e-itemfg 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "e-itemfg-vend" THEN DO: 
          &SCOPED-DEFINE deltable e-itemfg-vend 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "eb" THEN DO: 
          &SCOPED-DEFINE deltable eb 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ed" THEN DO: 
          &SCOPED-DEFINE deltable ed 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "EDAPCheck" THEN DO: 
          &SCOPED-DEFINE deltable EDAPCheck 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "EDCo" THEN DO: 
          &SCOPED-DEFINE deltable EDCo 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "EDICXref" THEN DO: 
          &SCOPED-DEFINE deltable EDICXref 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "EDIVAddon" THEN DO: 
          &SCOPED-DEFINE deltable EDIVAddon 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "EDIVLine" THEN DO: 
          &SCOPED-DEFINE deltable EDIVLine 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "EDIVTran" THEN DO: 
          &SCOPED-DEFINE deltable EDIVTran 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ef" THEN DO: 
          &SCOPED-DEFINE deltable ef 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ef-nsh" THEN DO: 
          &SCOPED-DEFINE deltable ef-nsh 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "emp" THEN DO: 
          &SCOPED-DEFINE deltable emp 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "er" THEN DO: 
          &SCOPED-DEFINE deltable er 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "est" THEN DO: 
          &SCOPED-DEFINE deltable est 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "est-qty" THEN DO: 
          &SCOPED-DEFINE deltable est-qty 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "est-flm" THEN DO: 
          &SCOPED-DEFINE deltable est-flm 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "est-inst" THEN DO: 
          &SCOPED-DEFINE deltable est-inst 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "est-op" THEN DO: 
          &SCOPED-DEFINE deltable est-op 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable} NO-ERROR. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "est-prep" THEN DO: 
          &SCOPED-DEFINE deltable est-prep 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "est-summ" THEN DO: 
          &SCOPED-DEFINE deltable est-summ 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "est-pf" THEN DO: 
          &SCOPED-DEFINE deltable est-pf 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "itemfg-bom" THEN DO: 
          &SCOPED-DEFINE deltable itemfg-bom 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-act" THEN DO: 
          &SCOPED-DEFINE deltable fg-act 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-bin" THEN DO: 
          &SCOPED-DEFINE deltable fg-bin 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable fg-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-hist" THEN DO: 
          &SCOPED-DEFINE deltable fg-hist 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-rcpth" THEN DO: 
          &SCOPED-DEFINE deltable fg-rcpth 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-rcpts" THEN DO: 
          &SCOPED-DEFINE deltable fg-rcpts 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-rctd" THEN DO: 
          &SCOPED-DEFINE deltable fg-rctd 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-rdtl" THEN DO: 
          &SCOPED-DEFINE deltable fg-rdtl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-rdtlh" THEN DO: 
          &SCOPED-DEFINE deltable fg-rdtlh 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fg-set" THEN DO: 
          &SCOPED-DEFINE deltable fg-set 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "fgcat" THEN DO: 
          &SCOPED-DEFINE deltable fgcat 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "flute" THEN DO: 
          &SCOPED-DEFINE deltable flute 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "account" THEN DO: 
          &SCOPED-DEFINE deltable account 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "gl-rptd" THEN DO: 
          &SCOPED-DEFINE deltable gl-rptd 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "gl-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable gl-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "gl-freq" THEN DO: 
          &SCOPED-DEFINE deltable gl-freq 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "gl-jrn" THEN DO: 
          &SCOPED-DEFINE deltable gl-jrn 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "gl-rpt" THEN DO: 
          &SCOPED-DEFINE deltable gl-rpt 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "glhist" THEN DO: 
          &SCOPED-DEFINE deltable glhist 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "gltrans" THEN DO: 
          &SCOPED-DEFINE deltable gltrans 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "inv-head" THEN DO: 
          &SCOPED-DEFINE deltable inv-head 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "inv-line" THEN DO: 
          &SCOPED-DEFINE deltable inv-line 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "inv-misc" THEN DO: 
          &SCOPED-DEFINE deltable inv-misc 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "item-bom" THEN DO: 
          &SCOPED-DEFINE deltable item-bom 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "item-comm" THEN DO: 
          &SCOPED-DEFINE deltable item-comm 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "item-spec" THEN DO: 
          &SCOPED-DEFINE deltable item-spec 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "itemfg" THEN DO: 
          &SCOPED-DEFINE deltable itemfg 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "itemfg-ink" THEN DO: 
          &SCOPED-DEFINE deltable itemfg-ink 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "itemfg-loc" THEN DO: 
          &SCOPED-DEFINE deltable itemfg-loc 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "itemfgdtl" THEN DO: 
          &SCOPED-DEFINE deltable itemfgdtl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "jc-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable jc-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job" THEN DO: 
          &SCOPED-DEFINE deltable job 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          /* Custom Code */
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              {util/dljobkey.i}    

              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job-sch" THEN DO: 
          &SCOPED-DEFINE deltable job-sch 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job-all" THEN DO: 
          &SCOPED-DEFINE deltable job-all 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job-brd" THEN DO: 
          &SCOPED-DEFINE deltable job-brd 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job-hdr" THEN DO: 
          &SCOPED-DEFINE deltable job-hdr 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job-mat" THEN DO: 
          &SCOPED-DEFINE deltable job-mat 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job-mch" THEN DO: 
          &SCOPED-DEFINE deltable job-mch 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job-prep" THEN DO: 
          &SCOPED-DEFINE deltable job-prep 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "job-set" THEN DO: 
          &SCOPED-DEFINE deltable job-set 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "loadtag" THEN DO: 
          &SCOPED-DEFINE deltable loadtag 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mach" THEN DO: 
          &SCOPED-DEFINE deltable mach 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mach-adder" THEN DO: 
          &SCOPED-DEFINE deltable mach-adder 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mach-attach-pat" THEN DO: 
          &SCOPED-DEFINE deltable mach-attach-pat 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mach-calendar" THEN DO: 
          &SCOPED-DEFINE deltable mach-calendar 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mach-panel" THEN DO: 
          &SCOPED-DEFINE deltable mach-panel 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mach-part" THEN DO: 
          &SCOPED-DEFINE deltable mach-part 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mach-attach" THEN DO: 
          &SCOPED-DEFINE deltable mach-attach 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mmtx" THEN DO: 
          &SCOPED-DEFINE deltable mmtx 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mat-act" THEN DO: 
          &SCOPED-DEFINE deltable mat-act 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "costtype" THEN DO: 
          &SCOPED-DEFINE deltable costtype 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mat" THEN DO: 
          &SCOPED-DEFINE deltable mat 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mch-act" THEN DO: 
          &SCOPED-DEFINE deltable mch-act 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "misc-act" THEN DO: 
          &SCOPED-DEFINE deltable misc-act 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mmtx2" THEN DO: 
          &SCOPED-DEFINE deltable mmtx2 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mmty" THEN DO: 
          &SCOPED-DEFINE deltable mmty 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "mstd" THEN DO: 
          &SCOPED-DEFINE deltable mstd 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-bolh" THEN DO: 
          &SCOPED-DEFINE deltable oe-bolh 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-boll" THEN DO: 
          &SCOPED-DEFINE deltable oe-boll 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-boll-qty" THEN DO: 
          &SCOPED-DEFINE deltable oe-boll-qty 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable oe-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-ord" THEN DO: 
          &SCOPED-DEFINE deltable oe-ord 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-ordl" THEN DO: 
          &SCOPED-DEFINE deltable oe-ordl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-ordm" THEN DO: 
          &SCOPED-DEFINE deltable oe-ordm 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-prmtx" THEN DO: 
          &SCOPED-DEFINE deltable oe-prmtx 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-rel" THEN DO: 
          &SCOPED-DEFINE deltable oe-rel 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-relh" THEN DO: 
          &SCOPED-DEFINE deltable oe-relh 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-rell" THEN DO: 
          &SCOPED-DEFINE deltable oe-rell 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-reth" THEN DO: 
          &SCOPED-DEFINE deltable oe-reth 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-retl" THEN DO: 
          &SCOPED-DEFINE deltable oe-retl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "oe-ship" THEN DO: 
          &SCOPED-DEFINE deltable oe-ship 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "pc-misc" THEN DO: 
          &SCOPED-DEFINE deltable pc-misc 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "pc-prdd" THEN DO: 
          &SCOPED-DEFINE deltable pc-prdd 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "pc-prdd-wip" THEN DO: 
          &SCOPED-DEFINE deltable pc-prdd-wip 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "pc-prdh" THEN DO: 
          &SCOPED-DEFINE deltable pc-prdh 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "period" THEN DO: 
          &SCOPED-DEFINE deltable period 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "permg" THEN DO: 
          &SCOPED-DEFINE deltable permg 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "loc" THEN DO: 
          &SCOPED-DEFINE deltable loc 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "po-all" THEN DO: 
          &SCOPED-DEFINE deltable po-all 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "po-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable po-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "po-ord" THEN DO: 
          &SCOPED-DEFINE deltable po-ord 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "po-ordl" THEN DO: 
          &SCOPED-DEFINE deltable po-ordl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END.

              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "po-ordl-add" THEN DO: 
          &SCOPED-DEFINE deltable po-ordl-add 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "po-rcpts" THEN DO: 
          &SCOPED-DEFINE deltable po-rcpts 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "prep" THEN DO: 
          &SCOPED-DEFINE deltable prep 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "matprep" THEN DO: 
          &SCOPED-DEFINE deltable matprep 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "printer" THEN DO: 
          &SCOPED-DEFINE deltable printer 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "probe" THEN DO: 
          &SCOPED-DEFINE deltable probe 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "probeit" THEN DO: 
          &SCOPED-DEFINE deltable probeit 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "probeit-price" THEN DO: 
          &SCOPED-DEFINE deltable probeit-price 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "procat" THEN DO: 
          &SCOPED-DEFINE deltable procat 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "prodl" THEN DO: 
          &SCOPED-DEFINE deltable prodl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "prod" THEN DO: 
          &SCOPED-DEFINE deltable prod 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "quote" THEN DO: 
          &SCOPED-DEFINE deltable quote 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "quotehd" THEN DO: 
          &SCOPED-DEFINE deltable quotehd 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "quotechg" THEN DO: 
          &SCOPED-DEFINE deltable quotechg 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "quoteitm" THEN DO: 
          &SCOPED-DEFINE deltable quoteitm 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "quoteqty" THEN DO: 
          &SCOPED-DEFINE deltable quoteqty 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "quoteit" THEN DO: 
          &SCOPED-DEFINE deltable quoteit 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "item" THEN DO: 
          &SCOPED-DEFINE deltable item 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "reftable" THEN DO: 
          &SCOPED-DEFINE deltable reftable 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rfidtag" THEN DO: 
          &SCOPED-DEFINE deltable rfidtag 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-bin" THEN DO: 
          &SCOPED-DEFINE deltable rm-bin 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable rm-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-rcpt" THEN DO: 
          &SCOPED-DEFINE deltable rm-rcpt 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-rcpth" THEN DO: 
          &SCOPED-DEFINE deltable rm-rcpth 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-rctd" THEN DO: 
          &SCOPED-DEFINE deltable rm-rctd 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-rcth" THEN DO: 
          &SCOPED-DEFINE deltable rm-rcth 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-rdtl" THEN DO: 
          &SCOPED-DEFINE deltable rm-rdtl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-rdtlh" THEN DO: 
          &SCOPED-DEFINE deltable rm-rdtlh 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rm-receipts" THEN DO: 
          &SCOPED-DEFINE deltable rm-receipts 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "routing" THEN DO: 
          &SCOPED-DEFINE deltable routing 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "routing-mtx" THEN DO: 
          &SCOPED-DEFINE deltable routing-mtx 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "sman" THEN DO: 
          &SCOPED-DEFINE deltable sman 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "smanbugt" THEN DO: 
          &SCOPED-DEFINE deltable smanbugt 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "smanbcat" THEN DO: 
          &SCOPED-DEFINE deltable smanbcat 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "smanbcst" THEN DO: 
          &SCOPED-DEFINE deltable smanbcst 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "smanmtrx" THEN DO: 
          &SCOPED-DEFINE deltable smanmtrx 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "sman-mtx" THEN DO: 
          &SCOPED-DEFINE deltable sman-mtx 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "scores" THEN DO: 
          &SCOPED-DEFINE deltable scores 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "shift" THEN DO: 
          &SCOPED-DEFINE deltable shift 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "shipto" THEN DO: 
          &SCOPED-DEFINE deltable shipto 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "soldto" THEN DO: 
          &SCOPED-DEFINE deltable soldto 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ssrelbol" THEN DO: 
          &SCOPED-DEFINE deltable ssrelbol 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "stack-flute" THEN DO: 
          &SCOPED-DEFINE deltable stack-flute 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "stack-size" THEN DO: 
          &SCOPED-DEFINE deltable stack-size 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "std-code" THEN DO: 
          &SCOPED-DEFINE deltable std-code 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "style" THEN DO: 
          &SCOPED-DEFINE deltable style 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "style-score" THEN DO: 
          &SCOPED-DEFINE deltable style-score 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "surcharge" THEN DO: 
          &SCOPED-DEFINE deltable surcharge 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "sys-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable sys-ctrl 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "sys-ctrl-shipto" THEN DO: 
          &SCOPED-DEFINE deltable sys-ctrl-shipto 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "stax-group" THEN DO: 
          &SCOPED-DEFINE deltable stax-group 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "stax" THEN DO: 
          &SCOPED-DEFINE deltable stax 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "terms" THEN DO: 
          &SCOPED-DEFINE deltable terms 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "terr" THEN DO: 
          &SCOPED-DEFINE deltable terr 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "test-red" THEN DO: 
          &SCOPED-DEFINE deltable test-red 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "truck" THEN DO: 
          &SCOPED-DEFINE deltable truck 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "truck-run-print" THEN DO: 
          &SCOPED-DEFINE deltable truck-run-print 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "usercomp" THEN DO: 
          &SCOPED-DEFINE deltable usercomp 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "user-batch" THEN DO: 
          &SCOPED-DEFINE deltable user-batch 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
              
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "user-print" THEN DO: 
          &SCOPED-DEFINE deltable user-print 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "usr" THEN DO: 
          &SCOPED-DEFINE deltable usr 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "usr-grp" THEN DO: 
          &SCOPED-DEFINE deltable usr-grp 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "usrx" THEN DO: 
          &SCOPED-DEFINE deltable usrx 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "vend" THEN DO: 
          &SCOPED-DEFINE deltable vend 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "ventype" THEN DO: 
          &SCOPED-DEFINE deltable ventype 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "waste-type" THEN DO: 
          &SCOPED-DEFINE deltable waste-type 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "wip-bin" THEN DO: 
          &SCOPED-DEFINE deltable wip-bin 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "wiptag" THEN DO: 
          &SCOPED-DEFINE deltable wiptag 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "wiptag-mch" THEN DO: 
          &SCOPED-DEFINE deltable wiptag-mch 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "usercust" THEN DO: 
          &SCOPED-DEFINE deltable usercust 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "usersman" THEN DO: 
          &SCOPED-DEFINE deltable usersman 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "uservend" THEN DO: 
          &SCOPED-DEFINE deltable uservend 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "asi" THEN DO: 
          &SCOPED-DEFINE deltable asi 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "cmpltjob" THEN DO: 
          &SCOPED-DEFINE deltable cmpltjob 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "contact" THEN DO: 
          &SCOPED-DEFINE deltable contact 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "empmach" THEN DO: 
          &SCOPED-DEFINE deltable empmach 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "emplogin" THEN DO: 
          &SCOPED-DEFINE deltable emplogin 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "rate" THEN DO: 
          &SCOPED-DEFINE deltable rate 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "employee" THEN DO: 
          &SCOPED-DEFINE deltable employee 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "machchrg" THEN DO: 
          &SCOPED-DEFINE deltable machchrg 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "machshft" THEN DO: 
          &SCOPED-DEFINE deltable machshft 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "machseq" THEN DO: 
          &SCOPED-DEFINE deltable machseq 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "machtran" THEN DO: 
          &SCOPED-DEFINE deltable machtran 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "shifts" THEN DO: 
          &SCOPED-DEFINE deltable shifts 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "shift_break" THEN DO: 
          &SCOPED-DEFINE deltable shift_break 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "vend-code-cust-xref" THEN DO: 
          &SCOPED-DEFINE deltable vend-code-cust-xref 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "vend-plant" THEN DO: 
          &SCOPED-DEFINE deltable vend-plant 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "vend-whse-item" THEN DO: 
          &SCOPED-DEFINE deltable vend-whse-item 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "vend-whse-trans" THEN DO: 
          &SCOPED-DEFINE deltable vend-whse-trans 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 
    
        WHEN "vend-whse-trans-hist" THEN DO: 
          &SCOPED-DEFINE deltable vend-whse-trans-hist 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 
     
          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 
     
              IF llCancel THEN 
                  LEAVE. 
     
              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 
         
              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 
         
              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 
           
        END. 

        WHEN "bolh" THEN DO: 
          &SCOPED-DEFINE deltable bolh 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 

          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 

              IF llCancel THEN 
                  LEAVE. 

              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 

              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 

              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 

        END. 


        WHEN "boll" THEN DO: 
          &SCOPED-DEFINE deltable boll
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 

          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 

              IF llCancel THEN 
                  LEAVE. 

              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 

              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 

              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 

        END. 

        WHEN "cust-itm" THEN DO: 
          &SCOPED-DEFINE deltable cust-itm
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 

          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 

              IF llCancel THEN 
                  LEAVE. 

              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 

              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 

              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 

        END. 

        WHEN "rfq" THEN DO: 
          &SCOPED-DEFINE deltable rfq 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 

          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 

              IF llCancel THEN 
                  LEAVE. 

              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 

              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 

              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 

        END. 

        WHEN "rfq-ctrl" THEN DO: 
          &SCOPED-DEFINE deltable rfq-ctrl
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 

          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 

              IF llCancel THEN 
                  LEAVE. 

              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 

              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 

              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 

        END. 

        WHEN "rfqitem" THEN DO: 
          &SCOPED-DEFINE deltable rfqitem 
          DISABLE TRIGGERS FOR LOAD OF {&deltable}. 
          IF llCancel THEN 
              RETURN. 
          PUBLISH "NUMDEL" (ipcTable, jCnt). 

          FOR EACH {&deltable}  
              WHERE {&deltable}.company EQ ipcCompany 
              EXCLUSIVE-LOCK: 

              IF llCancel THEN 
                  LEAVE. 

              FOR EACH notes WHERE notes.rec_key EQ {&deltable}.rec_key 
                  EXCLUSIVE-LOCK: 
                  EXPORT STREAM sNotes notes. 
                  DELETE notes. 
              END. 

              EXPORT STREAM sSave {&deltable}. 
              DELETE {&deltable}. 

              iCnt = iCnt + 1. 
              IF iCnt GT 999 THEN DO:  
                  iCnt = 0. PROCESS EVENTS. 
                  jCnt = jCnt + 1000. 
                  PUBLISH "NUMDEL" (ipcTable, jCnt). 
              END. 
          END. 

        END. 

END CASE.

OUTPUT STREAM sSave CLOSE.
OUTPUT STREAM sNotes CLOSE.

END PROCEDURE.

PROCEDURE cancelIt:
    /* respond to cancel event */
    llCancel = TRUE.
END.
