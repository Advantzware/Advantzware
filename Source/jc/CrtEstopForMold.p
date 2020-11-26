
/*------------------------------------------------------------------------
    File        : CrtEstopForMold.p
    Purpose     : create est-op for estimate 

    Syntax      :

    Description : Job Builder Procedure.

    Author(s)   : Sewa Singh
    Created     : Thur Nov 26 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO. 
DEFINE INPUT PARAMETER ipcMachine AS CHARACTER NO-UNDO.

{est/ttInputEst.i} 

DEFINE TEMP-TABLE ttRouting LIKE est-op
        FIELD isOk AS LOGICAL . 

DEFINE VARIABLE dOpWaste AS DECIMAL NO-UNDO.        
DEFINE VARIABLE dOpSpoil AS DECIMAL NO-UNDO.
DEFINE VARIABLE dOpMr    AS DECIMAL NO-UNDO.
DEFINE VARIABLE dOpSpeed AS DECIMAL NO-UNDO.
DEFINE BUFFER bf-est-op FOR est-op.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST eb NO-LOCK
    WHERE ROWID(eb) EQ ipriRowid NO-ERROR.
FIND FIRST est NO-LOCK
    WHERE est.company EQ eb.company
    AND est.est-no EQ eb.est-no NO-ERROR .
     
RUN pGetEstRouting (
    INPUT  eb.company,
     OUTPUT dOpWaste,
     OUTPUT dOpSpoil,
     OUTPUT dOpMr,
     OUTPUT dOpSpeed).
         
 FOR EACH ttRouting 
     WHERE ttRouting.isok :
     
  FIND FIRST mach NO-LOCK
         WHERE mach.company EQ est.company
         AND mach.m-code EQ ipcMachine NO-ERROR .   
    
  CREATE bf-est-op.
  BUFFER-COPY ttRouting EXCEPT company rec_key e-num est-no LINE qty s-num  b-num m-code m-dscr TO  bf-est-op .
  ASSIGN 
    bf-est-op.company   = est.company
    bf-est-op.e-num     = est.e-num
    bf-est-op.est-no    = est.est-no
    bf-est-op.LINE      = 1
    bf-est-op.qty       = est.est-qty[1]
    bf-est-op.s-num     = 1
    bf-est-op.b-num     = 0
    bf-est-op.m-code    = mach.m-code
    bf-est-op.m-dscr    = mach.m-dscr
    bf-est-op.op-waste  = dOpWaste
    bf-est-op.op-spoil  = dOpSpoil
    bf-est-op.op-mr     = dOpMr
    bf-est-op.op-speed  = dOpSpeed
    .     
        
 END.

/* **********************  Internal Procedures  *********************** */
PROCEDURE pGetEstRouting PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opdWaste AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSpoil AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMRHours AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSpeed AS DECIMAL NO-UNDO.
    
   FOR EACH ttInputEst NO-LOCK:
       IF ttInputEst.cFgEstNo NE "" THEN 
       DO:
          FOR EACH est-op WHERE est-op.company EQ ipcCompany
              AND est-op.est-no EQ ttInputEst.cFgEstNo
              AND est-op.line LT 500  NO-LOCK:
                 
              CREATE ttRouting.
              BUFFER-COPY est-op TO ttRouting.
          END.    
       END.        
   END.
   RELEASE ttRouting .
   FOR EACH ttRouting BREAK 
                      BY ttRouting.op-waste DESC:
      ttRouting.isOk = YES.
      opdWaste = ttRouting.op-waste .
      LEAVE.
   END.
   FOR EACH ttRouting BREAK 
                      BY ttRouting.op-spoil DESC:     
      opdSpoil = ttRouting.op-spoil .
      LEAVE.
   END.
   FOR EACH ttRouting BREAK 
                      BY ttRouting.op-mr DESC:     
      opdMRHours = ttRouting.op-mr .
      LEAVE.
   END.
   FOR EACH ttRouting BREAK 
                      BY ttRouting.op-speed:      
      opdSpeed = ttRouting.op-speed .
      LEAVE.
   END.

END PROCEDURE.

