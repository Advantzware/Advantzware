
/*------------------------------------------------------------------------
    File        : CopySheet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 05/24/2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est\ttEstCopySheet.i}
DEFINE VARIABLE glopError AS LOGICAL NO-UNDO.
DEFINE VARIABLE gcEstimateNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE glCEStyleFFound AS LOGICAL NO-UNDO.
    
DEFINE BUFFER buf-eb  FOR eb.
DEFINE BUFFER buf2-eb FOR eb.
DEFINE BUFFER buf-ef  FOR ef.
DEFINE BUFFER buf2-ef FOR ef.

DEFINE TEMP-TABLE ttBoxDesignHdr NO-UNDO LIKE box-design-hdr.
DEFINE TEMP-TABLE ttBoxDesignLine NO-UNDO LIKE box-design-line.

/* *************************** Parameter Definitions  ******************* */
DEFINE INPUT  PARAMETER ipriEb AS ROWID NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN pBuildTTBlanks.
RUN pProcessUI.

/* **********************  Internal Procedures  *********************** */
PROCEDURE pBuildTTBlanks PRIVATE:
     /*------------------------------------------------------------------------------
     Purpose: Build and intialize Temp-Tables
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttBlanksToCopy.
    EMPTY TEMP-TABLE ttBlanksToCopyInto.
    
    FIND FIRST buf-eb NO-LOCK 
        WHERE  ROWID (buf-eb) EQ ipriEb NO-ERROR.
        
    IF AVAILABLE buf-eb THEN 
    DO:
        CREATE ttBlanksToCopy.
        ASSIGN ttBlanksToCopy.riEb             = ipriEb
               ttBlanksToCopy.iFormNo          = buf-eb.form-no
               ttBlanksToCopy.iBlankNo         = buf-eb.blank-no
               ttBlanksToCopy.cPartNo          = buf-eb.part-no
               ttBlanksToCopy.cPartDescription = buf-eb.part-dscr1
               ttBlanksToCopy.lIsSelected      = YES
               gcEstimateNo                    = buf-eb.est-no.  
        
        FOR EACH  buf2-eb NO-LOCK
            WHERE buf2-eb.est-no EQ buf-eb.est-no
              AND ROWID(buf2-eb) NE ROWID (buf-eb)
              AND (buf2-eb.form-no NE 0 OR buf2-eb.blank-no NE 0):
                
            CREATE ttBlanksToCopy.
            ASSIGN ttBlanksToCopy.riEb             = ROWID(buf2-eb)
                   ttBlanksToCopy.iFormNo          = buf2-eb.form-no
                   ttBlanksToCopy.iBlankNo         = buf2-eb.blank-no
                   ttBlanksToCopy.cPartNo          = buf2-eb.part-no
                   ttBlanksToCopy.cPartDescription = buf2-eb.part-dscr1
                   ttBlanksToCopy.lIsSelected      = NO.  
                   
            CREATE ttBlanksToCopyInto.
            ASSIGN ttBlanksToCopyInto.riEb                 = ROWID(buf2-eb)
                   ttBlanksToCopyInto.iFormNo              = buf2-eb.form-no
                   ttBlanksToCopyInto.iBlankNo             = buf2-eb.blank-no
                   ttBlanksToCopyInto.cPartNo              = buf2-eb.part-no
                   ttBlanksToCopyInto.cPartDescription     = buf2-eb.part-dscr1
                   ttBlanksToCopyInto.lIsSelected          = NO
                   ttBlanksToCopyInto.lCopyCad             = NO 
                   ttBlanksToCopyInto.lCopyDie             = NO 
                   ttBlanksToCopyInto.lCopyOtherAttributes = NO 
                   ttBlanksToCopyInto.lDoReCalculation     = NO.
        END.                            
    END.  
    RELEASE buf-eb.
    RELEASE buf2-eb.       
    
END PROCEDURE. 

PROCEDURE pProcessUI PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Call UI to get the inputs from user
     Notes:
    ------------------------------------------------------------------------------*/      
    RUN est/d-CopySheet.w (INPUT-OUTPUT TABLE ttBlanksToCopy BY-REFERENCE, 
                           INPUT-OUTPUT TABLE ttBlanksToCopyInto BY-REFERENCE,
                           OUTPUT glopError).                   
    IF NOT glopError THEN 
        RUN pCopyBlanks(BUFFER ttBlanksToCopy, BUFFER ttBlanksToCopyInto).  
                   
               
END PROCEDURE.

PROCEDURE pCopyBlanks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Copy the requested attributes from source to target
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE PARAMETER BUFFER ipbf-ttBlanksToCopy FOR ttBlanksToCopy.
    DEFINE PARAMETER BUFFER ipbf-ttBlanksToCopyInto FOR ttBlanksToCopyInto. 
    
    DEFINE VARIABLE lStyleSame AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cBoxDesign AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBlankWidLenIndex AS INTEGER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    
    MESSAGE "Are you sure you want to Copy Blank Info? " VIEW-AS ALERT-BOX WARNING
        BUTTON YES-NO UPDATE lCopyBlankInfo AS LOG.        
    IF lCopyBlankInfo THEN 
    DO: 
        FIND FIRST ipbf-ttBlanksToCopy NO-LOCK 
            WHERE ipbf-ttBlanksToCopy.lisSelected = YES NO-ERROR.
        IF AVAILABLE ipbf-ttBlanksToCopy THEN
        DO:    
            FIND FIRST buf-eb NO-LOCK  
                WHERE  buf-eb.est-no EQ gcEstimateNo
                AND  buf-eb.form-no EQ ipbf-ttBlanksToCopy.iFormNo
                AND  buf-eb.blank-no EQ ipbf-ttBlanksToCopy.iBlankNo NO-ERROR.
          
            FIND FIRST buf-ef NO-LOCK
                WHERE buf-ef.est-no EQ gcEstimateNo
                AND buf-ef.form-no  EQ ipbf-ttBlanksToCopy.iFormNo NO-ERROR. 
        END. 
                 
        IF AVAILABLE buf-ef AND AVAILABLE buf-eb THEN 
        DO: 
            RUN sys/ref/nk1look.p (buf-eb.company, "CEStyleF", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
            IF lFound THEN glCEStyleFFound = cReturn EQ "YES". 
  
            FOR EACH  ipbf-ttBlanksToCopyInto NO-LOCK 
                WHERE ipbf-ttBlanksToCopyInto.lIsSelected = YES:
                
                FIND FIRST buf2-eb EXCLUSIVE-LOCK
                    WHERE  buf2-eb.est-no   EQ gcEstimateNo
                      AND  buf2-eb.form-no  EQ ipbf-ttBlanksToCopyInto.iFormNo
                      AND  buf2-eb.blank-no EQ ipbf-ttBlanksToCopyInto.iBlankNo NO-ERROR.
                      
                FIND FIRST buf2-ef EXCLUSIVE-LOCK
                    WHERE  buf2-ef.est-no   EQ gcEstimateNo
                      AND  buf2-ef.form-no  EQ ipbf-ttBlanksToCopyInto.iFormNo NO-ERROR.     
                      
                IF AVAILABLE buf2-ef AND AVAILABLE buf2-eb  THEN 
                DO:
                    IF ipbf-ttBlanksToCopyInto.lCopyDie EQ YES THEN
                        ASSIGN buf2-eb.die-no = buf-eb.die-no.
                
                    IF ipbf-ttBlanksToCopyInto.lCopyCad EQ YES THEN
                        ASSIGN buf2-eb.cad-no = buf-eb.cad-no.
                        
                    IF ipbf-ttBlanksToCopyInto.lCopyOtherAttributes EQ YES THEN
                    DO:
                        ASSIGN cBoxDesign        = "S"
                               lStyleSame        = buf2-eb.style EQ buf-eb.style
                               buf2-eb.spc-no    = buf-eb.spc-no
                               buf2-eb.upc-no    = buf-eb.upc-no
                               buf2-eb.style     = buf-eb.style
                               buf2-eb.len       = buf-eb.len
                               buf2-eb.wid       = buf-eb.wid
                               buf2-eb.dep       = buf-eb.dep
                               buf2-eb.adhesive  = buf-eb.adhesive
                               buf2-eb.dust      = buf-eb.dust
                               buf2-eb.fpanel    = buf-eb.fpanel
                               buf2-eb.lock      = buf-eb.lock
                               buf2-eb.gluelap   = buf-eb.gluelap
                               buf2-eb.k-len     = buf-eb.k-len
                               buf2-eb.k-wid     = buf-eb.k-wid
                               buf2-eb.tuck      = buf-eb.tuck
                               buf2-eb.lin-in    = buf-eb.lin-in
                               buf2-eb.t-wid     = buf-eb.t-wid
                               buf2-eb.t-len     = buf-eb.t-len
                               buf2-eb.t-sqin    = buf-eb.t-sqin
                               buf2-ef.cad-image = buf-ef.cad-image.  
                            
                        DO iBlankWidLenIndex = 1 TO 20:
                            ASSIGN buf2-eb.k-wid-array[iBlankWidLenIndex] = buf-eb.k-wid-array[iBlankWidLenIndex]
                                   buf2-eb.k-wid-scr-type[iBlankWidLenIndex] = buf-eb.k-wid-scr-type[iBlankWidLenIndex]
                                   buf2-eb.k-len-array[iBlankWidLenIndex] = buf-eb.k-len-array[iBlankWidLenIndex]
                                   buf2-eb.k-len-scr-type[iBlankWidLenIndex] = buf-eb.k-len-scr-type[iBlankWidLenIndex].  
                        END.
                        RUN est/u2kinc1.p (RECID(buf2-eb)).
                        RUN est/u2kinc2.p (RECID(buf2-eb)). 
                                                                   
                        IF  NOT lStyleSame AND glCEStyleFFound THEN  
                        DO:
                            IF ipbf-ttBlanksToCopyInto.lDoReCalculation EQ YES THEN 
                                ASSIGN cBoxDesign = "B".
                            ELSE 
                                ASSIGN cBoxDesign = "N".
                        END.
                        IF ipbf-ttBlanksToCopyInto.lDoReCalculation EQ YES THEN 
                            RUN pBuildBox (cBoxDesign, BUFFER buf2-eb).  
                    END. /* IF ipbf-ttBlanksToCopyInto.lCopyOtherAttributes EQ YES THEN */   
                                          
                END. /* IF AVAILABLE buf2-ef AND AVAILABLE buf2-eb  THEN */
                
            END. /* FOR EACH  ipbf-ttBlanksToCopyInto NO-LOCK */    
                      
        END. /* IF AVAILABLE buf-ef AND AVAILABLE buf-eb THEN */
        
        MESSAGE "Blank Information Copied."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
    END. /* IF lCopyBlankInfo THEN */ 
             
    RELEASE buf-eb.
    RELEASE buf2-eb.
    RELEASE buf-ef.
    RELEASE buf2-ef.       
                                                               
END PROCEDURE.

PROCEDURE pBuildBox PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: To copy box design
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcBoxDesign AS CHARACTER NO-UNDO. 
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE BUFFER buf-box-design-hdr  FOR box-design-hdr.
    DEFINE BUFFER buf-box-design-line FOR box-design-line. 
   
    EMPTY TEMP-TABLE ttBoxDesignHdr.
    EMPTY TEMP-TABLE ttBoxDesignLine.
    
    FOR EACH box-design-hdr NO-LOCK 
        WHERE box-design-hdr.design-no EQ 0 
        AND box-design-hdr.company   EQ ipbf-eb.company
        AND box-design-hdr.est-no    EQ ipbf-eb.est-no 
        AND box-design-hdr.form-no   EQ ipbf-eb.form-no
        AND box-design-hdr.blank-no  EQ ipbf-eb.blank-no :

        CREATE ttBoxDesignHdr.
        BUFFER-COPY box-design-hdr TO ttBoxDesignHdr.

        FOR EACH box-design-line OF box-design-hdr NO-LOCK:
            CREATE ttBoxDesignLine.
            BUFFER-COPY box-design-line TO ttBoxDesignLine.
        END.
    END. 

    IF ipcBoxDesign NE "N" THEN
    DO:
        FOR EACH box-design-hdr EXCLUSIVE-LOCK 
            WHERE box-design-hdr.design-no EQ 0 
            AND box-design-hdr.company   EQ ipbf-eb.company 
            AND box-design-hdr.est-no    EQ ipbf-eb.est-no
            AND box-design-hdr.form-no   EQ ipbf-eb.form-no
            AND box-design-hdr.blank-no  EQ ipbf-eb.blank-no :
              
            FOR EACH box-design-line OF box-design-hdr EXCLUSIVE-LOCK :
                DELETE box-design-line.
            END.
            DELETE box-design-hdr.
        END.
    END.

    FIND FIRST style NO-LOCK 
        WHERE style.company EQ ipbf-eb.company
        AND style.style   EQ ipbf-eb.style  NO-ERROR. 

    IF AVAILABLE  style THEN 
        FIND FIRST buf-box-design-hdr NO-LOCK  
            WHERE buf-box-design-hdr.design-no EQ  style.design-no 
            AND buf-box-design-hdr.company   EQ  style.company  
            AND buf-box-design-hdr.est-no    EQ  "" NO-ERROR.

    IF AVAILABLE buf-box-design-hdr THEN 
    DO:
        IF ipcBoxDesign NE "N" THEN
        DO:
            CREATE  box-design-hdr.
            ASSIGN   
                box-design-hdr.design-no    = 0
                box-design-hdr.company      = ipbf-eb.company
                box-design-hdr.est-no       = ipbf-eb.est-no
                box-design-hdr.form-no      = ipbf-eb.form-no
                box-design-hdr.blank-no     = ipbf-eb.blank-no
                box-design-hdr.description  = IF AVAILABLE  buf-box-design-hdr 
                                              THEN buf-box-design-hdr.description ELSE  ""
                box-design-hdr.lscore       = buf-box-design-hdr.lscore
                box-design-hdr.lcum-score   = buf-box-design-hdr.lcum-score
                box-design-hdr.wscore       = buf-box-design-hdr.wscore
                box-design-hdr.wcum-score   = buf-box-design-hdr.wcum-score
                box-design-hdr.box-text     = buf-box-design-hdr.box-text
                box-design-hdr.box-image    = buf-box-design-hdr.box-image
                box-design-hdr.box-3d-image = buf-box-design-hdr.box-3d-image.
          
            FOR EACH buf-box-design-line OF buf-box-design-hdr NO-LOCK:
                CREATE box-design-line.
                ASSIGN  
                    box-design-line.design-no  = box-design-hdr.design-no
                    box-design-line.company    = box-design-hdr.company
                    box-design-line.est-no     = box-design-hdr.est-no
                    box-design-line.form-no    = box-design-hdr.form-no
                    box-design-line.blank-no   = box-design-hdr.blank-no
                    box-design-line.line-no    = buf-box-design-line.line-no
                    box-design-line.line-text  = buf-box-design-line.line-text
                    box-design-line.wscore     = buf-box-design-line.wscore
                    box-design-line.wcum-score = buf-box-design-line.wcum-score.
            END.
        END. /*if ipcBoxDesign ne "N"*/

        IF ipcBoxDesign NE "B" AND ipcBoxDesign NE "N" THEN 
        DO: 
            FIND FIRST ttBoxDesignHdr NO-ERROR.
 
            IF AVAIL ttBoxDesignHdr THEN
            DO:
                IF ipcBoxDesign EQ "S" THEN
                    ASSIGN box-design-hdr.description  = ttBoxDesignHdr.description
                        box-design-hdr.box-image    = ttBoxDesignHdr.box-image
                        box-design-hdr.box-3d-image = ttBoxDesignHdr.box-3d-image.
                ELSE
                    ASSIGN box-design-hdr.lscore     = ttBoxDesignHdr.lscore
                        box-design-hdr.lcum-score = ttBoxDesignHdr.lcum-score
                        box-design-hdr.wscore     = ttBoxDesignHdr.wscore
                        box-design-hdr.wcum-score = ttBoxDesignHdr.wcum-score.
       
                FOR EACH ttBoxDesignLine OF box-design-hdr,
                    FIRST box-design-line OF ttBoxDesignHdr:
        
                    IF ipcBoxDesign NE "S" THEN
                        ASSIGN box-design-line.wscore     = ttBoxDesignLine.wscore
                            box-design-line.wcum-score = ttBoxDesignLine.wcum-score.
                END. /* FOR EACH ttBoxDesignLine OF box-design-hdr */
            END. /* IF AVAIL ttBoxDesignHdr THEN */
        END. /* IF ipcBoxDesign NE "B" AND ipcBoxDesign NE "N" THEN */
    END. /* IF AVAILABLE buf-box-design-hdr THEN */
END PROCEDURE.

 