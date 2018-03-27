/*---------------------------------------------------------------------------*/
/*  File:           DeploymentFiles\DataFixPrograms\InvRnoSeq.p               */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Utility to assign inv_r_no_seq                           */
/*                  values from raw data. Apply to versions 16.6.8 and below.*/
/*                  (non-destructive if applied twice)                       */
/*                                                                           */
/*  Included files:     none                                                 */
/*  External RUN/CALL:  none                                                 */
/*  External files:     READ inv-head                                        */                   
/*                      READ company                                         */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT     Description              */
/*                      12/01/17    wfk     24853   Original Version         */
/*---------------------------------------------------------------------------*/

    DEF VAR iCurrVal AS INT NO-UNDO.
    DEF VAR iLastDataValue AS INT NO-UNDO.
    DEF VAR iTries AS INT NO-UNDO.
    DEF VAR cCompSuffix AS CHAR NO-UNDO.
    
    /* Create inv_r_no_seq from last inv-head by r-no */
    ASSIGN
        iTries = 0
        iCurrVal = CURRENT-VALUE(inv_r_no_seq).
    INVHEAD_RNO:
    DO WHILE iCurrVal EQ 0:
    
        FIND LAST inv-head NO-LOCK
          USE-INDEX r-no 
          NO-ERROR.
          
        ASSIGN
            iLastDataValue = IF AVAIL inv-head THEN inv-head.r-no ELSE 0.
            
        /* If the record is in ambiguous state or otherwise returns 0, keep trying */
        IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
            PAUSE 1.
            FIND LAST inv-head NO-LOCK
              USE-INDEX r-no 
              NO-ERROR.
            ASSIGN
                iLastDataValue = IF AVAIL inv-head THEN inv-head.r-no ELSE 0
                iTries = iTries + 1.
            IF iTries GT 60 THEN DO: /* Try for 1 minute, then quit */
                MESSAGE
                    "Unable to set sequence value for inv-head.r-no" SKIP
                    "Please contact Advantzware Support for assistance."
                    VIEW-AS ALERT-BOX ERROR.
                LEAVE INVHEAD_RNO.
            END.
        END.
        
        ASSIGN
            CURRENT-VALUE(inv_r_no_seq) = iLastDataValue
            iCurrVal = CURRENT-VALUE(inv_r_no_seq).       
    END.

