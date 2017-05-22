/*---------------------------------------------------------------------------*/
/*  File:           fixRef.p                                                 */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Create temp files for emailcod and phone reftable entries*/
/*                                                                           */
/*  Included files:                                                          */
/*  External RUN/CALL:                                                       */
/*  External files:     WRITE c:\tmp\reftable-phone-save.d                   */
/*                      WRITE c:\tmp\reftable-phone-reckey.d                 */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT    Description               */
/*                      04/30/17    MYT            cleanup                   */
/*---------------------------------------------------------------------------*/

DEF BUFFER bf-reftable FOR reftable.

DEF STREAM s1.
DEF STREAM s2.

DEF VAR cnt AS INT.

DISABLE TRIGGERS FOR LOAD OF bf-reftable.

OUTPUT STREAM s1 TO c:\tmp\reftable-phone-save.d APPEND.
FOR EACH emailcod NO-LOCK:
    FOR EACH reftable NO-LOCK WHERE 
        reftable.reftable = "" AND 
        reftable.code = emailcod.emailcod:
        FIND FIRST phone NO-LOCK WHERE 
            RECID(phone) = INT(reftable.rec_key) 
            NO-ERROR.
        IF AVAIL phone THEN DO:
            EXPORT STREAM s1 reftable.
            FIND bf-reftable EXCLUSIVE WHERE 
                rowid(bf-reftable) = rowid(reftable).
            /* Replaces recid(phone) */
            ASSIGN 
                bf-reftable.rec_key = STRING(phone.rec_key)
                cnt = cnt + 1.
        END.
    END.     
END.
OUTPUT STREAM s1 CLOSE.

OUTPUT STREAM s2 TO c:\tmp\reftable-phone-reckey.d APPEND.
FOR EACH phone NO-LOCK:
    FOR EACH reftable NO-LOCK WHERE
        reftable.reftable = "" AND 
        reftable.rec_key = phone.TABLE_rec_key:
        EXPORT STREAM s2 reftable.
        FIND bf-reftable EXCLUSIVE WHERE 
            rowid(bf-reftable) = rowid(reftable).
        /* Replaces recid(phone) */
        ASSIGN 
            bf-reftable.rec_key = STRING(phone.rec_key)
            cnt = cnt + 1.
    END.
END.
OUTPUT STREAM s1 CLOSE.
