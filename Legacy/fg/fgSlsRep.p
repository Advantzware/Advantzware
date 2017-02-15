DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcCustNo  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcPartNo  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcINo     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcNewRep  AS CHARACTER   NO-UNDO.

DEF VAR ll AS LOG NO-UNDO.
DEF BUFFER bf-itemfg  FOR itemfg.
DEF BUFFER bf-quotehd FOR quotehd.
DEF    VAR      cNewRep  AS CHAR      NO-UNDO.

DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
IF ipcPartNo GT "" THEN 
DO:
    FIND FIRST cust-part 
        WHERE cust-part.company EQ ipcCompany
        AND cust-part.cust-no EQ ipcCustNo
        AND cust-part.part-no EQ ipcPartNo
        AND cust-part.i-no EQ ipcINo
        AND cust-part.spare-char-1 GT ""
        USE-INDEX part-no
        NO-LOCK NO-ERROR.
    IF NOT AVAIL cust-part THEN
        FIND FIRST cust-part 
            WHERE cust-part.company EQ ipcCompany
            AND cust-part.cust-no EQ ipcCustNo
            AND cust-part.part-no EQ ipcPartNo
            AND cust-part.spare-char-1 GT ""
            USE-INDEX part-no
            NO-LOCK NO-ERROR.
    IF NOT AVAIL cust-part THEN
        FIND FIRST bf-itemfg
            WHERE bf-itemfg.company EQ ipcCompany
            AND bf-itemfg.part-no EQ ipcPartNo
            AND bf-itemfg.cust-no EQ ipcCustNo
            AND bf-itemfg.i-no EQ ipcINo
            USE-INDEX cust-part NO-LOCK NO-ERROR.
    IF NOT AVAIL cust-part AND NOT AVAIL bf-itemfg THEN
        FIND FIRST bf-itemfg
            WHERE bf-itemfg.company EQ ipcCompany
            AND bf-itemfg.part-no EQ ipcPartNo
            AND bf-itemfg.cust-no EQ ipcCustNo          
            USE-INDEX cust-part NO-LOCK NO-ERROR.

    IF AVAIL cust-part OR AVAIL bf-itemfg THEN 
    DO:
        ASSIGN 
            cNewRep = "".
        IF AVAIL cust-part AND cust-part.spare-char-1 GT "" THEN
            cNewRep = cust-part.spare-char-1.
        ELSE IF AVAIL bf-itemfg THEN
                cNewRep = bf-itemfg.spare-char-3.       
    END.
    /* Nothing defined by cust-part and itemfg, so try to use what's set up for the cust */
    IF cNewRep EQ "" AND ipcCustNo GT "" THEN do:
        FIND FIRST cust WHERE cust.company = ipcCompany
            AND cust.cust-no = ipcCustNo NO-LOCK NO-ERROR .
        IF AVAIL cust THEN
            cNewRep = cust.sman.
    END.

END. /* If partNo was passed in */
ELSE IF ipcCompany GT "" AND ipcINo GT "" THEN DO:
    /* No Part Number passed in */
        FIND FIRST itemfg 
            WHERE itemfg.company EQ ipcCompany
            AND itemfg.i-no      EQ ipcINo
            NO-LOCK NO-ERROR.
        IF ipcCustNo GT "" AND AVAIL(itemfg) THEN 
        DO:
            FOR EACH cust-part 
                WHERE cust-part.company EQ ipcCompany   
                AND cust-part.i-no      EQ ipcINo
                AND cust-part.cust-no   EQ ipcCustNo
                NO-LOCK, 
                FIRST reftable 
                WHERE reftable.reftable EQ "cp-lab-p" 
                AND reftable.company    EQ cust-part.company  
                AND reftable.loc        EQ cust-part.i-no   
                AND reftable.code       EQ cust-part.cust-no 
                NO-LOCK:
        
                IF cust-part.spare-char-1 NE "" THEN 
                do:
                    FIND FIRST sman WHERE sman.company EQ itemfg.company
                        AND sman.sman EQ cust-part.spare-char-1 NO-LOCK NO-ERROR.
                    IF AVAIL sman THEN cNewRep = sman.sman.
                    LEAVE .
                END.

            END. /* end of cust-part */
            IF cNewRep EQ "" THEN 
            DO:
                FIND FIRST cust-part 
                    WHERE cust-part.company EQ ipcCompany
                    AND cust-part.i-no      EQ ipcINo
                    AND cust-part.cust-no   EQ ipcCustNo
                    NO-LOCK NO-ERROR.
                IF AVAIL cust-part AND cust-part.spare-char-1 GT "" THEN
                    cNewRep = cust-part.spare-char-1.

            END. /* If not found by the reftable */
        END. /* Find by cust-no */

        /* Nothing defined by cust-part, so try to use what's set up for the item */
        IF cNewRep EQ "" AND AVAIL itemfg AND itemfg.spare-char-3 GT "" THEN 
            cNewRep = itemfg.spare-char-3.
        /* Nothing defined by cust-part and itemfg, so try to use what's set up for the cust */
        IF cNewRep EQ "" AND ipcCustNo GT "" THEN do:
            FIND FIRST cust WHERE cust.company = ipcCompany
                AND cust.cust-no = ipcCustNo NO-LOCK NO-ERROR .
            IF AVAIL cust THEN
                cNewRep = cust.sman.
        END.

    END. /* No part number passed in */

opcNewRep = cNewRep.
