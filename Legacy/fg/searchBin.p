
/*------------------------------------------------------------------------
    File        : searchBin.p
    Purpose     : 

    Syntax      :

    Description : Identify best matching fg-bin record			

    Author(s)   : WFK
    Created     : Thu May 04 16:20:54 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJob-no     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiJob-no2    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiOrd-no     AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcINo        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdQty        AS DECIMAL   NO-UNDO.
DEFINE INPUT PARAMETER ipcLoc        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBin        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBolWhse    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTag        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER  oprFgBinRow  AS ROWID     NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

IF ipcTag GT "" THEN 
FIND FIRST fg-bin NO-LOCK
    WHERE fg-bin.company EQ ipcCompany
    AND fg-bin.tag       EQ ipcTag
    AND fg-bin.job-no    EQ ipcJob-No
    AND fg-bin.job-no2   EQ ipiJob-No2
    AND fg-bin.i-no      EQ ipcINO
    AND fg-bin.qty       GE ipdQty
    AND fg-bin.loc       EQ ipcLoc
    USE-INDEX tag NO-ERROR.

IF NOT AVAILABLE fg-bin AND ipcBin GT "" THEN     
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.job-no    EQ ipcJob-No
        AND fg-bin.job-no2   EQ ipiJob-No2
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.qty       GE ipdQty
        AND fg-bin.loc       EQ ipcLoc
        AND fg-bin.loc-bin   EQ ipcBin
        USE-INDEX job NO-ERROR.
    
IF NOT AVAILABLE fg-bin THEN     
FIND FIRST fg-bin NO-LOCK
    WHERE fg-bin.company EQ ipcCompany
    AND fg-bin.job-no    EQ ipcJob-No
    AND fg-bin.job-no2   EQ ipiJob-No2
    AND fg-bin.i-no      EQ ipcINO
    AND fg-bin.qty       GE ipdQty
    AND fg-bin.loc       EQ ipcLoc
    USE-INDEX job NO-ERROR.
    
IF NOT AVAIL fg-bin AND ipcBolWhse NE "ShipFromWhse" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.job-no    EQ ipcJob-No
        AND fg-bin.job-no2   EQ ipiJob-No2
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.qty       GE ipdQty
        USE-INDEX job NO-ERROR.
        
IF NOT AVAIL fg-bin AND ipcTag GT "" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.tag       EQ ipcTag
        AND fg-bin.job-no    EQ ipcJob-No
        AND fg-bin.job-no2   EQ ipiJob-No2
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.loc       EQ ipcLoc
        AND fg-bin.qty       GT 0
        USE-INDEX tag NO-ERROR.
        
IF NOT AVAIL fg-bin THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.job-no    EQ ipcJob-No
        AND fg-bin.job-no2   EQ ipiJob-No2
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.loc       EQ ipcLoc
        AND fg-bin.qty       GT 0
        USE-INDEX job NO-ERROR.

IF NOT AVAIL fg-bin AND ipcBolWhse NE "ShipFromWhse" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.job-no    EQ ipcJob-No
        AND fg-bin.job-no2   EQ ipiJob-No2
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.qty       GT 0
        USE-INDEX job NO-ERROR.

IF NOT AVAIL fg-bin THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.job-no    EQ ipcJob-No
        AND fg-bin.job-no2   EQ ipiJob-No2
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.loc       EQ ipcLoc
        USE-INDEX job NO-ERROR.

IF NOT AVAIL fg-bin AND ipcBolWhse NE "ShipFromWhse" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.job-no    EQ ipcJob-No
        AND fg-bin.job-no2   EQ ipiJob-No2
        AND fg-bin.i-no      EQ ipcINO
        USE-INDEX job NO-ERROR.
    
IF NOT AVAIL fg-bin AND ipcJob-No EQ "" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.ord-no    EQ ipiOrd-No
        AND fg-bin.loc       EQ ipcLoc
        AND fg-bin.qty       GT 0
        USE-INDEX co-ino NO-ERROR.

IF NOT AVAIL fg-bin AND ipcJob-No EQ "" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.ord-no    EQ ipiOrd-No
        AND fg-bin.loc       EQ ipcLoc
        USE-INDEX co-ino NO-ERROR.

IF NOT AVAIL fg-bin AND ipcJob-No EQ "" AND ipcBolWhse NE "ShipFromWhse" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.ord-no    EQ ipiOrd-No
        AND fg-bin.qty       GT 0
        USE-INDEX co-ino NO-ERROR.

IF NOT AVAIL fg-bin AND ipcJob-No EQ "" AND ipcBolWhse NE "ShipFromWhse" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.ord-no    EQ ipiOrd-No
        USE-INDEX co-ino NO-ERROR.

IF NOT AVAIL fg-bin AND ipcJob-No EQ "" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.loc       EQ ipcLoc
        AND fg-bin.qty       GT 0
        USE-INDEX co-ino NO-ERROR.
   
IF NOT AVAIL fg-bin AND ipcJob-No EQ "" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no    EQ ipcINO
        AND fg-bin.loc     EQ ipcLoc
        USE-INDEX co-ino NO-ERROR.
        
IF NOT AVAIL fg-bin AND ipcJob-No EQ "" AND ipcTag GT "" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.tag       EQ ipcTag
        AND fg-bin.i-no      EQ ipcINO
        AND fg-bin.qty       GT 0
        USE-INDEX co-ino NO-ERROR.
        
IF NOT AVAIL fg-bin AND ipcJob-No EQ "" AND ipcBolWhse NE "ShipFromWhse" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no    EQ ipcINO
        AND fg-bin.qty     GT 0
        USE-INDEX co-ino NO-ERROR.

IF NOT AVAIL fg-bin AND ipcJob-No EQ "" AND ipcBolWhse NE "ShipFromWhse" THEN
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no      EQ ipcINO
        USE-INDEX co-ino NO-ERROR.
IF AVAILABLE fg-bin THEN 
    oprFgBinRow = ROWID(fg-bin).
ELSE 
    oprFgBinRow = ?.