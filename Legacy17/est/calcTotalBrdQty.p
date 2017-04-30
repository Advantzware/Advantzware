
/*------------------------------------------------------------------------
    File        : calcTotalBrdQty.p
    Purpose     : 

    Syntax      :

    Description : Calculate all board qty matching the given board

    Author(s)   : 
    Created     : Fri Apr 17 18:03:25 EDT 2015
    Notes       :
  ----------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBoard AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdLen AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWid AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER iprEf AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER iplCeBoard-log AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVend AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCorr AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotBqty AS DECIMAL NO-UNDO.
/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

    DEF BUFFER bf-ef   FOR ef.
    DEF BUFFER bf-eb   FOR eb.
    DEF BUFFER bf2-ef  FOR ef.
    DEF BUFFER bf-item FOR ITEM.
    DEF VAR tmp-b-qty     AS DEC NO-UNDO.
    DEF VAR tot-b-qty     AS DEC NO-UNDO.
    DEF VAR calc-b-uom    AS CHAR NO-UNDO.
    DEF VAR calc-save-qty AS DEC NO-UNDO.
    DEF VAR v-vend-no AS CHAR NO-UNDO.
    DEF VAR ceboard-log AS LOG NO-UNDO.
    DEF VAR v-corr AS LOG NO-UNDO.
    ASSIGN tot-b-qty = 0
           v-vend-no = ipcVend
           ceBoard-log = iplCeBoard-log 
           v-corr = iplCorr.
           
    FIND bf2-ef WHERE ROWID(bf2-ef) EQ iprEf 
      NO-LOCK NO-ERROR.
    FOR EACH bf-ef 
        WHERE bf-ef.company EQ bf2-ef.company
        AND bf-ef.est-no EQ bf2-ef.est-no
        AND rowid(bf-ef) NE iprEf
        AND bf-ef.gsh-len EQ ipdLen
        AND bf-ef.gsh-wid EQ ipdWid
        AND bf-ef.board EQ ipcBoard
        NO-LOCK,
        FIRST bf-eb 
        WHERE bf-eb.company EQ bf-ef.company
        AND bf-eb.est-no  EQ bf-ef.est-no
        AND bf-eb.form-no EQ bf-ef.form-no        
        /* AND  bf-eb.blank-no EQ bf-ef.blank-no */
        NO-LOCK:
        FIND FIRST bf-item 
            WHERE bf-item.company EQ ipcCompany
            AND bf-item.i-no = bf-ef.board 
            NO-LOCK NO-ERROR.   
        IF AVAIL bf-item THEN
          calc-b-uom = bf-item.cons-uom.
        IF NOT bf-eb.pur-man AND (ceboard-log EQ NO OR v-vend-no EQ "") AND bf-ef.cost-msh GT 0 THEN
           calc-b-uom = bf-ef.cost-uom.
        tmp-b-qty = (IF v-corr THEN (bf-ef.gsh-len * bf-ef.gsh-wid * .007)
        ELSE (bf-ef.gsh-len * bf-ef.gsh-wid / 144)) *
            bf-ef.gsh-qty / 1000.
        RUN sys/ref/convquom.p("MSF", "EA", bf-item.basis-w,
            bf-ef.gsh-len, bf-ef.gsh-wid, bf-ef.gsh-dep,
            tmp-b-qty, OUTPUT calc-save-qty).

        IF bf-eb.pur-man AND calc-b-uom EQ "C" THEN
            ASSIGN
                tmp-b-qty = calc-save-qty / 100
                .      
        ELSE
            IF calc-b-uom EQ "MSH" OR calc-b-uom EQ "M" THEN
                ASSIGN
                    tmp-b-qty = calc-save-qty / 1000
                    .      
            ELSE
                IF calc-b-uom EQ "EA"                                                     OR
                    (LOOKUP(calc-b-uom,"LB,DRM,ROL,PKG,SET,DOZ,BDL") GT 0 AND bf-eb.pur-man) THEN
                    ASSIGN
                        tmp-b-qty = calc-save-qty.  
      
                ELSE
                    IF calc-b-uom EQ "TON" THEN
    
                        ASSIGN
                            tmp-b-qty = (tmp-b-qty * bf-ef.weight) / 2000.
   
                    ELSE
                        IF calc-b-uom EQ "LB" THEN
                            ASSIGN
                                tmp-b-qty = tmp-b-qty * bf-ef.weight.    

                        ELSE
                            IF calc-b-uom EQ "REM" THEN 
                            DO:
                                ASSIGN
                                    tmp-b-qty = (IF v-corr THEN (tmp-b-qty / .007) ELSE (tmp-b-qty * 144)) * 1000 /
           (bf-ef.gsh-wid * 10000).
  
                                {sys/inc/roundup.i tmp-b-qty}
                            END.
                            ELSE
                                IF calc-b-uom EQ "LF" THEN
                                    ASSIGN
                                        tmp-b-qty = tmp-b-qty * 1000 / (bf-ef.gsh-wid / 12)
                                        .    
        tot-b-qty = tot-b-qty + tmp-b-qty.                                  
    END.
    opdTotBqty = tot-b-qty.