
/*------------------------------------------------------------------------
    File        : CalcLayoutSizeTester.p
    Purpose     : 

    Syntax      :

    Description : Tester for CalcLayoutSizep

    Author(s)   : Sakshi Singh
    Created     : Wed Sep 16 16:39:16 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdOpProcs AS HANDLE.
DEFINE VARIABLE hdSession AS HANDLE.


//103335

{est/ttCalcLayoutSize.i}

RUN CalcLayoutSize.
//RUN pCompareLayoutSize.
//RUN pCompareRangeLayoutSize.

PROCEDURE pCompareLayoutSize:
    
    DEFINE VARIABLE cCompany  AS CHARACTER NO-UNDO INITIAL "001".
    DEFINE VARIABLE cEstID1   AS CHARACTER NO-UNDO INITIAL "  103335".
    DEFINE VARIABLE iFormID1  AS INTEGER   NO-UNDO INITIAL 1.
    DEFINE VARIABLE iBlankID1 AS INTEGER   NO-UNDO INITIAL 1.
    DEFINE VARIABLE cEstID2   AS CHARACTER NO-UNDO INITIAL "  103338".
    DEFINE VARIABLE iFormID2  AS INTEGER   NO-UNDO INITIAL 1.
    DEFINE VARIABLE iBlankID2 AS INTEGER   NO-UNDO INITIAL 1.
    
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER src-est FOR est.
    DEFINE BUFFER src-ef FOR ef.
    DEFINE BUFFER src-eb FOR eb.
    DEFINE BUFFER trg-est FOR est.
    DEFINE BUFFER trg-ef FOR ef.
    DEFINE BUFFER trg-eb FOR eb.
    
    FIND FIRST src-est NO-LOCK
        WHERE src-est.company = cCompany 
        AND src-est.est-no = cEstID1 NO-ERROR.
      
    FIND FIRST src-ef NO-LOCK
        WHERE src-ef.company = src-est.Company 
        AND src-ef.est-no = src-est.est-no
        AND src-ef.form-no = iFormID1 NO-ERROR.
      
    FIND FIRST src-eb NO-LOCK
        WHERE src-eb.company = src-est.Company  
        AND src-eb.est-no = src-est.est-no 
        AND src-eb.form-no = src-ef.form-no
        AND src-eb.blank-no = iBlankID1 NO-ERROR.
          
    FIND FIRST trg-est NO-LOCK
        WHERE trg-est.company = cCompany 
        AND trg-est.est-no = cEstID2 NO-ERROR.
      
    FIND FIRST trg-ef NO-LOCK
        WHERE trg-ef.company = trg-est.Company 
        AND trg-ef.est-no = trg-est.est-no
        AND trg-ef.form-no = iFormID2 NO-ERROR.
      
    FIND FIRST trg-eb NO-LOCK
        WHERE trg-eb.company = trg-est.Company  
        AND trg-eb.est-no = trg-est.est-no 
        AND trg-eb.form-no = trg-ef.form-no
        AND trg-eb.blank-no = iBlankID2 NO-ERROR.
        
    BUFFER-COMPARE src-ef to trg-ef save result in cFields. 
          
    MESSAGE 
    "cFields" cFields
    VIEW-AS ALERT-BOX.
    
    
ENd.    

PROCEDURE CalcLayoutSize:
    
    DEFINE VARIABLE gcCompany    AS CHARACTER NO-UNDO INITIAL "001".
    DEFINE VARIABLE gcEstimateID AS CHARACTER NO-UNDO INITIAL "  103446".
    
     
    FIND FIRST est NO-LOCK
        WHERE est.company = gcCompany 
        AND est.est-no = gcEstimateID NO-ERROR.
      
    FIND FIRST ef NO-LOCK
        WHERE ef.company = gcCompany 
        AND ef.est-no = gcEstimateID
        AND ef.form-no = 1 NO-ERROR.
      
    FIND FIRST eb NO-LOCK
        WHERE eb.company = gcCompany 
        AND eb.est-no = gcEstimateID 
        AND eb.form-no = ef.form-no
        AND eb.blank-no = 1 NO-ERROR.
          

    RUN est/CalcLayoutSize.p (INPUT ROWID(ef),
        INPUT ROWID(eb),
        NO,   // iplCalcSizeOnly
        OUTPUT TABLE ttLayoutSize).
        
        
    FOR EACH ttLayoutSize:
   // disp ttLayoutSize with 1 col scrollable.
   
   
        MESSAGE 
            "Gross Length" dGrossSheetLength ef.gsh-len SKIP
            "Gross width" dGrossSheetWidth  ef.gsh-wid SKIP
            "Gross depth" dGrossSheetDepth  ef.gsh-dep SKIP
            "Net Length" dNetSheetLength ef.nsh-len SKIP
            "Net width" dNetSheetwidth ef.nsh-wid SKIP
            "Net depth" dNetSheetdepth ef.nsh-dep SKIP
            "Side to Side" dLayoutSheetLength ef.lsh-len SKIP
            "Front to Back" dLayoutSheetWidth ef.lsh-wid SKIP
            "Die Length" dDieSizeLength ef.trim-l SKIP
            "Die Width" dDieSizewidth ef.trim-w SKIP
            "Die depth" dDieSizedepth ef.trim-d SKIP
           
            ef.xgrain skip
   
            "xef.lsh-lock" ef.lsh-lock
            VIEW-AS ALERT-BOX.
   
    END.     

END.

PROCEDURE pCompareRangeLayoutSize:

    DEFINE VARIABLE gcCompany    AS CHARACTER NO-UNDO INITIAL "001".
    DEFINE VARIABLE gcEstimateID AS CHARACTER NO-UNDO INITIAL "  103370".
    DEFINE VARIABLE iEstSt       AS INTEGER   NO-UNDO init 103370.
    DEFINE VARIABLE iEstEnd      AS INTEGER   NO-UNDO init 103375.  //103404.
    DEFINE VARIABLE icnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRec AS INTEGER NO-UNDO .
    DEFINE VARIABLE cEst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
    
    Do icnt = iEstSt to iEstEnd: 
        
        iRec = iRec + 1.
        IF iRec <> 1 then
        next.
        
        //cEst = STRING(icnt, "  999999").
        //cEst = "  103391".
        cEst = "  103400".
        
     
        FOR first est NO-LOCK
            WHERE est.company = gcCompany 
            AND est.est-no = cEst ,
            EACH ef NO-LOCK
            WHERE ef.company = gcCompany 
            AND ef.est-no = est.est-no ,
            EACH eb NO-LOCK
            WHERE eb.company = gcCompany 
            AND eb.est-no = est.est-no 
            AND eb.form-no = ef.form-no:
                  

            RUN est/CalcLayoutSize.p (INPUT ROWID(ef),
                INPUT ROWID(eb),
                OUTPUT TABLE ttLayoutSize).
            cFields = "".
        
            FOR FIRST ttLayoutSize:
        
  
                IF  ef.lsh-len  <> ttLayoutSize.dLayoutSheetLength THEN
                cFields = cFields + ", ef.lsh-len".
                  
                IF  ef.lsh-wid  <> ttLayoutSize.dLayoutSheetWidth   THEN
                cFields = cFields + ", ef.lsh-wid". 
                  
                IF  ef.nsh-len  <> ttLayoutSize.dNetSheetLength        THEN
                cFields = cFields + ", ef.nsh-len".
                
                IF  ef.nsh-wid  <> ttLayoutSize.dNetSheetWidth         THEN
                cFields = cFields + ", ef.nsh-wid".
                
                IF  ef.nsh-dep  <> ttLayoutSize.dNetSheetDepth         THEN
                cFields = cFields + ", ef.nsh-dep".
                
                IF  ef.gsh-len  <> ttLayoutSize.dGrossSheetLength      THEN
                cFields = cFields + ", ef.gsh-len".
                
                IF  ef.gsh-wid  <> ttLayoutSize.dGrossSheetWidth       THEN
                cFields = cFields + ", ef.gsh-wid".
                
                IF  ef.gsh-dep  <> ttLayoutSize.dGrossSheetDepth       THEN
                cFields = cFields + ", ef.gsh-dep".
                
                IF  ef.trim-l   <> ttLayoutSize.dDieSizeLength         THEN
                cFields = cFields + ", ef.trim-l".
                
                IF  ef.trim-w   <> ttLayoutSize.dDieSizeWidth          THEN
                cFields = cFields + ", ef.trim-w".
                
                IF  ef.trim-d   <> ttLayoutSize.dDieSizeDepth          THEN
                cFields = cFields + ", ef.lsh-len".
                
                IF  ef.roll-wid <> ttLayoutSize.dRollWidth             THEN
                cFields = cFields + ", ef.roll-wid".
                
                IF  ef.die-in   <> ttLayoutSize.dDieInchesRequired     THEN
                cFields = cFields + ", ef.die-in".
                
                IF  ef.roll     <> ttLayoutSize.IsRollMaterial         THEN
                cFields = cFields + ", ef.roll".
                
                IF  ef.n-out    <> ttLayoutSize.iNumOutWidth           THEN
                cFields = cFields + ", ef.n-out".
                
                IF  ef.n-out-l  <> ttLayoutSize.iNumOutLength          THEN
                cFields = cFields + ", ef.n-out-l".
                
                IF  ef.n-out-d  <> ttLayoutSize.iNumOutDepth           THEN
                cFields = cFields + ", ef.n-out-d".
                
                IF  ef.n-cuts   <> ttLayoutSize.iNumberCuts            THEN
                cFields = cFields + ", ef.n-cuts".
                
                IF  eb.num-up   <> ttLayoutSize.iBlankNumUp            THEN
                cFields = cFields + ", eb.num-up".
                
                IF  eb.num-wid  <> ttLayoutSize.iBlankNumOnWidth       THEN
                cFields = cFields + ", eb.num-wid".
                
                IF  eb.num-len  <> ttLayoutSize.iBlankNumOnLength      THEN
                cFields = cFields + ", eb.num-len".
                
                IF  eb.num-dep  <> ttLayoutSize.iBlankNumOnDepth THEN
                cFields = cFields + ", eb.num-dep".
                
            cFields = TRIM(cFields).
            cFields = left-TRIM(cFields,",").
            
            IF cFields NE "" THEN
                    MESSAGE 
                        "est.est-no " est.est-no  skip
                        "Form" ef.form-no "Blank" eb.blank-no skip
                        cFields skip
                        "Gross Length" dGrossSheetLength ef.gsh-len   SKIP
                        "Gross width" dGrossSheetWidth  ef.gsh-wid SKIP
                        "Gross depth" dGrossSheetDepth  ef.gsh-dep SKIP
                        "Net Length" dNetSheetLength ef.nsh-len SKIP
                        "Net width" dNetSheetwidth ef.nsh-wid SKIP
                        "Net depth" dNetSheetdepth ef.nsh-dep SKIP
                        "Side to Side" dLayoutSheetLength ef.lsh-len SKIP
                        "Front to Back" dLayoutSheetWidth ef.lsh-wid SKIP
                        "Die Length" dDieSizeLength ef.trim-l SKIP
                        "Die Width" dDieSizewidth ef.trim-w SKIP
                        "Die depth" dDieSizedepth ef.trim-d SKIP
                        "Roll"  ttLayoutSize.IsRollMaterial ef.roll   skip
                        "Roll wid " ttLayoutSize.dRollWidth  ef.roll-wid Skip
                        "Die In" ttLayoutSize.dDieInchesRequired ef.die-in SKip
                        "N Out " ttLayoutSize.iNumOutWidth ef.n-out skip         
                        "N OutL" ttLayoutSize.iNumOutLength ef.n-out-l skip        
                        "NOut D" ttLayoutSize.iNumOutDepth ef.n-out-d skip          
                        "n Cuts" ttLayoutSize.iNumberCuts ef.n-cuts  skip         
                        "Num Up" ttLayoutSize.iBlankNumUp eb.num-up skip     
                        "Num Wid" ttLayoutSize.iBlankNumOnWidth eb.num-wid skip     
                        "Num Len" ttLayoutSize.iBlankNumOnLength eb.num-len skip    
                        "Num dep" ttLayoutSize.iBlankNumOnDepth Eb.num-dep   skip
                        VIEW-AS ALERT-BOX.
                        
                ELSE
                MESSAGE 
                    "est.est-no " est.est-no  skip
                    "Form" ef.form-no "Blank" eb.blank-no skip
                    cFields VIEW-AS ALERT-BOX.
                        
   
            END.     

        END.  
    END.  
end.    
