
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

//RUN CalcLayoutSize.
RUN pCompareLayoutSize.

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
    DEFINE VARIABLE gcEstimateID AS CHARACTER NO-UNDO INITIAL "  103334".
    
     
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
            ef.roll SKIP
   INDEX("B",ef.xgrain) EQ 0 skip
            ef.xgrain skip
   
            "xef.lsh-lock" ef.lsh-lock
            VIEW-AS ALERT-BOX.
   
    END.     

END.
