{ed/sharedv.i "new"}
{ed/edivars.i       "new shared"}
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiInvoice AS INTEGER NO-UNDO.
DEFINE NEW SHARED STREAM s-out.
DEFINE NEW SHARED STREAM s-err.
DEFINE VARIABLE cReportPath AS CHARACTER NO-UNDO.



/* In case nothing was set */
ws_edi_path = "c:\tmp\edi".
FIND FIRST EDCo NO-LOCK WHERE EDCo.Company EQ ipCompany
                       NO-ERROR.
IF AVAILABLE EDCo THEN 
   cReportPath = EDCo.Path-err + "\" + "810report" + STRING(RANDOM(1,100000), "999999" ) + ".txt".
ELSE
    cReportPath = "c:\tmp\" + "810report" + STRING(RANDOM(1,100000), "999999" ) + ".txt".
    
OUTPUT stream s-out TO VALUE(cReportPath). 
    
FOR EACH EDCode NO-LOCK
    WHERE  edcode.setid EQ "810",
    
    EACH EDMast NO-LOCK 
       WHERE EDMast.partnerGrp EQ EDCode.partner,
    FIRST eddoc WHERE eddoc.partner EQ edmast.partner
      AND eddoc.posted = NO
       .
       
    IF AVAILABLE EDMast THEN 
        ws_partner_grp = EDMast.PartnerGrp.
        
    ASSIGN
        ws_partner    = edmast.Partner 
        ws_edcode_rec = RECID(edcode)
        .
            
    IF false  /* AVAILABLE EDCode AND EDCode.Customized */ THEN DO: 
      
      IF EDCode.Path-out GT "" THEN 
        ws_edi_path = EDCode.Path-out + fOutputFileName().
      ELSE 
        ws_edi_path = "c:\tmp\810" + fOutputFileName().
     
                        
        RUN VALUE(EDCode.Custom-proc + ".p").
        
    END. 
    ELSE DO:
      IF AVAILABLE EDCode AND EDCode.Path-out GT "" THEN
          ws_edi_path = EDcode.Path-out + fOutputFileName().
      ELSE  
          ws_edi_path = EDMast.Path-out + fOutputFileName().
      IF ipiInvoice GT 0 THEN 
         invoice_number = STRING(ipiInvoice).
      ELSE 
         invoice_number = "".
      RUN ed/tdf/o8104010.p.
      
      
    END.

    RUN ed/postProcessEDI.p (INPUT "810", ws_edi_path, ws_partner, ws_edi_path + ".edi").
     
END.
