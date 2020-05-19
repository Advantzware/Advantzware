{ed/sharedv.i "new"}
{ed/edivars.i "new shared"}
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiInvoice AS INTEGER NO-UNDO.
DEFINE NEW SHARED STREAM s-out.
DEFINE NEW SHARED STREAM s-err.
DEFINE VARIABLE cReportPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustNo     AS CHARACTER NO-UNDO.

/* In case nothing was set */
ws_edi_path = "c:\tmp\edi".

FIND FIRST EDCo NO-LOCK WHERE EDCo.Company EQ ipCompany
    NO-ERROR.
    
/* If no EdCo record then EDI is not configured */
IF NOT AVAILABLE EDCo THEN 
    RETURN.
    
cReportPath = EDCo.Path-err + "\" + "810report" + STRING(RANDOM(1,100000), "999999" ) + ".txt".   
 
IF ipiInvoice GT 0 THEN 
DO:
    /* Single invoice to process */
    cCustNo = "".
    FIND FIRST inv-head NO-LOCK 
        WHERE inv-head.company EQ ipCompany
          AND inv-head.inv-no EQ ipiInvoice
        NO-ERROR.
    IF AVAILABLE inv-head THEN 
        cCustNo = inv-head.cust-no.
    ELSE 
    DO:
        FIND FIRST ar-inv NO-LOCK 
            WHERE ar-inv.company EQ ipCompany
              AND ar-inv.inv-no EQ ipiInvoice
            NO-ERROR.
        IF AVAILABLE ar-inv THEN 
            cCustNo = ar-inv.cust-no. 
    END.
    IF cCustNo GT "" THEN 
    DO:
        FIND FIRST edMast NO-LOCK 
            WHERE edMast.cust EQ cCustNo
            NO-ERROR.

        /* Customer not set up for EDI */
        IF NOT AVAILABLE edMast THEN 
            RETURN. 
        FIND FIRST edCode NO-LOCK
            WHERE  edcode.setid EQ "810"
              AND EDCode.partner EQ EDMast.partnerGrp     
            NO-ERROR. 
        IF NOT AVAILABLE edCode THEN 
            RETURN.
        invoice_number = STRING(ipiInvoice).
        
        FIND FIRST eddoc NO-LOCK
            WHERE eddoc.setid = edcode.setid
              AND eddoc.partner = edmast.partner
              AND eddoc.error-count = 0
              // AND eddoc.posted = FALSE
              AND NOT eddoc.status-flag = "DEL"   /* 9809 CAH */
              AND eddoc.direction = edcode.direction 
              AND eddoc.docid EQ invoice_number
            NO-ERROR.
          
        IF NOT AVAILABLE eddoc THEN
            RETURN. /* nothing to do */
              
        OUTPUT stream s-out TO VALUE(cReportPath).
         
        ASSIGN
            ws_partner_grp = EDMast.PartnerGrp
            ws_partner     = edmast.Partner 
            ws_edcode_rec  = RECID(edcode)
            .
                
        IF AVAILABLE EDCode AND EDCode.Path-out GT "" THEN
            ws_edi_path = EDcode.Path-out + fOutputFileName().
        ELSE  
            ws_edi_path = EDMast.Path-out + fOutputFileName().
                      
           
        RUN ed/tdf/o8104010.p.
          
        RUN ed/postProcessEDI.p (INPUT "810", ws_edi_path, ws_partner, ws_edi_path + ".edi").
        
    END. /* If customer number was obtained based on the invoice number */
    

      
END. /* Single invoice number passed in */

ELSE 
DO:
    /* No invoice number passed in, process all unsent invoices */
    OUTPUT stream s-out TO VALUE(cReportPath). 
    
    FOR EACH EDCode NO-LOCK
        WHERE  edcode.setid EQ "810",
        
        EACH EDMast NO-LOCK 
        WHERE EDMast.partnerGrp EQ EDCode.partner,
        FIRST eddoc WHERE eddoc.partner EQ edmast.partner
          AND eddoc.posted = NO         
        :
           
        IF AVAILABLE EDMast THEN 
            ws_partner_grp = EDMast.PartnerGrp.
            
        ASSIGN
            ws_partner    = edmast.Partner 
            ws_edcode_rec = RECID(edcode)
            .
                
        IF AVAILABLE EDCode AND EDCode.Path-out GT "" THEN
            ws_edi_path = EDcode.Path-out + fOutputFileName().
        ELSE  
            ws_edi_path = EDMast.Path-out + fOutputFileName().
            
        IF ipiInvoice GT 0 THEN 
            invoice_number = STRING(ipiInvoice).
        ELSE 
            invoice_number = "".
           
        RUN ed/tdf/o8104010.p.
          
        RUN ed/postProcessEDI.p (INPUT "810", ws_edi_path, ws_partner, ws_edi_path + ".edi").
         
    END.
END. /* IF NO invoice passed IN */