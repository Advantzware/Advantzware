/* custom/asifax2.i */        
        
              CREATE "FaxComEx.FaxServer" hC-server .
              hC-server:CONNECT("").
              CREATE "FaxComEx.FaxDocument" hC-fax.


              ASSIGN hC-fax:Body = lv-outfile .
           /*
              IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
                 MESSAGE "bbody ERROR"  ERROR-STATUS:NUM-MESSAGES v-job-id skip
                       ERROR-STATUS:TYPE
                      ERROR-STATUS:GET-MESSAGE(i)
                      VIEW-AS ALERT-BOX.
                  op-return = 1.
                  RETURN .
              END.
           */
              hc-fax:priority = 1.
              hC-fax:Recipients:ADD(lv-faxto,lv-faxname). /*for more than one recipient*/
              /*hc-fax:Recipient:FaxNumber = lv-faxto.    /* single recipent */
              hc-fax:Recipient:Name = lv-faxname.
              */
              hC-fax:Attachfaxtoreceipt = TRUE.
              hc-fax:coverpagetype = 2.
              hc-fax:coverpage = "generic".
              hc-fax:note = ip-subject.
              hc-fax:receiptAddress = lv-rec-email.
              hc-fax:receipttype = 0.
                /*
              hc-fax:scheduletype = 1.
              hc-fax:scheduletime =
                  */

                     hC-fax:DocumentName = ip-body.
                     hC-fax:Subject = ip-subject.

              ASSIGN hC-fax:Sender:TITLE = ip-subject
                     hC-fax:sender:NAME = lv-from /*"Sender"*/
                     hC-fax:sender:city = lv-comp-city /*"Yardley"*/
                     hC-fax:sender:state = lv-comp-state /*"PA"*/
                     hc-fax:sender:zipcode = lv-comp-zip
                     hc-fax:sender:company = lv-company /*"Advanced Software"*/
                     hc-fax:sender:streetaddress = lv-comp-addr[1]
                     hc-fax:sender:Email = lv-from-email /*"ASI@advantzware.com" */
                     hc-fax:sender:faxnumber = lv-faxnumber
                     hc-fax:sender:officephone = lv-biznumber
                     hc-fax:sender:homephone = lv-homenumber                     
                     .

              v-job-id = hc-fax:ConnectedSubmit(hC-server BY-POINTER)  NO-ERROR .


              RELEASE OBJECT hC-server.
              RELEASE OBJECT hc-fax.
