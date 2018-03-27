/* edi/monitor.w */

{custom/monitor.w "edi" "edi"}
 
 DO TRANSACTION: 
   {sys/inc/edimonimp.i}
   {sys/inc/ediftp.i}
 END.
  
 DEFINE TEMP-TABLE ttEdiPo
        FIELD ttRecid AS RECID.
         
 DEF TEMP-TABLE ttConfig FIELD exportFormat  AS CHAR
                      FIELD destName AS CHAR FORMAT "x(20)"
                      FIELD ftp-site AS CHAR FORMAT "x(30)"
                      FIELD ftp-user AS CHAR 
                      FIELD ftp-passwd AS CHAR FORMAT "x(12)"
                      FIELD ftp-mode AS CHAR 
                      FIELD ftp-software AS CHAR
                      FIELD ftp-dir AS CHAR 
                      FIELD ftp-binary AS CHAR
                      FIELD ftp-script AS CHAR
                      FIELD ftp-cmd AS CHAR
                      INDEX exportFormat exportFormat
                      INDEX destName IS PRIMARY destName.


/* ************************  Function Prototypes ********************** */


/* **********************  Internal Procedures  *********************** */




PROCEDURE autoFtps:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   RUN EdiAsnftp.
   RUN EdiAckftp.
   

END PROCEDURE.

PROCEDURE autoImport:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cImpxmlfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRecid AS RECID NO-UNDO.
     
  cImpxmlfile = edimonimp-cha.
      
  IF cImpxmlfile <> "" THEN DO:
       RUN oe/oe850imp.p (cImpxmlfile).
/*       INPUT FROM c:\temp\edirec.tmp.     */
/*                                          */
/*          PROMPT cRecid.                  */
/*          CREATE ttEdiPo.                 */
/*          ASSIGN ttEdiPo.ttRecid = cRecid.*/
/*                                          */
           
       FOR EACH EDPOTran WHERE request-date = TODAY:
         RUN oe/oe850ord.p (recid(edpotran)).
       END.
  END.
     

END PROCEDURE.

PROCEDURE EdiAckFtp:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 OUTPUT TO VALUE(".\edi\ftpASNtoSPS.txt").    /* ftp text file */

  PUT UNFORMATTED 
      "open edi.ftp.sunclipse.com" SKIP   /* ftp server ip address */
      "fibreconttest"              SKIP   /* userid */
      "Must1356"                   SKIP   /* password */
      "cd /test/ckots/fibrecont"   SKIP
      "mput " ediftp-cha           SKIP   /* file to transfer */
      "quit" .
  OUTPUT CLOSE.

  OS-COMMAND VALUE("ftp -v -i -s:.\edi\ftpASNtoSPS.txt").
 

END PROCEDURE.

PROCEDURE EdiASNftp:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  OUTPUT TO VALUE(".\edi\ftpASNtoSPS.txt").    /* ftp text file */

  PUT UNFORMATTED 
      "open edi.ftp.sunclipse.com" SKIP   /* ftp server ip address */
      "fibreconttest"              SKIP   /* userid */
      "Must1356"                   SKIP   /* password */
      "cd /test/ckots/fibrecont"   SKIP
      "mput " ediftp-cha           SKIP   /* file to transfer */
      "quit" .
  OUTPUT CLOSE.

  OS-COMMAND VALUE("ftp -v -i -s:.\edi\ftpASNtoSPS.txt").
  

                      
END PROCEDURE.

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored file, create receipt record, post
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF edimonimp-log THEN RUN autoImport.
  IF ediftp-log THEN RUN autoftps.   
      
 
     
END PROCEDURE.


/* ************************  Function Implementations ***************** */

