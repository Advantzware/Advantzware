/* XMLOutput.i - RStark - Task: 05181205 (XML), 05291402 (cXML) */
/* syntax: {XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XML??? &Company=cocode} -or-
           {XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXML??? &Company=cocode &c=c} -or-
           {XMLOutput.i &XMLOutput=XML???} -or-
           {XMLOutput.i &cXMLOutput=cXML??? &c=c} -or-
           {XMLOutput.i &XMLOutput=XML??? &Company=cocode} -or-
           {XMLOutput.i &cXMLOutput=cXML??? &Company=cocode &c=c} -or-
           {XMLOutput.i &XMLClose} -or-
           {XMLOutput.i &c=c &XMLClose} */

&IF DEFINED(XMLClose) EQ 0 &THEN
  DEFINE {&NEW} SHARED VARIABLE {&c}cXMLOutput AS CHARACTER NO-UNDO.
  DEFINE {&NEW} SHARED VARIABLE {&c}lXMLOutput AS LOGICAL NO-UNDO.
  DEFINE {&NEW} SHARED VARIABLE {&c}iXMLOutput AS INTEGER NO-UNDO.
  
  DEFINE {&NEW} SHARED STREAM {&c}XMLOutput.

  DEFINE VARIABLE {&c}XMLHeader AS LOGICAL NO-UNDO INITIAL TRUE.
  DEFINE VARIABLE {&c}XMLLineNumber AS INTEGER NO-UNDO.
  DEFINE VARIABLE {&c}XMLPage AS LOGICAL NO-UNDO.
  
  &IF DEFINED({&c}XMLSysCtrl) NE 0 &THEN
    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ  {&Company}
           AND sys-ctrl.name    EQ '{&{&c}XMLSysCtrl}' NO-ERROR.
    IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN
    {&c}iXMLOutput = sys-ctrl.int-fld.
  &ENDIF
  
  &IF '{&{&c}XMLOutput}' NE '' &THEN
    &IF DEFINED(Company) NE 0 &THEN
      DEFINE VARIABLE {&c}XMLTemp AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLTimeStamp AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLPayloadID AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLIdentity AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLIdentityCust AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLSharedSecret AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLProduction AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLProcessID AS CHARACTER NO-UNDO.
      DEFINE VARIABLE {&c}XMLDTD AS CHARACTER NO-UNDO.

      IF {&c}lXMLOutput THEN DO:
        FIND FIRST sys-ctrl NO-LOCK
             WHERE sys-ctrl.company EQ  {&Company}
               AND sys-ctrl.name    EQ '{&{&c}XMLOutput}' NO-ERROR.
        IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
          ASSIGN
            {&c}XMLTimeStamp = STRING(YEAR(TODAY),'9999')
                             + '-'
                             + STRING(MONTH(TODAY),'99')
                             + '-'
                             + STRING(DAY(TODAY),'99')
                             + 'T'
                             + STRING(TIME,'hh:mm:ss')
                             + '-05:00'
            {&c}cXMLOutput = sys-ctrl.char-fld
            {&c}iXMLOutput = sys-ctrl.int-fld
            .
          &IF DEFINED(c) EQ 0 &THEN
          ASSIGN
            XMLFile = '/{&XMLOutput}.' + STRING(TIME,'99999') + STRING(RANDOM(1, 999))+ '.xml'
            XMLTemp = 'XMLOutput' + XMLFile
            .
          OUTPUT STREAM XMLOutput TO VALUE(XMLTemp).
          &ENDIF
        END.
        ELSE {&c}lXMLOutput = NO.
      END. /* if lXMLOutput */
    &ENDIF
    
    &IF '{&c}' EQ 'c' &THEN
      FUNCTION getPayLoadID RETURN CHARACTER (ipProcessID AS CHARACTER):
        DEFINE VARIABLE cPayLoadID AS CHARACTER NO-UNDO.
  
        ASSIGN
          cPayLoadID = STRING(NOW)
          cPayLoadID = REPLACE(cPayLoadID,'/','')
          cPayLoadID = REPLACE(cPayLoadID,' ','')
          cPayLoadID = REPLACE(cPayLoadID,' ','')
          cPayLoadID = REPLACE(cPayLoadID,':','')
          cPayLoadID = REPLACE(cPayLoadID,'-','')
          cPayLoadID = REPLACE(cPayLoadID,'.','')
          cPayLoadID = cPayLoadID + IF ipProcessID NE '' THEN '.' + ipProcessID ELSE ''
          cPayLoadID = cPayLoadID + '.' + STRING(RANDOM(1000,9999),'9999')
          cPayLoadID = cPayLoadID + '@PremPack.com'
          .
  
        RETURN cPayLoadID.
      END FUNCTION.
      /* syntax: RUN XMLOutput (lXMLOutput,'<Label>',<Value>,'<Header|Element|Value>' */
      PROCEDURE cXMLOutput:
        DEFINE INPUT PARAMETER plOutput AS LOGICAL NO-UNDO.
        DEFINE INPUT PARAMETER pcLabel AS CHARACTER NO-UNDO.
        DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.
        DEFINE INPUT PARAMETER pcType AS CHARACTER NO-UNDO.

        IF NOT plOutput THEN RETURN.

        CASE pcType:
          WHEN 'Header' THEN
          PUT STREAM cXMLOutput UNFORMATTED
            '<?xml version="1.0" encoding="UTF-8" ?>'
            '<!DOCTYPE cXML SYSTEM "' + cXMLDTD + '">'
            '<cXML payloadID="' getPayloadID(cXMLProcessID) '" timestamp="' cXMLTimeStamp '" xml:lang="en-US" version="1.2.024">'
            '<Header>'
            '<From>'
            '<Credential domain="NetworkID">'
            '<Identity>' + cXMLIdentityCust + '</Identity>'
			'<SharedSecret>' + cXMLSharedSecret + '</SharedSecret>'
            '</Credential>'
            '</From>'
            '<To>'
            '<Credential domain="NetworkID">'
            '<Identity>' cXMLIdentity '</Identity>'
            '</Credential>'
            '</To>'
            '<Sender>'
            '<Credential domain="NetworkID">'
            '<Identity>' + cXMLIdentityCust + '</Identity>'
			'<SharedSecret>' + cXMLSharedSecret + '</SharedSecret>'
            '</Credential>'
            '<UserAgent>Supplier</UserAgent>'
            '</Sender>'
            '</Header>'
            .
          WHEN 'Close' THEN
          PUT STREAM cXMLOutput UNFORMATTED '</cXML>' SKIP.
          WHEN 'Row' THEN
          PUT STREAM cXMLOutput UNFORMATTED '<' pcLabel '>'.
          WHEN 'Col' THEN
          IF pcValue NE '' AND pcValue NE ? THEN DO:
            ASSIGN /* remove special characters with escape values */
              pcValue = REPLACE(pcValue,'~&','~&amp;')
              pcValue = REPLACE(pcValue,'~'','~&apos;')
              pcValue = REPLACE(pcValue,'"','~&quot;')
              pcValue = REPLACE(pcValue,'<','~&lt;')
              pcValue = REPLACE(pcValue,'>','~&gt;')
              .
            IF pcLabel NE '' AND pcLabel NE ? THEN
            PUT STREAM cXMLOutput UNFORMATTED '<' pcLabel '>' pcValue '</' pcLabel '>'.
            ELSE
            PUT STREAM cXMLOutput UNFORMATTED pcValue.
          END.
          ELSE
          PUT STREAM cXMLOutput UNFORMATTED '<' pcLabel '/>'.
        END CASE.
      END PROCEDURE.
    &ELSE
      /* syntax: RUN XMLOutput (lXMLOutput,'<Label>',<Value>,'<Header|Element|Value>' */
      PROCEDURE XMLOutput:
        DEFINE INPUT PARAMETER plOutput AS LOGICAL NO-UNDO.
        DEFINE INPUT PARAMETER pcLabel AS CHARACTER NO-UNDO.
        DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.
        DEFINE INPUT PARAMETER pcType AS CHARACTER NO-UNDO.

        IF NOT plOutput THEN RETURN.

        CASE pcType:
          WHEN 'Header' THEN
          PUT STREAM XMLOutput UNFORMATTED '<?xml version="1.0"?>'
            '<dsXML xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
          WHEN 'Close' THEN
          PUT STREAM XMLOutput UNFORMATTED '</dsXML>' SKIP.
          WHEN 'Page' THEN DO:
            XMLLineNumber = 0.
            IF pcLabel EQ 'Last' THEN
            PUT STREAM XMLOutput UNFORMATTED '</Page_' pcValue '>'.
            ELSE DO:
              IF INT(pcValue) GT 1 AND XMLPage THEN
              PUT STREAM XMLOutput UNFORMATTED '</Page_' INT(pcValue) - 1 '>'.
              PUT STREAM XMLOutput UNFORMATTED '<Page_' pcValue '>'.
              XMLPage = YES.
            END.
          END.
          WHEN 'Row' THEN
          PUT STREAM XMLOutput UNFORMATTED '<' pcLabel '>'.
          WHEN 'Col' THEN
          IF pcValue NE '' AND pcValue NE ? THEN DO:
            ASSIGN /* remove special characters with escape values */
              pcValue = REPLACE(pcValue,'~&','~&amp;')
              pcValue = REPLACE(pcValue,'~'','~&apos;')
              pcValue = REPLACE(pcValue,'"','~&quot;')
              pcValue = REPLACE(pcValue,'<','~&lt;')
              pcValue = REPLACE(pcValue,'>','~&gt;')
              .
            PUT STREAM XMLOutput UNFORMATTED '<' pcLabel '>' pcValue '</' pcLabel '>'.
          END.
          ELSE
          PUT STREAM XMLOutput UNFORMATTED '<' pcLabel '/>'.
        END CASE.
      END PROCEDURE.
    &ENDIF
  &ENDIF
&ELSE
  IF {&c}lXMLOutput THEN DO:
    RUN {&c}XMLOutput ({&c}lXMLOutput,'','','Close').
    OUTPUT STREAM {&c}XMLOutput CLOSE.
    OS-RENAME VALUE({&c}XMLTemp) VALUE({&c}cXMLOutput + {&c}XMLFile).
    &IF '{&c}' EQ 'c' &THEN
    OUTPUT STREAM cXMLOutput TO 'XMLOutput/{&sysCtrlcXML}.log' APPEND.
    PUT STREAM cXMLOutput UNFORMATTED
      'Transmitted ' TODAY ' @ ' STRING(TIME,'hh:mm:ss am')
      ' File: ' ccXMLOutput + cXMLFile SKIP.
    OUTPUT STREAM cXMLOutput CLOSE.
    &ENDIF
  END. /* if lxmloutput */
&ENDIF
