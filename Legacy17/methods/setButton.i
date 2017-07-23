/* setButton.i - rstark - 2.22.2017 */

&SCOPED-DEFINE imageType .png

&IF "{&ADM-DISPATCH-QUALIFIER}" EQ "winkit" OR DEFINED(winkitactive) NE 0 &THEN
{methods/buttonImage.i {1} "{2}" {3}}

DO:
    IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
        IF CAN-DO ("Save,Update","{2}") AND hUpdateButton EQ ? THEN 
        hUpdateButton = {1}:HANDLE.
        {methods/loadImageLabel.i {1} "{2}"}
    END.
    IF {1}:LABEL NE "" THEN 
    {1}:LABEL = (IF {1}:LABEL BEGINS "~&" THEN "~&" ELSE "") + "{2}".
END.
&ENDIF
