/* setButton.i - rstark - 2.22.2017 */

&SCOPED-DEFINE imageType .png

{methods/buttonImage.i {1} "{2}" {3}}

DO:
    IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
        {methods/loadImageLabel.i {1} "{2}"}
    END.
END.
