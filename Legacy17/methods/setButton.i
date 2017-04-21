/* setButton.i - rstark - 2.22.2017 */

&SCOPED-DEFINE imageType .png

{methods/buttonImage.i {1} "{2}" {3}}

DO:
    IF slError EQ NO THEN DO: 
        IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
            {methods/loadImageLabel.i {1} "{2}"}
        END. /* if winkitactive */
        IF {1}:LABEL NE "" THEN 
        {1}:LABEL = (IF {1}:LABEL BEGINS "~&" THEN "~&" ELSE "") + "{2}".
    END. /* if slerror */
END. /* do */
