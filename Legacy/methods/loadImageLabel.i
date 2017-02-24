/* loadImageLabel.i - rstark - 2.22.2017 */

&IF "{&imageName}" NE "" &THEN
{1}:LOAD-IMAGE ("{&imageFolder}{&imageName}{&imageType}").
    &IF "{&imageType}" EQ ".ico" &THEN
    {1}:LOAD-IMAGE-INSENSITIVE ("{&imageFolder}inactive{&imageType}").
    &ENDIF
&ENDIF
IF {1}:LABEL NE "" THEN 
{1}:LABEL = (IF {1}:LABEL BEGINS "~&" THEN "~&" ELSE "") + "{2}".
