/* genButtonImage.p */

DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImage AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttButtonImages NO-UNDO
    FIELD labelName AS CHARACTER
    FIELD imageName AS CHARACTER
        INDEX ttButtonImage AS PRIMARY
              labelName.
/* My new comment explaining new feature */

INPUT FROM "C:\Advantzware\v16\Scripts\Conv16to17\ScriptsDat\buttonImage.dat" NO-ECHO.
IMPORT ^.
REPEAT:
    IMPORT DELIMITER "~t" cLabel cImage.
    CREATE ttButtonImages.
    ASSIGN
        ttButtonImages.labelName = cLabel
        ttButtonImages.imageName = cImage
        .
/* new comment */
END.
INPUT CLOSE.

OUTPUT TO "c:\Advantzware\v16\Legacy17\methods\buttonImage.i".
PUT UNFORMATTED
    "/* buttonImage.i - rstark - 2.22.2017 */" SKIP
    "/* generated " STRING(TODAY,"99.99.9999") " @ " STRING(TIME,"hh:mm:ss am") " */" SKIP(1)
    "~&GLOBAL-DEFINE imageName" SKIP
    "~&GLOBAL-DEFINE imageFolder Graphics/32x32/" SKIP
    "~&IF ~"~{3}~" NE ~"~" ~&THEN" SKIP
    "~&GLOBAL-DEFINE imageFolder Graphics/~{3}x~{3}/" SKIP
    "~&ENDIF" SKIP(1)
    "    ~&IF ~"~{2}~" EQ ~" ~" ~&THEN ~&GLOBAL-DEFINE imageName inactive" SKIP
    .
FOR EACH ttButtonImages:
    IF ttButtonImages.imageName NE "" THEN
    PUT UNFORMATTED
        "~&ELSEIF ~"~{2}~" EQ ~"" ttButtonImages.labelName
        "~" ~&THEN ~&GLOBAL-DEFINE imageName " ttButtonImages.imageName
        SKIP.
END.
PUT UNFORMATTED "~&ENDIF" SKIP.
OUTPUT CLOSE.
