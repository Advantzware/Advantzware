/* folderLookup.i - rstark - 1.14.2021 */

selected-name = SELF:SCREEN-VALUE.
SYSTEM-DIALOG GET-DIR selected-name
    INITIAL-DIR selected-name
    TITLE "Select Folder"
    .
IF selected-name NE ? THEN
SELF:SCREEN-VALUE = selected-name.
ELSE
RETURN NO-APPLY.
