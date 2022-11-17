
DEFINE VARIABLE c1099Code as CHARACTER INIT "1,2,3,7" NO-UNDO.
DEFINE VARIABLE c1099CodeDesc AS CHARACTER NO-UNDO.

ASSIGN
  c1099CodeDesc = "Box 1 Rent,Box 2 Royalties,Box 3 Other Income,Box 7 Non Employee Compensation" .

FOR EACH formLayouts NO-LOCK
    WHERE formLayouts.formType EQ "xPrint"
      AND formLayouts.formGroup EQ "US Tax Forms" 
      AND formLayouts.formID EQ "1099-MISC" BREAK BY formLayouts.formType:

    IF FIRST(formLayouts.formType) THEN
    ASSIGN
        c1099Code = ""
        c1099CodeDesc = "".
          
     ASSIGN
          c1099Code = c1099Code + STRING(formLayouts.formLine) + IF NOT LAST(formLayouts.formType) THEN "," ELSE ""
	  c1099CodeDesc = c1099CodeDesc + STRING(formLayouts.formLineLabel) + IF NOT LAST(formLayouts.formType) THEN "," ELSE "". 

END.
