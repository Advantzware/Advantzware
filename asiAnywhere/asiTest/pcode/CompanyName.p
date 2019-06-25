
/*------------------------------------------------------------------------
    File        : CompanyNmae.p
    Purpose     : Company

    Syntax      :

    Description : Return a Company name

    Author(s)   : Kuldeep
    Created     : Sep 28 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER vUserId      AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER vComp        AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vCompName as char no-undo.

if vUserId <> "" then do:

   find first usercomp where
        usercomp.user_id =  vUserId AND
        usercomp.loc = '' AND  
        usercomp.company_default = Yes
        NO-LOCK NO-ERROR.
   assign                       
      vCompName =  if available usercomp THEN usercomp.company ELSE "001".
   
END.
if vComp <> "" then do:
   find first company where company.company = vComp no-lock.
   if available company THEN
      assign 
         vCompName = company.name.
end.
      
/*display vCompName.*/
