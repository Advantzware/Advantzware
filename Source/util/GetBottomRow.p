/*********************************************************************************************
PROGRAM NAME : UTIL/GETBOTTOMROW.P
BY AND DATE  : SHASHI BHOSALE ON 2/3/07
DESCRIPTION  : THIS PROGRAM IS USED TO GET THE ROW FROM WHERE REPORT BOTTOM WILL START IN
               EXCEL.
PARAMETER    : IpHeaderLine - # of rows in excel gets printed as Header
               IpDetailLine - How many Detail Lines are allowed per page in excel
               IpBottomLine - How many lines used to print the bottom of the page
               IpLastRow    - Last Row printed in excel
               OpInRowSount - Passes Row# where the bottom will start printing.               

*********************************************************************************************/
DEFINE INPUT PARAMETER IpHeaderLines AS INT NO-UNDO.
DEFINE INPUT PARAMETER IpDetailLines AS INT NO-UNDO.
DEFINE INPUT PARAMETER IpBottomLines AS INT NO-UNDO.
DEFINE INPUT PARAMETER IpLastRow     AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER OpInRowCount AS INT NO-UNDO.

IF IpDetailLines - ( (  IpLastRow  - IpHeaderLines ) MOD IpDetailLines ) >= IpBottomLines THEN
DO:
  OpInrowcount = IpLastRow +
                 IpDetailLines - ( (  IpLastRow  - IpHeaderLines ) MOD IpDetailLines ) -
                 IpBottomLines + 1 .
END.
ELSE DO :
  OpInrowcount = IpLastRow +
                 IpDetailLines - ( (  IpLastRow  - IpHeaderLines ) MOD IpDetailLines ) +
                 IpDetailLines - IpBottomLines + 1.
                 
END.

