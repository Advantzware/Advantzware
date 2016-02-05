/*r-bkgrnd.p*/

DEFINE VARIABLE item-tot AS DECIMAL LABEL "Value" NO-UNDO.
DEFINE RECTANGLE vline1 SIZE .4 BY 5 EDGE-PIXELS 2.
DEFINE RECTANGLE vline2 LIKE vline1.
DEFINE RECTANGLE vline3 LIKE vline1.
DEFINE RECTANGLE vline4 LIKE vline1.
DEFINE RECTANGLE vline5 LIKE vline1.
DEFINE RECTANGLE vline6 LIKE vline1.
DEFINE RECTANGLE hline SIZE 78 BY .1 EDGE-PIXELS 2.
                    
 DEFINE FRAME item-info
             item.i-no  item.i-name
              BACKGROUND skip(1) hline    vline1 AT 9  
       vline2 AT 25    vline3 AT 33    vline4 AT 42    vline5 AT 51
          vline6 AT 65  WITH TITLE "Inventory Current Value" 
                    CENTERED USE-TEXT 5 DOWN.  
   
   FOR EACH item NO-LOCK WITH FRAME item-info:
        DISPLAY 
