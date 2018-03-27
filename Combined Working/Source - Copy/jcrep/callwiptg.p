/* callwiptg.p 

    Program is shell to pass blank input parameter to wipldtg.w
    
    Other programs will pass the actual RM Tag no
    
*/

DEFINE VARIABLE vlc-output AS CHAR INIT "" NO-UNDO.

RUN jcrep/wipldtg.w PERSISTENT 
                    (INPUT "",
                     INPUT "",
                     INPUT 0,
                     INPUT "",
                     INPUT 0,
                     INPUT 0,
                     INPUT 0,
                     INPUT "",
                     OUTPUT vlc-output).
