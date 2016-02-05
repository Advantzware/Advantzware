 
 FOR EACH mach.
     IF lookup(mach.industry,",1,2,X") <= 0 THEN do:
         DISP m-code industry ASC(industry) .
         IF ASC(industry) = 32 THEN mach.industry = "".
         PAUSE 0.
     END.
 END.
   
   
