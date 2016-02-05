/* t-boxhdr.p */

for each box-design-hdr :
    for each box-design-line of box-design-hdr no-lock:
        
        assign box-design-hdr.company = "001"
               box-design-line.company = "001"
               box-design-hdr.box-text = box-design-hdr.box-text + 
                                         box-design-line.line-text + chr(13).
               box-design-hdr.wcum-score = box-design-hdr.wcum-score +
                                           box-design-line.wcum-score + chr(13).
               box-design-hdr.wscore = box-design-hdr.wscore +
                                       box-design-line.wscore + chr(13).
                                                                     
                                         
               
               
/*============               
               def var i as int no-undo.

for each box-design-hdr  :
     disp design-no est-no lscore.
                                                                     
     do i = 1 to length(wscore).
        disp substring(wscore,i,1)
             asc(substring(wscore,i,1))
             keylabel(asc(substring(wscore,i,1))).                                     
        down.
  /*
       if     asc(substring(wscore,i,1)) = 10 then substring(wscore,i,1) = chr(13).
  */     
   end.
   pause 0.
   end.

=================*/
