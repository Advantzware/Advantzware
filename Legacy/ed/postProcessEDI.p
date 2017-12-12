input from c:\temp\o810.txt.

def var inln as char no-undo.
def var cOutLine as char no-undo.
def var iDelimPos as int no-undo.
def var iElemNum as int no-undo.
def var cElem as char no-undo.
def var lISASent as log no-undo.
def var iISAControlNum as int no-undo.
def var cSenderDuns as char no-undo init "PREMIER        ". 
  def var cISASeg as char.
  def var cGSSeg as char.
  def var cEleDelim as char init "*".
  def var cSTSeg as char.
  def var iSTControl as int.
def var iSegmentcount as int.
def var iCurrentElem as int.
def var cSegment as char.

lISASent = NO.
iISAControlNum = 35.
iSTControl = 27.

def stream sOutput.
output stream sOutput to c:\temp\o810-clean.txt.
repeat:
  inln = "".
  cOutLine = "".
  import delimiter "|" inln.
  
  cSegment = "".
  iCurrentElem = 0.
  do iDelimPos = 1 to num-entries(inln, "*"):
    cElem = "".
    
    if iDelimPos = 1 or
      iDelimPos = 2 or
      iDelimPos = 4 or 
      iDelimPos = 5 then 
      next.
      
    if iDelimPos GT 2 then do:
    
      cElem = trim(entry(iDelimPos, inln, "*")).
      if cElem begins "00000000000" then 
        cElem = "GS".
        
      if cSegment eq "" then 
        cSegment = cElem.
      else
        iCurrentElem = iCurrentElem + 1.
        
      if cSegment EQ "IT1" and iCurrentElem EQ 5 then 
        cElem = "*" + cElem.
      if cSegment EQ "IT1" and iCurrentElem EQ 2 then 
        cElem = string(integer(cElem)).
      if cSegment EQ "IT1" and iCurrentElem EQ 4 then 
        assign
          cElem = substring(cElem, 2, 13) + "." + 
                  substring(cElem, 15, 4)
          cElem = string(decimal(cElem)).
      

      if cSegment EQ "TDS" and iCurrentElem EQ 1 then
      assign
        cElem = substring(cElem, 2)
        cElem = string(integer(cElem))
        .
        
      if not (cSegment EQ "TDS" and iCurrentElem gt 1) then
      cOutLine = cOutLine + "*" + cElem. 
         
    end.
    
  end. /* examine each element of segment */
  cOutLine = trim(cOutLine, "*") + "~~".
      disp cOutline.
    down.
  

  if not lISASEnt then do:
    cISASeg = "ISA" + cEleDelim + 
              "00"  + cEleDelim + 
              fill(" ", 10) + cEleDelim  +
              "00"  + cEleDelim + 
              fill(" ", 10) + cEleDelim  +
              "ZZ"  + cEleDelim + 
              cSenderDuns + cEleDelim + 
              "ZZ"  + cEleDelim + 
              "AMAZON         " + cEleDelim +               
              substring(string(year(today), "9999"),3, 2) + 
              string(month(today), "99") + 
              string(day(today), "99") + cEleDelim +               
              substring(string(time, "hh:mm"), 1, 2) 
                 + substring(string(time, "hh:mm"), 4, 2) 
                 + cEleDelim +                
              "U" + cEleDelim +               
              "00401" + cEleDelim +               
              string(iISAControlNum, "99999999999") + cEleDelim +               
              "O" + cEleDelim +               
              "I" + cEleDelim +           
              ">" +
              "~~".
              
    put stream sOutput unformatted cISASeg skip.
    cGsSeg = "GS" + cEleDelim + 
             "IN" + cEleDelim + 
             cSenderDuns + cEleDelim + 
             "AMAZON" + cEleDelim + 
              substring(string(year(today), "9999"),1, 4) + 
              string(month(today), "99") + 
              string(day(today), "99") + cEleDelim +               
              substring(string(time, "hh:mm"), 1, 2) 
                 + substring(string(time, "hh:mm"), 4, 2) 
                 + cEleDelim + 
              string(iISAControlNum) + cEleDelim +
              "X" + cEleDelim + 
              "004010" + 
              "~~"
             . 
    put stream sOutput unformatted cGsSeg skip.
    
        cStSeg = "ST" + cEleDelim + 
                 "810" + cEleDelim + 
                 string(iStControl, "9999") + cEleDelim + 
                "~~"
             . 
    
        put stream sOutput unformatted cStSeg skip.
    lISASent = true.

  end.
  else DO:
    put stream sOutput unformatted cOutLine skip.  
    iSegmentCount = iSegmentCount + 1.
  end.
end.
if lISASent then do:
  iSegmentCount = iSegmentCount + 2.
  put stream sOutput unformatted 
                         "SE" + cEleDelim +
                         string(iSegmentCount) + cEleDelim
                         string(iSTControl) 
                         + "~~"
                         skip
                         .
  

  put stream sOutput unformatted 
                         "GE" + cEleDelim +
                         "1" + cEleDelim
                         string(iISAControlNum) 
                         + "~~"
                         skip
                         .
  put stream sOutput unformatted 
                         "IEA" + cEleDelim +
                         "1" + cEleDelim
                         string(iISAControlNum, "999999999") 
                         + "~~"
                         skip
                         .                         
end.
output stream sOutput close.
dos silent notepad c:\temp\o810-clean.txt. 
