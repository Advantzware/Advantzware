DEFINE VARIABLE aa AS DECIMAL NO-UNDO FORMAT '>9.9999'.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

OUTPUT TO 'srupe.csv'.
EXPORT DELIMITER ','
  'Bib' 'USAG#' 'First Name' 'Last Name' 'Club' 'DOB' 'Level'
  'Floor' 'Horse' 'Rings' 'Vault' 'PBars' 'HBar' 'AA'.
FOR EACH gymnast NO-LOCK WHERE meet EQ 26,
    FIRST club OF gymnast NO-LOCK:
  aa = 0.
  DO i = 1 TO EXTENT(gymnast.score):
    aa = aa + gymnast.score[i].
  END.
  EXPORT DELIMITER ','
    gymnast.id
    gymnast.usag_id
    gymnast.first_name
    gymnast.last_name
    REPLACE(club.name,'-',' ')
    gymnast.dob
    gymnast.level
    gymnast.score[1]
    gymnast.score[2]
    gymnast.score[3]
    gymnast.score[4]
    gymnast.score[5]
    gymnast.score[6]
    aa.
END.
OUTPUT CLOSE.
OS-COMMAND NO-WAIT START excel.exe srupe.csv.
