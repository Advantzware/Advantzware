    DEFINE VARIABLE wave-name   AS CHARACTER NO-UNDO INITIAL ?.
    DEFINE VARIABLE play-status AS INTEGER   NO-UNDO.

/*SYSTEM-DIALOG GET-FILE wave-name
  TITLE "Choose the Sound"
  FILTERS "Wave Files (*.wav)" "*.wav"
  MUST-EXIST USE-FILENAME.*/
  
    wave-name = "C:\Users\seema.rani\Desktop\Tickets\Success.wav".

    RUN sndPlaySoundA (
        INPUT wave-name, 
        INPUT 2,
        OUTPUT play-status
        ).

    PROCEDURE sndPlaySoundA EXTERNAL "winmm.dll":
        DEFINE INPUT  PARAMETER ic  AS CHARACTER.
        DEFINE INPUT  PARAMETER ish AS LONG.
        DEFINE OUTPUT PARAMETER osh AS LONG.
    END PROCEDURE.

/*You can select any .wav file from the below given directory:
C:\Windows\Media*/
