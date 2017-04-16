/* custom/playsound.p  YSK  5 example included
*/

&GLOB SND_ASYNC 1
&GLOB SND_NODEFAULT 2
&GLOB SND_LOOP 8
&GLOB SND_PURGE 64
&GLOB SND_APPLICATION 128
&GLOB SND_ALIAS 65536
&GLOB SND_FILENAME 131072
&GLOB SND_RESOURCE 262148
 
PROCEDURE PlaySoundA EXTERNAL "winmm.dll" PERSISTENT :
  DEFINE INPUT PARAMETER  pszSound    AS LONG.
  DEFINE INPUT PARAMETER  hmod        AS LONG.
  DEFINE INPUT PARAMETER  fdwSound    AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

/*=========  examples =====*/

/*===========
/* example 1  
In the first example, the first parameter is interpreted as a filename.
De default system sound ('ting') will be played if the specified filename can not be found, 
because the flag SND_NODEFAULT is not specified. 
*/

DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
DEFINE VARIABLE szSound     AS MEMPTR  NO-UNDO.
DEFINE VARIABLE wavfile     AS CHAR    NO-UNDO.
 
wavfile = "c:\windows\media\logoff.wav".
SET-SIZE(szSound) = LENGTH(wavfile, "raw":U) + 1.
PUT-STRING(szSound,1) = wavfile.
 
RUN PlaySoundA (GET-POINTER-VALUE(szSound), 
                0, 
                {&SND_FILENAME},
                OUTPUT ReturnValue). 
SET-SIZE(szSound) = 0.
/*=========== end example 1 ====================*/
*/

/* example 2
The next example plays the system sound associated in Registry with eventname "SystemExit". This sound and 
others are found in registry key "HKCU\AppEvents\Schemes\Apps\.Default". 
*/
DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
DEFINE VARIABLE szSound     AS MEMPTR  NO-UNDO.
DEFINE VARIABLE eventname   AS CHAR    NO-UNDO.
 
eventname = "SystemExit".
SET-SIZE(szSound) = LENGTH(eventname, "raw":U) + 1.
PUT-STRING(szSound,1) = eventname.
 
RUN PlaySoundA (GET-POINTER-VALUE(szSound), 
                0, 
                {&SND_ALIAS} + {&SND_NODEFAULT},
                OUTPUT ReturnValue).
SET-SIZE(szSound) = 0.
/* =========== example 2 end ========*/

/*========
/* example 3
The next example plays an application-specific sound event. 
These can be registered in key "HKCU\AppEvents\Schemes\Apps\prowin32". 
*/

DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
DEFINE VARIABLE szSound     AS MEMPTR  NO-UNDO.
DEFINE VARIABLE eventname   AS CHAR    NO-UNDO.
 
eventname = "CUSTOMER_DELETED".
SET-SIZE(szSound) = LENGTH(eventname, "raw":U) + 1.
PUT-STRING(szSound,1) = eventname.
 
RUN PlaySoundA (GET-POINTER-VALUE(szSound), 
                0, 
                {&SND_APPLICATION} + {&SND_NODEFAULT},
                OUTPUT ReturnValue).
SET-SIZE(szSound) = 0.

/* =========== example 3 end ========*/ 

/* example 4
It is also possible to link WAV resources into an executable or DLL.
Suppose the DLL is identified by handle hSounds and contains a sound resource named "LOGIN_REJECTED" : 
*/

DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
DEFINE VARIABLE szSound     AS MEMPTR  NO-UNDO.
DEFINE VARIABLE eventname   AS CHAR    NO-UNDO.
 
eventname = "LOGIN_REJECTED".
SET-SIZE(szSound) = LENGTH(eventname, "raw":U) + 1.
PUT-STRING(szSound,1) = eventname.
 
RUN PlaySoundA (GET-POINTER-VALUE(szSound), 
                hSounds, 
                {&SND_RESOURCE} + {&SND_NODEFAULT},
                OUTPUT ReturnValue).
SET-SIZE(szSound) = 0.
/* example 4 end ============*/
 


/*example 5
This example is submitted by Nenad Orlovic [norlovic@zg.tel.hr].
Sounds should always be played asynchronous, especially when it is a long sound clip, so the program can continue while the sound is playing. This is done by adding the SND_ASYNC flag. This is only possible when the PlaySoundA procedure is declared as persistent: otherwise Progress would free the winmm.dll library immediately which would cause the sound to stop. The example also uses SND_LOOP to repeat the sound. 
SND_LOOP can not be used without SND_ASYNC. 
*/

DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
DEFINE VARIABLE szSound     AS MEMPTR  NO-UNDO.
DEFINE VARIABLE wavfile     AS CHAR    NO-UNDO.
 
wavfile = "c:\windows\media\logoff.wav".
SET-SIZE(szSound) = LENGTH(wavfile, "raw":U) + 1.
PUT-STRING(szSound,1) = wavfile.
 
RUN PlaySoundA (GET-POINTER-VALUE(szSound),
                0,
                {&SND_FILENAME} + {&SND_ASYNC} + {&SND_LOOP} + {&SND_NODEFAULT},
                OUTPUT ReturnValue).
SET-SIZE(szSound) = 0.
 
MESSAGE "Press OK to stop the music" VIEW-AS ALERT-BOX.
 
RUN PlaySoundA (0,
                0,
                {&SND_PURGE},
                OUTPUT ReturnValue).
 
MESSAGE "The music stopped" VIEW-AS ALERT-BOX.
/* example 5 end ==================*/
 
*/

