/*how to have splash screen/window 

First: create a new program that have a frame defined (f-splash), define the frame with the new shared prhase, then run the second program. 

Second: Inside the second program, define a shared instance for the frame, and when the interface is realized hide the shared frame. 

-------------------- 
Ex.: def new shared frame f-splash 
"Brazil..." 
with width 132 centered row 10 view-as dialog-box. 

run second-program.p. 

----------------- 
def shared frame f-splash 
"Brazil..." 
with width 132 centered row 10 view-as dialog-box. 

Initialized = yes? 
hide frame f-splash no-pause. 

PS: I´m not test this sample. It´s only an idea 4 you... 

====================
If you''re using 9.1C or higher, you can suffice with replacing the splashscreen.bmp in the %dlc%\bin directory. 

Otherwise, check %DLC%\gui\aderes\u-logo.p for an example.. 
 ===================    
Or if you want, just create a welcome .w program with this line in main-block to pause for 2 seconds: 
"IF NOT THIS-PROCEDURE:PERSISTENT THEN 
WAIT-FOR CLOSE OF THIS-PROCEDURE PAUSE 2." 

And then in the main menu just before viewing it "RUN welcome.w ." 

    
    */
