def var lContinue as log no-undo.

message
    "This function will COMPLETELY disable all users'" skip 
    "ability to Add, Create or Delete records anywhere" skip 
    "in the system. This change cannot be undone without" skip 
    "significant manual effort." skip(1)
    "ARE YOU SURE?"
    view-as alert-box question buttons yes-no update lContinue.
    
if lContinue then do:
    for each prgrms exclusive:
        assign 
            can_create = "asi"
            can_delete = "asi"
            can_update = "asi".
        if prgrms.prgmname eq "prgrms." then assign 
            can_run = "asi".
     end.
    message 
        "System now set to 'Archival Mode'"
        view-as alert-box.   
end.
else message
    "Archive mode set was cancelled."
    view-as alert-box.
 
