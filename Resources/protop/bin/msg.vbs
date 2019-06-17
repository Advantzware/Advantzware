' msg.vbs
'
' cscript msg.vbs "Title Text" messageType "Message Body"
'
' messageType:
'
'   0 = Ok Button  
'   1 = Ok/Cancel Button  
'   2 = Abort/Retry/Ignore button  
'   3 = Yes/No/Cancel  
'   4 = Yes/No
'
'  16 – Critical Icon  
'  32 – Warning Icon  
'  48 – Warning Message Icon   
'  64 – Information Icon 

Set objArgs = WScript.Arguments

messageTitle = objArgs(0)
messageType  = objArgs(1)
messageText  = objArgs(2)

X=MsgBox(messageText,messageType,messageText)

