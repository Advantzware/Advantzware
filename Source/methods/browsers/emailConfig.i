/* emailConfig.i
"ASI" "emailConfig"
"configID" "Config ID" ""
"smtpServer" "SMTP Server" ""
"smtpPort" "SMTP Port" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"yes" "no" "no"
"no"
*/

&Global-define FLDNAME1 emailConfig.configID
&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>9
&Global-define SORTBY-1 BY {&FLDNAME1}
&Global-define IDXNAME1 
&Global-define DESCRIP1 Config ID
&Global-define FLDNAME2 emailConfig.smtpServer
&Global-define FORMAT-2 X(30)
&Global-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Global-define IDXNAME2 
&Global-define DESCRIP2 SMTP Server
&Global-define FLDNAME3 emailConfig.smtpPort
&Global-define DATATYP3 INTEGER
&Global-define FORMAT-3 >>>>9
&Global-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-1}
&Global-define IDXNAME3 
&Global-define DESCRIP3 SMTP Port
&Global-define FLDNAME4 
&Global-define SORTBY-4 
&Global-define IDXNAME4 
&Global-define DESCRIP4 
&Global-define FLDNAME5 
&Global-define SORTBY-5 
&Global-define IDXNAME5 
&Global-define DESCRIP5 
&Global-define FLDNAME6 
&Global-define SORTBY-6 
&Global-define IDXNAME6 
&Global-define DESCRIP6 
&Global-define FLDNAME7 
&Global-define SORTBY-7 
&Global-define IDXNAME7 
&Global-define DESCRIP7 
&Global-define FLDNAME8 
&Global-define SORTBY-8 
&Global-define IDXNAME8 
&Global-define DESCRIP8 
&Global-define FLDNAME9 
&Global-define SORTBY-9 
&Global-define IDXNAME9 
&Global-define DESCRIP9 
&Global-define FLDNAME10 
&Global-define SORTBY-10 
&Global-define IDXNAME10 
&Global-define DESCRIP10 
&Global-define FLDNAME11 
&Global-define SORTBY-11 
&Global-define IDXNAME11 
&Global-define DESCRIP11 
&Global-define FLDNAME12 
&Global-define SORTBY-12 
&Global-define IDXNAME12 
&Global-define DESCRIP12 
&Global-define FLDNAME13 
&Global-define SORTBY-13 
&Global-define IDXNAME13 
&Global-define DESCRIP13 
&Global-define ENHANCE yes
