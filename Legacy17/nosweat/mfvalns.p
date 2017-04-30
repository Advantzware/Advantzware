/* mfvalns.p */
/* ALERT: This program cannot be compiled with the task manager.
   See documented instructions here: P:\ASI Procedures\How to Compile Misc Fields Programs.docx
1.	Run the Addon test application.
2.	Run TF3 (Machine Transactions) and click the red checkbox to run the misc fields program.
3.	Press control-break to quit the program and force exit to the procedure editor. This allows you to keep a connection to the ASINOS database (if you exit the program normally, it will disconnect and remove the ASINOS database and we need it for the compile).
4.	From the procedure editor, go to the data dictionary to confirm presense of the ASINOS database.
5.	Exit to procedure editor and compile the program:   "COMPILE nosweat/mfvalns.p SAVE".
6.	Copy the runcode (mfvalns.r) to four locations:
a.	P:\asi10test\rco1010\nosweat
b.	P:\asi10test\patch\rco1010\nosweat
c.	P:\asi10ship\rco1010\nosweat
d.	P:\asi10ship\patch\rco1010\nosweat
*/

DEFINE INPUT PARAMETER ipcGroup    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKey   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcHeader   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iphSmartMsg AS HANDLE    NO-UNDO.
                            
&SCOPED-DEFINE dbnm NOSWEAT.
{UDF/mfvalues.w}
