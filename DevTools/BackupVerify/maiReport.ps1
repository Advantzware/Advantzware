$MyDomain=’ASI’ 
$MyClearTextUsername=’wade.kaldawi@advantzware.com’ 
$MyClearTextPassword=’Chester1!’
$MyUsernameDomain=$MyClearTextUsername
$SecurePassword=Convertto-SecureString –String $MyClearTextPassword –AsPlainText –force
$MyCredentials=New-object System.Management.Automation.PSCredential $MyUsernameDomain,$SecurePassword
$From = "wade.kaldawi@advantzware.com"
$To = "wade.kaldawi@advantzware.com"
$Subject = "Compile Errors Found"
$Body = "Please Review these errors"
$SMTPServer = "smtp.office365.com"
$SMTPPort = "587"
$Attachments = "c:\temp\errlog.txt"
Send-MailMessage -From $From -to $To  -Subject $Subject `
-Body $Body -SmtpServer $SMTPServer -port $SMTPPort -UseSsl `
-Credential $MyCredentials -Attachments $Attachments