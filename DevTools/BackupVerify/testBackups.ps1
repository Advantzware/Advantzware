C:
cd \asigui\Databases\customer\backupTest
get-date > c:\asigui\databases\customer\backupTest\backupReport.txt
get-date > c:\asigui\databases\customer\backupTest\debugLog.txt
$firstOrderReport = "c:\asigui\databases\customer\backupTest\firstOrderReport.txt"
$errorReport = "c:\temp\backupErrors.txt"
$backupReport = "c:\asigui\databases\customer\backupTest\backupReport.txt"
$debuglog = "c:\asigui\databases\customer\backupTest\debuglog.txt"
$finalBackupReport = "c:\temp\backupReport.txt"
$restoreErrsFile = "c:\asigui\databases\customer\backupTest\restoreErrs.txt"
$restoreLog = "c:\asigui\databases\customer\backupTest\restore.txt"
Function Get-CurrentLine {
    $Myinvocation.ScriptlineNumber
}

if ( Test-Path $restoreErrsFile ) {
    Remove-Item –path $restoreErrsFile
}

if ( Test-Path $restoreLog ) {
  Remove-Item –path $restoreLog
}
$Systems = import-csv “systemlist.csv”
#ForEach ($system in Get-Content "systemlist1.txt")
ForEach ($item in $Systems)
{
  $system = $item.("ID")
  $systemName = $item.("Name")
  write-output $system
  write-output $systemName

# Start Report
  Add-Content $backupReport "System:"
  $system >> $backupReport
  if ( Test-Path $firstOrderReport ) {
    Remove-Item –path $firstOrderReport
  }
  

  # Initialize to very early dates for comparison with backups that are found
  $latestBackupFile = ""
  $latestBackupFileDate = (get-date).adddays(-780)
  $latestStFile = ""
  $latestStFileDate = (get-date).adddays(-780)
  
  # starting date - look for files after this
  $afterdate = (get-date).adddays(-780)

  $obj = get-S3Object -BucketName "Online-Backups" -keyprefix $system/C:/asigui/Backups/Databases/asiProd -maxkey 100
  # If nothing try the E: drive
  if ($obj -eq $null) {    
    $obj = get-S3Object -BucketName "Online-Backups" -keyprefix $system/E:/asigui/Backups/Databases/asiProd -maxkey 100
  } 
  $earlyDate = (get-date).adddays(-730)

  # Grab backups found for this system and review each to find most recent
  ForEach ($obj2 in $obj)
  {
     
    $sLastMod = $obj2.LastModified
    $kname = $obj2.Key

      ForEach ($sfilename in $kname)
      {
               
         $newfile = Split-Path -Path $sfilename -Leaf         
         $lline = Get-CurrentLine
         $i = 0
         while ($i -le 99){
           
           $i += 1
           $fileNumstr = "asiProd" + $i.ToString("00") + ".bak"
          
            if ($newfile -eq $fileNumStr)
             {
               "Test $lline $newfile $fileNumStr" | out-file -filepath c:\asigui\databases\customer\backupTest\debugLog.txt -append -width 200
               $BackupFile01 = $sfilename
               read-S3Object -BucketName "Online-Backups" -Key $sfilename -File $fileNumstr        
               IF (Test-Path $fileNumStr) 
               {
                 $newName = $fileNumStr + ".gz"
                 Rename-item -path $fileNumstr -newName $newName
                 & "c:\temp\wade\gzip\bin\gzip" -d $newName
               }       
             }
 
         }
    }
    $lline = Get-CurrentLine
     "$lline $sLastMod $afterdate" | out-file -filepath c:\asigui\databases\customer\backupTest\debugLog.txt -append -width 200
    if ($sLastMod -ge $afterdate) {
      # Examine files in object key
      ForEach ($sfilename in $kname)
      {
       
        
         $newfile = Split-Path -Path $sfilename -Leaf
         "$newfile $sLastMod $earlydate"  | out-file -filepath c:\asigui\databases\customer\backupTest\debugLog.txt -append -width 200
         # if the file found is newer, use it as the most recent backup
         if ($sLastMod -ge $earlyDate) {
         
             if ($newfile -eq "asiProd.bak")
             {
               $latestBackupFile = $sfilename
               $latestBackupFileDate = $sLastMod
               
             }

             $earlyDate = $sLastMod
               
         }         
       
      }
       
    }
    
  }
  
  # Most recent backup was identified
   "$system $latestStFile $BackupFile01 $latestBackupFileDate $latestStFile $latestStFileDate"  | out-file -filepath c:\asigui\databases\customer\backupTest\debugLog.txt -append -width 200

   if (-not ([string]::IsNullOrEmpty($latestStFile))) {
     read-S3Object -BucketName "Online-Backups" -Key $latestStFile -File asiProd.st     
   }

   # If a database backup was found, try to restore it and report on it
   if (-not ([string]::IsNullOrEmpty($latestBackupFile))) { 
     read-S3Object -BucketName "Online-Backups" -Key $latestBackupFile -File asiProd.bak

     $latestBackupFileDate = 'BackupFileDate=' + $latestBackupFileDate
     $latestBackupFile = 'BackupFileName=' + $latestBackupFile
     Add-Content $backupReport $latestBackupFileDate
     Add-Content $backupReport $latestBackupFile

     $fileNumStr = "c:\asigui\databases\customer\backupTest\asiProd01.bak"
      "$fileNumStr"  | out-file -filepath c:\asigui\databases\customer\backupTest\debugLog.txt -append -width 200
     IF (Test-Path $fileNumStr) 
     {
      "$system"  | out-file -filepath c:\asigui\databases\customer\backupTest\debugLog.txt -append -width 200
        Remove-item "asiProd.bak"
        . .\Split-File.ps1
         
        dir asiProd??.bak | Join-File c:\asigui\databases\customer\backupTest\asiProd.bak
         
     }  

     
     c:\asigui\databases\customer\backupTest\RestoreBackup.bat
     
     $custSystemDir = $system.Substring($system.IndexOf("_") + 1)
     $custSystemDir = $custSystemDir.Replace(' ', '_')
     if ( Test-Path ..\$systemName ) {
       copy asiProd.bak ..\$systemName
     }
     if ( Test-Path $firstOrderReport ) {
       Get-Content firstOrderReport.txt | Add-Content $backupReport
     }

     Get-Content c:\asigui\databases\customer\backupTest\backupRestoreErrs.txt | Add-Content $backupReport
   }
  # pause
   
}
c:\asigui\databases\customer\backupTest\runReport.bat

if ( Test-Path $errorReport ) {
    if((Get-Item $errorReport).length -gt 0kb){
    Get-Content $restoreErrsFile | Add-Content $errorReport
    $MyDomain=’ASI’ 
    $MyClearTextUsername=’wade.kaldawi@advantzware.com’ 
    $MyClearTextPassword=’Chester1!’
    $MyUsernameDomain=$MyClearTextUsername
    $SecurePassword=Convertto-SecureString –String $MyClearTextPassword –AsPlainText –force
    $MyCredentials=New-object System.Management.Automation.PSCredential $MyUsernameDomain,$SecurePassword
    $From = "wade.kaldawi@advantzware.com"
    $To = "wade.kaldawi@advantzware.com"
    $Subject = "Customer Db Restore Errors Found"
    $Body = "Please Review these errors"
    $SMTPServer = "smtp.office365.com"
    $SMTPPort = "587"
    $Attachments = $errorReport
    Send-MailMessage -From $From -to $To  -Subject $Subject `
    -Body $Body -SmtpServer $SMTPServer -port $SMTPPort -UseSsl `
    -Credential $MyCredentials -Attachments $Attachments
    }
}
if ( Test-Path $finalBackupReport ) {
   
    $MyDomain=’ASI’ 
    $MyClearTextUsername=’wade.kaldawi@advantzware.com’ 
    $MyClearTextPassword=’Chester1!’
    $MyUsernameDomain=$MyClearTextUsername
    $SecurePassword=Convertto-SecureString –String $MyClearTextPassword –AsPlainText –force
    $MyCredentials=New-object System.Management.Automation.PSCredential $MyUsernameDomain,$SecurePassword
    $From = "wade.kaldawi@advantzware.com"
    $To = "wade.kaldawi@advantzware.com"
    $Subject = "Customer Db Restore Report"
    $Body = "Please Review these errors"
    $SMTPServer = "smtp.office365.com"
    $SMTPPort = "587"
    $Attachments = $finalBackupReport
    Send-MailMessage -From $From -to $To  -Subject $Subject `
    -Body $Body -SmtpServer $SMTPServer -port $SMTPPort -UseSsl `
    -Credential $MyCredentials -Attachments $Attachments
}