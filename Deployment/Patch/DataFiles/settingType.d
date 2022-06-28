"202204291401109351779" 714 "Attribute Name of the user count field in Zoho Desk" "Character" "" "cf_advantzware_user_count" "" "" no "ZohoDesk" "ZohoUserCountAttribute" 1000 no
"202204291396809351777" 713 "Advantzware Account ID" "Character" "" "" "" "" no "ZohoDesk" "AdvantzwareAccountID" 1000 no
"202204248562509317253" 712 "Backorders not automatically released
Yes - allow back orders to create a planned release and release it to an actual release
No - allow back orders to create a planned release, but not an actual release" "Logical" "Yes,No" "Yes" "" "" no "" "OEBackorderAutoRelease" 0 no
"202203312081309179412" 711 "Set the default status of the Vendor Item Cost upon creation to be Approved" "Logical" "Yes,No" "No" "" "" no "FGItemApproval" "DefaultVendorCostStatusFGIApproval" 0 no
"202203145437509094602" 709 "FTP Request Method" "Character" "cURL,Internal" "cURL" "" "" no "" "FTPRequestMethod" 0 no
"202203145080109094077" 708 "Finished good item mask to auto-number finished good items" "Character" "" "TLLWW9999" "" "" no "AutoNumberMasks" "FinishedGoodItemMask" 0 no
"202203145069609094075" 707 "Raw material item mask to auto-number raw material items" "Character" "" "TLLWW9999" "" "" no "AutoNumberMasks" "RawMaterialItemMask" 0 no
"202203145064009094073" 706 "Vendor mask to auto-number vendor numbers" "Character" "" "CCC9999" "" "" no "AutoNumberMasks" "VendorNumberMask" 0 no
"202203145057209094071" 705 "Customer mask to auto-number of customer numbers" "Character" "" "CCC9999" "" "" no "AutoNumberMasks" "CustomerNumberMask" 0 no
"202202224354008566309" 704 "Default RM Item to select" "Character" "User Select,First Board,First Item" "User Select" "" "" no "SSRMIssue" "SSIssueDefaultRM" 0 no
"202202224082108566298" 703 "Post Scanned items automatically" "Logical" "YES,NO" "NO" "" "" no "SSRMIssue" "AutoPost" 0 no
"202202074618808553143" 702 "Allow user to override the status id of on hold tag
Logical = YES
This allows the user to scan a tag that is already on hold.
If the pallet tag that is scanned has a status of On Hold, the user will be prompted to enter a status update

Logical = NO
This will not allow the user to scan a tag that is already on hold and will show them a message that this tag is on hold and cannot be used.  If the pallet tag that was scanned has a status of On Hold, then the user will be prompted with zMessage(55) indicating the tag is on hold and will display the tag status in the zMessage.  We want to include the Status id and status description - using the new zMessage framework) and return to the tag field (in focus) to enter a different tag.
" "Logical" "YES,NO" "NO" "" "" no "SSTagStatus" "AllowOnHoldTagScan" 0 no
"202202074613408553113" 701 "This can be defaulted so that the user cannot enter any tag status and
the default here forces the tag to be updated with this status
automatically.  If left blank, then the user will be prompted for the
tag status to apply.  For example, if the tag status is forced to be
'001' - Quarantine, then the user will not be prompted or permitted to
change the status.
" "Character" "" "" "" "" no "SSTagStatus" "DefaultTagScanStatusID" 0 no
"202201274048108513598" 700 "Vertex Client Secret
NK1=VertexClientSecret" "Character" "" "" "" "" no "Vertex" "VertexClientSecret" 0 no
"202201274048108513595" 699 "Vertex Client ID
NK1=VertexClientID" "Character" "" "" "" "" no "Vertex" "VertexClientID" 0 no
"202201274048108513592" 698 "Vertex API Password
NK1=VertexAPIPassword" "Character" "" "" "" "" no "Vertex" "VertexAPIPassword" 0 no
"202201274048108513589" 697 "Vertex API Key
NK1=VertexAPIKey" "Character" "" "" "" "" no "Vertex" "VertexAPIKey" 0 no
"202201274048108513586" 696 "Vertex API Access Token Time
NK1=VertexAccessToken" "Integer" "" "" "" "" no "Vertex" "VertexAccessTokenTime" 0 no
"202201274048108513583" 695 "Vertex API Access Token Date
NK1=VertexAccessToken" "Date" "" "" "" "" no "Vertex" "VertexAccessTokenDate" 0 no
"202201260234908504662" 694 "AP Posting UserId" "Logical" "Yes,No" "No" "" "" no "APPosting" "APPostingUserId" 0 no
"202201260227508504660" 693 "AP Posting Period Rules" "Character" "CurrentPeriodOnly,OpenPeriodOnly" "CurrentPeriodOnly" "" "" no "APPosting" "APPostingPeriodRules" 0 no
"202201260215608504658" 692 "" "Character" "" "" "" "" no "APPosting" "APPostingPassword" 0 no
"202201183834308473427" 691 "ADO Client" "Character" "" "" "" "" no "" "ADOClient" 0 no
"202201131503508459089" 690 "Prompt for Schedule Board Notes" "Logical" "NO,YES" "NO" "" "" no "" "ScheduleBoardNotes" 0 no
"202201121445008455830" 689 "PO Date Change Requires Reason" "Logical" "Yes,No" "No" "" "" no "" "PODateChangeRequiresReason" 0 no
"202112234873608386358" 677 "Verify that the freight terms are valid
NK1=InvoiceApprovalFreightTerms" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalFreightTerms" 0 yes
"202112234873608386360" 676 "Do not auto approve if invoice has a misc item
NK1=InvoiceApprovalMiscCharge" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalMiscCharge" 0 yes
"202112234873608386364" 675 "Unable to calculate tax
NK1=InvoiceApprovalTaxCalc" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalTaxCalc" 0 yes
"202112234873608386363" 674 "Ship to is taxable and any line is not set as taxable
NK1=InvoiceApprovalTaxableCheck" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalTaxableCheck" 0 yes
"202112234873608386362" 673 "Item Price is greater than the cost of the item
NK1=InvoiceApprovalPriceGTCost" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalPriceGTCost" 0 yes
"202112234873608386361" 672 "Order line manually overridden
NK1=InvoiceApprovalOrderlineChange" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalOrderlineChange" 0 yes
"202112218115808389101" 671 "Auto Set Job Machine (Routing) Date Values Type" "Character" "NoDate,None,PlanDate" "NoDate" "" "" no "Scheduler" "JobRoutingDateType" 0 no
"202112234873608386359" 670 "Invoice status is not on Hold
NK1=InvoiceApprovalInvoiceStatus" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalInvoiceStatus" 0 yes
"202112218115808389094" 669 "Auto Set Job Machine (Routing) Date Values Active" "Logical" "YES,NO" "NO" "" "" no "Scheduler" "JobRoutingDate" 0 no
"202112234873608386357" 668 "If freight is billable, that freight amount is not zero
NK1=InvoiceApprovalFreightAmount" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalFreightAmount" 0 yes
"202112234873608386356" 667 "Expected revenue is zero for invoice in total
NK1=InvoiceApprovalExpectZero" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalExpectZero" 0 yes
"202112234873608386355" 666 "Invoice Approval Bill Notes
NK1=InvoiceApprovalBillNotes" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalBillNotes" 0 yes
"202112234873608386353" 665 "Utilize auto approval of invoices
NK1=ApplyInvoiceApprovals" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApproval" 0 yes
"202112184469908383826" 664 "Create component receipts for set header" "Logical" "YES,NO" "NO" "" "" no "SSFGReceiveTransfer" "CreateFGCompReceiptForSetHeader" 0 no
"202112174394408382796" 663 "When enables displays the window size controls that can let the user re-size the window" "Logical" "YES,NO" "YES" "" "" no "Window" "ShowWindowControls" 900 no
"202112160422708378093" 659 "RFQ Print Mode" "Character" "Customer/CustX" "Customer" "" "" no "RFQ" "RFQPrint" 0 no
"202112160422708378085" 657 "ISO Code" "Character" "" "" "" "" no "RFQ" "RFQISO#" 0 no
"202112160422708378078" 655 "Show records when first entering RFQ browser limit." "Integer" "0" "0" "" "" no "RFQ" "RFQBrowseLimit" 0 no
"202112160422708378071" 653 "Show records when first entering RFQ browsers. Active" "Logical" "YES,NO" "NO" "" "" no "RFQ" "RFQBrowse" 0 no
"202112160010508378065" 651 "Notepad instead of Standard Screen Output? Value" "Character" ",Notepad" "" "" "" no "Notepad" "NotepadValue" 0 no
"202112160010508378058" 649 "Notepad instead of Standard Screen Output? Active" "Logical" "YES,NO" "NO" "" "" no "Notepad" "Notepad" 0 no
"202112158510908378051" 647 "1099-MISC Print Format Left Margin" "Decimal" "" "10" "" "" no "1099Misc" "1099MiscLeftMargin" 0 no
"202112158510908378045" 645 "1099-MISC Print Format" "Character" "1Up1099,2Up1099,Fibre" "1Up1099" "" "" no "1099Misc" "1099Misc" 0 no
"202112155474508377724" 643 "Vertex Active
NK1=VertexAccessToken" "Logical" "YES,NO" "NO" "" "" no "Vertex" "Vertex" 0 no
"202112138627508369532" 642 "Vertex API Access Token Refresh Interval" "Integer" "" "" "" "" no "Vertex" "VertexAccessTokenRefreshInterval" 0 no
"202112138623708369530" 641 "Vertex API Access Token" "Character" "" "" "" "" no "Vertex" "VertexAccessToken" 0 no
"202112138464208369219" 640 "Vertex API Access Token Date Time" "DateTime-TZ" "" "" "" "" no "Vertex" "VertexAccessTokenDateTime" 0 no
"202112131902608366717" 639 "No Prompt,
Warn if Length of Scan not equal to Length of Mask, 
Block if Length of Scan not equal to Length of Mask" "Character" "No Prompt,Warn if Length of Scan not equal to Length of Mask,Block if Length of Scan not equal to Length of Mask" "No Prompt" "" "" no "" "SSScanVendorTagPrompt" 0 yes
"202112131871608366715" 638 "Format mask for parsing the vendor tags received from POs" "Character" "PPPPPPLLLQQQQQXXXX,PPPPPPLLLQQQQQ,PPPPPPPPLLLQQQQQXXXX,PPPPPPLLLQQQQQXXX" "PPPPPPLLLQQQQQXXXX" "" "" no "" "SSScanVendorTagMask" 0 yes
"202112100949208363439" 636 "Always enable TS Time Buttons?" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSTimeB" 0 no
"202112100949208363431" 634 "Touch-Screen Time Source - Server or Workstation" "Character" "Sserver,Workstation,NO" "NO" "" "" no "TouchScreen" "TSTime" 0 no
"202112100949208363426" 632 "Show Schedule Board Pending Jobs" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSShowPending" 0 no
"202112100949208363417" 630 "Password Option to Show Labor Rates Password" "Character" "" "" "" "" yes "TouchScreen" "TSSecurePassword" 0 no
"202112100949108363410" 628 "Password Option to Show Labor Rates Active" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSSecure" 0 no
"202112100949108363408" 626 "Validate Touch Screen Counts per Machine?" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSQty" 0 no
"202112100949108363406" 624 "Set Logical Value to Invoke this Logic Sets" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSPostFGSets" 0 no
"202112100949108363404" 622 "Set Logical Value to Invoke this Logic Machine Codes" "Character" "" "" "" "" no "TouchScreen" "TSPostFGMachineCodes" 0 no
"202112100949108363403" 620 "Set Logical Value to Invoke this Logic Active" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSPostFG" 0 no
"202112100949108363392" 618 "Post ACTUAL or STANDARD Direct Labor Costs?" "Character" "Actual,Standard,NO" "NO" "" "" no "TouchScreen" "TSPost" 0 no
"202112100949008363388" 616 "Prompt Login ID/Password for Touch-Screen" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSLogin" 0 no
"202112100949008363381" 614 "Display Touch Screen On Screen Keyboard?" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSKeyboard" 0 no
"202112100949008363379" 612 "Complete All Machines or Last Machine?" "Character" "All Machines,Last Machine,YES,NO" "NO" "" "" no "TouchScreen" "TSFinish" 0 no
"202112100949008363375" 610 "Complete Always No for TS End Wash Operation?" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSEndWash" 0 no
"202112100949008363374" 608 "TS Add Second to Logout Times of Emp. with Dock Min. if shift change" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSDockSec" 0 no
"202112100948908363369" 606 "Show Touchscreen Is Opearation Complete Message?" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSComplete" 0 no
"202112100948908363361" 604 "Touch-Screen requires CLOCK IN/OUT? Password" "Character" "" "" "" "" yes "TouchScreen" "TSClockPassword" 0 no
"202112100948908363360" 602 "Touch-Screen requires CLOCK IN/OUT? Active" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSClock" 0 no
"202112100948908363359" 600 "Automatically allocate Run/Waste Qty across Break Transactions" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSBreaksQty" 0 no
"202112100948908363352" 598 "Automatically post breaks to machines?" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSBreaks" 0 no
"202112100948908363351" 596 "AM/PM Toggle Button - Warning when changing AM/PM" "Logical" "YES,NO" "NO" "" "" no "TouchScreen" "TSAMPMWarn" 0 no
"202112075982208341605" 594 "ZoHo Refresh Token
NK1=ZohoRefreshToken" "Character" "1000.9d3c98f80d53e7b27dcef14b80bb3e20.f08610cd5b76f5b3a4ade67f17a6c956" "1000.9d3c98f80d53e7b27dcef14b80bb3e20.f08610cd5b76f5b3a4ade67f17a6c956" "" "" no "ZoHoCRM" "ZohoRefreshToken" 0 no
"202112075982208341604" 592 "ZoHo Client Secret
NK1=ZohoClientSecret" "Character" "a54de56bc72f068a546b25c539f5f0b01858a4ac8b" "a54de56bc72f068a546b25c539f5f0b01858a4ac8b" "" "" no "ZoHoCRM" "ZohoClientSecret" 0 no
"202112075982208341603" 590 "ZoHo Client ID
NK1=ZohoClientID" "Character" "1000.JZ0PMBMAND7RPQ1LZP1QQHPG0KWMLZ" "1000.JZ0PMBMAND7RPQ1LZP1QQHPG0KWMLZ" "" "" no "ZoHoCRM" "ZohoClientID" 0 no
"202112075982208341602" 588 "ZoHo CRM Active
NK1=ZohoClientID" "Logical" "YES,NO" "NO" "" "" no "ZoHoCRM" "ZohoCRM" 0 no
"202112075982208341601" 586 "Vertex Tax Class Default
NK1=VertexTaxClassDefault" "Character" "" "" "" "" no "Vertex" "VertexTaxClassDefault" 0 no
"202112075982208341600" 584 "Vertex Freight Tax Class
NK1=VertexFreightTaxClass" "Character" "Freight" "Freight" "" "" no "Vertex" "VertexFreightTaxClass" 0 no
"202112075982108341584" 568 "Tasker Not Running Time Limit
NK1=TaskerNotRunning" "Integer" "" "0" "" "" no "MainMenu,dAOA" "TaskerNotRunningTimeLimit" 0 no
"202112075982108341583" 566 "Tasker Not Running Email Configuration ID
NK1=TaskerNotRunning" "Integer" "" "0" "" "" no "MainMenu,dAOA" "TaskerNotRunningEmailID" 0 no
"202112075982108341582" 564 "Tasker Not Running Active
NK1=TaskerNotRunning" "Logical" "YES,NO" "NO" "" "" no "MainMenu,dAOA" "TaskerNotRunning" 0 no
"202112075982108341581" 562 "Main Menu Link to ASI ZoHo URL
NK1=MenuLinkZoHo" "Character" "https://desk.zoho.com/portal/advantzware/signin" "https://desk.zoho.com/portal/advantzware/signin" "" "" no "MainMenu" "MenuLinkZoHoURL" 0 no
"202112075982108341580" 560 "Main Menu Link to ASI ZoHo Transparent Attribute
NK1=MenuLinkZoHo" "Logical" "YES,NO" "YES" "" "" no "MainMenu" "MenuLinkZoHoTransparent" 0 no
"202112075982108341579" 558 "Main Menu Link to ASI ZoHo Image
NK1=MenuLinkZoHo" "Character" "Graphics\32x32\question.ico" "Graphics\32x32\question.ico" "" "" no "MainMenu" "MenuLinkZoHoImage" 0 no
"202112075982108341578" 556 "Main Menu Link to ASI ZoHo Active
NK1=MenuLinkZoHo" "Logical" "YES,NO" "YES" "" "" no "MainMenu" "MenuLinkZoHo" 0 no
"202112075982108341577" 554 "Menu Link Upgrade URL
NK1=MenuLinkUpgrade" "Character" "https://helpsvr.advantzware.com/patches/asiUpdate.html" "https://helpsvr.advantzware.com/patches/asiUpdate.html" "" "" no "MainMenu" "MenuLinkUpgrade" 0 no
"202112075982108341576" 552 "Main Menu ASI URL
NK1=MenuLinkASI" "Character" "http://www.advantzware.com" "http://www.advantzware.com" "" "" no "MainMenu" "MenuLinkASIURL" 0 no
"202112075982108341575" 550 "Main Menu ASI Transparent Attribute
NK1=MenuLinkASI" "Logical" "YES,NO" "YES" "" "" no "MainMenu" "MenuLinkASITransparent" 0 no
"202112075982108341574" 548 "Main Menu ASI Image
NK1=MenuLinkASI" "Character" "Graphics\asiicon.ico" "Graphics\asiicon.ico" "" "" no "MainMenu" "MenuLinkASIImage" 0 no
"202112075982108341573" 546 "Main Menu ASI Active
NK1=MenuLinkASI" "Logical" "YES,NO" "YES" "" "" no "MainMenu" "MenuLinkASI" 0 no
"202112075982108341572" 544 "Menu Line #8 URL
NK1=MenuLink8" "Character" "" "" "" "" no "MainMenu" "MenuLink8URL" 0 no
"202112075982108341571" 542 "Menu Line #8 Stretch To Fit Attribute
NK1=MenuLink8" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink8StretchToFit" 0 no
"202112075982108341570" 540 "Menu Line #8 Transparent Attribute
NK1=MenuLink8" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink8Transparent" 0 no
"202112075982108341569" 538 "Menu Line #8 Image
NK1=MenuLink8" "Character" "" "" "" "" no "MainMenu" "MenuLink8Image" 0 no
"202112075982108341565" 536 "Menu Line #8 Active
NK1=MenuLink8" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink8" 0 no
"202112075982108341564" 534 "Menu Line #7 URL
NK1=MenuLink7" "Character" "" "" "" "" no "MainMenu" "MenuLink7URL" 0 no
"202112075982108341563" 532 "Menu Line #7 Stretch To Fit Attribute
NK1=MenuLink7" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink7StretchToFit" 0 no
"202112075982108341562" 530 "Menu Line #7 Transparent Attribute
NK1=MenuLink7" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink7Transparent" 0 no
"202112075982108341561" 528 "Menu Line #7 Image
NK1=MenuLink7" "Character" "" "" "" "" no "MainMenu" "MenuLink7Image" 0 no
"202112075982008341557" 526 "Menu Line #7 Active
NK1=MenuLink7" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink7" 0 no
"202112075982008341556" 524 "Menu Line #6 URL
NK1=MenuLink6" "Character" "" "" "" "" no "MainMenu" "MenuLink6URL" 0 no
"202112075982008341555" 522 "Menu Line #6 Stretch To Fit Attribute
NK1=MenuLink6" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink6StretchToFit" 0 no
"202112075982008341554" 520 "Menu Line #6 Transparent Attribute
NK1=MenuLink6" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink6Transparent" 0 no
"202112075982008341553" 518 "Menu Line #6 Image
NK1=MenuLink6" "Character" "" "" "" "" no "MainMenu" "MenuLink6Image" 0 no
"202112075982008341549" 516 "Menu Line #6 Active
NK1=MenuLink6" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink6" 0 no
"202112075982008341548" 514 "Menu Line #5 URL
NK1=MenuLink5" "Character" "" "" "" "" no "MainMenu" "MenuLink5URL" 0 no
"202112075982008341547" 512 "Menu Line #5 Stretch To Fit Attribute
NK1=MenuLink5" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink5StretchToFit" 0 no
"202112075982008341546" 510 "Menu Line #5 Transparent Attribute
NK1=MenuLink5" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink5Transparent" 0 no
"202112075982008341545" 508 "Menu Line #5 Image
NK1=MenuLink5" "Character" "" "" "" "" no "MainMenu" "MenuLink5Image" 0 no
"202112075982008341528" 506 "Menu Line #5 Active
NK1=MenuLink5" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink5" 0 no
"202112075982008341523" 504 "Menu Line #4 URL
NK1=MenuLink4" "Character" "" "" "" "" no "MainMenu" "MenuLink4URL" 0 no
"202112075982008341517" 502 "Menu Line #4 Stretch To Fit Attribute
NK1=MenuLink4" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink4StretchToFit" 0 no
"202112075982008341511" 500 "Menu Line #4 Transparent Attribute
NK1=MenuLink4" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink4Transparent" 0 no
"202112075982008341506" 498 "Menu Line #4 Image
NK1=MenuLink4" "Character" "" "" "" "" no "MainMenu" "MenuLink4Image" 0 no
"202112075981908341492" 496 "Menu Line #4 Active
NK1=MenuLink4" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink4" 0 no
"202112075981908341491" 494 "Menu Line #3 URL
NK1=MenuLink3" "Character" "" "" "" "" no "MainMenu" "MenuLink3URL" 0 no
"202112075981908341490" 492 "Menu Line #3 Stretch To Fit Attribute
NK1=MenuLink3" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink3StretchToFit" 0 no
"202112075981908341489" 490 "Menu Line #3 Transparent Attribute
NK1=MenuLink3" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink3Transparent" 0 no
"202112075981908341488" 488 "Menu Line #3 Image
NK1=MenuLink3" "Character" "" "" "" "" no "MainMenu" "MenuLink3Image" 0 no
"202112075981908341484" 486 "Menu Line #3 Active
NK1=MenuLink3" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink3" 0 no
"202112075981908341483" 484 "Menu Line #2 URL
NK1=MenuLink2" "Character" "" "" "" "" no "MainMenu" "MenuLink2URL" 0 no
"202112075981908341482" 482 "Menu Line #2 Stretch To Fit Attribute
NK1=MenuLink2" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink2StretchToFit" 0 no
"202112075981908341481" 480 "Menu Line #2 Transparent Attribute
NK1=MenuLink2" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink2Transparent" 0 no
"202112075981908341480" 478 "Menu Line #2 Image
NK1=MenuLink2" "Character" "" "" "" "" no "MainMenu" "MenuLink2Image" 0 no
"202112075981808341476" 476 "Menu Line #2 Active
NK1=MenuLink2" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink2" 0 no
"202112075981808341475" 474 "Menu Line #1 URL
NK1=MenuLink1" "Character" "" "" "" "" no "MainMenu" "MenuLink1URL" 0 no
"202112075981808341474" 472 "Menu Line #1 Stretch To Fit Attribute
NK1=MenuLink1" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink1StretchToFit" 0 no
"202112075981808341473" 470 "Menu Line #1 Transparent Attribute
NK1=MenuLink1" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink1Transparent" 0 no
"202112075981808341472" 468 "Menu Line #1 Image
NK1=MenuLink1" "Character" "" "" "" "" no "MainMenu" "MenuLink1Image" 0 no
"202112075981808341468" 466 "Menu Line #1 Active
NK1=MenuLink1" "Logical" "YES,NO" "NO" "" "" no "MainMenu" "MenuLink1" 0 no
"202112075981808341467" 464 "Dynamic Task Ticker Minutes
NK1=DynTaskTicker" "Integer" "" "0" "" "" no "MainMenu,dAOA" "DynTaskTickerMinutes" 0 no
"202112075981808341466" 462 "Dynamic Task Ticker Active
NK1=DynTaskTicker" "Logical" "YES,NO" "NO" "" "" no "MainMenu,dAOA" "DynTaskTicker" 0 no
"202112075981708341437" 460 "Main Menu Default Image
NK1=BitMap" "Character" "" "" "" "" no "MainMenu" "MainMenuImage" 0 no
"202112075981708341436" 458 "ASI Help Service URL 
NK1=ASIHelpService" "Character" "-WSDL 'http:\\34.203.15.64/asihelpServices/helpmaintenance.asmx?WSDL'" "-WSDL 'http:\\34.203.15.64/asihelpServices/helpmaintenance.asmx?WSDL'" "" "" no "MainMenu" "ASIHelpService" 0 no
"202111192418908276487" 181 "This auto approval compares the Submitted Price and Submitted Price UOM as compared to the Invoice Price and Invoice UOM" "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalEdiPriceVariance" 0 yes
"202111192413308276485" 180 "This auto approval compares the Quantity Ordered and the Shipped Quantity " "Character" "Off,On,On Also During Post" "Off" "" "" no "InvoiceApproval" "InvoiceApprovalShipOverage" 0 yes
"202111192406308276483" 179 "Show Edi Field on Order and order line " "Logical" "YES,NO" "NO" "" "" no "" "EDIOrderChanged" 0 no
"202111084750608228611" 40 "Select the source of the CRM to which APIs need to connect " "Character" "Zoho,Hubspot" "Zoho" "" "" no "CRM" "CRMSource" 0 no
"202110264717008185817" 39 "Display the FG location summary on items tab based on the first release of the item" "Logical" "NO,YES" "NO" "" "" no "" "DisplayFGLocationSummary" 0 no
"202110263559608184341" 38 "Allow the user to view the FG location balances during order entry.  If set to Yes, this screen will be displayed to the user upon entry and edit of the order line item.  If set to No, this will not be an option to view (Default) and if on demand, will allow the user to open and close this on demand." "Character" "NO,YES,On Demand" "NO" "" "" no "" "DisplayFGLocationDetails" 0 no
"202110068639808071483" 37 "Post FG receipt automatically when scanned" "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSFGReceiveTransfer" "SSFGPost" 0 no
"202110068628208071481" 36 "Close job after a receipt" "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSFGReceiveTransfer" "SSCloseJob" 0 no
"202110068593108071479" 35 "Location source to update the location and bin on a receipt transaction
FGItem - From item
UserDefault - From user's default location and bin
Loadtag - From tag" "Character" "FGItem,UserDefault,Loadtag" "Loadtag" "" "" no "SSFGReceiveTransfer" "SSLocationSource" 0 no
"202110212250208167725" 34 "Setting to YES to active Auto Post of Transfers" "Logical" "Yes,No" "No" "" "" no "CreateTransfer,PrintBOl" "BOLTransferAutoPost" 0 no
"202108102268507847805" 32 "Create set components quantity with maximum available quantity in case of insufficient inventory" "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSCreateLoadTag" "UpdateSetWithMaxQuantity" 0 yes
"202108102268507847797" 30 "Select the source of the loadtag's location and bin" "Character" "FGItem,FGBin" "FGItem" "" "" no "SSCreateLoadTag" "LoadtagLocationSource" 0 yes
"202108102268507847795" 29 "NK1 LoadTagSSCC - Update Loadtag with SSCC" "Date" "" "MM/DD/YYYY" "" "" no "SSCreateLoadTag" "UpdateLoadTagSSCC" 0 yes
"202109160219607979124" 28 "SS Create BOL
When a scanned trailer for release is not matching the trailer on  release.
Warning - This will show a warning message, but will let the user continue
Error - Blocks the user from scanning a trailer other release trailer
None - No validation" "Character" "None,Warning,Error" "None" "" "" no "SSCreateBOL" "SSCreateBOLTrailerValidation" 0 no
"202109084820407919056" 27 "Setting to scan a trailer either after release scan or after tag scan or not at all" "Character" "Tag,Release,Both,None" "None" "" "" no "SSCreateBOL" "SSCreateBOLScanTrailer" 0 no
"202109154934507978843" 26 "Sharp Shooter BOL Creation Button - Print BOL Button.
This provides option to bypass the BOL Print Selection Screen when printing the Bill of Lading via Sharp Shooter BOL Button.  Please note, Laser Printers should be defined as default printers by Forklift Drivers via the Windows Printer Setup.

Value = ShowBOLPrintScreen  
This will display the BOL Print Screen.

Value = PrintSilently
This print the BOL silently without displaying the BOL Print Screen.

Value = PrintAndPostSilently 
This will Check the toggle box called Post BOL? when pressing the Sharp Shooter Print BOL Button.  This will simultaneously print the BOL and Create the Invoice.

Value = DoNotPrint
Does not print BOL" "Character" "ShowBOLPrintScreen,PrintSilently,PrintAndPostSilently,DoNotPrint" "DoNotPrint" "" "" no "SSCreateBOL" "SSCreateBOLPrint" 0 no
"202109086155907977525" 25 "This parameter is ideal for partial pallet shipments.
Pallet -  Transfer quantity of selected tags to the bill of lading
Release - Adds remaining release quantity
Release+Overs - Add remaining release quantity plus overs
Prompt - Allow the user to select the quantity" "Character" "Pallet,Release,Prompt" "Prompt" "" "" no "SSCreateBOL" "SSBOLQuantitySelection" 0 no
"202109167322107979140" 24 "Display virtual keyboard
Yes - Pops up virtual keyboard when required
No - Virtual keyboard is not displayed" "Logical" "Yes,No" "No" "" "" no "Keyboard,SSCreateBOL,SSPrintBOLTag,SSFGInquiry,SSFGReceiveTransfer" "ShowVirtualKeyboard" 0 no
"202109176503507979212" 23 "Option to show/hide settings icon and text" "Character" "Text,Icon,TextAndIcon,None" "TextAndIcon" "" "" no "SSCreateBOL,SSPrintBOLTag,SSFGInquiry,SSFGReceiveTransfer,SSCreateLoadTag" "ShowSettings" 0 no
"202109176316007979208" 22 "Displays FG Item Inquiry button" "Character" "Text,Icon,TextAndIcon,None" "TextAndIcon" "" "" no "SSCreateBOL" "ShowFGItemInquiry" 0 no
"202109177763807979221" 21 "Show/Hide Adjust quantity icon and label" "Character" "Text,Icon,TextAndIcon,None" "TextAndIcon" "" "" no "SSFGInquiry" "ShowAdjustQuantity" 0 no
"202109118595807977787" 20 "Browse search limits" "Integer" "" "100" "" "" no "SearchLimit" "SearchLimits" 0 no
"202108102268507847807" 19 "Path to save the loadtag output file" "Character" "" "C:\BA\Label\" "" "" no "SSCreateLoadTag" "LoadTagOutputFilePath" 0 yes
"202108102268507847808" 18 "File name to save the output file" "Character" "" "loadtag.txt" "" "" no "SSCreateLoadTag" "LoadTagOutputFile" 0 yes
"202108102268507847798" 17 "ASI, BarOne, SSLabel, SSBarOne and Triad dictate what bar code software is used to create the proper text file format. " "Character" "ASI,Printer" "ASI" "" "" no "SSCreateLoadTag" "LoadtagPrintSoftware" 0 yes
"202108102268507847804" 16 "Creates adjustments for the components of assembled sets." "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSCreateLoadTag" "FGSetCreateAdjustment" 0 yes
"202108102268507847803" 15 "NK1 FGReceiptRules" "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSCreateLoadTag" "FGReceiptRules" 0 yes
"202108102268507847799" 14 "NK1 LOADTAG - Integer field. The default print copies" "Integer" "" "1" "" "" no "SSCreateLoadTag" "DefaultPrintCopies" 0 yes
"202108102268507847814" 13 "Create loadtags for BOL lines if BOL line's tag is empty while printing" "Logical" "TRUE,FALSE" "TRUE" "" "" no "SSCreateLoadTag" "CreateTagsForEmptyBOLLineTags" 0 yes
"202108102268507847816" 12 "Create a separate loadtag for partial quantity" "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSCreateLoadTag" "CreateTagForPartial" 0 yes
"202108102268507847800" 11 "NK1 RFIDTag - Create rfIDTag for a loadtag" "Logical" "TRUE,FALSE" "TRUE" "" "" no "SSCreateLoadTag" "CreateRFIDTag" 0 yes
"202108102268507847802" 10 "NK1 FGRECPT - Is Job# or PO# mandatory on FG Receipts?" "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSCreateLoadTag" "CreateFGReceipts" 0 yes
"202108102268507847806" 9 "When enabled creates tags for components in set header item" "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSCreateLoadTag" "CreateComponenetTagsForSetHeaderItem" 0 yes
"202108102268507847801" 8 "Validate Job/PO status before creating FG Receipts" "Logical" "TRUE,FALSE" "FALSE" "" "" no "SSCreateLoadTag" "CheckClosedStatus" 0 yes
"202108074511907847594" 7 "List of all category tags for settings" "Character" "SSCreateLoadTag,SSCreateBOL,SSFGInquiry,SSFGReceiveTransfer" "SSCreateLoadTag" "" "" no "" "CategoryTags" 1000 no
"202108102268507847809" 6 "Path to save the BOL tag output file" "Character" "" "C:\BA\Label\BOL\" "" "" no "SSCreateLoadTag" "BOLLoadTagOutputFilePath" 0 yes
"202108102268507847810" 5 "File name to save the BOL tag output file" "Character" "" "BOLTag.txt" "" "" no "SSCreateLoadTag" "BOLLoadTagOutputFile" 0 yes
"202108102268507847812" 4 "Print loadtag on a valid tag scan on reprint screen" "Logical" "TRUE,FALSE" "TRUE" "" "" no "SSCreateLoadTag" "AutoPrintLoadtagOnTagScan" 0 yes
"202108102268507847811" 3 "Print loadtag on a valid job scan" "Logical" "TRUE,FALSE" "TRUE" "" "" no "SSCreateLoadTag" "AutoPrintLoadtagOnJobScan" 0 yes
"202108102268507847813" 1 "Create a temporary record for verifying before printing a loadtag on a valid job scan" "Logical" "TRUE,FALSE" "TRUE" "" "" no "SSCreateLoadTag" "AutoCreateLoadtagOnJobScan" 0 yes
