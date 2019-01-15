<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="loadtag_report" Codebehind="loadtag_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Loadtag Creation</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="include/insert.js"></script>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"></script>
   <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.18/jquery-ui.min.js"></script> 
    
    
     <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript" type="text/javascript">
    function jobpo() {
       
        var fromlabel = document.getElementById("fromjobpoLabel")
        var tolabel = document.getElementById("tojobpoLabel")
        var commaLabel = document.getElementById("commaLabel")
        var rd1 = document.getElementById("RD1");
        var rd2 = document.getElementById("RD2");
        if (rd1.checked == true) {
            fromlabel.innerText = "From Order#:";
            tolabel.innerText = "To Order#:";
            commaLabel.innerText = "Enter Order(s) separated by Comma";
            document.forms[0].jobcomm_TextBox.readOnly = false;            
            document.forms[0].fromjob_TextBox.readOnly = false;
            document.forms[0].fromjob2_TextBox.readOnly = false;
            document.forms[0].tojob_TextBox.readOnly = false;
            document.forms[0].tojob2_TextBox.readOnly = false;
            document.forms[0].CheckBox5.disabled = false;
            document.forms[0].CheckBox6.disabled = false;
            document.forms[0].CheckBox2.disabled = false;
            document.forms[0].jobcomm_TextBox.style.backgroundColor = 'white';
            document.forms[0].fromjob_TextBox.style.backgroundColor = 'white';
            document.forms[0].fromjob2_TextBox.style.backgroundColor = 'white';
            document.forms[0].tojob_TextBox.style.backgroundColor = 'white';
            document.forms[0].tojob2_TextBox.style.backgroundColor = 'white';
            
    }
    if (rd2.checked == true) {
        fromlabel.innerText = "From Po#:";
        tolabel.innerText = "To Po#:";
        commaLabel.innerText = "Enter Po(s) separated by Comma";
        document.forms[0].jobcomm_TextBox.readOnly = true;
        document.forms[0].fromjob_TextBox.readOnly = true;
        document.forms[0].fromjob2_TextBox.readOnly = true;
        document.forms[0].tojob_TextBox.readOnly = true;
        document.forms[0].tojob2_TextBox.readOnly = true;
        document.forms[0].CheckBox5.disabled = true;
        document.forms[0].CheckBox6.disabled = true;
        document.forms[0].CheckBox2.disabled = true;
        document.forms[0].jobcomm_TextBox.style.backgroundColor = '#EEF3FF';
        document.forms[0].fromjob_TextBox.style.backgroundColor = '#EEF3FF';
        document.forms[0].fromjob2_TextBox.style.backgroundColor = '#EEF3FF';
        document.forms[0].tojob_TextBox.style.backgroundColor = '#EEF3FF';
        document.forms[0].tojob2_TextBox.style.backgroundColor = '#EEF3FF';
    }

    }
</script>
<script language="javascript" type="text/javascript">
    function confirmPost() {
        var chk1 = document.getElementById("CheckBox3");
        if (chk1.checked) {
            var retVal = makeMsgBox("Confirmation", "Are you Sure you Want to Reprint Loadtag File?", 48, 4, 256, 4096);
            if (retVal == 6) {
                document.forms[0].HiddenFieldPost.value = "Yes";
            }
            else {
                document.forms[0].HiddenFieldPost.value = "No";
            }
        }
    }
</script>
        
    <script language ="JavaScript">
        window.onload = reprint;
        
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    }

    function CallAlert() {
        var button = document.getElementById("Button1");
        button.click();
    }

    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }

    function preEnter(fieldObj, canEdit) {
        //fieldObj.style.backgroundColor = 'blue';
        //fieldObj.style.color = 'white';
        if (canEdit == "no") {
            fieldObj.blur();
            leaveField(fieldObj);
        }

        enterField(fieldObj);
        return;
    }
    
    function preLeave( fieldObj, fieldType, fieldFormat ){
    //fieldObj.style.backgroundColor='Window';
    //fieldObj.style.color='WindowText';
    fieldType = fieldType.toLowerCase();
    if ((fieldType == "") || (fieldType == "text")) {
        leaveField(fieldObj);
    }
    if (fieldType == "date") {
        if (fieldFormat == "") {
            var dateFormat = "99/99/9999";
        } else { var dateFormat = fieldFormat; }
        checkDate(dateFormat, fieldObj, '01/01/1950', '12/31/3000', 0);
    }

    if (fieldType == "number") {
        if (fieldFormat == "") {
            var numFormat = "(>>>>9)";
        } else { var numFormat = fieldFormat; }
        checkNum(numFormat, fieldObj, '?', '?', 0);
    }   
}

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
}


    function loadtaglookup() {
        var chk1 = document.getElementById("CheckBox3");
        if (chk1.checked) {
            var NewWindow = window.open("reprnt_tag_look.aspx", "Loadtaglookup", "width=700,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        else {
            var NewWindow = window.open("loadtg_lookup.aspx", "Loadtaglookup2", "width=700,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
         }
     }

     function ReprintTagLook(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6) {
         document.forms[0].ScanLabelTextBox.value = ReturnObj1;
         document.forms[0].fromitem_TextBox.value = ReturnObj2;
         document.forms[0].toitem_TextBox.value = ReturnObj2;
         document.forms[0].fromjob_TextBox.value = ReturnObj3;
         document.forms[0].tojob_TextBox.value = ReturnObj3;
         document.forms[0].fromjob2_TextBox.value = ReturnObj4;
         document.forms[0].tojob2_TextBox.value = ReturnObj4;
         document.forms[0].fromOrder_TextBox.value = ReturnObj5;
         document.forms[0].torder_TextBox.value = ReturnObj5;
         document.forms[0].Labelpath_TextBox.value = ReturnObj6;
     }

     function LoadTagLook(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5,ReturnObj6) {
         document.forms[0].ScanLabelTextBox.value = ReturnObj1;
         document.forms[0].fromitem_TextBox.value = ReturnObj2;
         document.forms[0].toitem_TextBox.value = ReturnObj2;
         document.forms[0].fromjob_TextBox.value = ReturnObj3;
         document.forms[0].tojob_TextBox.value = ReturnObj3;
         document.forms[0].fromjob2_TextBox.value = ReturnObj4;
         document.forms[0].tojob2_TextBox.value = ReturnObj4;
         document.forms[0].fromOrder_TextBox.value = ReturnObj5;
         document.forms[0].torder_TextBox.value = ReturnObj5;
         document.forms[0].Labelpath_TextBox.value = ReturnObj6;
     }
     var jobl = "";
     function job1look(obj) {
         jobl = obj;
         var NewWindow = window.open("job1_lookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
     }
     function Job1Lookup(ReturnObj1) {
         if (jobl == "1") {
             document.forms[0].fromjob_TextBox.value = ReturnObj1;             
         }
         else {
             document.forms[0].tojob_TextBox.value = ReturnObj1;             
         }
     }
     var ord1 = "";
     function ordlook(obj) {
        var rd1 = document.getElementById("RD1");
        var rd2 = document.getElementById("RD2");
        if (rd1.checked == true) {
            ord1 = obj;
            var NewWindow = window.open("ldord_lookup.aspx", "OrderLineLookupWindow", "width=700,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        if (rd2.checked == true) {
            ord1 = obj;
            var NewWindow = window.open("ldtag_po_lookss.aspx", "PoLineLookupWindow", "width=700,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
    }
    function LoadPOLookup(ReturnObj1) {
        if (ord1 == "1") {
            document.forms[0].fromOrder_TextBox.value = ReturnObj1;
            
        }
        else {
            document.forms[0].torder_TextBox.value = ReturnObj1;
            
        }
    }
     function OrderLineLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5) {
         if (ord1 == "1") {
             document.forms[0].fromOrder_TextBox.value = ReturnObj1;
             document.forms[0].fromjob_TextBox.value = ReturnObj2;
             document.forms[0].fromjob2_TextBox.value = ReturnObj3;
             document.forms[0].fromitem_TextBox.value = ReturnObj4;
             document.forms[0].Labelpath_TextBox.value = ReturnObj5;             
         }
         else {
             document.forms[0].torder_TextBox.value = ReturnObj1;
             document.forms[0].tojob_TextBox.value = ReturnObj2;
             document.forms[0].tojob2_TextBox.value = ReturnObj3;
             document.forms[0].toitem_TextBox.value = ReturnObj4;
             document.forms[0].Labelpath_TextBox.value = ReturnObj5;            
         }
     }
     var fgitm = "";
     function loaditemlook(fgobj2) {
         fgitm = fgobj2;
         var NewWindow = window.open("ldtag_item_look.aspx", "LoadTagItemLook", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
     }

     function LoadItemLookup(ReturnObj1) {
         if (fgitm == "1") {
             document.forms[0].fromitem_TextBox.value = ReturnObj1;             
         }
         else {
             document.forms[0].toitem_TextBox.value = ReturnObj1;             
         }
     }




     function reprint() {
         var fromlabel = document.getElementById("fromjobpoLabel")
         var tolabel = document.getElementById("tojobpoLabel")
         var commaLabel = document.getElementById("commaLabel")
            var chk1 = document.getElementById("CheckBox3");
            if (chk1.checked) {
                
                var rd2 = document.getElementById("RadioButtonList2");
                var rd3 = document.getElementById("RadioButtonList3");
                var rd4 = document.getElementById("RadioButtonList4");
                var rdc1 = document.getElementById("RD1");
                var rdc2 = document.getElementById("RD2");
                rdc1.disabled = true;
                rdc1.disabled = true;
                
                rd3.disabled = true;
                rd4.disabled = true;
                rd2.disabled = true;
                document.forms[0].CheckBox2.disabled = true;
                document.forms[0].ordercomma_TextBox.readOnly = true;
                document.forms[0].jobcomm_TextBox.readOnly = true;

                document.forms[0].fromOrder_TextBox.readOnly = true;
                document.forms[0].torder_TextBox.readOnly = true;
                document.forms[0].fromjob_TextBox.readOnly = true;
                document.forms[0].fromjob2_TextBox.readOnly = true;
                document.forms[0].tojob_TextBox.readOnly = true;
                document.forms[0].tojob2_TextBox.readOnly = true;
                document.forms[0].fromitem_TextBox.readOnly = true;
                document.forms[0].toitem_TextBox.readOnly = true;

                document.forms[0].fromdate_TextBox.readOnly = true;
                document.forms[0].todate_TextBox.readOnly = true;
                document.forms[0].CheckBox4.disabled = true;
                document.forms[0].CheckBox8.disabled = false;
                document.forms[0].CheckBox9.disabled = true;
                document.forms[0].deptnotes_TextBox.readOnly = true;
                document.forms[0].shipnotes_TextBox.readOnly = true;
                document.forms[0].CheckBox5.disabled = true;
                document.forms[0].CheckBox6.disabled = true;
                document.forms[0].CheckBox7.disabled = true;
                //document.forms[0].printerfrom_TextBox.readonly = true;
                var pri = document.getElementById("printerfrom_TextBox");
                pri.readOnly = true;
                document.forms[0].labelpallet_TextBox.readOnly = true;
                document.forms[0].textpath_TextBox.readOnly = true;
                
                document.forms[0].ordercomma_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].jobcomm_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].fromOrder_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].torder_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].fromjob_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].fromjob2_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].tojob_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].tojob2_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].fromitem_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].toitem_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].deptnotes_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].shipnotes_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].fromdate_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].todate_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].printerfrom_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].labelpallet_TextBox.style.backgroundColor = '#EEF3FF';
                document.forms[0].textpath_TextBox.style.backgroundColor = '#EEF3FF';
                if (rdc1.checked == true) {
                    fromlabel.innerText = "From Order#:";
                    tolabel.innerText = "To Order#:";
                    commaLabel.innerText = "Enter Order(s) separated by Comma";
                    document.forms[0].jobcomm_TextBox.readOnly = false;
                    document.forms[0].fromjob_TextBox.readOnly = false;
                    document.forms[0].fromjob2_TextBox.readOnly = false;
                    document.forms[0].tojob_TextBox.readOnly = false;
                    document.forms[0].tojob2_TextBox.readOnly = false;
                    document.forms[0].CheckBox5.disabled = false;
                    document.forms[0].CheckBox6.disabled = false;
                    document.forms[0].CheckBox2.disabled = false;
                    document.forms[0].jobcomm_TextBox.style.backgroundColor = 'white';
                    document.forms[0].fromjob_TextBox.style.backgroundColor = 'white';
                    document.forms[0].fromjob2_TextBox.style.backgroundColor = 'white';
                    document.forms[0].tojob_TextBox.style.backgroundColor = 'white';
                    document.forms[0].tojob2_TextBox.style.backgroundColor = 'white';

                }
                if (rdc2.checked == true) {
                    fromlabel.innerText = "From Po#:";
                    tolabel.innerText = "To Po#:";
                    commaLabel.innerText = "Enter Po(s) separated by Comma";
                    document.forms[0].jobcomm_TextBox.readOnly = true;
                    document.forms[0].fromjob_TextBox.readOnly = true;
                    document.forms[0].fromjob2_TextBox.readOnly = true;
                    document.forms[0].tojob_TextBox.readOnly = true;
                    document.forms[0].tojob2_TextBox.readOnly = true;
                    document.forms[0].CheckBox5.disabled = true;
                    document.forms[0].CheckBox6.disabled = true;
                    document.forms[0].CheckBox2.disabled = true;
                    document.forms[0].jobcomm_TextBox.style.backgroundColor = '#EEF3FF';
                    document.forms[0].fromjob_TextBox.style.backgroundColor = '#EEF3FF';
                    document.forms[0].fromjob2_TextBox.style.backgroundColor = '#EEF3FF';
                    document.forms[0].tojob_TextBox.style.backgroundColor = '#EEF3FF';
                    document.forms[0].tojob2_TextBox.style.backgroundColor = '#EEF3FF';
                }

            }
            else {
                
                var rd2 = document.getElementById("RadioButtonList2");
                var rd3 = document.getElementById("RadioButtonList3");
                var rd4 = document.getElementById("RadioButtonList4");
                var rdc1 = document.getElementById("RD1");
                var rdc2 = document.getElementById("RD2");
                
                rdc1.disabled = false;
                rdc2.disabled = false;
                
                rd3.disabled = false;
                rd4.disabled = false;
                rd2.disabled = false;

                document.forms[0].CheckBox2.disabled = false;
                document.forms[0].ordercomma_TextBox.readOnly = false;
                document.forms[0].jobcomm_TextBox.readOnly = false;

                document.forms[0].fromOrder_TextBox.readOnly = false;
                document.forms[0].torder_TextBox.readOnly = false;
                document.forms[0].fromjob_TextBox.readOnly = false;
                document.forms[0].fromjob2_TextBox.readOnly = false;
                document.forms[0].tojob_TextBox.readOnly = false;
                document.forms[0].tojob2_TextBox.readOnly = false;
                document.forms[0].fromitem_TextBox.readOnly = false;
                document.forms[0].toitem_TextBox.readOnly = false;

                document.forms[0].fromdate_TextBox.readOnly = false;
                document.forms[0].todate_TextBox.readOnly = false;
                document.forms[0].CheckBox4.disabled = false;

                document.forms[0].CheckBox9.disabled = false;
                document.forms[0].deptnotes_TextBox.readOnly = false;
                document.forms[0].shipnotes_TextBox.readOnly = false;
                document.forms[0].CheckBox5.disabled = false;
                document.forms[0].CheckBox6.disabled = false;
                document.forms[0].CheckBox7.disabled = false;
                document.forms[0].printerfrom_TextBox.readOnly = false;
                document.forms[0].labelpallet_TextBox.readOnly = false;
                document.forms[0].textpath_TextBox.disabled = false;
                
                document.forms[0].ordercomma_TextBox.style.backgroundColor = 'white';
                document.forms[0].jobcomm_TextBox.style.backgroundColor = 'white';
                document.forms[0].fromOrder_TextBox.style.backgroundColor = 'white';
                document.forms[0].torder_TextBox.style.backgroundColor = 'white';
                document.forms[0].fromjob_TextBox.style.backgroundColor = 'white';
                document.forms[0].fromjob2_TextBox.style.backgroundColor = 'white';
                document.forms[0].tojob_TextBox.style.backgroundColor = 'white';
                document.forms[0].tojob2_TextBox.style.backgroundColor = 'white';
                document.forms[0].fromitem_TextBox.style.backgroundColor = 'white';
                document.forms[0].toitem_TextBox.style.backgroundColor = 'white';
                document.forms[0].deptnotes_TextBox.style.backgroundColor = 'white';
                document.forms[0].shipnotes_TextBox.style.backgroundColor = 'white';
                document.forms[0].fromdate_TextBox.style.backgroundColor = 'white';
                document.forms[0].todate_TextBox.style.backgroundColor = 'white';
                document.forms[0].printerfrom_TextBox.style.backgroundColor = 'white';
                document.forms[0].labelpallet_TextBox.style.backgroundColor = 'white';
                document.forms[0].textpath_TextBox.style.backgroundColor = 'white';
                if (rdc1.checked == true) {
                    fromlabel.innerText = "From Order#:";
                    tolabel.innerText = "To Order#:";
                    commaLabel.innerText = "Enter Order(s) separated by Comma";
                    document.forms[0].jobcomm_TextBox.readOnly = false;
                    document.forms[0].fromjob_TextBox.readOnly = false;
                    document.forms[0].fromjob2_TextBox.readOnly = false;
                    document.forms[0].tojob_TextBox.readOnly = false;
                    document.forms[0].tojob2_TextBox.readOnly = false;
                    document.forms[0].CheckBox5.disabled = false;
                    document.forms[0].CheckBox6.disabled = false;
                    document.forms[0].CheckBox2.disabled = false;
                    document.forms[0].jobcomm_TextBox.style.backgroundColor = 'white';
                    document.forms[0].fromjob_TextBox.style.backgroundColor = 'white';
                    document.forms[0].fromjob2_TextBox.style.backgroundColor = 'white';
                    document.forms[0].tojob_TextBox.style.backgroundColor = 'white';
                    document.forms[0].tojob2_TextBox.style.backgroundColor = 'white';

                }
                if (rdc2.checked == true) {
                    fromlabel.innerText = "From Po#:";
                    tolabel.innerText = "To Po#:";
                    commaLabel.innerText = "Enter Po(s) separated by Comma";
                    document.forms[0].jobcomm_TextBox.readOnly = true;
                    document.forms[0].fromjob_TextBox.readOnly = true;
                    document.forms[0].fromjob2_TextBox.readOnly = true;
                    document.forms[0].tojob_TextBox.readOnly = true;
                    document.forms[0].tojob2_TextBox.readOnly = true;
                    document.forms[0].CheckBox5.disabled = true;
                    document.forms[0].CheckBox6.disabled = true;
                    document.forms[0].CheckBox2.disabled = true;
                    document.forms[0].jobcomm_TextBox.style.backgroundColor = '#EEF3FF';
                    document.forms[0].fromjob_TextBox.style.backgroundColor = '#EEF3FF';
                    document.forms[0].fromjob2_TextBox.style.backgroundColor = '#EEF3FF';
                    document.forms[0].tojob_TextBox.style.backgroundColor = '#EEF3FF';
                    document.forms[0].tojob2_TextBox.style.backgroundColor = '#EEF3FF';
                }
            }
        }
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
         <asp:HiddenField ID="HiddenFieldPost" runat="server" />    
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=left nowrap><font size=+0><b>Loadtag Creation&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       
       <table class="shade"><tr><td>
       <fieldset class="shade" style="width:500px"><legend>Selection Parameter</legend>
       <table><tr><td  rowspan="2">
           
           <asp:RadioButton ID="RD1" onclick="jobpo()"  GroupName="editstatus1" runat="server" />Job/Order Receipt <br />
           <asp:RadioButton ID="RD2" onclick="jobpo()"  GroupName="editstatus1" runat="server" />Purchased Item Receipt
       </td>
       <td> 
           <asp:CheckBox ID="CheckBox2" Text="Return?" runat="server" />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <asp:CheckBox ID="CheckBox3" Text="Reprint Tag?"  OnClick="reprint()" runat="server" /></td></tr>
         <tr>
         <td><b>Scan Case Label:-</b>
             <asp:TextBox ID="ScanLabelTextBox" Width="180px" runat="server"></asp:TextBox>
             <a href="#" tabindex="1" onClick="loadtaglookup(); tagcol(ScanLabelTextBox); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
         </td></tr></table>
       </fieldset>
       </td></tr><tr><td>
       
       <fieldset class="shade" style="width:500px"><legend><b>Data Parameter</b></legend>
       <table><tr><td style="width:250px" colspan="2">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:Label ID="commaLabel" runat="server"></asp:Label>  <br /> 
           &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:TextBox ID="ordercomma_TextBox" TextMode="MultiLine" Width="180px" runat="server"></asp:TextBox>
       </td>
       
       <td colspan="2" style="width:250px">Enter Job separated by Comma <br />
       <asp:TextBox ID="jobcomm_TextBox" TextMode="MultiLine"  Width="180px" runat="server"></asp:TextBox>

       </td></tr>
       <tr><td align="right" style="padding-right:5px"><asp:Label ID="fromjobpoLabel" runat="server"></asp:Label></td>
       <td>
           <asp:TextBox ID="fromOrder_TextBox" Width="100px" MaxLength="8" runat="server"></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator6" ControlToValidate="fromOrder_TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator>
           <a href="#" tabindex="1" onClick="ordlook(1); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>           
       <td align="right" style="padding-right:5px"><asp:Label ID="tojobpoLabel" runat="server"></asp:Label></td>
       <td> <asp:TextBox ID="torder_TextBox" Width="100px" MaxLength="8" runat="server"></asp:TextBox>
       <a href="#" tabindex="1" onClick="ordlook(2); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
       <asp:CompareValidator ID="CompareValidator1" ControlToValidate="torder_TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td></tr>
       <tr><td align="right" style="padding-right:5px">From Job#:</td>
       <td> <asp:TextBox ID="fromjob_TextBox" MaxLength="6" Width="60px" runat="server"></asp:TextBox>
       <asp:TextBox ID="fromjob2_TextBox" Width="30px" runat="server"></asp:TextBox>
       <a href="#" tabindex="1" onClick="job1look(1); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
       <asp:CompareValidator ID="CompareValidator2" ControlToValidate="fromjob2_TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
       <td align="right" style="padding-right:5px">To Job#:</td>
       <td> <asp:TextBox ID="tojob_TextBox" MaxLength="6" Width="60px" runat="server"></asp:TextBox>
       <asp:TextBox ID="tojob2_TextBox" Width="30px" runat="server"></asp:TextBox>
       <a href="#" tabindex="1" onClick="job1look(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
       <asp:CompareValidator ID="CompareValidator3" ControlToValidate="tojob2_TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator> 
       </td></tr>
       <tr><td  align="right" style="padding-right:5px">From Item#:</td>
       <td> <asp:TextBox ID="fromitem_TextBox" MaxLength="15"  runat="server"></asp:TextBox>
       <a href="#" tabindex="1" onClick="loaditemlook(1); return false"><asp:Image ID="loaditem" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
       <td align="right" style="padding-right:5px">To Item#:</td>
       <td> <asp:TextBox ID="toitem_TextBox" MaxLength="15" runat="server"></asp:TextBox>
       <a href="#" tabindex="1" onClick="loaditemlook(2); return false"><asp:Image ID="loaditem2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
       <tr><td align="right" style="padding-right:5px">Order/Job Status:</td>
       <td colspan="3"> 
           <asp:RadioButtonList ID="RadioButtonList2" Width="400px" RepeatColumns="3" RepeatLayout="Flow" CellSpacing="1" runat="server">
           <asp:ListItem Value="O" Text="Open"></asp:ListItem>
           <asp:ListItem Value="C" Text="Close"></asp:ListItem>
           <asp:ListItem Value="A" Text="All"></asp:ListItem>
           </asp:RadioButtonList>
       </td></tr></table>
       </fieldset>
       </td></tr><tr><td>
       
       <fieldset class="shade" style="width:500px"> <legend> <b>Print Options</b></legend>
       <table> <tr><td nowrap> Print Po from? <asp:RadioButtonList ID="RadioButtonList3" RepeatColumns="3" RepeatLayout="Flow" CellSpacing="1" runat="server">
           <asp:ListItem Value="H" Text="Header"></asp:ListItem>
           <asp:ListItem Value="L" Text="Line"></asp:ListItem>
           <asp:ListItem Value="R" Text="Release"></asp:ListItem>
           </asp:RadioButtonList>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           From  <asp:TextBox ID="fromdate_TextBox" Width="60px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' )" runat="server"></asp:TextBox>
           <a href="#" onblur="document.getElementById('fromdate_TextBox').focus()"  tabindex="1" onClick="showCalendarControl(fromdate_TextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a> &nbsp;&nbsp;&nbsp;         
           To  <asp:TextBox ID="todate_TextBox" Width="60px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' )" runat="server"></asp:TextBox>
           <a href="#" onblur="document.getElementById('todate_TextBox').focus()"  tabindex="1" onClick="showCalendarControl(todate_TextBox); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
           </td></tr>
       <tr><td>Print Set Components For: <asp:RadioButtonList ID="RadioButtonList4" RepeatColumns="3" RepeatLayout="Flow" CellSpacing="1" runat="server">
           <asp:ListItem Value="A" Text="Assembled"></asp:ListItem>
           <asp:ListItem Value="U" Text="Unassembled"></asp:ListItem>
           <asp:ListItem Value="B" Text="Both"></asp:ListItem>
           </asp:RadioButtonList> &nbsp;&nbsp;
           <asp:CheckBox ID="CheckBox4"  Text="Transfer Release Lot#" runat="server" />
           </td></tr>
       <tr><td>
       <asp:CheckBox ID="CheckBox8"  Text="Print Department Notes?" runat="server" />
        <asp:TextBox ID="deptnotes_TextBox" Width="80px" runat="server"></asp:TextBox>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
       <asp:CheckBox ID="CheckBox9"  Text="Print Ship ID?" runat="server" />
           <asp:TextBox ID="shipnotes_TextBox" Width="80px" runat="server"></asp:TextBox>
       </td></tr>
       <tr><td><asp:CheckBox ID="CheckBox5"  Text="Print Posted Release in BOL File?" runat="server" />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
       <asp:CheckBox ID="CheckBox6"  Text="Include Overrun" runat="server" />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
       <asp:CheckBox ID="CheckBox7"  Text="Show LWD in 16ths?" runat="server" />
       </td></tr>
       </table>
       </fieldset></td></tr><tr><td>
       
       <fieldset class="shade" style="width:500px"><legend><b>Output Option</b></legend>
       <table><tr><td></td>
       <td>  <asp:CheckBox ID="CheckBox10"  Text="Auto Print Label?" runat="server" /> 
        <asp:CheckBox ID="CheckBox11"  Text="Freeze Label Choice?" runat="server" /></td></tr>
       <tr><td  align="right" style="padding-right:5px">Label Matrix Label File: </td>
       <td>
           <asp:TextBox ID="Labelpath_TextBox" Width="240px" runat="server"></asp:TextBox>
           <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="Labelpath_TextBox" Display="Dynamic" runat="server" ErrorMessage="Please Enter Server Path"></asp:RequiredFieldValidator>
           </td></tr>
       <tr><td align="right" style="padding-right:5px"> # of Labels/Pallet:</td>
       <td> <asp:TextBox ID="labelpallet_TextBox" Width="50px" runat="server"></asp:TextBox> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
       <asp:CompareValidator ID="CompareValidator4" ControlToValidate="labelpallet_TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator>
       Printer Form#:<asp:TextBox ID="printerfrom_TextBox" Width="50px" runat="server"></asp:TextBox>
       <asp:CompareValidator ID="CompareValidator5" ControlToValidate="printerfrom_TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator>
       </td></tr>
       <tr><td align="right" style="padding-right:5px">Text File Path:</td>
       <td><asp:TextBox ID="textpath_TextBox" Width="240px" runat="server"></asp:TextBox></td></tr></table>
       </fieldset>
       </td></tr>
       <tr><td>
      
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              <fieldset style="display:none">
          <asp:Button ID="Button1" OnClick="createtagbutton_click" runat="server" class="buttonM" Text="Createtag" />  </fieldset>
              </td></tr> </table>  
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                           
               
              <ItemTemplate>
                  ldtag:
                  <asp:Label ID="ldtagLabel" runat="server" 
                      Text='<%# Bind("ldtag") %>'></asp:Label><br />                 
                  ldtag2:
                  <asp:Label ID="ldtag2Label" runat="server" Text='<%# Bind("ldtag2") %>' />
                  <br />
                  ord-no:
                  <asp:Label ID="ord_noLabel" runat="server" Text='<%# Bind("[ord-no]") %>' />
                  <br />
                  job-no:
                  <asp:Label ID="job_noLabel" runat="server" Text='<%# Bind("[job-no]") %>' />
                  <br />
                  job-no2:
                  <asp:Label ID="job_no2Label" runat="server" Text='<%# Bind("[job-no2]") %>' />
                  <br />
                  i-no:
                  <asp:Label ID="i_noLabel" runat="server" Text='<%# Bind("[i-no]") %>' />
                  <br />
                  ord-qty:
                  <asp:Label ID="ord_qtyLabel" runat="server" Text='<%# Bind("[ord-qty]") %>' />
                  <br />
                  over-pct:
                  <asp:Label ID="over_pctLabel" runat="server" Text='<%# Bind("[over-pct]") %>' />
                  <br />
                  pcs:
                  <asp:Label ID="pcsLabel" runat="server" Text='<%# Bind("pcs") %>' />
                  <br />
                  bundle:
                  <asp:Label ID="bundleLabel" runat="server" Text='<%# Bind("bundle") %>' />
                  <br />
                  total-unit:
                  <asp:Label ID="total_unitLabel" runat="server" 
                      Text='<%# Bind("[total-unit]") %>' />
                  <br />
                  total-tags:
                  <asp:Label ID="total_tagsLabel" runat="server" 
                      Text='<%# Bind("[total-tags]") %>' />
                  <br />
                  partial:
                  <asp:Label ID="partialLabel" runat="server" Text='<%# Bind("partial") %>' />
                  <br />
                  unit-wt:
                  <asp:Label ID="unit_wtLabel" runat="server" Text='<%# Bind("[unit-wt]") %>' />
                  <br />
                  pallt-wt:
                  <asp:Label ID="pallt_wtLabel" runat="server" Text='<%# Bind("[pallt-wt]") %>' />
                  <br />
                  lot:
                  <asp:Label ID="lotLabel" runat="server" Text='<%# Bind("lot") %>' />
                  <br />
                  i-name:
                  <asp:Label ID="i_nameLabel" runat="server" Text='<%# Bind("[i-name]") %>' />
                  <br />
                  cust-po-no:
                  <asp:Label ID="cust_po_noLabel" runat="server" 
                      Text='<%# Bind("[cust-po-no]") %>' />
                  <br />
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SharpShooterLoadTag" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmOrd_lst" Type="String" />
                  <asp:Parameter Name="prmJob_lst" Type="String" />
                  <asp:Parameter Name="prmBeg_ord" Type="Int32" />
                  <asp:Parameter Name="prmEnd_ord" Type="Int32" />
                  <asp:Parameter Name="prmBeg_job" Type="String" />
                  <asp:Parameter Name="prmBeg_job2" Type="Int32" />
                  <asp:Parameter Name="prmEnd_job" Type="String" />
                  <asp:Parameter Name="prmEnd_job2" Type="Int32" />
                  <asp:Parameter Name="prmBeg_itm" Type="String" />
                  <asp:Parameter Name="prmEnd_itm" Type="String" />
                  <asp:Parameter Name="prmJobPur_rct" Type="String" />
                  <asp:Parameter Name="prmReturn" Type="String" />
                  <asp:Parameter Name="prmReprnt_tg" Type="String" />
                  <asp:Parameter Name="prmScnCs_lbl" Type="String" />
                  <asp:Parameter Name="prmOrdJb_stat" Type="String" />
                  <asp:Parameter Name="prmPrntPo_frm" Type="String" />
                  <asp:Parameter Name="prmFrm_dt" Type="String" />
                  <asp:Parameter Name="prmTo_dt" Type="String" />
                  <asp:Parameter Name="prmPrntSt_comp" Type="String" />
                  <asp:Parameter Name="prmTrnsRls_lot" Type="String" />
                  <asp:Parameter Name="prmPrntDpt_not" Type="String" />
                  <asp:Parameter Name="prmPrntPst_bol" Type="String" />
                  <asp:Parameter Name="prmPrntShp_id" Type="String" />
                  <asp:Parameter Name="prmIncl_Over" Type="String" />
                  <asp:Parameter Name="prmLwd_16th" Type="String" />
                  <asp:Parameter Name="prmAuto_prnt" Type="String" />
                  <asp:Parameter Name="prmFrez_Chos" Type="String" />
                  <asp:Parameter Name="prmLabl_matrx" Type="String" />
                  <asp:Parameter Name="prmLabl_pallt" Type="Int32" />
                  <asp:Parameter Name="prmPrnt_form" Type="Int32" />
                  <asp:Parameter Name="prmText_file" Type="String" />
                  <asp:Parameter Name="prmDpt_lst" Type="String" />
                  <asp:Parameter Name="prmShp_id" Type="String" />
                  <asp:Parameter Name="prmExtra" Type="String" />
                  <asp:Parameter Name="ip_rowid" Type="String" />
                  <asp:Parameter Name="extra" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="prmReckey" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


