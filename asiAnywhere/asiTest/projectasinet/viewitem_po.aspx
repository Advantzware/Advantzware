<%@ Page Language="C#" MasterPageFile="MasterPagepo.master" Debug="true" AutoEventWireup="true" Inherits="viewitem_po" Title="Purchase Order" Codebehind="viewitem_po.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1"  runat="server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    
    <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmAdd(edit) {
        var retVal = makeMsgBox("Confirmation", "Update item on Job file?", 48, 4, 256, 4096);
        if (retVal == 6) {
            var mode = ""
            mode = edit;
            var snum = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poSNumTextBox").value;
            var bnum = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poBNumTextBox").value;
            var jobno = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poJobNoTextBox").value;
            var jobno2 = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poJobNo2TextBox").value;            
            var qty = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poOrdQtyTextBox").value;
            var qtyuom = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poPrQtyUomTextBox").value;
            var pruom = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poPrUomTextBox").value;
            var cost = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poCostTextBox").value;
            var itemlabel = "";
            var line = "";
            if (mode == 1) {
                itemlabel = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemNoTextBox").value;
                line = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_LabelpoLineinsert").innerText;
            }
            if (mode == 2) {
                itemlabel = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemNolabel").innerText;
                line = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_LabelpoLineedit").innerText;
            }
            
            var NewWindow = window.open("replacejobmt.aspx?sno="+ snum +"&bno="+ bnum+"&job="+jobno+"&job2="+jobno2+"&item="+itemlabel+"&qty="+qty+"&qtyuom="+qtyuom+"&pruom="+pruom+"&cost="+cost+"&line="+ line+"&mode="+mode+"", "JobMatLookupWindow", "width=400,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        else {
           
        }
    }
    function ordqty_change() {
        var qtyhid = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
        var retVal = makeMsgBox("Confirmation", "Import Cost?", 48, 4, 256, 4096);
        if (retVal == 6) {
            
            qtyhid.value = "True"            
            var bt = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_qty_button");
            bt.click();
        }
        else {
            qtyhid.value = "False"            
        }

    }
</script>
    
    
<script type="text/javascript" language="javascript">
    window.onload = widfracshow;
    function caljob(mode) {
        var calc = "";
        calc = mode;
        if (mode == 1) {
            var bt = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_job_button");
            bt.click();
        }
        if (mode == 1) {
            var nt = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_job_button");
            nt.click();
        }

    }
    function jobsumchange() {
        var snum = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poSNumTextBox");
        var bnum = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poBNumTextBox");
       // snum.value = "1";
        //bnum.value = "0";
    }
    function widfracshow() {
        var bt = "";
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemTypelabel"))
            bt = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemTypelabel").innerText;
        if (document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemTypeTextBox"))
            bt = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemTypeTextBox").value;

        var show = document.getElementById("Fieldset1");
       
                if (bt == "FG" || bt == "fg")
                    show.style.display = 'none';   
                else
                    show.style.display = 'inline';  
    }
    
function orderlook() {
//    var cust = document.getElementById("ctl00_ContentPlaceHolder1_txt_customer").value;
    var NewWindow = window.open("order_lookup.aspx?customer=", "OrderLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function OrderLookup(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poOrdNoTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poOrdNoTextBox.focus();
}

function AccountLook() {
    
    var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AccountLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poActNumTextBox.value = ReturnObj1;
    // document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poGlDescLabel.innerText = ReturnObj2;
    var dec = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poGlDesclabel");
    dec.innerText = ReturnObj2;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poActNumTextBox.focus();
}

function JobmatLookup(fgitem) {
    var item = "";
    if (fgitem == 2) {
        var itemtext = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemNoTextBox").value;
        item = itemtext;
    }
    else {
        var itemlabel = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemNolabel").innerText;
        item = itemlabel;
    }
      
    
    var job = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poJobNoTextBox").value;
    var job2 = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poJobNo2TextBox").value;
    var NewWindow = window.open("jobmatlook.aspx?item="+item+"&job="+ job +"&job2="+ job2 +"", "jobmatWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function JobMatLook(ReturnObj1, ReturnObj2,ReturnObj3, ReturnObj4,ReturnObj5, ReturnObj6) {
  
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poSNumTextBox.value = ReturnObj5;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poBNumTextBox.value = ReturnObj6;
}

function fgitemall() {
    var NewWindow = window.open("fgitemall_lookup.aspx", "fgitemLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGallLookup(ReturnObj1, ReturnObj2 ) {
  if(ReturnObj1.indexOf(":"))
  {
    var val=ReturnObj1;
    ReturnObj1 = val.replace(":", "\"");  
    }  

    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poItemNoTextBox.value = ReturnObj1;

    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poItemTypeTextBox.value = ReturnObj2;
   
   document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poItemNoTextBox.onchange();
}
 


function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$FormView3$poCustNoTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView3$poCustNoTextBox.focus();
}


function customerpolook() {
    var cust = "";
    //    if(cust=="")
    //    {
    //        var cust=document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1").value;    
    //    }
    var NewWindow = window.open("customerpo_entry_lookup.aspx?customer=" + cust + "", "EstimateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerPOLookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_po.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_po.focus();
}

function jobhdrlookup(fgitem) {
    var item = "";
    if (fgitem == 2) {
        var itemtext = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemNoTextBox").value;
        item = itemtext;
    }
    else {
        var itemlabel = document.getElementById("ctl00_ContentPlaceHolder1_FormView3_poItemNolabel").innerText;
        item = itemlabel;
    }
    var NewWindow = window.open("jobhdrlook.aspx?item="+ item +"", "JobhdrLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function JobhdrLook(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4,ReturnObj5, ReturnObj6) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poJobNoTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poJobNo2TextBox.value = ReturnObj2;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poBNumTextBox.value = ReturnObj3;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poSNumTextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poCustNoTextBox.value = ReturnObj5;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poOrdNoTextBox.value = ReturnObj6;

    document.forms[0].ctl00_ContentPlaceHolder1_FormView3_poJobNoTextBox.onchange();
 }

 function focusval(obj) {
     obj.style.backgroundColor = 'blue';
     obj.style.color = 'white';
 }
 function blurval(obj) {
     obj.style.backgroundColor = 'Window';
     obj.style.color = 'WindowText';
 }
 function setfocus(obj) {
     obj.style.backgroundColor = 'Window';
     obj.style.color = 'WindowText';

 }
 function preEnter(fieldObj, canEdit) {
    fieldObj.style.backgroundColor = 'blue';
    fieldObj.style.color = 'white';
    if (canEdit == "no") {
        fieldObj.blur();
        leaveField(fieldObj);
    }

    enterField(fieldObj);
    return;
}
function preLeave(fieldObj, fieldType, fieldFormat) {
    fieldObj.style.backgroundColor = 'Window';
    fieldObj.style.color = 'WindowText';

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


function clickButton(e, ctl00$ContentPlaceHolder1$btnSearch){ 

      var evt = e ? e : window.event;

      var bt = document.getElementById(ctl00$ContentPlaceHolder1$btnSearch); 

      if (bt){ 

          if (evt.keyCode == 13){ 

                bt.click(); 

                return false; 

          } 

      } 

}

</script>
<div>
    <asp:HiddenField ID="HiddenField1" runat="server" />
<fieldset  style="height:30px;width:670px" class="shade">

    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3">
        
        
        <ItemTemplate>
        <table class="shade">
        <tr><td><b>Po Number:</b></td>
        <td> <asp:Label ID="poNoLabel"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("poNo") %>' /></td>
        <td><b>Po Date#:</b></td>
        <td><asp:Label ID="poDateLabel" runat="server"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" Text='<%# Bind("poDate") %>' /></td>
        <td><b>Whse:</b></td>
        <td><asp:Label ID="poLocLabel" runat="server"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" Text='<%# Bind("poLoc") %>' /></td>
        <td><b>Type:</b></td>
        <td><asp:Label ID="poTypeLabel" runat="server"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" Text='<%# Bind("poType") %>' /></td>
        <td><asp:Label ID="poTMsfLabel" runat="server"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" Text='<%# Bind("poTMsf") %>' /></td>
        <td><b>MSF</b></td>
        </tr>
        </table>            
        </ItemTemplate>
    </asp:FormView></fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectListItemPO" 
        TypeName="browspo">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmpoNo" SessionField="pur_ord_po" 
                Type="Int32" />
            <asp:Parameter Name="prmpoItemNo" Type="String" />
            <asp:Parameter Name="prmpoRecKey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
<div>
   
    <asp:FormView ID="FormView3" OnDataBound="FormView3_DataBound" runat="server" OnUnload="FormView3_Unload" DataSourceID="ObjectDataSource1" >
    
    <EditItemTemplate>
            <asp:Panel ID="Edit_Panel" runat="server" CssClass="shade" DefaultButton="UpdateButton">
            <fieldset>            
            <table class="shade"><tr><td>
            <table class="shade">
            
            <tr><td   nowrap="nowrap">
            <asp:label ID="poItemTypelabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="30px" runat="server" Text='<%# Bind("poItemType") %>' />
            <b>Item#:</b> 
            <asp:label ID="poItemNolabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="110px" runat="server" Text='<%# Bind("poItemNo") %>' /></td>
            <td nowrap="nowrap" align="right" style="padding-right:5px;"><b>Job#:</b>
            <asp:TextBox ID="poJobNoTextBox" MaxLength="6" Width="40" OnTextChanged="pojobedit_TextChange" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poJobNo") %>' />
            <asp:TextBox ID="poJobNo2TextBox" Width="15" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poJobNo2") %>' />
            <a href="#" tabindex="1" onClick="jobhdrlookup(1); return false"><asp:Image ID="JobhdrLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="poJobNo2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td nowrap="nowrap" align="right" style="padding-right:5px;"> <b>S:</b>
            <asp:TextBox ID="poSNumTextBox" MaxLength="2" Width="15" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" OnTextChanged="pojobedit_TextChange" AutoPostBack="true" Text='<%# Bind("poSNum") %>' />
            <a href="#" tabindex="1" onClick="JobmatLookup(1); return false"><asp:Image ID="jobmatlookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="poSNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td nowrap="nowrap" align="right" style="padding-right:5px;"><b>B:</b>
            <asp:TextBox ID="poBNumTextBox" MaxLength="2" Width="15" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" OnTextChanged="pojobedit_TextChange" AutoPostBack="true" Text='<%# Bind("poBNum") %>' />
            <a href="#" tabindex="1" onClick="JobmatLookup(1); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="poBNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            </td>
            <td nowrap="nowrap" align="left" style="padding-right:5px;"><b>Due Date:</b>
            <asp:TextBox ID="poDateTextBox" Width="60" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("poDate") %>' />
                <a href="#" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView3_poDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap="nowrap" align="left" style="padding-right:5px;"><b>Stat:</b>
            <asp:label ID="poStatlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="20px" runat="server" Text='<%# Bind("poStat") %>' /></td>
            
           
            </table></td></tr>
            
            <tr><td><fieldset><table  class="shade"><br />
            <tr><td nowrap align="right"  style="padding-right:5px;"><b>Name:</b></td>
            <td><asp:TextBox ID="poItemNameTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" MaxLength="30" Width="180" runat="server" Text='<%# Bind("poItemName") %>' /></td>
            <td align="right"  style="padding-right:5px;"><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Quantity:</b></td>
            <td><asp:TextBox ID="poOrdQtyTextBox" Width="65" onfocus= "javascript:focusval(this)" onblur="setfocus(this)"  OnTextChanged="editpoOrdQty_TextChange" AutoPostBack="true"  runat="server" Text='<%# Bind("poOrdQty") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="poOrdQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <asp:TextBox ID="poPrQtyUomTextBox" Width="25" OnTextChanged="editpoOrdQty_TextChange" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poPrQtyUom") %>' /></td>
            <td align="right"  style="padding-right:5px;"> <b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Qty:</b> </td>
            <td><asp:label ID="poConsQtylabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poConsQty") %>' />    </td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:label ID="poScrConsUomlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poScrConsUom") %>' /></td>
            </tr>
            <tr><td nowrap align="left"  style="padding-right:5px;"> <b>Desc:</b></td>
            <td><asp:TextBox ID="poDscr1TextBox" Width="180" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" MaxLength="30" runat="server" Text='<%# Bind("poDscr1") %>' /></td>
            <td align="right"  style="padding-right:5px;"> <b>Cost:</b></td>
            <td><asp:TextBox ID="poCostTextBox" OnTextChanged="editpoOrdQty_TextChange" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="65" runat="server" Text='<%# Bind("poCost") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="poCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double"  ErrorMessage="Number Only"></asp:CompareValidator>            
            <asp:TextBox ID="poPrUomTextBox" Width="25" OnTextChanged="editpoOrdQty_TextChange" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poPrUom") %>' /></td>
            <td></td><td><asp:label ID="poConsCostlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poConsCost") %>' /></td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:label ID="poConsUomlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poConsUom") %>' /></td>
            </tr>
            <tr><td nowrap align="right"  style="padding-right:5px;"> &nbsp;<b>Desc:</b></td>
            <td><asp:TextBox ID="poDscr2TextBox" Width="180" MaxLength="30" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poDscr2") %>' /></td>
            <td align="right"  style="padding-right:5px;"> <b>Setup:</b></td>
            <td><asp:TextBox ID="poSetupTextBox" Width="65" OnTextChanged="editpoOrdQty_TextChange" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poSetup") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="poSetupTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td></td>
            <td><asp:label ID="poTMsflabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poTMsf") %>' /></td>
                 <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>MSF</b>
            </td></tr>
            <tr>
            <td colspan="2"> <b>Width:</b>  &nbsp;  &nbsp;  &nbsp; <b>Length:</b> &nbsp;  &nbsp;  &nbsp; 
            <b>Depth:</b>&nbsp; &nbsp;</td>
            <td align="right"  style="padding-right:5px;"><b>Discount: </b></td>
            <td><asp:TextBox ID="poDiscTextBox" OnTextChanged="editpoOrdQty_TextChange" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Width="65" Text='<%# Bind("poDisc") %>' />
            <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="poDiscTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td></td><td><asp:label ID="poTonnagelabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poTonnage") %>' /></td>
                 <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Tons</b>            
            <br /></td>
            </tr>
            <tr><td nowrap align="left" style="padding-right:5px;" colspan="3"><asp:label ID="poSwidlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSwid") %>' />         
            <asp:label ID="poSlenlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSlen") %>' />
            <asp:label ID="poSDeplabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSDep") %>' /></td></tr>
            
            <tr><td nowrap align="left" style="padding-right:5px;" colspan="3">
            <asp:label ID="poWidFraclabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poWidFrac") %>' />                      
            <asp:Label ID="poLenFracLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poLenFrac") %>' />
            <asp:Label ID="poDepFracLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poDepFrac") %>' />
            </td></tr>
            
            </table></fieldset></td></tr><tr><td><fieldset><table>
            
            <tr><td align="right" style="padding-right:5px;"><b>GL#:</b> </td><td><asp:TextBox ID="poActNumTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="130" runat="server" Text='<%# Bind("poActNum") %>' />
               <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td></td><td colspan="2" rowspan="4"> 
            <asp:TextBox ID="poaddressTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Height="100px"  Width="250px" TextMode="MultiLine" Text='<%# Bind("poaddress") %>' />
            </td>
               </tr>
            <tr><td align="right" style="padding-right:5px;"><b>GL Desc:</b></td><td><asp:label ID="poGlDesclabel"  runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="150px" Text='<%# Bind("poGlDesc") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Vendor Item#:</b></td><td><asp:TextBox ID="poVendINoTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="95" MaxLength="15" runat="server" Text='<%# Bind("poVendINo") %>' /></td>
            <td align="right" style="padding-right:5px;"><b>Tax:</b>
            <asp:CheckBox ID="poTaxCheckBox"   runat="server" Width="20"  />
            <asp:label ID="poTextCheckBoxLabel" Visible="false" runat="server" Text='<%# Bind("poTax") %>' ></asp:label></td></tr>
            
            <tr><td align="right" style="padding-right:5px;"><b>Overrun%:</b></td><td><asp:TextBox ID="poOverPctTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Width="35" Text='<%# Bind("poOverPct") %>' />
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="poOverPctTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Underrun%:</b></td><td><asp:TextBox ID="poUnderPctTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Width="35" Text='<%# Bind("poUnderPct") %>' />
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="poUnderPctTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Customer#:</b></td><td> <asp:TextBox ID="poCustNoTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Width="65" MaxLength="8" Text='<%# Bind("poCustNo") %>' />
                <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td><td></td>
            <td align="right" style="padding-right:5px;"><b>&nbsp;&nbsp;&nbsp;&nbsp;Next Price Break Qty:</b></td>
            <td><asp:Label ID="poPbQtyLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="55px" runat="server" Text='<%# Bind("poPbQty") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Order Number:</b></td><td colspan="2"> 
            <asp:TextBox ID="poOrdNoTextBox" runat="server" onfocus= "javascript:focusval(this)"  onblur="setfocus(this);document.getElementById('ctl00_ContentPlaceHolder1_FormView3_poJobNoTextBox').focus()" Width="65" Text='<%# Bind("poOrdNo") %>' />
               <a href="#" tabindex="1" onClick="orderlook(); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
               <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="poOrdNoTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            &nbsp; &nbsp; &nbsp;<b>Total Cost:</b> 
            <asp:Label ID="poTCostLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("poTCost") %>' /></td>
            <td align="right" style="padding-right:5px;"><b>&nbsp;&nbsp;&nbsp;&nbsp;Next Price Break Cost:</b></td>
            <td><asp:Label ID="poPbCstLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="55px" runat="server" Text='<%# Bind("poPbCst") %>' /></td></tr>
            
            
            
            
            </table ></fieldset></td></tr><tr><td><fieldset>
            <table><tr>
            <br /><td>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <b>On Hand</b></td><td><b>On Order</b></td>
            <td><b><asp:label ID="poahdrlabel"  BorderColor="White"  Width="80px" runat="server" /></b></td><td><b>Back Ordered</b></td><td><b>Available</b></td>
            </tr>
            <tr><td>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <asp:TextBox ID="poFiUomlabel" Enabled="false"  Width="30px" runat="server" Text='<%# Bind("poFiUom") %>' /> 
            <asp:TextBox ID="poQOnhlabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poQOnh") %>' />
            </td>
            <td>
            <asp:TextBox ID="poQOnolabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poQOno") %>' />
            </td>
            <td>
            <asp:TextBox ID="poQCommlabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poQComm") %>' />
            </td>
            <td>
            <asp:TextBox ID="poQBacklabel" Enabled="false" Width="98px" runat="server" Text='<%# Bind("poQBack") %>' />
           </td><td>
            <asp:TextBox ID="poQAvaillabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poQAvail") %>' />
            </td></tr>
            <tr><fieldset id="Fieldset1"  > <td>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <asp:TextBox ID="poMsflabel" runat="server" Enabled="false"  Width="30px" Text='<%# Bind("poMsf") %>' />
            <asp:TextBox ID="poMOnhlabel" runat="server" Enabled="false"  Width="90px" Text='<%# Bind("poMOnh") %>' /></td>
            <td><asp:TextBox ID="poMOnolabel" Enabled="false"  Width="90px" runat="server" Text='<%# Bind("poMOno") %>' /></td>
            <td><asp:TextBox ID="poMCommlabel"  Enabled="false" Width="90px" runat="server" Text='<%# Bind("poMComm") %>' /></td>
            <td><asp:TextBox ID="poMBacklabel"  Enabled="false" Width="98px" runat="server" Text='<%# Bind("poMBack") %>' /></td>
            <td><asp:TextBox ID="poMAvaillabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poMAvail") %>' /></td></fieldset></tr>
            
            <tr><td ><fieldset id="displayqty" class="shade"   style="display:none;">
            <asp:label ID="LabelpoLineedit" BackColor="Turquoise"  BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("poLine") %>' />
            <asp:Button ID="qty_button" runat="server"  Text="qtybutton" OnClick="editpoOrdQty_TextChange" />
            <asp:Button ID="job_button" runat="server"  Text="jobbutton" OnClick="pojobedit_TextChange" /></fieldset>
            
            </td></tr>
            </table></fieldset>
            </td>
            </tr>
            <tr><td  colspan="4">
            
            <asp:Button ID="UpdateButton" runat="server" class="buttonM" OnClick="UpdateButton_Click" CausesValidation="True" 
                 Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" class="buttonM"
                CausesValidation="False" CommandName="Cancel" Text="Cancel" />            
            
            </td></tr> 
            </table> </fieldset> 
            </asp:Panel>
               
        </EditItemTemplate>
    <InsertItemTemplate>
            <asp:Panel ID="Insert_Panel" runat="server" CssClass="shade" DefaultButton="InsertButton">
            <fieldset>
            
            <table class="shade" ><tr><td nowrap>
            <table class="shade" >
           
            <tr><td >
            <asp:TextBox ID="poItemTypeTextBox" Width="20" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poItemType") %>' />
            <b>Item#:</b> 
            <asp:TextBox ID="poItemNoTextBox" Width="100" OnTextChanged="poitemno_TextChange" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" MaxLength="15" AutoPostBack="true" runat="server" Text='<%# Bind("poItemNo") %>' />
            <a href="#" tabindex="1" onClick="fgitemall(); return false"><asp:Image ID="fgitemlookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Job#:</b>
            <asp:TextBox ID="poJobNoTextBox" Width="35" OnTextChanged="pojobno_TextChange" onkeyup="jobsumchange()" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" MaxLength="6" runat="server" Text='<%# Bind("poJobNo") %>' />
            <asp:TextBox ID="poJobNo2TextBox" Width="15" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poJobNo2") %>' />
            <a href="#" tabindex="1" onClick="jobhdrlookup(2); return false"><asp:Image ID="JobhdrLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="poJobNo2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td nowrap align="right" style="padding-right:5px;"> <b>S:</b>
            <asp:TextBox ID="poSNumTextBox" MaxLength="2" Width="15" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" OnTextChanged="pojobno_TextChange" AutoPostBack="true" Text='<%# Bind("poSNum") %>' />
            <a href="#" tabindex="1" onClick="JobmatLookup(2); return false"><asp:Image ID="jobmatlookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
             <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="poSNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td nowrap align="right" style="padding-right:5px;"><b>B:</b>
            <asp:TextBox ID="poBNumTextBox" MaxLength="2" Width="15" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" OnTextChanged="pojobno_TextChange" AutoPostBack="true" Text='<%# Bind("poBNum") %>' />
            <a href="#" tabindex="1" onClick="JobmatLookup(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="poBNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Due Date:</b>
            <asp:TextBox ID="poDateTextBox" Width="60" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("poDate") %>' />
                <a href="#" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView3_poDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap ><b>Stat:</b>
            <asp:label ID="poStatlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="20px" runat="server" Text='<%# Bind("poStat") %>' /></td>
            </tr>
            </table></td></tr>
            
            <tr><td ><fieldset><table  class="shade" ><br />
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Name:</b></td>
            <td><asp:TextBox ID="poItemNameTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="180" MaxLength="30" runat="server" Text='<%# Bind("poItemName") %>' /></td>
            <td align="right" style="padding-right:5px;"><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Quantity:</b></td>
            <td><asp:TextBox ID="poOrdQtyTextBox" onfocus= "javascript:focusval(this)"  onblur="ordqty_change();setfocus(this)" OnTextChanged="poqty_TextChange" AutoPostBack="true" Width="65"  runat="server" Text='<%# Bind("poOrdQty") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="poOrdQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <asp:TextBox ID="poPrQtyUomTextBox" Width="25" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" OnTextChanged="poqty_TextChange" AutoPostBack="true" runat="server" Text='<%# Bind("poPrQtyUom") %>' /></td>
            <td align="right" style="padding-right:5px;"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty:</b> </td>
            <td><asp:label ID="poConsQtylabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poConsQty") %>' /> </td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:label ID="poScrConsUomlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poScrConsUom") %>' /></td>
            </tr>
            
            <tr><td nowrap align="left" style="padding-right:5px;"> &nbsp;<b>Desc:</b></td>
            <td><asp:TextBox ID="poDscr1TextBox" Width="180" MaxLength="30" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poDscr1") %>' /></td>
            <td align="right" style="padding-right:5px;"> <b>Cost:</b></td>
            <td><asp:TextBox ID="poCostTextBox" Width="65" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" OnTextChanged="poqty_TextChange" AutoPostBack="true" runat="server" Text='<%# Bind("poCost") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="poCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>            
            <asp:TextBox ID="poPrUomTextBox" Width="25" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" OnTextChanged="poqty_TextChange" AutoPostBack="true" runat="server" Text='<%# Bind("poPrUom") %>' /></td>
            <td></td><td><asp:label ID="poConsCostlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poConsCost") %>' /></td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:label ID="poConsUomlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poConsUom") %>' /></td>
            </tr>            
            <tr><td nowrap align="left" style="padding-right:5px;"> &nbsp;<b>Desc:</b></td>
            <td><asp:TextBox ID="poDscr2TextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="180" MaxLength="30" runat="server" Text='<%# Bind("poDscr2") %>' /></td>
            <td align="right" style="padding-right:5px;"><b>Setup:</b></td>
            <td><asp:TextBox ID="poSetupTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="65" OnTextChanged="poqty_TextChange" AutoPostBack="true" runat="server" Text='<%# Bind("poSetup") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="poSetupTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td></td><td><asp:label ID="poTMsflabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poTMsf") %>' /></td>
                <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>MSF</b>
            </td></tr>
            <tr>
            <td colspan="2"> <b>Width:</b>  &nbsp;  &nbsp;  &nbsp; <b>Length:</b> &nbsp;  &nbsp;  &nbsp;<b>Depth:</b></td>
            <td align="right" style="padding-right:5px;"><b>Discount: </b></td>
            <td><asp:TextBox ID="poDiscTextBox"  onfocus= "javascript:focusval(this)" onblur="setfocus(this)" OnTextChanged="poqty_TextChange" AutoPostBack="true" runat="server" Width="65" Text='<%# Bind("poDisc") %>' />
            <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="poDiscTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator></td>
            <td></td>
            <td><asp:label ID="poTonnagelabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poTonnage") %>' /></td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Tons</b>            
            </td> </tr>
            <tr><td  colspan="2"><asp:label ID="poSwidlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSwid") %>' />         
            <asp:label ID="poSlenlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSlen") %>' />
            <asp:label ID="poSDeplabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSDep") %>' /></td></tr>
            
            <tr><td colspan="3">
            <asp:label ID="poWidFraclabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poWidFrac") %>' />                      
            <asp:Label ID="poLenFracLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poLenFrac") %>' />
            <asp:Label ID="poDepFracLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poDepFrac") %>' />
            </td></tr>
           
            </table></fieldset></td></tr><tr><td nowrap><fieldset><table class="shade" >
           
            <tr><td align="right" style="padding-right:5px;"><b>GL#:</b></td>
            <td><asp:TextBox ID="poActNumTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("poActNum") %>' />
               <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td></td><td colspan="2" rowspan="4">
            <asp:TextBox ID="poaddressTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" TextMode="MultiLine" Height="100px" Width="250px" Text='<%# Bind("poaddress") %>' />
            </td>
               </tr>
            <tr><td align="right" style="padding-right:5px;"><b>GL Desc:</b></td><td><asp:label ID="poGlDesclabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="150px" Text='<%# Bind("poGlDesc") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Vendor Item#:</b></td>
            <td><asp:TextBox ID="poVendINoTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="95" MaxLength="15" runat="server" Text='<%# Bind("poVendINo") %>' /></td>
            <td align="right" style="padding-right:5px;"><b>Tax:</b>
            <asp:CheckBox ID="poTaxCheckBox"  runat="server" Width="20" /></td></tr>
            
            <tr><td align="right" style="padding-right:5px;"><b>Overrun%:</b></td>
            <td><asp:TextBox ID="poOverPctTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="35" Text='<%# Bind("poOverPct") %>' />
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="poOverPctTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Underrun%:</b></td>
            <td><asp:TextBox ID="poUnderPctTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Width="35" Text='<%# Bind("poUnderPct") %>' />
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="poUnderPctTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Customer#:</b></td>
            <td><asp:TextBox ID="poCustNoTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Width="65" MaxLength="8" Text='<%# Bind("poCustNo") %>' />
                <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td><td></td>
            <td align="right" style="padding-right:5px;"><b>&nbsp;&nbsp;&nbsp;&nbsp; Next Price Break Qty:</b></td>
            <td><asp:Label ID="poPbQtyLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="55px" runat="server" Text='<%# Bind("poPbQty") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Order Number:</b></td>
            <td colspan="2"><asp:TextBox ID="poOrdNoTextBox" onfocus= "javascript:focusval(this)"  runat="server" Width="65" onblur="setfocus(this);document.getElementById('ctl00_ContentPlaceHolder1_FormView3_poItemNoTextBox').focus()" Text='<%# Bind("poOrdNo") %>' />
               <a href="#" tabindex="1" onClick="orderlook(); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
               <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="poOrdNoTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <b>&nbsp;&nbsp; Total Cost:</b>
            <asp:Label ID="poTCostLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("poTCost") %>' /></td>
            <td><b align="right" style="padding-right:5px;">&nbsp;&nbsp;&nbsp;&nbsp; Next Price Break Cost:</b></td>
            <td> <asp:Label ID="poPbCstLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="55px" runat="server" Text='<%# Bind("poPbCst") %>' /></td></tr>
            
            
            
            
            </table ></fieldset></td></tr><tr><td nowrap><br />
            <fieldset><table ><tr>
            <td> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <b>On Hand</b></td><td><b>On Order</b></td>
            <td><asp:label ID="poahdrlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" runat="server"  /></td><td><b>Back Ordered</b></td><td><b>Available</b></td>
            </tr>
            <tr><td>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <asp:Label ID="notreturn"  runat="server" ></asp:Label>
            <asp:TextBox ID="poFiUomlabel" Enabled="false" Width="30px" runat="server" Text='<%# Bind("poFiUom") %>' />
            <asp:TextBox ID="poQOnhlabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poQOnh") %>' />
            </td>
            <td>
            <asp:TextBox ID="poQOnolabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poQOno") %>' />
            </td>
            <td>
            <asp:TextBox ID="poQCommlabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poQComm") %>' />
            </td>
            <td>
            <asp:TextBox ID="poQBacklabel" Enabled="false" Width="98px" runat="server" Text='<%# Bind("poQBack") %>' />
           </td><td>
            <asp:TextBox ID="poQAvaillabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poQAvail") %>' />
            </td></tr>
            <tr><div id="Fieldset1"  >
            <td> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <asp:TextBox ID="poMsflabel" Enabled="false" runat="server"  Width="30px" Text='<%# Bind("poMsf") %>' />
            <asp:TextBox ID="poMOnhlabel" Enabled="false" runat="server"  Width="90px" Text='<%# Bind("poMOnh") %>' /></td>
            <td><asp:TextBox ID="poMOnolabel"  Enabled="false" Width="90px" runat="server" Text='<%# Bind("poMOno") %>' /></td>
            <td><asp:TextBox ID="poMCommlabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poMComm") %>' /></td>
            <td><asp:TextBox ID="poMBacklabel" Enabled="false" Width="98px" runat="server" Text='<%# Bind("poMBack") %>' /></td>
            <td><asp:TextBox ID="poMAvaillabel" Enabled="false" Width="90px" runat="server" Text='<%# Bind("poMAvail") %>' /></td></div></tr>
            
            
                <tr><td ><fieldset id="displayqty" class="shade"   style="display:none;">
                <asp:label ID="LabelpoLineinsert"  BackColor="Turquoise"  BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("poLine") %>' />
                <asp:Button ID="qty_button" runat="server"  Text="qtybutton" OnClick="poqty_TextChange" />
                <asp:Button ID="job_button" runat="server"  Text="jobbutton" OnClick="pojobno_TextChange" /></fieldset></td></tr>
            </table></fieldset>
            </td>
            </tr>
            <tr><td colspan="4"><br />            
            <asp:Button ID="InsertButton" class="buttonM" OnClick="insertadd" runat="server" CausesValidation="True" 
                 Text="Save" />
            <asp:Button ID="InsertCancelButton" class="buttonM" OnClick="InsertCancelButton_Click" runat="server" 
                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                
                </td></tr> 
            </table>   </fieldset> </asp:Panel>
            
        </InsertItemTemplate>
    <ItemTemplate>
            <fieldset>
            <table class="shade"><tr><td>
            <table class="shade">           
            <tr><td nowrap align="left" style="padding-right:5px;">
            <asp:label ID="poItemTypelabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="30px" runat="server" Text='<%# Bind("poItemType") %>' />
            <b>Item#:</b> 
            <asp:label ID="poItemNolabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px" runat="server" Text='<%# Bind("poItemNo") %>' /></td>
            <td nowrap align="left" style="padding-right:5px;"><b>Job#:</b>
            <asp:Label ID="poJobNoLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="60" runat="server" Text='<%# Bind("poJobNo") %>' />
            <asp:Label ID="poJobNo2Label" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="25" runat="server" Text='<%# Bind("poJobNo2") %>' />
            </td>
            <td nowrap align="left" style="padding-right:5px;"> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;<b>S:</b>
            <asp:label ID="poSNumlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="25px" runat="server" Text='<%# Bind("poSNum") %>' /></td>
            <td nowrap align="left" style="padding-right:5px;"><b>B:</b>
            <asp:label ID="poBNumlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="25px" runat="server" Text='<%# Bind("poBNum") %>' /></td>
            <td nowrap align="left" style="padding-right:5px;"><b>Due Date:</b>
            <asp:Label ID="poDateLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65" runat="server"  Text='<%# Bind("poDate") %>' />
                </td>
            <td nowrap align="left" style="padding-right:5px;"><b>Stat:</b>
            <asp:label ID="poStatlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="30px" runat="server" Text='<%# Bind("poStat") %>' /></td>
            
            </tr>
            </table>
            
            <tr><td><fieldset><table  class="shade"><br /> 
            <tr><td align="right" style="padding-right:5px;"><b>Name:</b></td>
            <td><asp:Label ID="poItemNameLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="180" runat="server" Text='<%# Bind("poItemName") %>' /></td>
            <td align="right" style="padding-right:5px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Quantity:</b></td>
            <td><asp:Label ID="poOrdQtyLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65" runat="server" Text='<%# Bind("poOrdQty") %>' />
            <asp:Label ID="poPrQtyUomLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="25" runat="server" Text='<%# Bind("poPrQtyUom") %>' /></td></td>
            <td align="right" style="padding-right:5px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty:</b></td>
            <td><asp:label ID="poConsQtylabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("poConsQty") %>' /></td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:label ID="poScrConsUomlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poScrConsUom") %>' /></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Desc:</b></td>
            <td><asp:Label ID="poDscr1Label" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="180" runat="server" Text='<%# Bind("poDscr1") %>' /></td>
            <td align="right" style="padding-right:5px"><b>Cost:</b></td>
            <td><asp:Label ID="poCostLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65" runat="server" Text='<%# Bind("poCost") %>' /> 
            <asp:Label ID="poPrUomlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="25" runat="server" Text='<%# Bind("poPrUom") %>' /></td><td></td>
            <td><asp:label ID="poConsCostlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("poConsCost") %>' /></td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:label ID="poConsUomlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poConsUom") %>' /></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Desc:</b></td>
            <td><asp:Label ID="poDscr2Label" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="180" runat="server" Text='<%# Bind("poDscr2") %>' /></td>
            <td align="right" style="padding-right:5px"><b>Setup:</b></td>
            <td><asp:Label ID="poSetupLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65" runat="server" Text='<%# Bind("poSetup") %>' /></td>
            <td></td>
            <td><asp:label ID="poTMsflabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("poTMsf") %>' /></td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>MSF</b></td></tr>
            <tr><td colspan="2"><b>Width:</b>  &nbsp;  &nbsp;  &nbsp; <b>Length:</b> &nbsp;  &nbsp;  &nbsp; <b>Depth:</b></td>
            <td align="right" style="padding-right:5px;"><b>Discount:</b></td>
            <td><asp:Label ID="poDiscLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65" Text='<%# Bind("poDisc") %>' /></td>
            <td></td><td><asp:label ID="poTonnagelabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("poTonnage") %>' /></td>
            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Tons</b>   </td></tr>
            <tr><td colspan="2">
            <asp:label ID="poSwidlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSwid") %>' />         
            <asp:label ID="poSlenlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSlen") %>' />
            <asp:label ID="poSDeplabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poSDep") %>' />
            </td><td></td><td></td><td></td><td></td><td></td></tr>
            
            <tr><td colspan="2">
            <asp:label ID="poWidFraclabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poWidFrac") %>' />                      
            <asp:Label ID="poLenFracLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poLenFrac") %>' />
            <asp:Label ID="poDepFracLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" runat="server" Text='<%# Bind("poDepFrac") %>' />
            </td><td></td><td></td><td></td><td></td><td></td></tr>
                        
            </table></fieldset></td></tr>
            <tr><td><fieldset><table>
            
            <tr><td align="right" style="padding-right:5px;"><b>GL#:</b></td>
            <td><asp:Label ID="poActNumLabel" Width="95" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("poActNum") %>' />
               </td><td></td><td colspan="2" rowspan="4">
               <asp:TextBox ID="poaddressTextBox" runat="server" Enabled="false" TextMode="MultiLine" Height="100px" Width="250px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  Text='<%# Bind("poaddress") %>' />
               </td>
               
               </tr>
            <tr><td align="right" style="padding-right:5px;"><b>GL Desc:</b></td><td><asp:label ID="poGlDesclabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="150px" Text='<%# Bind("poGlDesc") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Vendor Item#:</b></td><td><asp:Label ID="poVendINoLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="95" runat="server" Text='<%# Bind("poVendINo") %>' /></td>
            <td><b align="right" style="padding-right:5px;">Tax:</b>
            <asp:CheckBox ID="poTaxCheckBox" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Enabled="false" Width="20px"  />
            <asp:label ID="poTextCheckBoxLabel" Visible="false" runat="server" Text='<%# Bind("poTax") %>' ></asp:label>
            </td></tr>
            
            <tr><td align="right" style="padding-right:5px;"><b>Overrun%:</b></td>
            <td><asp:Label ID="poOverPctLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="35" Text='<%# Bind("poOverPct") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Underrun%:</b></td>
            <td><asp:Label ID="poUnderPctLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="35" Text='<%# Bind("poUnderPct") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Customer#:</b></td>
            <td><asp:Label ID="poCustNoLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65" Text='<%# Bind("poCustNo") %>' />
                </td><td></td>
            <td align="right" style="padding-right:5px;"><b> &nbsp; &nbsp; &nbsp; &nbsp; Next Price Break Qty:</b></td>
            <td><asp:Label ID="poPbQtyLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="55px" runat="server" Text='<%# Bind("poPbQty") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Order Number:</b></td>
            <td colspan="2"><asp:Label ID="poOrdNoLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65" Text='<%# Bind("poOrdNo") %>' />
              &nbsp;&nbsp; <b>Total Cost:</b>
            <asp:Label ID="poTCostLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("poTCost") %>' /></td>
            <td align="right" style="padding-right:5px;"><b> &nbsp; &nbsp; &nbsp; &nbsp; Next Price Break Cost:</b></td>
            <td> <asp:Label ID="poPbCstLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="55px" runat="server" Text='<%# Bind("poPbCst") %>' /></td></tr>
            
            
            
            
            </table ></fieldset></td></tr><tr><td><fieldset>
            <table><tr>
            <br /><td>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <b>On Hand</b></td><td><b>On Order</b></td>
            <td><b><asp:label ID="poahdrlabel"  BorderColor="White"  Width="80px" runat="server" /></b></td><td><b>Back Ordered</b></td><td><b>Available</b></td>
            </tr>
            <tr><td>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <asp:label ID="poFiUomlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="30px" runat="server" Text='<%# Bind("poFiUom") %>' /> 
            <asp:label ID="poQOnhlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poQOnh") %>' />
            </td>
            <td>
            <asp:label ID="poQOnolabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poQOno") %>' />
            </td>
            <td>
            <asp:label ID="poQCommlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poQComm") %>' />
            </td>
            <td>
            <asp:label ID="poQBacklabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="98px" runat="server" Text='<%# Bind("poQBack") %>' />
           </td><td>
            <asp:label ID="poQAvaillabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poQAvail") %>' />
            </td></tr>
            <tr><div id="Fieldset1"  ><td>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <asp:label ID="poMsflabel" runat="server"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="30px" Text='<%# Bind("poMsf") %>' />
            <asp:label ID="poMOnhlabel" runat="server"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" Text='<%# Bind("poMOnh") %>' /></td>
            <td><asp:label ID="poMOnolabel"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poMOno") %>' /></td>
            <td><asp:label ID="poMCommlabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poMComm") %>' /></td>
            <td><asp:label ID="poMBacklabel"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="98px" runat="server" Text='<%# Bind("poMBack") %>' /></td>
            <td><asp:label ID="poMAvaillabel"  BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="90px" runat="server" Text='<%# Bind("poMAvail") %>' /></td></div></tr>
            </table></fieldset>
            </td>
            </tr>
            <tr><td colspan="4">
            <asp:label ID="LabelpoLine" BackColor="Turquoise" Visible="false"  BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("poLine") %>' />
            <asp:Button ID="NewButton" runat="server" CausesValidation="False"  CommandName="New" CssClass="buttonM" 
                   Text="Add">
                </asp:Button> 
            <asp:Button ID="EditButton" runat="server" CausesValidation="False" CommandName="Edit" CssClass="buttonM" 
                   Text="Update">
                </asp:Button>
                <asp:Button ID="DeleteButton" runat="server"  CssClass="buttonM" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')"
                    Text="Delete">
                </asp:Button>                  
            
            </td></tr></table>   </fieldset>                                 
                
                
                
        </ItemTemplate>
    
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectViewItemPo" TypeName="browspo">
        <SelectParameters>            
            <asp:Parameter Name="prmUser"  Type="String" />            
            <asp:Parameter Name="prmAction" DefaultValue="View"  Type="String"  />
            <asp:SessionParameter Name="prmpoLine" SessionField="pur_ord_po_line_no" Type="Int32" />
            <asp:SessionParameter Name="prmPoNo" SessionField="pur_ord_po" Type="Int32"   />
            <asp:Parameter Name="prmPoDate"  Type="String" />
            <asp:Parameter Name="prmpoLoc"  Type="String" />
            <asp:Parameter Name="prmpoType"  Type="String" />
            <asp:Parameter Name="prmpoTMsf"  Type="Decimal" />
            <asp:Parameter Name="prmpoStat"  Type="String" />
            <asp:Parameter Name="prmpoItemNo" Type="String"></asp:Parameter>
            <asp:Parameter Name="prmpoItemName" Type="String" />
            <asp:Parameter Name="prmpoJobNo" Type="String" />
            <asp:Parameter Name="prmpoJobNo2" Type="Int32" />
            <asp:Parameter Name="prmpoSNum" Type="Int32" />
            <asp:Parameter Name="prmpoOrdQty" Type="Decimal" />
            <asp:Parameter Name="prmpoCost" Type="Decimal" />
            <asp:Parameter Name="prmpoCustNo" Type="String" />
            <asp:Parameter Name="prmpoDueDate" Type="String" />
            <asp:Parameter Name="prmpoItemType" Type="String" />
            <asp:Parameter Name="prmpoBNum" Type="Int32" />
            <asp:Parameter Name="prmpoPrQtyUom" Type="String" />
            <asp:Parameter Name="prmpoConsQty" Type="Decimal" />
            <asp:Parameter Name="prmpoDscr" Type="String" />
            <asp:Parameter Name="prmpoDscr2" Type="String" />
            <asp:Parameter Name="prmpoPrUom" Type="String" />
            <asp:Parameter Name="prmpoConsCost" Type="Decimal" />
            <asp:Parameter Name="prmpoConsUom" Type="String" />
            <asp:Parameter Name="prmpoSetup" Type="Decimal" />
            <asp:Parameter Name="prmpoSwid" Type="Decimal" />
            <asp:Parameter Name="prmpoSlen" Type="Decimal" />
            <asp:Parameter Name="prmpoDisc" Type="Decimal" />
            <asp:Parameter Name="prmpoActNum" Type="String" />
            <asp:Parameter Name="prmpoVendINo" Type="String" />
            <asp:Parameter Name="prmpoTax" Type="String" />
            <asp:Parameter Name="prmpoOverPct" Type="Decimal" />
            <asp:Parameter Name="prmpoUnderPct" Type="Decimal" />
            <asp:Parameter Name="prmpoOrdNo" Type="Int32" />
            <asp:Parameter Name="prmpoTCost" Type="Decimal" />
            <asp:Parameter Name="prmpoFiUom" Type="String" />
            <asp:Parameter Name="prmpoScrConsUom" Type="String" />
            <asp:Parameter Name="prmpoSDep" Type="Decimal" />
            <asp:Parameter Name="prmpoWidFrac" Type="String" />
            <asp:Parameter Name="prmpoLenFrac" Type="String" />
            <asp:Parameter Name="prmpoDepFrac" Type="String" />
            <asp:Parameter Name="prmpoGlDesc" Type="String" />
            <asp:Parameter Name="prmpoPbQty" Type="Decimal" />
            <asp:Parameter Name="prmpoPbCst" Type="Decimal" />
            <asp:Parameter Name="prmpoQOnh" Type="Decimal" />
            <asp:Parameter Name="prmpoQOno" Type="Decimal" />
            <asp:Parameter Name="prmpoQComm" Type="Decimal" />
            <asp:Parameter Name="prmpoQBack" Type="Decimal" />
            <asp:Parameter Name="prmpoQAvail" Type="Decimal" />
            <asp:Parameter Name="prmpoMOnh" Type="Decimal" />
            <asp:Parameter Name="prmpoMOno" Type="Decimal" />
            <asp:Parameter Name="prmpoMComm" Type="Decimal" />
            <asp:Parameter Name="prmpoMBack" Type="Decimal" />
            <asp:Parameter Name="prmpoMAvail" Type="Decimal" />
            <asp:Parameter Name="prmpoMsf" Type="String" />
            <asp:Parameter Name="prmpoTonnage" Type="Decimal" />
            <asp:Parameter Name="prmpoaddress" Type="String" />
            <asp:Parameter Name="prmpoRecKey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

</asp:Content>
