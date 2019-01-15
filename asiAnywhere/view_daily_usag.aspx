<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="view_daily_usag" Codebehind="view_daily_usag.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Customer Plant Usages</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">    
    </script>
    
    <script>
    function Datelook(){ 
    var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function Datelookup(obj)
    {
     document.forms[0].FormView1_vUsgDateTextBox.value=obj;
 }
 function setdate() {
     var vdate = document.getElementById("FormView1_vUsgDateTextBox");
     vdate.focus();
 }
    function usagpolook(){ 
        var NewWindow = window.open("usagpo_lookup.aspx","CustomerPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function UsagPoLookup(obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12)
    {
        var custpart = document.getElementById("FormView1_vCustPartNumTextBox");
        var fgitem = document.getElementById("FormView1_vFgItmNumTextBox");
        var line = document.getElementById("FormView1_vCustPoLineNumTextBox");
        var vandercode = document.getElementById("FormView1_vCustVenCodeTextBox");
        var deptcode = document.getElementById("FormView1_vCustDptCodTextBox");
        document.forms[0].FormView1_vCustPoNumTextBox.value=obj2;     
        document.forms[0].FormView1_vVenOrdNumTextBox.value=obj3;
     
        document.forms[0].FormView1_vCustPlantIdTextBox.value=obj8;
     
        document.forms[0].FormView1_vVenJobNumTextBox.value=obj10;
        document.forms[0].FormView1_vVenJob2NumTextBox.value=obj11;
        document.forms[0].FormView1_vItmSelPriceTextBox.value=obj12;
        document.forms[0].HiddenFGitem.value=obj1;
        document.forms[0].HiddenVanderCode.value=obj7;
        document.forms[0].HiddenDeptCode.value=obj9;
        document.forms[0].HiddenLine.value=obj4;
        custpart.value = obj6;
        fgitem.value = obj1;
        line.value = obj4;
        vandercode.value = obj7;
        deptcode.value = obj9;
        document.forms[0].FormView1_vCustPoNumTextBox.onchange();
    }
    function usagorderlook(){ 
        var order1= document.getElementById("FormView1_vVenOrdNumTextBox").value;
        var fgitem = document.getElementById("FormView1_vFgItmNumTextBox").value;
        var NewWindow = window.open("usagorder_lookup.aspx?order="+ order1+"&item="+ fgitem +"","CustomerPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function UsagOrderLookup(obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8)
    {
        document.forms[0].FormView1_vVenOrdNumTextBox.value=obj1;    
        document.forms[0].FormView1_vVenJobNumTextBox.value=obj6;
        document.forms[0].FormView1_vVenJob2NumTextBox.value = obj7;
        document.forms[0].FormView1_vVenJobNumTextBox.focus();
    }
    function usagcustpart(){ 
        var custpart = document.getElementById("FormView1_vCustPartNumTextBox").value;
        var NewWindow = window.open("usagcustpart_lookup.aspx?part="+ custpart+"","CustomerPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function UsagCustPartLookup(obj1,obj2,obj3,obj4,obj5)
    {
        var custpart = document.getElementById("FormView1_vCustPartNumTextBox");
        var fgitem = document.getElementById("FormView1_vFgItmNumTextBox");    
        var vandercode = document.getElementById("FormView1_vCustVenCodeTextBox");
        var deptcode = document.getElementById("FormView1_vCustDptCodTextBox");
          
     document.forms[0].FormView1_vCustPlantIdTextBox.value=obj4;
     
     custpart.value = obj1;
     fgitem.value = obj2;    
     vandercode.value = obj3;
     deptcode.value = obj5;
     document.forms[0].HiddenFGitem.value=obj2;
     document.forms[0].HiddenVanderCode.value=obj3;
     document.forms[0].HiddenDeptCode.value = obj5;
     document.forms[0].FormView1_vCustPartNumTextBox.onchange();
         
    }
     function usagplantlook(){ 
     var vandercode = document.getElementById("FormView1_vCustVenCodeTextBox").value;
     var fgitem = document.getElementById("FormView1_vFgItmNumTextBox").value;
     var NewWindow = window.open("usagplant_lookup.aspx?vander="+ vandercode+"&item="+ fgitem +"","CustomerPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function UsagPlantLookup(obj1,obj2,obj3,obj4,obj5,obj6)
    {
    var deptcode = document.getElementById("FormView1_vCustDptCodTextBox");
     document.forms[0].FormView1_vCustPlantIdTextBox.value=obj2;   
     deptcode.value = obj3;

     document.forms[0].HiddenDeptCode.value = obj3;
     document.forms[0].FormView1_vCustPlantIdTextBox.onchange();
     

    }

    function Datelook1()
    {
    document.forms[0].FormView1_vUsgDateTextBox.value="";
    Datelook();
    }
function datevalidate()
{
    var date=document.getElementById("FormView1$vUsgDateTextBox").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("FormView1$vUsgDateTextBox").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("FormView1$vUsgDateTextBox").value = date + "/";
    }
    
}
    
    </script>
    </head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
        <asp:HiddenField ID="HiddenFGitem" runat="server" />
        <asp:HiddenField ID="HiddenVanderCode" runat="server" />
        <asp:HiddenField ID="HiddenDeptCode" runat="server" />
        <asp:HiddenField ID="HiddenLine" runat="server" />
        <asp:HiddenField ID="HiddenField5" runat="server" />
    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Customer Plant Usages&nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;&nbsp;&nbsp;
            <b>Company:</b>&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
    
    <div>
    
    <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li  >
    <asp:LinkButton ID="lnk_list" runat="server" OnClick="lnk_list_click">List Usage</asp:LinkButton></li>
    <li class="selected" ><asp:LinkButton ID="lnk_view" runat="server" OnClick="lnk_view_click">View Usage</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
    <br />
        <asp:Button ID="addnewbutton" CssClass = "button" OnClick="newbutton_click" runat="server" Text="Add" />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender">
            <EditItemTemplate>
                <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Seq#:</b></td>
                <td><asp:Label ID="vSeqNumTextBox" Width="80px" runat="server" Text='<%# Bind("vSeqNum") %>'>
                </asp:Label></td>
                <%--<td align="right" style="padding-right:5px"><b>Customer Usage Date:</b></td>
                <td><asp:TextBox ID="vUsgDateTextBox" runat="server" Text='<%# Bind("vUsgDate") %>'>
                </asp:TextBox></td></tr>--%>
               <td style="padding-right:5px;" align="right"><b> Customer Usage Date: </b></td>
               <td nowrap><b>              
                <asp:TextBox ID="vUsgDateTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" runat="server" Text='<%# Bind("vUsgDate","{0:MM/dd/yyyy}") %>'>
                </asp:TextBox> 
                
               </b>
               <a href="#" tabindex="1" onblur="setdate()" onClick="showCalendarControl(FormView1_vUsgDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
               </td>
               </tr>
                <tr><td align="right" style="padding-right:5px"><b>Quantity Used:</b></td>
                <td><asp:TextBox ID="vQtyUsedTextBox" MaxLength="7" runat="server" Text='<%# Bind("vQtyUsed") %>'>
                </asp:TextBox><asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vQtyUsedTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                    <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vQtyUsedTextBox" runat="server" Display="dynamic"  ErrorMessage="Usage Quantity must be grater than 0"></asp:RequiredFieldValidator>     </td>        
                <td align="right" style="padding-right:5px"><b>Cust Plant on Hand Qty:</b></td>
                <td><asp:Label ID="vCustHandQtyTextBox" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("vCustHandQty") %>'>
                </asp:Label></td>
                </tr>
                <tr>
                <td align="right" style="padding-right:5px"><b>Customer Po#:</b></td>
                <td><asp:TextBox ID="vCustPoNumTextBox" MaxLength="20" runat="server" OnTextChanged="Custpo_Text_Click" AutoPostBack="true" Text='<%# Bind("vCustPoNum") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="usagpolook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                
                <td align="right" style="padding-right:5px"><b>Customers Part#:</b></td>
                <td><asp:TextBox ID="vCustPartNumTextBox"  AutoPostBack="true" OnTextChanged="cust_part_txt_Click" runat="server" Text='<%# Bind("vCustPartNum") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="usagcustpart(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                <td><asp:TextBox ID="vFgItmNumTextBox" Enabled="false" runat="server" Text='<%# Bind("vFgItmNum") %>'>
                </asp:TextBox></td>
                <td align="right" style="padding-right:5px"><b>Customer A/P Code:</b></td>
                <td><asp:TextBox ID="vCustVenCodeTextBox" Enabled="false" runat="server" Text='<%# Bind("vCustVenCode") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Customer Plant ID:</b></td>
                <td><asp:TextBox ID="vCustPlantIdTextBox" AutoPostBack="true" OnTextChanged="custplantid_txt_Click" runat="server" Text='<%# Bind("vCustPlantId") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="usagplantlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td align="right" style="padding-right:5px"><b>Customer Dept Code:</b></td>
                <td><asp:TextBox ID="vCustDptCodTextBox" Enabled="false" runat="server" Text='<%# Bind("vCustDptCod") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>suppliers Order#:</b></td>
                <td><asp:TextBox ID="vVenOrdNumTextBox" runat="server" Text='<%# Bind("vVenOrdNum") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="usagorderlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                <asp:CompareValidator ID="CompareValidator7" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vVenOrdNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="integer"></asp:CompareValidator></td>
                <td align="right" style="padding-right:5px"><b>Line#:</b></td>
                <td><asp:TextBox ID="vCustPoLineNumTextBox" Enabled="false" runat="server" Text='<%# Bind("vCustPoLineNum") %>'>
                </asp:TextBox></td>
                </tr>
                <tr>
                <td align="right" style="padding-right:5px"><b>Vendors Job#:</b></td>
                <td><asp:TextBox ID="vVenJobNumTextBox" runat="server" Text='<%# Bind("vVenJobNum") %>'>
                </asp:TextBox></td>                
                <td align="right" style="padding-right:5px"><b>Vendors Job2#:</b></td>
                <td><asp:TextBox ID="vVenJob2NumTextBox" runat="server" Text='<%# Bind("vVenJob2Num") %>'>
                </asp:TextBox><asp:CompareValidator ID="CompareValidator6" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vVenJob2NumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="Integer"></asp:CompareValidator></td>
                </tr>
                <tr>
                <td align="right" style="padding-right:5px"><b>Vendors Item Sell Price:</b></td>
                <td><asp:TextBox ID="vItmSelPriceTextBox" onblur="setdate()" runat="server" Text='<%# Bind("vItmSelPrice") %>'>
                </asp:TextBox><asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vItmSelPriceTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <td align="right" style="padding-right:5px"><b>Trans-type</b></td>
                <td><asp:Label ID="vTransTypeTextBox" Width="80px" runat="server" Text='<%# Bind("vTransType") %>'>
                </asp:Label></td></tr>
                <tr><td colspan="2">
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Update_Button_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr></table>
            </EditItemTemplate>
            <InsertItemTemplate>
                <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Seq#:</b></td>
                <td><asp:Label ID="vSeqNumTextBox" runat="server" Text='<%# Bind("vSeqNum") %>'>
                </asp:Label></td>
                <%--<td align="right" style="padding-right:5px"><b>Customer Usage Date:</b></td>
                <td><asp:TextBox ID="vUsgDateTextBox" runat="server" Text='<%# Bind("vUsgDate") %>'>
                </asp:TextBox></td></tr>--%>
                <td style="padding-right:5px;" align="right"><b> Customer Usage Date:</b></td>
               <td nowrap><b>              
                <asp:TextBox ID="vUsgDateTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" runat="server" Text='<%# Bind("vUsgDate") %>'>
                </asp:TextBox>
                
               </b>
               <a href="#" onblur="setdate()" tabindex="1" onClick="showCalendarControl(FormView1_vUsgDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
               </td>
               </tr>
                <tr><td align="right" style="padding-right:5px"><b>Quantity Used:</b></td>
                <td><asp:TextBox ID="vQtyUsedTextBox" MaxLength="7" runat="server" Text='<%# Bind("vQtyUsed") %>'>
                </asp:TextBox><asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vQtyUsedTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vQtyUsedTextBox" runat="server" Display="dynamic"  ErrorMessage="Usage Quantity must be grater than 0"></asp:RequiredFieldValidator>   </td>
                                
                <td align="right" style="padding-right:5px"><b>Cust Plant on Hand Qty:</b></td>
                <td><asp:Label ID="vCustHandQtyTextBox" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("vCustHandQty") %>'>
                </asp:Label></td>
                </tr>
                <tr>
                <td align="right" style="padding-right:5px"><b>CustomerPo#:</b></td>
                <td><asp:TextBox ID="vCustPoNumTextBox" MaxLength="20" runat="server" OnTextChanged="cust_part_txt_Click" runat="server" Text='<%# Bind("vCustPoNum") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="usagpolook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                
                <td align="right" style="padding-right:5px"><b>Customers Part#:</b></td>
                <td><asp:TextBox ID="vCustPartNumTextBox" AutoPostBack="true" OnTextChanged="cust_part_txt_Click" runat="server" Text='<%# Bind("vCustPartNum") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="usagcustpart(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                <td><asp:TextBox ID="vFgItmNumTextBox" Enabled="false" runat="server" Text='<%# Bind("vFgItmNum") %>'>
                </asp:TextBox></td>
                <td align="right" style="padding-right:5px"><b>Customer A/P Code:</b></td>
                <td><asp:TextBox ID="vCustVenCodeTextBox" Enabled="false"  runat="server" Text='<%# Bind("vCustVenCode") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Customer Plant ID:</b></td>
                <td><asp:TextBox ID="vCustPlantIdTextBox" AutoPostBack="true" OnTextChanged="custplantid_txt_Click" runat="server" Text='<%# Bind("vCustPlantId") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="usagplantlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td align="right" style="padding-right:5px"><b>Customer Dept Code:</b></td>
                <td><asp:TextBox ID="vCustDptCodTextBox" Enabled="false" runat="server" Text='<%# Bind("vCustDptCod") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>suppliers Order#:</b></td>
                <td><asp:TextBox ID="vVenOrdNumTextBox" runat="server" Text='<%# Bind("vVenOrdNum") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="usagorderlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                <asp:CompareValidator ID="CompareValidator7" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vVenOrdNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="integer"></asp:CompareValidator></td>
                <td align="right" style="padding-right:5px"><b>Line#:</b></td>
                <td><asp:TextBox ID="vCustPoLineNumTextBox" Enabled="false"  runat="server" Text='<%# Bind("vCustPoLineNum") %>'>
                </asp:TextBox></td>
                </tr>
                <tr>
                <td align="right" style="padding-right:5px"><b>Vendors Job#:</b></td>
                <td><asp:TextBox ID="vVenJobNumTextBox" runat="server" Text='<%# Bind("vVenJobNum") %>'>
                </asp:TextBox></td>                
                <td align="right" style="padding-right:5px"><b>Vendors Job2#:</b></td>
                <td><asp:TextBox ID="vVenJob2NumTextBox" runat="server" Text='<%# Bind("vVenJob2Num") %>'>
                </asp:TextBox></td>
                </tr>
                <tr>
                <td align="right" style="padding-right:5px"><b>Vendors Item Sell Price:</b></td>
                <td><asp:TextBox ID="vItmSelPriceTextBox" onblur="setdate()" runat="server" Text='<%# Bind("vItmSelPrice") %>'>
                </asp:TextBox><asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vItmSelPriceTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <%--<td align="right" style="padding-right:5px"><b>Trans-type</b></td>
                <td><asp:Label ID="vTransTypeTextBox" Width="80px" runat="server" Text='<%# Bind("vTransType") %>'>
                </asp:Label></td>--%></tr>
                <tr><td>
                <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Insert_Button_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr>
                </table>
            </InsertItemTemplate>
            <ItemTemplate>
                  <table class="shade" width="600px">
                 <tr><td align="right" style="padding-right:5px"><b>Seq#:</b></td>
                 <td><asp:Label ID="vSeqNumLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSeqNum") %>'></asp:Label></td>
                 <td align="right" style="padding-right:5px"><b>Customer Usage Date:</b></td>
                 <td><asp:Label ID="vUsgDateLabel" runat="server" Text='<%# Bind("vUsgDate","{0:MM/dd/yyyy}") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td></tr>
                 <tr><td align="right" style="padding-right:5px"><b>Quantity Used:</b></td>
                 <td><asp:Label ID="vQtyUsedLabel" runat="server" Text='<%# Bind("vQtyUsed") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                 <td align="right" style="padding-right:5px"><b>Cust Plant on Hand Qty:</b></td>
                 <td><asp:Label ID="vCustHandQtyLabel" runat="server" Text='<%# Bind("vCustHandQty") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px">
                </asp:Label></td>                 
                 </tr>
                 <tr>
                 <td align="right" style="padding-right:5px"><b>Customers Po#:</b></td>
                 <td><asp:Label ID="vCustPoNumLabel" runat="server" Text='<%# Bind("vCustPoNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td>
                 <td align="right" style="padding-right:5px"><b>Customer Part#:</b></td>
                 <td><asp:Label ID="vCustPartNumLabel" runat="server" Text='<%# Bind("vCustPartNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td></tr>
                 <tr><td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                 <td><asp:Label ID="vFgItmNumLabel" runat="server" Text='<%# Bind("vFgItmNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                 <td align="right" style="padding-right:5px"><b>Customer A/P Code:</b></td>
                 <td><asp:Label ID="vCustVenCodeLabel" runat="server" Text='<%# Bind("vCustVenCode") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td></tr>
                 <tr><td align="right" style="padding-right:5px"><b>Customer Plant ID:</b></td>
                 <td><asp:Label ID="vCustPlantIdLabel" runat="server" Text='<%# Bind("vCustPlantId") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td>
                 <td align="right" style="padding-right:5px"><b>Customer Dept Code:</b></td>
                 <td><asp:Label ID="vCustDptCodLabel" runat="server" Text='<%# Bind("vCustDptCod") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td></tr>
                 <tr><td align="right" style="padding-right:5px"><b>Suppliers Order#:</b></td>
                 <td><asp:Label ID="vVenOrdNumLabel" runat="server" Text='<%# Bind("vVenOrdNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px">
                </asp:Label></td>
                 <td align="right" style="padding-right:5px"><b>Line#:</b></td>
                 <td><asp:Label ID="vCustPoLineNumLabel" runat="server" Text='<%# Bind("vCustPoLineNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                   </tr>
                 <tr>
                 <td align="right" style="padding-right:5px"><b>Suppliers Job#:</b></td>
                 <td><asp:Label ID="vVenJobNumLabel" runat="server" Text='<%# Bind("vVenJobNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px">
                </asp:Label></td>
                 <td align="right" style="padding-right:5px"><b>Suppliers Job2#:</b></td>
                 <td><asp:Label ID="vVenJob2NumLabel" runat="server" Text='<%# Bind("vVenJob2Num") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px">
                </asp:Label></td>
                 </tr>
                 <tr>
                 <td align="right" style="padding-right:5px"><b>Suppliers Item Sell Price:</b></td>
                 <td><asp:Label ID="vItmSelPriceLabel" runat="server" Text='<%# Bind("vItmSelPrice") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px">
                </asp:Label></td>
                 <td align="right" style="padding-right:5px"><b>Trans-type</b></td>
                 <td><asp:Label ID="vTransTypeLabel" runat="server" Text='<%# Bind("vTransType") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px">
                </asp:Label></td></tr>     
                 <tr><td colspan="3"> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;              
                <asp:Button ID="AddButton" runat="server" CssClass="button" CausesValidation="False" CommandName="new" Text="Add">
                </asp:Button>
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Update"></asp:Button>
                <asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')"
                Text="Delete"></asp:Button>
            </td></tr>   
               
                </table>
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="UpdateCustPlant" TypeName="custitem">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:SessionParameter Name="prmSeqNum" SessionField="itemdaily_seq_no" Type="Int32" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:Parameter Name="prmRcptDate" Type="DateTime" />
                <asp:Parameter Name="prmQtyUsed" Type="Decimal" />
                <asp:Parameter Name="prmCustPoNum" Type="String" />
                <asp:Parameter Name="prmCustLineNum" Type="Int32" />
                <asp:Parameter Name="prmCustPartNum" Type="String" />
                <asp:Parameter Name="prmFgItemNum" Type="String" />
                <asp:Parameter Name="prmCustVnCode" Type="String" />
                <asp:Parameter Name="prmCustPlantId" Type="String" />
                <asp:Parameter Name="prmCustDptCode" Type="String" />
                <asp:Parameter Name="prmCustOrdNum" Type="Int32" />
                <asp:Parameter Name="prmCustJobNum" Type="String" />
                <asp:Parameter Name="prmCustJob2Num" Type="Int32" />
                <asp:Parameter Name="prmSellingPrice" Type="Decimal" />
                <asp:Parameter Name="prmOnHandQty" Type="Decimal" />
                <asp:Parameter Name="prmTranType" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
        <br />
        </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>