<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="view_receipt" Codebehind="view_daily_receipt.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Customer Plant Receipts</title>    
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
    function Bollook()
    { 
        var NewWindow = window.open("VendorBolLookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function VendorBolLookup(obj1,obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, obj11, obj12, obj13, obj14, obj15, obj16)
    {
        document.forms[0].FormView1$vVendBolNoTextBox.value =obj1;
        document.forms[0].FormView1$vFgItmNumTextBox.value =obj2;        
        document.forms[0].FormView1$vVenJobNumTextBox.value =obj4;
        document.forms[0].FormView1$vVenJob2NumTextBox.value =obj5;
        document.forms[0].FormView1$vVenOrdNumTextBox.value =obj6;
        document.forms[0].FormView1$vCustPoLineNumTextBox.value =obj7;
        document.forms[0].FormView1$vCustPoNumTextBox.value =obj8;
        document.forms[0].FormView1$vQtyUsedTextBox.value =obj9;
        document.forms[0].FormView1$vCustVenCodeTextBox.value =obj10;
        document.forms[0].FormView1$vCustDptCodTextBox.value =obj11;
        document.forms[0].FormView1$vCustPlantIdTextBox.value =obj12;
        document.forms[0].FormView1$vCustPartNumTextBox.value =obj13;
        document.forms[0].FormView1$vCustHandQtyTextBox.value =obj14;
        document.forms[0].FormView1$vUsgDateTextBox.value =obj15;
        document.forms[0].FormView1$vItmSelPriceTextBox.value = obj16;
        //alert(document.getElementById("FormView1$vVendBolNoTextBox").value);

        document.getElementById("FormView1$vVendBolNoTextBox").onchange();
    }
    
    function Datelook(){ 
    var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function Datelookup(obj)
    {
     document.forms[0].FormView1_vUsgDateTextBox.value=obj;
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

function custplantlook() {
    //var custNum = document.getElementById("FormView1_vCustNumTextBox").value;
    var NewWindow = window.open("cust_plant_lookup.aspx", "CustomerPoLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustPlantLookup(obj1, obj2, obj3, obj4, obj5, obj6) {
    document.forms[0].FormView1$vCustPlantIdTextBox.value = obj2;
    document.forms[0].FormView1$vCustDptCodTextBox.value = obj3;
}
    
    </script>
    </head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>
    <div>
    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Customer Plant Receipts&nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
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
		<ul nowrap><li >
    <asp:LinkButton ID="lnk_list" runat="server" OnClick="lnk_list_click">List Receipt</asp:LinkButton></li>
    <li class="selected"><asp:LinkButton ID="lnk_view" runat="server" OnClick="lnk_view_click">View Receipt</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
    <br />
        <asp:Button ID="addnewbutton" CssClass = "button" OnClick="newbutton_click" runat="server" Text="Add" />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender">
            <EditItemTemplate>
                <table class="shade">
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Seq#:</b></td>
                        <td><asp:Label ID="vSeqNumTextBox" Width="80px" runat="server" Text='<%# Bind("vSeqNum") %>'></asp:Label></td>                                       
                        <td style="padding-right:5px;" align="right"><b>Suppliers BOL No: </b></td>
                        <td><asp:TextBox ID="vVendBolNoTextBox" ReadOnly="true" BackColor="Turquoise" MaxLength="10" runat="server" Text='<%# Bind("vVendBolNo") %>'></asp:TextBox></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers Part#:</b></td>
                        <td><asp:TextBox ID="vCustPartNumTextBox" runat="server" ReadOnly="true" BackColor="Turquoise" Text='<%# Bind("vCustPartNum") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Receipt Quantity:</b></td>
                        <td><asp:TextBox ID="vQtyUsedTextBox" runat="server" Text='<%# Bind("vQtyUsed") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vQtyUsedTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Cust. Plant On Hand Qty:</b></td>
                        <td><asp:TextBox ID="vCustHandQtyTextBox" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vOnHandQty") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator10" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vCustHandQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>                        
                        <td style="padding-right:5px;" align="right"><b>Customers Receipt Date: </b></td>
                        <td nowrap><b><asp:TextBox ID="vUsgDateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="100px" runat="server" Text='<%# Bind("vUsgDate","{0:MM/dd/yyyy}") %>'></asp:TextBox> </b>
                        <a href="#" onClick="showCalendarControl(FormView1_vUsgDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                        <td><asp:TextBox ID="vCustVenCodeTextBox" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vCustVenCode") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Customers Plant ID:</b></td>
                        <td><asp:TextBox ID="vCustPlantIdTextBox" Width="100px" runat="server" Text='<%# Bind("vCustPlantId") %>'></asp:TextBox>
                        <a href="#" onClick="custplantlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers Dept Code:</b></td>
                        <td><asp:TextBox ID="vCustDptCodTextBox" runat="server" Text='<%# Bind("vDeptCode") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                        <td><asp:TextBox ID="vFgItmNumTextBox" ReadOnly="true" BackColor="Turquoise"  runat="server" Text='<%# Bind("vItemFgNum") %>'></asp:TextBox></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Suppliers Order#:</b></td>
                        <td><asp:TextBox ID="vVenOrdNumTextBox" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vVenOrdNum") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator7" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vVenOrdNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="integer"></asp:CompareValidator></td>
                        <td align="right" style="padding-right:5px"><b> Line#:</b></td>
                        <td><asp:TextBox ID="vCustPoLineNumTextBox" ReadOnly="true" BackColor="Turquoise" MaxLength="2" runat="server" Text='<%# Bind("vCustPoLineNum") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator9" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vCustPoLineNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="integer"></asp:CompareValidator></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers PO#:</b></td>
                        <td><asp:TextBox ID="vCustPoNumTextBox" MaxLength="10" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vCustPoNum") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Suppliers Item Sell Price:</b></td>
                        <td><asp:TextBox ID="vItmSelPriceTextBox" runat="server" ReadOnly="true" BackColor="Turquoise" Text='<%# Bind("vSellPrice") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vItmSelPriceTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Suppliers Job#:</b></td>
                        <td><asp:TextBox ID="vVenJobNumTextBox" runat="server" ReadOnly="true" BackColor="Turquoise" Text='<%# Bind("vVenJobNum") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Suppliers Job2#:</b></td>
                        <td><asp:TextBox ID="vVenJob2NumTextBox" runat="server" ReadOnly="true" BackColor="Turquoise" Text='<%# Bind("vVenJob2Num") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vVenJob2NumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="Integer"></asp:CompareValidator></td>
                    </tr>                                    
                
                    <tr>
                        <td><asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Update_Button_Click" Text="Save"></asp:Button>
                            <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel"></asp:Button>
                        </td>
                    </tr>
                </table>
            </EditItemTemplate>
            
            <InsertItemTemplate>
                                
                 <asp:UpdatePanel ID="LengUpdate" runat="server">
                    <ContentTemplate>
                <div >
                <asp:UpdateProgress ID="UpdateProgress1" runat="server" 
                    AssociatedUpdatePanelID="LengUpdate"
                    DisplayAfter="100" DynamicLayout="true">                    
                    <ProgressTemplate>                       
                        <asp:Label ID="lblProgress" runat="server" ></asp:Label>               
                    Please wait ...             
                    </ProgressTemplate>                    
                </asp:UpdateProgress>
                 <asp:Label ID="Errorlabel" runat="server" ForeColor="Red"></asp:Label>
                </div>
                <table class="shade">
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Seq#:</b></td>
                        <td><asp:Label ID="vSeqNumTextBox" runat="server" Text='<%# Bind("vSeqNum") %>'></asp:Label></td>               
                        <td style="padding-right:5px;" align="right"><b>Suppliers BOL No: </b></td>
                        <td><asp:TextBox ID="vVendBolNoTextBox" OnTextChanged="bolno_textchange" AutoPostBack="true" Width="100px"  runat="server" Text='<%# Bind("vVendBolNo") %>'></asp:TextBox>
                        <a href="#" onClick="Bollook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator2" Display="Dynamic" ControlToValidate="vVendBolNoTextBox" runat="server" ErrorMessage="Bol must be enter"></asp:RequiredFieldValidator>
                        
                        </td>
                    </tr>              
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers Part#:</b></td>
                        <td><asp:TextBox ID="vCustPartNumTextBox" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vCustPartNum") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Receipt Quantity:</b></td>
                        <td><asp:TextBox ID="vQtyUsedTextBox" runat="server" Text='<%# Bind("vQtyUsed") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vQtyUsedTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Cust. Plant On Hand Qty:</b></td>
                        <td><asp:TextBox ID="vCustHandQtyTextBox" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vOnHandQty") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator10" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vCustHandQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>                        
                        <td style="padding-right:5px;" align="right"><b>Customers Receipt Date: </b></td>
                        <td nowrap><b><asp:TextBox ID="vUsgDateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="100px" runat="server" Text='<%# Bind("vUsgDate","{0:MM/dd/yyyy}") %>'></asp:TextBox> </b>
                        <a href="#" onClick="showCalendarControl(FormView1_vUsgDateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                        
                            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" Display="Dynamic" ControlToValidate="vUsgDateTextBox" runat="server" ErrorMessage="Ener the date"></asp:RequiredFieldValidator>
                        </td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                        <td><asp:TextBox ID="vCustVenCodeTextBox" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vCustVenCode") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Customers Plant ID:</b></td>
                        <td><asp:TextBox ID="vCustPlantIdTextBox" Width="100px" runat="server" Text='<%# Bind("vCustPlantId") %>'></asp:TextBox>
                        <a href="#" onClick="showCalendarControl(FormView1_vUsgDateTextBox); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers Dept Code:</b></td>
                        <td><asp:TextBox ID="vCustDptCodTextBox" runat="server" Text='<%# Bind("vDeptCode") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                        <td><asp:TextBox ID="vFgItmNumTextBox" runat="server" ReadOnly="true" BackColor="Turquoise" Text='<%# Bind("vItemFgNum") %>'></asp:TextBox></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Suppliers Order#:</b></td>
                        <td><asp:TextBox ID="vVenOrdNumTextBox" runat="server" ReadOnly="true" BackColor="Turquoise" Text='<%# Bind("vVenOrdNum") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator7" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vVenOrdNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="integer"></asp:CompareValidator></td>
                        <td align="right" style="padding-right:5px"><b> Line#:</b></td>
                        <td><asp:TextBox ID="vCustPoLineNumTextBox" MaxLength="2" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vCustPoLineNum") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator9" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vCustPoLineNumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="integer"></asp:CompareValidator></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers PO#:</b></td>
                        <td><asp:TextBox ID="vCustPoNumTextBox" MaxLength="10" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vCustPoNum") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Suppliers Item Sell Price:</b></td>
                        <td><asp:TextBox ID="vItmSelPriceTextBox" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vSellPrice") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vItmSelPriceTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Suppliers Job#:</b></td>
                        <td><asp:TextBox ID="vVenJobNumTextBox" runat="server" ReadOnly="true" BackColor="Turquoise" Text='<%# Bind("vVenJobNum") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Suppliers Job2#:</b></td>
                        <td><asp:TextBox ID="vVenJob2NumTextBox" runat="server" ReadOnly="true" BackColor="Turquoise" Text='<%# Bind("vVenJob2Num") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vVenJob2NumTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="Integer"></asp:CompareValidator></td>
                    </tr>   </table>          
                       </ContentTemplate>                                             
                     </asp:UpdatePanel> 
                     <table class="shade">                      
                <tr><td>
                <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Insert_Button_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" OnClick="Cancel_button_click"
                    Text="Cancel">
                </asp:Button></td></tr>
                </table>
            </InsertItemTemplate>
            <ItemTemplate>
                <table class="shade" width="600px">
                <tr>
                    <td align="right" style="padding-right:5px"><b>Seq#:</b></td>
                    <td><asp:Label ID="vSeqNumLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSeqNum") %>'></asp:Label></td>                                        
                    <td align="right" style="padding-right:5px"><b>Suppliers BOL No:</b></td>
                    <td><asp:Label ID="vVendBolNoLabel" runat="server" Text='<%# Bind("vVendBolNo") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>
                <tr>
                    <td align="right" style="padding-right:5px"><b>Customer Part#:</b></td>
                    <td><asp:Label ID="vCustPartNumLabel" runat="server" Text='<%# Bind("vCustPartNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Receipt Quantity:</b></td>
                    <td><asp:Label ID="vQtyUsedLabel" runat="server" Text='<%# Bind("vQtyUsed") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>
                <tr>
                    <td align="right" style="padding-right:5px"><b>Cust. Plant On Hand Qty:</b></td>
                    <td><asp:Label ID="vCustHandQtyLabel" runat="server" Text='<%# Bind("vOnHandQty") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Customers Receipt Date:</b></td>
                    <td><asp:Label ID="vUsgDateLabel" runat="server" Text='<%# Bind("vUsgDate","{0:MM/dd/yyyy}") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>
                <tr>
                    <td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                    <td><asp:Label ID="vCustVenCodeLabel" runat="server" Text='<%# Bind("vCustVenCode") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Customers Plant ID:</b></td>
                    <td><asp:Label ID="vCustPlantIdLabel" runat="server" Text='<%# Bind("vCustPlantId") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td>
                </tr>
                <tr>
                    <td align="right" style="padding-right:5px"><b>Customers Dept Code:</b></td>
                    <td><asp:Label ID="vCustDptCodLabel" runat="server" Text='<%# Bind("vDeptCode") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                    <td><asp:Label ID="vFgItmNumLabel" runat="server" Text='<%# Bind("vItemFgNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>
                <tr>
                    <td align="right" style="padding-right:5px"><b>Suppliers Order#:</b></td>
                    <td><asp:Label ID="vVenOrdNumLabel" runat="server" Text='<%# Bind("vVenOrdNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td> 
                    <td align="right" style="padding-right:5px"><b>Line#:</b></td>
                    <td><asp:Label ID="vCustPoLineNumLabel" runat="server" Text='<%# Bind("vCustPoLineNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>
                <tr>
                    <td align="right" style="padding-right:5px"><b>Customers PO#:</b></td>
                    <td><asp:Label ID="vCustPoNumLabel" runat="server" Text='<%# Bind("vCustPoNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Suppliers Item Sell Price:</b></td>
                    <td><asp:Label ID="vItmSelPriceLabel" runat="server" Text='<%# Bind("vSellPrice") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>
                <tr>
                    <td align="right" style="padding-right:5px"><b>Suppliers Job#:</b></td>
                    <td><asp:Label ID="vVenJobNumLabel" runat="server" Text='<%# Bind("vVenJobNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                     <td align="right" style="padding-right:5px"><b>Suppliers Job2#:</b></td>
                    <td><asp:Label ID="vVenJob2NumLabel" runat="server" Text='<%# Bind("vVenJob2Num") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>
                               
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
            SelectMethod="UpdateReceipt" TypeName="custitem">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:SessionParameter Name="prmSeqNum" SessionField="itemreceipt_seq_no" Type="Int32" />
                <asp:Parameter Name="prmVendBolNo" Type="Int32" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:Parameter Name="prmTranDate" Type="DateTime" />
                <asp:Parameter Name="prmQtyUsed" Type="Decimal" />
                <asp:Parameter Name="prmCustPoNum" Type="String" />
                <asp:Parameter Name="prmCustLineNum" Type="Int32" />
                <asp:Parameter Name="prmPartNum" Type="String" />
                <asp:Parameter Name="prmFgItmNum" Type="String" />
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