<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="projectasinet.sharpshooter.view_cons_receiptss" Codebehind="view_cons_receiptss.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Finished Goods Warehouse Transaction Transfer</title>    
    <LINK href="../include/styless.css" type="text/css" rel="stylesheet"/>
     <link href="../include/tree.css" rel="stylesheet" type="text/css" />
    <link href="../include/tree.css" rel="stylesheet" type="text/css" />
    <link href="../include/tree.css" rel="stylesheet" type="text/css" />
    <link href="../include/tree.css" rel="stylesheet" type="text/css" />
    <link href="../include/tree.css" rel="stylesheet" type="text/css" />
    <link href="../include/tree.css" rel="stylesheet" type="text/css" />
    <link href="../include/tree.css" rel="stylesheet" type="text/css" />
    <LINK REL="stylesheet" TYPE="text/css" HREF="../include/CalendarControl.css" >
     
    <script language="javascript" src="../include/date.js"></script>
    <script language="javascript" src="../include/event.js"></script>
    <script language="javascript" src="../include/insert.js"></script>
    <script language = "JavaScript" src="../include/CalendarControl.js">    
    </script>
    
               
    <script>
        function focuset() {
            window.scroll(800, 900);
        }
        function focuset2() {
            window.scroll(10, 900);
        }
    function Bollook()
    { 
        var NewWindow = window.open("../VendorBolLookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
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

    function trnsfgbinlook() {
        var NewWindow = window.open("../trns_fgbin_lookup.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function trnsfgbinlookup(obj1, obj2, obj3, obj4, obj5, obj6, obj7) {
        document.forms[0].FormView1_vItemTextBox.value = obj1;
        document.forms[0].FormView1_vTagTextBox.value = obj2;
        document.forms[0].FormView1_vJobnoTextBox.value = obj3;
        document.forms[0].FormView1_vJobno2TextBox.value = obj4;
        document.forms[0].FormView1_vLocTextBox.value = obj5;
        document.forms[0].FormView1_vLocBinTextBox.value = obj6;
        document.forms[0].FormView1_vcustTextBox.value = obj7;

        document.forms[0].FormView1_vTagTextBox.focus();

    }
    var lookup = "";
       
    function trnstaglook(var1) {
        lookup = var1;
        var NewWindow = window.open("../trnstaglook.aspx?conslook=" + "cons" + "", "TagLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function trnstaglookup(obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, obj11) {
        if (lookup == "1") {
            document.forms[0].FormView1_vTagTextBox.value = obj1;
            document.forms[0].FormView1_vItemNameTextBox.value = obj2;
            document.forms[0].FormView1_vJobnoTextBox.value = obj3;
            document.forms[0].FormView1_vJobno2TextBox.value = obj4;
            document.forms[0].FormView1_vLocTextBox.value = obj5;
            document.forms[0].FormView1_vLocBinTextBox.value = obj6;
            //document.forms[0].FormView1_vcustTextBox.value = obj7;
            document.forms[0].FormView1_vCasesTextBox.value = obj7;
            document.forms[0].FormView1_vQtyCasTextBox.value = obj8;
            document.forms[0].FormView1_vTag2TextBox.value = obj9;
            document.forms[0].FormView1_vItemTextBox.value = obj10;
            document.forms[0].PartialHiddenField.value = obj11;

            document.forms[0].FormView1_vTagTextBox.onchange();
        }
        else {
            document.forms[0].FormView1_vTag2TextBox.value = obj1;
            document.forms[0].FormView1_vTag2TextBox.onchange();
        }   


    }

    function locationlook() {
        var NewWindow = window.open("../location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function LocationLookUp(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_vLoc2TextBox.value = ReturnObj1;
        document.forms[0].FormView1_vLoc2TextBox.focus();
    }



    function binlook() {
        var loc1 = document.forms[0].FormView1_vLocBin2TextBox.value;
        var NewWindow = window.open("../custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CustBinLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_vLocBin2TextBox.value = ReturnObj1;
        document.forms[0].FormView1_vLocBin2TextBox.focus();
    }
    
    function Datelook(){ 
    var NewWindow = window.open("../date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
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
    var NewWindow = window.open("../cust_plant_lookup.aspx", "CustomerPoLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustPlantLookup(obj1, obj2, obj3, obj4, obj5, obj6) {
    document.forms[0].FormView1$vCustPlantIdTextBox.value = obj2;
    document.forms[0].FormView1$vCustDptCodTextBox.value = obj3;
}
function tagblank() {
    var tag = document.getElementById("FormView1_vTagTextBox");
    if (tag.value == "") {
        alert("Tag# can not be blank");
        tag.focus();
    }
}

function open_pst() {
    var NewWindow = window.open("finish_good_rep_popupss.aspx", "FGpostReport", "width=700,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
    
    </script>
    </head>
<body>
    <form id="form1" runat="server">
    
    <asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>
            
     <table width="100%"><tr><td>
    <table align="left" border="1" width="75%">
                <tr align="left" bgcolor="maroon">
                   
                    
                    <td nowrap align="left" width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" OnClick="img_btn_add_click" ToolTip="Add"  />
                      </td>
                     <%--<td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="printspec(); return false"><asp:Image ID="Image3" Width="35px" runat="server" ToolTip="Spec Notes" ImageUrl="~/Images/dict.ico" /></a>
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="orderhelp(); return false"><asp:Image ID="img_help" Width="35px" ToolTip="Help" runat="server" ImageUrl="~/Images/help.ico" /></a>
                        </td>--%>
                      <td nowrap align="left" width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" ImageUrl="~/Images/exit-au.bmp" Width="35px" OnClick="hlnkLogOut_Click" ToolTip="LogOut"  />
                    </td>
                     <td align="left" nowrap> &nbsp;</td>
                </tr>
      </table></td></tr>
    <tr><td>
    <div>
     <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          
          <TD align=left nowrap><font size=+0><b>Finished Goods Warehouse Transaction Transfer &nbsp;</asp:label> </b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                        
            Company:&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
     </td></tr></table>
    
    <table>
    <tr style="background-color:Gray">
    <td>
    <asp:LinkButton ID="lnk_list" runat="server" OnClick="lnk_list_click"><img src="../Images/list_receipt_0.jpg" border="0" width="150px" alt="List Receipt" /></asp:LinkButton>
    <asp:LinkButton ID="lnk_view" runat="server" OnClick="lnk_view_click"><img src="../Images/view_reciept_1.jpg" border="0" width="150px" alt="View Receipt" /></asp:LinkButton>
    </td>
    </tr></table>
    <br />
        <asp:HiddenField ID="PartialHiddenField" runat="server" />
       
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender">
            <EditItemTemplate>
                <asp:Panel ID="Edit_panel" runat="server" DefaultButton="UpdateButton">
                <table class="shade">
                <tr>
                <td nowrap align="left" style="padding-right:5px;"><b>From Tag:</b> <br />
                <asp:TextBox ID="vTagTextBox" runat="server" OnTextChanged="tag_TextBoxChange" AutoPostBack="true" MaxLength="20" Text='<%# Bind("vTag") %>' />
                <a href="#" tabindex="1" onClick="trnstaglook(1); return false"><asp:Image ID="Image10" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Name:</b> <br />
                <asp:TextBox ID="vItemNameTextBox" onkeyup="tagblank()" Enabled="false" runat="server" Text='<%# Bind("vItemName") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Job#:</b> <br />
                <asp:TextBox ID="vJobnoTextBox" Width="70px" MaxLength="6" runat="server" Enabled="false" Text='<%# Bind("vJobno") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b></b> <br />
                <asp:TextBox ID="vJobno2TextBox" runat="server" Width="20px" MaxLength="2" Enabled="false" Text='<%# Bind("vJobno2") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Whse:</b> <br />
                <asp:TextBox ID="vLocTextBox" runat="server" MaxLength="5" Enabled="false" Width="50px" Text='<%# Bind("vLoc") %>' />
                <a href="#" tabindex="1" onClick="trnsfgbinlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Bin:</b> <br />
                <asp:TextBox ID="vLocBinTextBox" runat="server" Enabled="false" MaxLength="8" Width="70px" Text='<%# Bind("vLocBin") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Customer:</b> <br />
                <asp:TextBox ID="vcustTextBox" runat="server" Width="100px" Enabled="false" MaxLength="8" Text='<%# Bind("vcust") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Units:</b> <br />
                <asp:TextBox ID="vCasesTextBox" runat="server" Width="45px"  MaxLength="6" Text='<%# Bind("vCases") %>' />
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Qty/Unit:</b> <br />
                <asp:TextBox ID="vQtyCasTextBox" runat="server" Width="45px" Enabled="false" MaxLength="6" Text='<%# Bind("vQtyCas") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Tag:</b> <br />
                <asp:TextBox ID="vTag2TextBox" runat="server" MaxLength="20" OnTextChanged="totag_textboxChange" AutoPostBack="true" Text='<%# Bind("vTag2") %>' />
                <a href="#" tabindex="1" tabindex="1" onClick="trnstaglook(2); return false"><asp:Image ID="Image9" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Whse:</b> <br />
                <asp:TextBox ID="vLoc2TextBox" runat="server" MaxLength="5" Enabled="false" Width="50px" Text='<%# Bind("vLoc2") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Bin:</b> <br />
                <asp:TextBox ID="vLocBin2TextBox" runat="server" MaxLength="8" Enabled="false" Width="70px" onblur="focuset()" Text='<%# Bind("vLocBin2") %>' />
                
                </td>
                
                <td nowrap align="left" style="padding-right:5px;"><b>Item No:</b> <br />
                <asp:TextBox ID="vItemTextBox" runat="server" Width="100px" Enabled="false" onblur="focuset2()" Text='<%# Bind("vItem") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Date:</b> <br />
                <asp:Label ID="vDateTextBox" Width="80px" runat="server"  Text='<%# Bind("vDate") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Time:</b> <br />
                <asp:label ID="vTransTimeTextBox" Width="80px" runat="server" Text='<%# Bind("vTransTime") %>' />
                </td>  
                <td nowrap align="left" style="padding-right:5px;"> <br />
                <asp:Label ID="vRecKeyLabel" Width="100px" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' />
                </td>  </tr>
                
                <tr><td colspan="4"><br />
                <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="Update_Button_Click" Text="Save" />
                &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                    CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td></tr>
                
                </table>              
                </asp:Panel>             
            </EditItemTemplate>
            
            <InsertItemTemplate>
                                
                <asp:Panel ID="Insert_panel" runat="server" DefaultButton="InsertButton">
                <table class="shade">
                <tr>
                <td nowrap align="left" style="padding-right:5px;"><b>From Tag:</b> <br />
                <asp:TextBox ID="vTagTextBox" runat="server" OnTextChanged="tag_TextBoxChange" AutoPostBack="true" MaxLength="20" Text='<%# Bind("vTag") %>' />
                <a href="#" tabindex="1" onClick="trnstaglook(1); return false"><asp:Image ID="Image10" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Name:</b> <br />
                <asp:TextBox ID="vItemNameTextBox" onkeyup="tagblank()" Enabled="false" runat="server" Text='<%# Bind("vItemName") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Job#:</b> <br />
                <asp:TextBox ID="vJobnoTextBox" runat="server" Width="70px" Enabled="false" MaxLength="6" Text='<%# Bind("vJobno") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b></b> <br />
                <asp:TextBox ID="vJobno2TextBox" runat="server" Width="20px" Enabled="false" MaxLength="2" Text='<%# Bind("vJobno2") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Whse:</b> <br />
                <asp:TextBox ID="vLocTextBox" runat="server" MaxLength="5" Enabled="false" Width="50px" Text='<%# Bind("vLoc") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Bin:</b> <br />
                <asp:TextBox ID="vLocBinTextBox" runat="server" MaxLength="8" Width="70px" Enabled="false" Text='<%# Bind("vLocBin") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Customer:</b> <br />
                <asp:TextBox ID="vcustTextBox" runat="server" Width="100px" Enabled="false" MaxLength="8" Text='<%# Bind("vcust") %>' />
               
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Units:</b> <br />
                <asp:TextBox ID="vCasesTextBox" runat="server" Width="45px" MaxLength="6"  Text='<%# Bind("vCases") %>' />
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Qty/Unit:</b> <br />
                <asp:TextBox ID="vQtyCasTextBox" runat="server" Width="45px" MaxLength="6" Enabled="false" Text='<%# Bind("vQtyCas") %>' />
               
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Tag:</b> <br />
                <asp:TextBox ID="vTag2TextBox" runat="server" MaxLength="20" OnTextChanged="totag_textboxChange" AutoPostBack="true" Text='<%# Bind("vTag2") %>' />
                <a href="#" tabindex="1" tabindex="1" onClick="trnstaglook(2); return false"><asp:Image ID="Image9" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Whse:</b> <br />
                <asp:TextBox ID="vLoc2TextBox" runat="server" MaxLength="5" Width="50px" Enabled="false" Text='<%# Bind("vLoc2") %>' />
                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Bin:</b> <br />
                <asp:TextBox ID="vLocBin2TextBox" runat="server" MaxLength="8" Width="70px" Enabled="false" onblur="focuset()" Text='<%# Bind("vLocBin2") %>' />
                
                </td>
                
                <td nowrap align="left" style="padding-right:5px;"><b>Item No:</b> <br />
                <asp:TextBox ID="vItemTextBox" runat="server" Width="100px" Enabled="false" onblur="focuset2()" Text='<%# Bind("vItem") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Date:</b> <br />
                <asp:Label ID="vDateTextBox" runat="server" Width="70px"  Text='<%# Bind("vDate") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Time:</b> <br />
                <asp:Label ID="vTransTimeTextBox" runat="server" Width="70px" Text='<%# Bind("vTransTime") %>' />
                </td>  
                <td nowrap align="left" style="padding-right:5px;"> <br />
                <asp:Label ID="vRecKeyLabel" runat="server" Visible="false" Text='<%# Bind("vRecKey") %>' />
                </td>  </tr>
                <tr><td colspan="4"><br />
                <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button"
                     OnClick="Insert_Button_Click" Text="Save" />
                 &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                     CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td></tr>
                </table></asp:Panel>
                 
            </InsertItemTemplate>
            
            <ItemTemplate>
                <table class="shade">
                <tr>
                <td nowrap align="left" style="padding-right:5px;"><b>From Tag:</b> <br />
                <asp:Label ID="vTagLabel" runat="server" Text='<%# Bind("vTag") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Name:</b> <br />
                <asp:Label ID="vItemNameLabel" runat="server" Text='<%# Bind("vItemName") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Job#:</b> <br />
                <asp:Label ID="vJobnoLabel" runat="server" Text='<%# Bind("vJobno") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="80px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b></b> <br />
                <asp:Label ID="vJobno2Label" runat="server" Text='<%# Bind("vJobno2") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="20px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Whse:</b> <br />
                <asp:Label ID="vLocLabel" runat="server" Text='<%# Bind("vLoc") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Bin:</b> <br />
                <asp:Label ID="vLocBinLabel" runat="server" Text='<%# Bind("vLocBin") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Customer:</b> <br />
                <asp:Label ID="vcustLabel" runat="server" Text='<%# Bind("vcust") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Units:</b> <br />
                <asp:Label ID="vCasesLabel" runat="server" Text='<%# Bind("vCases") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Qty/Unit:</b> <br />
                <asp:Label ID="vQtyCasLabel" runat="server" Text='<%# Bind("vQtyCas") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Tag:</b> <br />
                <asp:Label ID="vTag2Label" runat="server" Text='<%# Bind("vTag2") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Whse:</b> <br />
                <asp:Label ID="vLoc2Label" runat="server" Text='<%# Bind("vLoc2") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Bin:</b> <br />
                <asp:Label ID="vLocBin2Label" runat="server" Text='<%# Bind("vLocBin2") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                
                <td nowrap align="left" style="padding-right:5px;"><b>Item No:</b> <br />
                <asp:Label ID="vItemLabel" runat="server" Text='<%# Bind("vItem") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Date:</b> <br />
                <asp:Label ID="vDateLabel" runat="server" Text='<%# Bind("vDate") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Time:</b> <br />
                <asp:Label ID="vTransTimeLabel" runat="server"  Text='<%# Bind("vTransTime") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" />
                </td>  
                <td nowrap align="left" style="padding-right:5px;"> <br />
                <asp:Label ID="vRecLabel" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" />
                </td>  </tr>
                <tr><td colspan="8"><br />
                <asp:Button ID="addButton" runat="server" CommandName="new"  CssClass="button" Text="Add"></asp:Button>
                <asp:Button ID="UpdatButton"  runat="server" CommandName="Edit" CssClass="buttonM"  Text="Update" />
                <asp:Button ID="deleteButton" runat="server" CssClass="button" CausesValidation="False" Text="Delete" OnClick="DeleteButton_Click"  OnClientClick="return confirm('Are you sure you want to delete this record')"></asp:Button>
                <input type="button" id="Button1" value="Post" runat="server" class="buttonM" onClick="open_pst()" />
                 </td></tr></table>
                </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="trnsViewRece" TypeName="itemhistory">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                <asp:Parameter Name="prmFgItem" Type="String" />
                <asp:Parameter Name="prmJobno" Type="String" />
                <asp:Parameter Name="prmRcptDate" Type="String" />
                <asp:Parameter Name="prmTagno" Type="String" />
                <asp:Parameter Name="prmTransTime" Type="String" />
                <asp:Parameter Name="prmJob_no2" Type="String" />
                <asp:Parameter Name="prmName" Type="String" />
                <asp:Parameter Name="prmLoc" Type="String" />
                <asp:Parameter Name="prmLocBin" Type="String" />
                <asp:Parameter Name="prmCases" Type="String" />
                <asp:Parameter Name="prmQty_Cas" Type="String" />
                <asp:Parameter Name="prmCasUnit" Type="String" />
                <asp:Parameter Name="prmPartial" Type="String" />
                <asp:Parameter Name="prmLoc2" Type="String" />
                <asp:Parameter Name="prmLocBin2" Type="String" />
                <asp:Parameter Name="prmTagno2" Type="String" />
                <asp:SessionParameter Name="prmRecKey" SessionField="cons_recept_list_seq" 
                    Type="String" />
                <asp:Parameter Name="prmcontrans" Type="String" DefaultValue="Yes" />
                <asp:Parameter Name="prmSeq" Type="String" />
                <asp:Parameter Name="prmOut" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
        <br />
        </div>
   
    </form>
</body>
</html>