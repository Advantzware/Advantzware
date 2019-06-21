<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="view_returns_rcpt" Codebehind="view_returns_rcpt.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>FG Wharehouse Returns</title>    
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
        function focuset() {
            window.scroll(800, 900);
        }
        function focuset2() {
            window.scroll(10, 900);
        }

        function retrntaglook() {
            var loc1 = document.forms[0].FormView1_vTagTextBox.value;
            var NewWindow = window.open("return_tag_lookup.aspx?binloc=" + loc1 + "", "retrntagLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function retrntaglookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13) {
            document.forms[0].FormView1_vTagTextBox.value = ReturnObj1;
            document.forms[0].FormView1_vLocTextBox.value = ReturnObj2;
            document.forms[0].FormView1_vLocBinTextBox.value = ReturnObj3;
            document.forms[0].FormView1_vItemTextBox.value = ReturnObj4;
            document.forms[0].FormView1_vItemNameTextBox.value = ReturnObj5;
            document.forms[0].FormView1_vJob_noTextBox.value = ReturnObj6;
            document.forms[0].FormView1_vJob_no2TextBox.value = ReturnObj7;
            document.forms[0].FormView1_vT_QtyTextBox.value = ReturnObj8;
            document.forms[0].FormView1_vCasesTextBox.value = ReturnObj9;
            document.forms[0].FormView1_vCasUnitTextBox.value = ReturnObj10;
            document.forms[0].FormView1_vQtyCasTextBox.value = ReturnObj11;            
            document.forms[0].FormView1_vStdCostTextBox.value = ReturnObj12;
            document.forms[0].FormView1_vCostUomTextBox.value = ReturnObj13;
            
            document.forms[0].FormView1_vTagTextBox.onchange();

        }
    

    

    function locationlook() {
        var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function LocationLookUp(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_vLocTextBox.value = ReturnObj1;
        document.forms[0].FormView1_vLocTextBox.focus();
    }



    function binlook() {
        var loc1 = document.forms[0].FormView1_vLocBinTextBox.value;
        var NewWindow = window.open("custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CustBinLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_vLocBinTextBox.value = ReturnObj1;
        document.forms[0].FormView1_vLocBinTextBox.focus();
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
function tagblank() {
    var tag = document.getElementById("FormView1_vTagTextBox");
    if (tag.value == "") {
        alert("Tag# can not be blank");
        tag.focus();
    }
}

function open_pst() {
    var NewWindow = window.open("finish_good_rep_popup.aspx", "FGpostReport", "width=700,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
    
    </script>
    </head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>
            
     <table width="100%"><tr><td>
    <table align="left" border="1" width="75%">
                <tr align="left" class="topheadcolor">
                   
                    
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
          
          <TD align=left nowrap><font size=+0><b>FG Wharehouse Returns &nbsp;</asp:label> </b></font></TD>
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
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li >
    <asp:LinkButton ID="lnk_list" runat="server" OnClick="lnk_list_click">List Receipt</asp:LinkButton></li>
    <li class="selected"><asp:LinkButton ID="lnk_view" runat="server" OnClick="lnk_view_click">View Receipt</asp:LinkButton></li></ul></div>
    </td></tr></table>
    <br />
        <asp:HiddenField ID="PartialHiddenField" runat="server" />
       
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender">
            <EditItemTemplate>
                <asp:Panel ID="Edit_panel" runat="server" DefaultButton="UpdateButton">
                <table class="shade">
                <tr>
                <td nowrap align="left" style="padding-right:5px;"><b> 
            Tag#: </b><br>
            <asp:TextBox ID="vTagTextBox" MaxLength="20" OnTextChanged="Tagtextbox_Click" AutoPostBack="true" Width="118px" runat="server" Text='<%# Bind("vTag") %>' />
             <a href="#" tabindex="1" onClick="retrntaglook(); return false"><asp:Image ID="retrntagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td> 
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:TextBox ID="vLocTextBox" onkeyup="tagtxtbox()" MaxLength="5" Width="45px" runat="server" Text='<%# Bind("vLoc") %>' />
             <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:TextBox ID="vLocBinTextBox" MaxLength="8" Width="55px" runat="server" Text='<%# Bind("vLocBin") %>' />
            <a href="#" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
        Count Date: </b><br>
            <asp:Textbox ID="vDateTextBox" Width="65px" Enabled="false" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vDate") %>' />
            <a href="#" tabindex="1" onblur="FormView1_vDateTextBox.focus()" onClick="showCalendarControl(FormView1_vDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Count Time: </b><br>
            <asp:TextBox ID="vTransTimeTextBox" enabled="false" Width="70px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vTransTime") %>' />
                <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Item No: </b><br>
            <asp:TextBox ID="vItemTextBox" MaxLength="15" Enabled="false" Width="95px"  runat="server" Text='<%# Bind("vItem") %>' />
            
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Name: </b><br>
            <asp:TextBox ID="vItemNameTextBox" MaxLength="30" Enabled="false" Width="110px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:TextBox ID="vJob_noTextBox" MaxLength="6" Enabled="false" Width="50px" runat="server" Text='<%# Bind("vJob_no") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:TextBox ID="vJob_no2TextBox" MaxLength="2" Enabled="false" Width="15px" runat="server" 
                Text='<%# Bind("vJob_no2") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vJob_no2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            
            </br></td>   
                
                 
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Qty: </b><br>
            <asp:TextBox ID="vT_QtyTextBox" Width="65px" runat="server" Text='<%# Bind("vT_Qty") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vT_QtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>                        
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cases: </b><br>
            <asp:TextBox ID="vCasesTextBox" MaxLength="6" Enabled="false" Width="45px" onkeyup="coltotqty()" runat="server" Text='<%# Bind("vCases") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cases/Bundles Per Unit: </b><br>
            <asp:TextBox ID="vCasUnitTextBox" MaxLength="3" Enabled="false" Width="125px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vCasUnitTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Qty/Cases: </b><br>            
            <asp:TextBox ID="vQtyCasTextBox" MaxLength="6" Enabled="false" Width="65px" onkeyup="coltotqty()" onblur="unitcount()" runat="server" Text='<%# Bind("vQtyCas") %>' />
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vQtyCasTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:TextBox ID="vPartialTextBox" MaxLength="6" Enabled="false" onkeyup="coltotqty()" Width="45px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vPartialTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost: </b><br>
            <asp:TextBox ID="vStdCostTextBox"  Width="65px" Enabled="false" runat="server" 
                Text='<%# Bind("vStdCost") %>' />
                <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vStdCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost UOM: </b><br>
            <asp:TextBox ID="vCostUomTextBox"  MaxLength="3" Enabled="false" Width="50px" runat="server" 
                Text='<%# Bind("vCostUom") %>' />
            <br />
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Ext. Cost: </b><br>
            <asp:TextBox ID="vExtCostTextBox" Enabled="false"  Width="77px" runat="server" 
                Text='<%# Bind("vExtCost") %>' />
                <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vExtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Counted Qty: </b><br>
            <asp:TextBox ID="vinvnoTextBox" Width="65px" runat="server" 
                Text='<%# Bind("vinvno") %>' />
                <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vinvnoTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
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
                 <td nowrap align="left" style="padding-right:5px;"><b> 
            Tag#: </b><br>
            <asp:TextBox ID="vTagTextBox" MaxLength="20" OnTextChanged="Tagtextbox_Click" AutoPostBack="true"  Width="118px" runat="server" Text='<%# Bind("vTag") %>' />
             <a href="#" tabindex="1" onClick="retrntaglook(); return false"><asp:Image ID="retrntagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td> 
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:TextBox ID="vLocTextBox" onkeyup="tagtxtbox()" MaxLength="5" Width="45px" runat="server" Text='<%# Bind("vLoc") %>' />
             <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:TextBox ID="vLocBinTextBox" MaxLength="8" Width="55px" runat="server" Text='<%# Bind("vLocBin") %>' />
            <a href="#" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
        Count Date: </b><br>
            <asp:TextBox ID="vDateTextBox" Width="65px" Enabled="false" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vDate") %>' />
            <a href="#" tabindex="1" onblur="FormView1_vDateTextBox.focus()" onClick="showCalendarControl(FormView1_vDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Count Time: </b><br>
            <asp:TextBox ID="vTransTimeTextBox" Enabled="false" Width="70px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vTransTime") %>' />
                <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Item No: </b><br>
            <asp:TextBox ID="vItemTextBox" MaxLength="15" Enabled="false" Width="95px"  runat="server" Text='<%# Bind("vItem") %>' />
            
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Name: </b><br>
            <asp:TextBox ID="vItemNameTextBox" MaxLength="30" Enabled="false" Width="110px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:TextBox ID="vJob_noTextBox" MaxLength="6" Enabled="false" Width="55px" runat="server" Text='<%# Bind("vJob_no") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:TextBox ID="vJob_no2TextBox" MaxLength="2" Enabled="false" Width="15px" runat="server" 
                Text='<%# Bind("vJob_no2") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vJob_no2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            
            </br></td>   
                
                 
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Qty: </b><br>
            <asp:TextBox ID="vT_QtyTextBox" Width="65px" runat="server" Text='<%# Bind("vT_Qty") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vT_QtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>                        
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cases: </b><br>
            <asp:TextBox ID="vCasesTextBox" MaxLength="6" Enabled="false" Width="45px" onkeyup="coltotqty()" runat="server" Text='<%# Bind("vCases") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cases/Bundles Per Unit: </b><br>
            <asp:TextBox ID="vCasUnitTextBox" MaxLength="3" Enabled="false" Width="125px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vCasUnitTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Qty/Cases: </b><br>            
            <asp:TextBox ID="vQtyCasTextBox" MaxLength="6" Enabled="false" Width="65px" onkeyup="coltotqty()" onblur="unitcount()" runat="server" Text='<%# Bind("vQtyCas") %>' />
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vQtyCasTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:TextBox ID="vPartialTextBox" MaxLength="6" Enabled="false" onkeyup="coltotqty()" Width="45px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vPartialTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost: </b><br>
            <asp:TextBox ID="vStdCostTextBox"  Width="65px" Enabled="false" runat="server" 
                Text='<%# Bind("vStdCost") %>' />
                <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vStdCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost UOM: </b><br>
            <asp:TextBox ID="vCostUomTextBox"  MaxLength="3" Enabled="false" Width="50px" runat="server" 
                Text='<%# Bind("vCostUom") %>' />
            <br />
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Ext. Cost: </b><br> 
            <asp:TextBox ID="vExtCostTextBox" Enabled="false" Width="77px" runat="server" 
                Text='<%# Bind("vExtCost") %>' />
                <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vExtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Counted Qty: </b><br>
            <asp:TextBox ID="vinvnoTextBox" Width="65px" runat="server" 
                Text='<%# Bind("vinvno") %>' />
                <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vinvnoTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
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
                <td nowrap align="left" style="padding-right:5px;"><b>Tag#:</b> <br />
                <asp:Label ID="vTagLabel" runat="server" Text='<%# Bind("vTag") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Whse:</b> <br />
                <asp:Label ID="Label1" runat="server" Text='<%# Bind("vLoc") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Bin:</b> <br />
                <asp:Label ID="Label2" runat="server" Text='<%# Bind("vLocBin") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Count Date:</b> <br />
                <asp:Label ID="Label3" runat="server" Text='<%# Bind("vDate") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Count Time:</b> <br />
                <asp:Label ID="Label4" runat="server"  Text='<%# Bind("vTransTime") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Item No:</b> <br />
                <asp:Label ID="vItemLabel" runat="server" Text='<%# Bind("vItem") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Name:</b> <br />
                <asp:Label ID="vItemNameLabel" runat="server" Text='<%# Bind("vItemName") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Job#:</b> <br />
                <asp:Label ID="vJobnoLabel" runat="server" Text='<%# Bind("vJob_no") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b></b> <br />
                <asp:Label ID="vJobno2Label" runat="server" Text='<%# Bind("vJob_no2") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="20px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Qty:</b> <br />
                <asp:Label ID="vT_QtyLabel" runat="server" Text='<%# Bind("vT_Qty") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Cases:</b> <br />
                <asp:Label ID="vCasesLabel" runat="server" Text='<%# Bind("vCases") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Cases/Bundles Per Unit:</b> <br />
                <asp:Label ID="vCasUnitLabel" runat="server" Text='<%# Bind("vCasUnit") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="130px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Qty/Cases:</b> <br />
                <asp:Label ID="vQtyCasLabel" runat="server" Text='<%# Bind("vQtyCas") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Partial:</b> <br />
                <asp:Label ID="vPartialLabel" runat="server" Text='<%# Bind("vPartial") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="50px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Cost:</b> <br />
                <asp:Label ID="vStdCostLabel" runat="server" Text='<%# Bind("vStdCost") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" />
                </td>
                
                <td nowrap align="left" style="padding-right:5px;"><b>Cost Uom:</b> <br />
                <asp:Label ID="vCostUomLabel" runat="server" Text='<%# Bind("vCostUom") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Ext. Cost:</b> <br />
                <asp:Label ID="vExtCostLabel" runat="server" Text='<%# Bind("vExtCost") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Counted Qty:</b> <br />
                <asp:Label ID="vinvnoLabel" runat="server" Text='<%# Bind("vinvno") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b></b> <br />
                <asp:Label ID="LabelvRno" Visible="false" runat="server" Text='<%# Bind("vRno") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="70px" />
                </td>
                 </tr>
                <tr><td colspan="4"><br />
                <asp:Button ID="addButton" runat="server" CommandName="new"  CssClass="button" Text="Add"></asp:Button>
                <asp:Button ID="UpdatButton"  runat="server" CommandName="Edit" CssClass="buttonM"  Text="Update" />
                <asp:Button ID="deleteButton" runat="server" CssClass="button" CausesValidation="False" Text="Delete" OnClick="DeleteButton_Click"  OnClientClick="return confirm('Are you sure you want to delete this record')"></asp:Button>
                <input type="button" id="Button1" value="Post" runat="server" class="buttonM" onClick="open_pst()" />
                 </td></tr></table>
                </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectReturnsGoods" TypeName="itemhistory">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                      <asp:Parameter Name="prmAction" Type="String" DefaultValue="GridSelect" />
                      <asp:Parameter Name="prmFgItem" Type="String" />
                      <asp:Parameter  Name="prmJobno" Type="String" />
                      <asp:Parameter  Name="prmPono" Type="String" />
                      <asp:SessionParameter SessionField="return_recept_list_seq"  Name="prmSeqno" Type="String" />
                      <asp:Parameter  Name="prmRcptDate" Type="String" />
                      <asp:Parameter  Name="prmTagno" Type="String" />
                      <asp:Parameter Name="prmTransTime" Type="String" />
                      <asp:Parameter Name="prmJob_no2" Type="Int32" />
                      <asp:Parameter Name="prmName" Type="String" />
                      <asp:Parameter Name="prmloc" Type="String" />
                      <asp:Parameter Name="prmlocbin" Type="String" />
                      <asp:Parameter Name="prmCases" Type="Int32" />
                      <asp:Parameter Name="prmQty_Cas" Type="Int32" />
                      <asp:Parameter Name="prmCasUnit" Type="Int32" />
                      <asp:Parameter Name="prmPartial" Type="Int32" />
                      <asp:Parameter Name="prmStdCost" Type="Decimal" />
                      <asp:Parameter Name="prmCost_Uom" Type="String" />
                      <asp:Parameter Name="prmTqty" Type="Decimal" />
<asp:Parameter Name="prmExtCost" Type="Decimal"></asp:Parameter>
<asp:Parameter Name="prmInvNo" Type="Int32"></asp:Parameter>
                      <asp:Parameter Name="prmRecKey" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
        <br />
        </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>