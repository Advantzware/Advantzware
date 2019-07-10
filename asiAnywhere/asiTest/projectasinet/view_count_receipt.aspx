<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="view_count_receipt" Codebehind="view_count_receipt.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Finished Goods Physical Count Processing</title>    
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



        function fgtaglook() {
            var loc1 = document.forms[0].FormView1_TextBox1.value;
            var NewWindow = window.open("fgtaglookup.aspx?binloc=" + loc1 + "", "fgtagLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function fgtaglookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj13) {
            document.forms[0].FormView1_TextBox1.value = ReturnObj1;
            document.forms[0].FormView1_TextBox2.value = ReturnObj2;
            document.forms[0].FormView1_TextBox3.value = ReturnObj3;
            document.forms[0].FormView1_vJob_noTextBox.value = ReturnObj4;
            document.forms[0].FormView1_vJob_no2TextBox.value = ReturnObj5;
            document.forms[0].FormView1_TextBox4.value = ReturnObj6;
            document.forms[0].FormView1_TextBox5.value = ReturnObj7;

            document.forms[0].FormView1_TextBox6.value = ReturnObj8;
            document.forms[0].FormView1_TextBox7.value = ReturnObj9;
            document.forms[0].FormView1_TextBox8.value = ReturnObj10;
//            document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vStdCostTextBox.value = ReturnObj11;
//            document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCostUomTextBox.value = ReturnObj12;
            document.forms[0].FormView1_vT_QtyTextBox.value = ReturnObj13;
            document.forms[0].FormView1_TextBox1.onchange();

        }

        function fglook() {

            var NewWindow = window.open("lfglook.aspx", "FGLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function fglLookup(ReturnObj1, ReturnObj2) {
            document.forms[0].FormView1_TextBox4.value = ReturnObj1;
            document.forms[0].FormView1_TextBox5.value = ReturnObj2;
            document.forms[0].FormView1_TextBox4.focus();
        }
    
    
    
    function locationlook() {
        var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function LocationLookUp(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_TextBox2.value = ReturnObj1;
        document.forms[0].FormView1_TextBox2.focus();
    }



    function binlook() {
        var loc1 = document.forms[0].FormView1_TextBox3.value;
        var NewWindow = window.open("custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CustBinLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_TextBox3.value = ReturnObj1;
        document.forms[0].FormView1_TextBox3.focus();
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
function open_transfer() {
    //var custNum = document.getElementById("FormView1_vCustNumTextBox").value;
    var NewWindow = window.open("count_trans_popup.aspx", "CountTransPopupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function open_pst() {
    var NewWindow = window.open("count_finish_goods_psting.aspx", "CountFGpost", "width=700,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
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
     <TABLE id="TABLE1" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          
          <TD align=left nowrap><font size=+0><b>Finished Goods Physical Count Processing &nbsp;</asp:label> </b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton2" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="Linkbutton3" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                        
            Company:&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
     </td></tr></table>
            
            
   
    
    <div>
    
    <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li >
    <asp:LinkButton ID="lnk_list" runat="server" OnClick="lnk_list_click">List Count</asp:LinkButton></li>
    <li class="selected" ><asp:LinkButton ID="lnk_view" runat="server" OnClick="lnk_view_click">View Count</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
    <br />
        
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnUnload="FormView1_Unload" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender">
            <EditItemTemplate>
            <asp:Panel ID="Edit_panel" runat="server" DefaultButton="UpdateButton">
            <br />
         <fieldset style="background-color:#EFF3FB">
        <table class="shade">
        <tr>
        <td nowrap align="left" style="padding-right:5px;"><b>
            Seq#: </b><br>
            <asp:Label ID="Label1" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vRno") %>' />
            <br />                        
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Tag#: </b><br>
            <asp:TextBox ID="TextBox1" MaxLength="20" OnTextChanged="Tagtextbox_Click" AutoPostBack="true" Width="118px" runat="server" Text='<%# Bind("vTag") %>' />
             <a href="#" tabindex="1" onClick="fgtaglook(); return false"><asp:Image ID="fgtagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td> 
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Count Date: </b><br>
            <asp:TextBox ID="vDateTextBox" Width="65px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vcntDate") %>' />
            <a href="#" tabindex="1" onblur="FormView1_vDateTextBox.focus()" onClick="showCalendarControl(FormView1_vDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Count Time: </b><br>
            <asp:Label ID="vTransTimeLabel" Visible="false" Width="70px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vcntTime") %>' />
                <br />
        </td> 
         <td nowrap align="left" style="padding-right:5px;"><b> 
            FG Item#: </b><br>
            <asp:TextBox ID="TextBox4" MaxLength="15" Width="95px" runat="server" Text='<%# Bind("vItem") %>' />
            <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Name: </b><br>
            <asp:TextBox ID="TextBox5" MaxLength="30" Width="110px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
         </td> 
         <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:TextBox ID="vJob_noTextBox" MaxLength="6" Width="40px" runat="server" Text='<%# Bind("vJobno") %>' />
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:TextBox ID="vJob_no2TextBox" MaxLength="2" Width="15px" runat="server" 
                Text='<%# Bind("vJobno2") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vJob_no2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            
         </br></td> 
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:TextBox ID="TextBox2" onkeyup="tagtxtbox()" MaxLength="5" Width="35px" runat="server" Text='<%# Bind("vLoc") %>' />
             <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:TextBox ID="TextBox3" MaxLength="8" Width="55px" runat="server" Text='<%# Bind("vLocBin") %>' />
            <a href="#" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Units: </b><br>
            <asp:TextBox ID="TextBox6" MaxLength="6" Width="45px" onkeyup="coltotqty()" runat="server" Text='<%# Bind("vCases") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="TextBox6" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Units Count: </b><br>            
            <asp:TextBox ID="TextBox7" MaxLength="6" Width="65px" onkeyup="coltotqty()" onblur="unitcount()" runat="server" Text='<%# Bind("vQtyCas") %>' />
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="TextBox7" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Unit / Pallet: </b><br>
            <asp:TextBox ID="TextBox8" MaxLength="3" Width="75px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="TextBox8" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:TextBox ID="TextBox9" MaxLength="6" onkeyup="coltotqty()" Width="45px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="TextBox9" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
           
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Qty: </b><br>
            <asp:TextBox ID="vT_QtyTextBox" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vTQty") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vT_QtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
                       
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Created By: </b><br>
            <asp:Label ID="Label2" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Last Update By: </b><br>
            <asp:Label ID="Label3" Width="78px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
            <br />
         </td>
            
            </tr>
            
            <tr>
            <td colspan="2">
            <br />
            <asp:Button ID="UpdateButton" CssClass="buttonM" OnClick="UpdateButton_Click" runat="server" CausesValidation="True" 
                CommandName="Save" Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" 
                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td>
            </tr>
        </table>
        </fieldset>
        </asp:Panel>
        </EditItemTemplate>
            
            <InsertItemTemplate>
                                
                <asp:Panel ID="Insert_panel" runat="server" DefaultButton="InsertButton">
            <br />
        <fieldset style="background-color:#EFF3FB">
        <table class="shade">
        <tr>
         <td nowrap align="left" style="padding-right:5px;"><b>
            Seq#: </b><br>
            <asp:Label ID="Label1" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vRno") %>' />
            <br />                        
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Tag#: </b><br>
            <asp:TextBox ID="TextBox1" MaxLength="20" OnTextChanged="Tagtextbox_Click" AutoPostBack="true" Width="118px" runat="server" Text='<%# Bind("vTag") %>' />
             <a href="#" tabindex="1" onClick="fgtaglook(); return false"><asp:Image ID="fgtagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td> 
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Count Date: </b><br>
            <asp:TextBox ID="vDateTextBox" Width="65px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vcntDate") %>' />
            <a href="#" tabindex="1" onblur="FormView1_vDateTextBox.focus()" onClick="showCalendarControl(FormView1_vDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Count Time: </b><br>
            <asp:Label ID="vTransTimeLabel"  Width="70px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vcntTime") %>' />
                <br />
        </td> 
         <td nowrap align="left" style="padding-right:5px;"><b> 
            FG Item#: </b><br>
            <asp:TextBox ID="TextBox4" MaxLength="15" Width="95px" runat="server" Text='<%# Bind("vItem") %>' />
            <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Name: </b><br>
            <asp:TextBox ID="TextBox5" MaxLength="30" Width="110px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
         </td> 
         <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:TextBox ID="vJob_noTextBox" MaxLength="6" Width="40px" runat="server" Text='<%# Bind("vJobno") %>' />
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:TextBox ID="vJob_no2TextBox" MaxLength="2" Width="15px" runat="server" 
                Text='<%# Bind("vJobno2") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vJob_no2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            
         </br></td> 
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:TextBox ID="TextBox2" onkeyup="tagtxtbox()" MaxLength="5" Width="35px" runat="server" Text='<%# Bind("vLoc") %>' />
             <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:TextBox ID="TextBox3" MaxLength="8" Width="55px" runat="server" Text='<%# Bind("vLocBin") %>' />
            <a href="#" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Units: </b><br>
            <asp:TextBox ID="TextBox6" MaxLength="6" Width="45px" onkeyup="coltotqty()" runat="server" Text='<%# Bind("vCases") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="TextBox6" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Units Count: </b><br>            
            <asp:TextBox ID="TextBox7" MaxLength="6" Width="65px" onkeyup="coltotqty()" onblur="unitcount()" runat="server" Text='<%# Bind("vQtyCas") %>' />
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="TextBox7" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Unit / Pallet: </b><br>
            <asp:TextBox ID="TextBox8" MaxLength="3" Width="75px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="TextBox8" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:TextBox ID="TextBox9" MaxLength="6" onkeyup="coltotqty()" Width="45px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="TextBox9" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
           
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Qty: </b><br>
            <asp:TextBox ID="vT_QtyTextBox" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vTQty") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vT_QtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
         </td>
                       
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Created By: </b><br>
            <asp:Label ID="Label2" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            <br />
         </td>
         <td nowrap align="left" style="padding-right:5px;"><b> 
            Last Update By: </b><br>
            <asp:Label ID="Label3" Width="78px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
            <br />
         </td>
            
            
            </tr>
            <tr>
            <td colspan="4">
            <br />
            <asp:Button ID="InsertButton" CssClass="buttonM" OnClick="InsertButton_Click" runat="server" CausesValidation="True" 
                CommandName="Save" Text="Save" />
            &nbsp;<asp:Button ID="InsertCancelButton" CssClass="buttonM" OnClick="InserCancelButton_Click" runat="server" 
                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td>
             </tr>
             </table>   
             </fieldset>
             </asp:Panel>
                 
            </InsertItemTemplate>
            
            <ItemTemplate>
                <br />
        <fieldset style="background-color:#EFF3FB">
        <table class="shade">
        <tr>
        <td nowrap align="left" style="padding-right:5px;"><b>
            Seq#: </b><br>
            <asp:Label ID="vRnoLabel" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vRno") %>' />
            <br />                        
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Tag#: </b><br>
            <asp:Label ID="vTagLabel" MaxLength="20" Width="118px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTag") %>' />
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Count Date: </b><br>
            <asp:Label ID="vDateLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcntDate") %>' />
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Count Time: </b><br>
            <asp:Label ID="vTransTimeLabel" Width="70px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vcntTime") %>' />
                <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Item: </b><br>
            <asp:Label ID="vItemLabel" MaxLength="15" Width="95px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vItem") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Name/Desc: </b><br>
            <asp:Label ID="vItemNameLabel" MaxLength="30" Width="140px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:Label ID="vJob_noLabel" MaxLength="6" Width="40px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vJobno") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:Label ID="vJob_no2Label" MaxLength="2" Width="15px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vJobno2") %>' />
            
            </br></td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:Label ID="vLocLabel" MaxLength="5" Width="35px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vLoc") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:Label ID="vLocBinLabel" MaxLength="8" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vLocBin") %>' />
            <br />
            </td>                        
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units: </b><br>
            <asp:Label ID="vCasesLabel" MaxLength="6" Width="45px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCases") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units Count: </b><br>
            <asp:Label ID="vQtyCasLabel" MaxLength="6" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vQtyCas") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Unit Per Pallet: </b><br>
            <asp:Label ID="vCasUnitLabel" MaxLength="3" Width="75px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:Label ID="vPartialLabel" MaxLength="6" Width="45px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
            <br />
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Quantity: </b><br>
            <asp:Label ID="vT_QtyLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTQty") %>' />
            <br />
            </td>
                       
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Created By: </b><br>
            <asp:Label ID="vCreatedByLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Last Update By: </b><br>
            <asp:Label ID="vCreate2Label" Width="78px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
            <br />
            </td>            
            </tr>
            <tr>

    <td colspan="6">
    <br />
        <asp:Button ID="addButton" runat="server" CommandName="new"  CssClass="button" Text="Add"></asp:Button>
        <asp:Button ID="UpdatButton"  runat="server" CommandName="Edit" CssClass="buttonM"  Text="Update" />
        <%--<asp:Button ID="copybutton" runat="server" CommandName="Edit" OnClick="CopyButton_click" CssClass="buttonM" Text="Copy"  OnClick="DeleteButton_Click" />--%>
        <asp:Button ID="deleteButton" runat="server" CssClass="button" CausesValidation="False" Text="Delete" OnClick="DeleteButton_Click"  OnClientClick="return confirm('Are you sure you want to delete this record')"></asp:Button>
        <input type="button" id="Button2" value="Transfer" runat="server" class="buttonM" onClick="open_transfer()" />
        <input type="button" id="Button1" value="Post" runat="server" class="buttonM" onClick="open_pst()" />
        
        
        
    </td>
  </tr>
  </table>
  </fieldset>
                </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="selectcountrcpt" TypeName="itemhistory">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                <asp:Parameter Name="prmFgItem" Type="String" />
                <asp:Parameter Name="prmName" Type="String" />
                <asp:Parameter Name="prmJobno" Type="String" />
                <asp:Parameter Name="prmJobno2" Type="Int32" />
                <asp:Parameter Name="prmPono" Type="String" />
                <asp:SessionParameter Name="prmSeqno" SessionField="count_recept_list_seq" Type="String" />
                <asp:Parameter Name="prmRcptDate" Type="String" />
                <asp:Parameter Name="prmTagno" Type="String" />
                <asp:Parameter Name="prmLoc" Type="String" />
                <asp:Parameter Name="prmLocBin" Type="String" />
                <asp:Parameter Name="prmTqty" Type="Decimal" />
                <asp:Parameter Name="prmCases" Type="Int32" />
                <asp:Parameter Name="prmQty_Cas" Type="Int32" />
                <asp:Parameter Name="prmCasUnit" Type="Int32" />
                <asp:Parameter Name="prmPartial" Type="Int32" />
                <asp:Parameter Name="prmRecKey" type="String" />                   
                <asp:Parameter Name="prmllSetParts" Type="String" />
                <asp:Parameter Name="prmTransTime" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
        <br />
        </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>