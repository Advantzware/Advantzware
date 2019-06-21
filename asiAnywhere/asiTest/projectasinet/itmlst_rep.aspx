<%@ Page Language="C#" AutoEventWireup="true" Inherits="itmlst_rep" Codebehind="itmlst_rep.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Manufactured Item Master List</title>
     <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language = JavaScript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
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
     function samevalue()
    {
    var beginc=document.getElementById("txt_begincust");
    var endc=document.getElementById("txt_endcust");
    endc.value=beginc.value;
    }
    
     function samevalue2()
    {
    var beginc=document.getElementById("txt_begincust");
    var endc=document.getElementById("txt_endcust");
    if(endc.value!=beginc.value)
    {
    alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    }
    }

    var cust = 0;
    function contactcustomerlook(cust1) {
        cust = cust1;
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
    if (cust == 1) {
        document.forms[0].txt_begincust.value = ReturnObj1;
        document.forms[0].txt_endcust.value = ReturnObj1;
        document.forms[0].txt_endcust.focus();
    }
    if (cust == 2) {
        document.forms[0].txt_endcust.value = ReturnObj1;
        document.forms[0].txt_endcust.focus();
    }
  
    
}

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){
    document.forms[0].txt_beginitem.value = ReturnObj1;
    document.forms[0].txt_beginitem.focus();
}
function fg2look(){ 
  var NewWindow = window.open("fgitem_translookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FG2Lookup(ReturnObj1){
    document.forms[0].txt_enditem.value = ReturnObj1;
    document.forms[0].txt_enditem.focus();
}


function procatlook() {
    var NewWindow = window.open("procat_lookup.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procatLookup(ReturnObj1) {
    document.forms[0].txt_begcat.value = ReturnObj1;
    document.forms[0].txt_begcat.focus();
}
function procat2look() {
    var NewWindow = window.open("procat_lookup2.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procat2Lookup(ReturnObj1) {
    document.forms[0].txt_endcat.value = ReturnObj1;
    document.forms[0].txt_endcat.focus();
}
function LocationLook() {
    var NewWindow = window.open("location_lookup.aspx", "LocationLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1) {
    document.forms[0].txt_beginware.value = ReturnObj1;
    document.forms[0].txt_beginware.focus();
}
function Location2Look() {
    var NewWindow = window.open("location_lookup2.aspx", "LocationLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Location2LookUp(ReturnObj1) {
    document.forms[0].txt_endware.value = ReturnObj1;
    document.forms[0].txt_endware.focus();
}
    </script> 
</head>
<body>
    <form id="form1" runat="server" defaultfocus="txt_beginware">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
        <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Manufactured Item Master List&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" ></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
          <asp:HiddenField ID="HiddenField_inc_cust" runat="server" />
          <asp:HiddenField ID="HiddenField_out" runat="server" />
          
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
           <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>
                  
                  <asp:Label ID="CustLabel" Visible="false" runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
      <fieldset class="shade" style="width:562px">
      <table class="shade">
      <tr><td nowrap align="right" style="padding-right:5px"><b>Beginning Warehouse:</b></td>
      <td nowrap>
          <asp:TextBox ID="txt_beginware" MaxLength="5" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="LocationLook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      <td nowrap align="right" style="padding-right:5px"><b>Ending Warehouse:</b></td>
      <td nowrap>
          <asp:TextBox ID="txt_endware" MaxLength="5" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Location2Look(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
      <tr><td nowrap align="right" style="padding-right:5px"><b>Beginning Customer#:</b></td>
      <td nowrap>
          <asp:TextBox ID="txt_begincust" onkeyup="samevalue()" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td nowrap align="right" style="padding-right:5px"><b>Ending Customer#:</b></td>
      <td nowrap>
          <asp:TextBox ID="txt_endcust" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(2); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
      <tr><td align="right" style="padding-right:5px"><b>Beginning Item#:</b></td>
      <td nowrap>
          <asp:TextBox ID="txt_beginitem" MaxLength="15" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right:5px"><b>Ending Item:</b></td>
      <td nowrap>
          <asp:TextBox ID="txt_enditem" MaxLength="15" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="fg2look(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
      
          <tr>
              <td align="right" style="padding-right:5px"><b>Beginning Category#:</b></td>
      <td nowrap>
          <asp:TextBox ID="txt_begcat" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procatlook(); return false"><asp:Image ID="Image3" 
              runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right:5px"><b>Ending Category:</b></td>
      <td nowrap>
          <asp:TextBox ID="txt_endcat" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procat2look(); return false"><asp:Image ID="Image4" 
              runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
      <tr><td align="right" style="padding-right:5px">&nbsp;</td>
      <td>
          &nbsp;</td>
      <td align="right" style="padding-right:5px">&nbsp;</td>
      <td>
          &nbsp;</td></tr>
      
          <tr><td></td><td>
              <b><asp:CheckBox ID="Zero_CheckBox" Text="Include Zero Balances?"  runat="server" /></b></td></tr>          
              <tr><td align="right" style="padding-right:5px"><b>Sort?</b></td><td>
              <b>
                  <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1"  RepeatColumns="2" runat="server">
                  <asp:ListItem Value="Customer#" Text="Customer#"></asp:ListItem>
                  <asp:ListItem Value="Item#" Text="Item#"></asp:ListItem>
                  </asp:RadioButtonList>
              </b></td></tr>          
              <tr><td></td><td colspan="2">
              <b><asp:CheckBox ID="Ware_CheckBox" Text="Include Customer Owned Warehouse?" runat="server" /></b></td></tr>          
              <tr><td></td><td>
              <b><asp:CheckBox ID="Cat_CheckBox" Text="Print Product Category?" runat="server" /></b></td></tr>          
          
          <tr><td colspan="2">
           &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <b>Output To:</b><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Text" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>
           </td></tr>
          <tr><td colspan="4"><asp:Button ID="SubmitButton" OnClick="SubmitButton_Click" runat="server" CssClass="button" Text="Submit"></asp:Button> 
          &nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> 
                  
                              </td></tr></table></fieldset>     
                              
           <asp:FormView ID="FormView1" Visible="False" runat="server" DataSourceID="ObjectDataSource1">
                                           
              <ItemTemplate>
                  itmlst:
                  <asp:Label ID="itmlstLabel" runat="server" Text='<%# Bind("itmlst") %>'></asp:Label><br />
              </ItemTemplate>
               
          </asp:FormView>             
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="RitmListReport" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmBeWare" Type="String" />
                  <asp:Parameter Name="prmEndWare" Type="String" />
                  <asp:Parameter Name="prmBeCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBeItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBeCat" Type="String" />
                  <asp:Parameter Name="prmEndCat" Type="String" />
                  <asp:Parameter Name="prmZero" Type="String" />
                  <asp:Parameter Name="prmCustWhse" Type="String" />
                  <asp:Parameter Name="prmSort" Type="String" />
                  <asp:Parameter Name="prmProdCat" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
