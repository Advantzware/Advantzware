<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="alpha_itemlist" Codebehind="alpha_itemlist.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Alphabetic Item List</title>
    
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
    var beginc=document.getElementById("begincust_TextBox");
    var endc=document.getElementById("endcust_TextBox");
    endc.value=beginc.value;
    }
    
     function samevalue2()
    {
    var beginc=document.getElementById("begincust_TextBox");
    var endc=document.getElementById("endcust_TextBox");
    if(endc.value!=beginc.value)
    {
    alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    }
    }
    
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].begincust_TextBox.value = ReturnObj1;
  document.forms[0].endcust_TextBox.value = ReturnObj1;
  document.forms[0].endcust_TextBox.focus(); 
    
}
function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){

    document.forms[0].endcust_TextBox.value = ReturnObj1;
    document.forms[0].endcust_TextBox.focus();
  }

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){
    document.forms[0].beginitem_TextBox.value = ReturnObj1;
    document.forms[0].beginitem_TextBox.focus();
  
}
function fg2look(){ 
  var NewWindow = window.open("fgitem_translookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FG2Lookup(ReturnObj1){
    document.forms[0].enditem_TextBox.value = ReturnObj1;
    document.forms[0].enditem_TextBox.focus();
}

function procatlook() {
    var NewWindow = window.open("procat_lookup.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procatLookup(ReturnObj1) {
    document.forms[0].cate_TextBox.value = ReturnObj1;
    document.forms[0].cate_TextBox.focus();
}
function procat2look() {
    var NewWindow = window.open("procat_lookup2.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procat2Lookup(ReturnObj1) {
    document.forms[0].endcate_TextBox.value = ReturnObj1;
    document.forms[0].endcate_TextBox.focus();
}

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='beginitem_TextBox' >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Alphabetic Item List&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" ></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
          <asp:HiddenField ID="HiddenField1" runat="server" />
          <asp:HiddenField ID="HiddenField2" runat="server" />
          <asp:HiddenField ID="HiddenField3" runat="server" />
          <asp:HiddenField ID="HiddenField4" runat="server" />
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
          
      <table class="shade"><tr><td align="right" style="padding-right:5px"><b>Beginning Item#:</b></td>
      <td>
          <asp:TextBox ID="beginitem_TextBox" MaxLength="15" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right:5px"><b>Ending Item:</b></td>
      <td>
          <asp:TextBox ID="enditem_TextBox" MaxLength="15" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="fg2look(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
      <tr><td align="right" style="padding-right:5px"><b>Beginning Customer#:</b></td>
      <td nowrap>
          <asp:TextBox ID="begincust_TextBox" onkeyup="samevalue()" MaxLength="8"  runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right:5px"><b>Ending Customer#:</b></td>
      <td>
          <asp:TextBox ID="endcust_TextBox" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
      <tr><td align="right" style="padding-right:5px"><b>Beginning Category:</b></td>
      <td>
          <asp:TextBox ID="cate_TextBox" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procatlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      <td align="right" style="padding-right:5px"><b>End Category:</b></td>
      <td>
          <asp:TextBox ID="endcate_TextBox" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procat2look(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
      
          <tr><td></td><td>
              <b><asp:CheckBox ID="cust_CheckBox" Text="Include Customer Owned Warehouse" runat="server" /></b></td></tr>
          <tr><td></td><td><b><asp:CheckBox ID="sort_CheckBox" Text="Sort By Item#?" runat="server" /></b></td></tr>
          <tr><td></td><td><b><asp:CheckBox ID="CheckBox1" Text="Show only Item with Ouantity greater then Zero?" runat="server" /></b></td></tr>
          
          <tr><td colspan="2">
           &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <b>Print To:</b><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Pdf" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>
           </td></tr>
          <tr><td colspan="2"><asp:Button ID="SubmitButton" OnClick="SubmitButton_Click" runat="server" CssClass="button" Text="Submit"></asp:Button> 
          &nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> 
                  
                              </td></tr></table>
                              
           <asp:FormView ID="FormView1" Visible="false" runat="server" OnPreRender="FormView1_PreRender" DataSourceID="ObjectDataSource1">
              
              <ItemTemplate>
                  vFile:
                  <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>             
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="AlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:SessionParameter Name="prmUser" SessionField="appha_user_login" Type="String" />
                  <asp:Parameter Name="vBeginCust" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="vEndCust" Type="String" />
                  <asp:Parameter Name="vBeginCat" Type="String" />
                  <asp:Parameter Name="vEndCat" Type="String" />
                  <asp:Parameter Name="vBeginItem" Type="String" />
                  <asp:Parameter Name="vEndItem" Type="String" />
                  <asp:Parameter Name="vCustWhse" Type="String" />
                  <asp:Parameter Name="vSort" Type="String" />
                  <asp:Parameter Name="vZero" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>            
    </div>
    </form>
</body>
</html>
