<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="RedFlagReport" Codebehind="~/RedFlagReport.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Red Flag Report</title>
    
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
  
    
}

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){ 
  document.forms[0].beginitem_TextBox.value = ReturnObj1;
  
}
function fg2look(){ 
  var NewWindow = window.open("fgitem_translookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FG2Lookup(ReturnObj1){   
  document.forms[0].enditem_TextBox.value = ReturnObj1;
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='ProductToStock_Text' >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Red Flag Report&nbsp;</b></font></TD>
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
          <%--<asp:HiddenField ID="HiddenField2" runat="server" />
          <asp:HiddenField ID="HiddenField3" runat="server" />--%>
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
          
      <table class="shade">
      <tr><td align="right" style="padding-right:5px"><b>Weeks To Product To Stock</b></td>
      <td><asp:TextBox ID="ProductToStock_Text" runat="server"></asp:TextBox></td>
      <td align="right" style="padding-right:5px"><b></b></td>
      <td><b><asp:CheckBox ID="PrintMatReqCheckBox" Text="Print Materials Required?" runat="server" /></b></td>
      </tr>
      
      <tr><td align="right" style="padding-right:5px"><b>Beginning Vendor Code:</b></td>
      <td>
          <asp:TextBox ID="BegVendorCode_TextBox" runat="server"></asp:TextBox>
          <a> </a>
          </td>
      <td align="right" style="padding-right:5px"><b>Ending Vendor Code:</b></td>
      <td><asp:TextBox ID="EndVendorCode_TextBox" runat="server"></asp:TextBox></td>
      </tr>
      <tr><td align="right" style="padding-right:5px"><b>Beginning Vendor Plant Code:</b></td>
      <td ><asp:TextBox ID="BegVendorPlantCode_TextBox"  runat="server"></asp:TextBox></td>
      <td align="right" style="padding-right:5px"><b>Ending Vendor Plant Code:</b></td>
      <td><asp:TextBox ID="EndVendorPlantCode_TextBox"  runat="server"></asp:TextBox></td></tr>
      <tr><td align="right" style="padding-right:5px"><b>Beginning Suppliers FG Item:</b></td>
      <td><asp:TextBox ID="BegSuppliers_FGitems_TextBox" runat="server"></asp:TextBox></td>
      <td align="right" style="padding-right:5px"><b>Ending Suppliers FG Item:</b></td>
      <td><asp:TextBox ID="EndSuppliers_FGitems_TextBox" runat="server"></asp:TextBox></td></tr>
      <tr><td align="right" style="padding-right:5px"><b>Beginning Customers Part No:</b></td>
      <td><asp:TextBox ID="BegCustomers_PartNo_TextBox" runat="server"></asp:TextBox></td>
      <td align="right" style="padding-right:5px"><b>Ending Customers Part No:</b></td>
      <td><asp:TextBox ID="EndCustomers_PartNo_TextBox" runat="server"></asp:TextBox></td></tr>
      
      <tr><td colspan="2">
           &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          <%-- <b>Print To:</b><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Pdf" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>--%>
           </td></tr>
           
          <tr><td colspan="3"><asp:Button ID="SubmitButton" OnClick="SubmitButton_Click" runat="server" CssClass="button" Text="Submit"></asp:Button> 
          &nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>         
                              </td></tr></table>
                              
           <asp:FormView ID="FormView1" Visible="false" runat="server" OnPreRender="FormView1_PreRender" DataSourceID="ObjectDataSource1">
              
              <ItemTemplate>
                  vRedFlagFile:
                  <asp:Label ID="vRedFlagFileLabel" runat="server" Text='<%# Bind("vRedFlagFile") %>'>
                  </asp:Label><br />
              </ItemTemplate>
               
          </asp:FormView>             
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="RedFlagReport" TypeName="reports">
              <SelectParameters>
                  <asp:SessionParameter Name="prmUser" SessionField="appha_user_login" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="vFIBegCustPartNo" Type="String" />
                  <asp:Parameter Name="vFIBegFgItemNo" Type="String" />
                  <asp:Parameter Name="vFIBegVendCode" Type="String" />
                  <asp:Parameter Name="vFIBegVendPlantCode" Type="String" />
                  <asp:Parameter Name="vFIEndCustPartNo" Type="String" />
                  <asp:Parameter Name="vFIEndFgItemNo" Type="String" />
                  <asp:Parameter Name="vFIEndVendCode" Type="String" />
                  <asp:Parameter Name="vFIEndVendPlantCode" Type="String" />
                  <asp:Parameter Name="vFINumberOfWeeks" Type="Int32" />
                  <asp:Parameter Name="vTGPrintRqMaterials" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>            
    </div>
    </form>
</body>
</html>
