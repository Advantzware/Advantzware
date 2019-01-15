<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="invoice_high_report" Codebehind="invoice_high_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Invoice Highlights</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
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

    function companylook() {
        var NewWindow = window.open("company_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function CompanyLookup(ReturnObj1) {
        document.forms[0].CompanyTextBox.value = ReturnObj1;
    }
    
 
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='DateTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
                    
          <asp:Label ID="Label1" ForeColor="red" Font-Bold="true" runat="server" ></asp:Label>
                 
       <table><tr><td>       
      <TABLE id="tblTop" cellSpacing="3" align="left" border="0" >
        <TR>
            
          <TD align="left" nowrap><font size=+0><b>Invoice Highlights Report&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" ></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="left" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      </td></tr>
       <tr><td>
       
      <table class="shade" >   
      
        <tr> <td align="right" style="padding-right: 5px">AS of Date:</td>
        <td>
            <asp:TextBox ID="DateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onclick="showCalendarControl(DateTextBox); return false"><asp:Image ID="datelook" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>            
            
            </td>
            
            <td>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="DateTextBox" Display="Dynamic" runat="server" ErrorMessage="Enter the Date!"></asp:RequiredFieldValidator>
            </td>
        <td align="right" style="padding-right: 5px">Company:</td>
        <td><asp:TextBox ID="CompanyTextBox"  MaxLength="10" Width="100px" runat="server"></asp:TextBox>   
        <a href="#" tabindex="1" onclick="companylook(); return false"><asp:Image ID="compLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
        
        </td>
        
        <td><asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="CompanyTextBox" Display="Dynamic" runat="server" ErrorMessage="Enter the Company!"></asp:RequiredFieldValidator></td>
        </tr>
        
        <tr><td>&nbsp;</td></tr>
                
           <tr><td align="left" colspan="4">
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" Width="50px" class="buttonM"  Text="Submit" />
              &nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Label ID="OutputLabel" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              &nbsp;
              <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              </td>
          </tr>
                 
        </table>  
       
        </td></tr>
       </table>                                                    
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                                                                     
              <ItemTemplate>
                  vInvoiceFile:
                  <asp:Label ID="vInvoiceFileLabel" runat="server" 
                      Text='<%# Bind("vInvoiceFile") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
              OldValuesParameterFormatString="original_{0}" 
              SelectMethod="SelectInvoiceHighReport" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmDate" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmOutexcel" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

