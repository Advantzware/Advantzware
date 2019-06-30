<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="prod_high_report" Codebehind="prod_high_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Production Highlights</title>
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

    function company() {
        var NewWindow = window.open("company_lookup.aspx", "CompanylookWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function CompanyLookup(ReturnObj1) {
        document.forms[0].CompanyTextBox.value = ReturnObj1;
        document.getElementById("CompanyTextBox").onchange();
    }
 
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='DateTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
                    
          <asp:Label ID="Label1" ForeColor="red" Font-Bold="true" runat="server" ></asp:Label>
                 
       <table><tr><td>
        <TABLE id="tblTop" cellSpacing="3" align="left" border="0">
        <TR>
            
          <TD align="left"><font size=+0><b>Production Highlights Report&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b></font></TD>
         
          <TD  align="left" nowrap >Logged as&nbsp;
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
             <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="DateTextBox" Display="Dynamic" runat="server" ErrorMessage="Enter the Date!"></asp:RequiredFieldValidator>
            </td>
        <td align="right" style="padding-right: 5px">Company:</td>
        <td><asp:TextBox ID="CompanyTextBox" AutoPostBack="true" OnTextChanged="company_text_change" MaxLength="10" Width="100px" runat="server"></asp:TextBox>               
        <a href="#" tabindex="1" onclick="company(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
        </td></tr>
        <tr><td colspan="4" align="center">
        <font size="2"><b><asp:Label ID="massageLabel" runat="server" Text="Select up to 15 Machines for Production"></asp:Label></b></font>
        <br />
        </td></tr>
        <tr><td align="center" colspan="4">
        <asp:Label ID="graterLabel" runat="server" ForeColor="Red"></asp:Label>
        <asp:Panel ID="gridpanel" runat="server" Width="230px" ScrollBars="vertical" Height="300px">
        <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1"
          AutoGenerateColumns="False" AllowSorting="True"  OnSelectedIndexChanged="GridView1_SelectedIndexChanged" 
            EmptyDataText="No Records Found"  BorderStyle="Dotted" CssClass="Grid" >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />            
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle  BackColor="Teal" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <RowStyle CssClass="shade"  />
            <Columns>
                            <asp:TemplateField>
                     <ItemTemplate>
                      <asp:CheckBox ID="chk1" runat="server" />
                      </ItemTemplate>
                      </asp:TemplateField>
                <asp:BoundField DataField="vMachine" HeaderText="Code" SortExpression="vMachine" />
                <asp:BoundField DataField="vMachDesc" HeaderText="Description" SortExpression="vMachDesc" />
            </Columns>
             <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
        </asp:Panel>
        </td>        
        </tr>
           <tr><td align="left" colspan="4">
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server"  class="buttonM"  Text="Submit" />
              &nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Label ID="OutputLabel" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              &nbsp;
              <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              </td>
          </tr>
        
        
        </table>
       </td></tr>
       </table>       
     
      
        
             
        
        
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectProdHighReport" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmDate" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmMachine1" Type="String" />
                  <asp:Parameter Name="prmMachine2" Type="String" />
                  <asp:Parameter Name="prmMachine3" Type="String" />
                  <asp:Parameter Name="prmMachine4" Type="String" />
                  <asp:Parameter Name="prmMachine5" Type="String" />
                  <asp:Parameter Name="prmMachine6" Type="String" />
                  <asp:Parameter Name="prmMachine7" Type="String" />
                  <asp:Parameter Name="prmMachine8" Type="String" />
                  <asp:Parameter Name="prmMachine9" Type="String" />
                  <asp:Parameter Name="prmMachine10" Type="String" />
                  <asp:Parameter Name="prmMachine11" Type="String" />
                  <asp:Parameter Name="prmMachine12" Type="String" />
                  <asp:Parameter Name="prmMachine13" Type="String" />
                  <asp:Parameter Name="prmMachine14" Type="String" />
                  <asp:Parameter Name="prmMachine15" Type="String" />
                  <asp:Parameter Name="prmOutexcel" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
          <asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>
                  vDashFile:
                  <asp:Label ID="vDashFileLabel" runat="server" Text='<%# Bind("vDashFile") %>'></asp:Label><br />
                 
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectProdHighReport" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String"  />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmDate" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmMachine1" Type="String" />
                  <asp:Parameter Name="prmMachine2" Type="String" />
                  <asp:Parameter Name="prmMachine3" Type="String" />
                  <asp:Parameter Name="prmMachine4" Type="String" />
                  <asp:Parameter Name="prmMachine5" Type="String" />
                  <asp:Parameter Name="prmMachine6" Type="String" />
                  <asp:Parameter Name="prmMachine7" Type="String" />
                  <asp:Parameter Name="prmMachine8" Type="String" />
                  <asp:Parameter Name="prmMachine9" Type="String" />
                  <asp:Parameter Name="prmMachine10" Type="String" />
                  <asp:Parameter Name="prmMachine11" Type="String" />
                  <asp:Parameter Name="prmMachine12" Type="String" />
                  <asp:Parameter Name="prmMachine13" Type="String" />
                  <asp:Parameter Name="prmMachine14" Type="String" />
                  <asp:Parameter Name="prmMachine15" Type="String" />
                  <asp:Parameter Name="prmOutexcel" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

