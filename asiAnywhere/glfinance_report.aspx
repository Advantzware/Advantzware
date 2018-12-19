<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="glfinance_report" Codebehind="glfinance_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>GL Financial Statements</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language ="javascript">
    
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
    <form id="frmList" runat="server"  defaultfocus='headTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
                    
          <asp:Label ID="Label1" ForeColor="red" Font-Bold="true" runat="server" ></asp:Label>
                 
       <table><tr><td>
        <TABLE id="tblTop" cellSpacing="3" align="left" border="0">
        <TR>
            
          <TD align="left"><font size=+0><b>GL Financial Statements &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
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
     
      
        <tr> <td align="right" style="padding-right: 5px"><b>Report Heading:</b></td>
        <td colspan="2">
            <asp:TextBox ID="headTextBox"  Width="200px" runat="server"></asp:TextBox>
           
             
            </td></tr>
       <tr><td align="right" style="padding-right: 5px"><b>Transaction Date:</b></td>
        <td><asp:TextBox ID="dateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"   Width="100px" runat="server"></asp:TextBox>               
         <a href="#" tabindex="1" onblur="document.getElementById('dateTextBox').focus()" onclick="showCalendarControl(dateTextBox); return false"><asp:Image ID="datelook" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
        <%--<a href="#" tabindex="1" onclick="company(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>--%>
        </td>
        <td rowspan="8">  
        <asp:Panel ID="gridpanel" runat="server" Width="230px" ScrollBars="vertical" Height="180px">
        
        <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1"
          AutoGenerateColumns="False" AllowSorting="True"  OnSelectedIndexChanged="GridView1_SelectedIndexChanged" 
            EmptyDataText="No Records Found"  BorderStyle="Dotted"  >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />            
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle  BackColor="Teal" ForeColor="White" Height="20px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <RowStyle BackColor="White" />           
            <Columns>
                            <asp:TemplateField>
                     <ItemTemplate>
                      <asp:CheckBox ID="chk1" runat="server" />
                      </ItemTemplate>
                      </asp:TemplateField>
                <asp:BoundField DataField="vValue" HeaderText="Value" SortExpression="vValue" />
                <asp:BoundField DataField="lvrept" HeaderText="Description" SortExpression="lvrept" />
            </Columns>
             <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    
            </asp:GridView>
        </asp:Panel>        
        </td></tr>
        
        <tr><td align="right" style="padding-right: 5px"><b>Period:</b></td>
        <td><asp:Label ID="periodLabel" Width="40px" runat="server" ></asp:Label> </td></tr>
        
        <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Pre Close Period?" runat="server" /></b></td></tr>
        <tr><td><b><asp:CheckBox ID="CheckBox2" Text="Skip Zero Line?" runat="server" /></b></td></tr>
        <tr><td><b><asp:CheckBox ID="CheckBox3" Text="Suppress Zero Fields?" runat="server" /></b></td></tr>
        <tr><td><b><asp:CheckBox ID="CheckBox4" Text="Print GL Acc#?" runat="server" /></b></td></tr>
        <tr><td><b><asp:CheckBox ID="CheckBox5" Text="Suppress Decimals?" runat="server" /></b></td></tr>
        <tr><td><b><asp:CheckBox ID="CheckBox6" Text="Multiple Companies?" runat="server" /></b></td>
        <td><b><asp:TextBox ID="CompanyTextBox" runat="server" ></asp:TextBox> </b></td>
        </tr>
        <tr><td colspan="5"><br />
        <fieldset><table>
        <tr><td colspan="4" align="center"><b>SORT OPTIONS</b></td></tr>
        <tr><td><b>Sub Account Level:</b></td>
        <td><asp:TextBox ID="acclabelTextBox" runat="server" Width="100px"></asp:TextBox> </td></tr>
        <tr><td><b>Beginning SubAcct:</b></td>
        <td><asp:TextBox ID="begsubTextBox" runat="server" Width="100px"></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="begsubTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer Value"></asp:CompareValidator></td>
        <td><b>Ending SubAcct:</b></td>
        <td><asp:TextBox ID="endsubTextBox" runat="server" Width="100px"></asp:TextBox>
        <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="endsubTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer Value"></asp:CompareValidator> </td></tr>
        </table></fieldset>
        </td></tr>
        
        
       
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
              SelectMethod="FinancialStatement" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter  Name="prmAction" Type="String" />
                  <asp:Parameter DefaultValue="gettt" Name="prmGetTt" Type="String" />
                  <asp:Parameter Name="prmdscr" Type="String" />
                  <asp:Parameter Name="prmtrnsdt" Type="String" />
                  <asp:Parameter Name="prmslctrp" Type="String" />
                  <asp:Parameter Name="prmpred" Type="Int32" />
                  <asp:Parameter Name="prmprecls" Type="String" />
                  <asp:Parameter Name="prmzeroln" Type="String" />
                  <asp:Parameter Name="prmsuppze" Type="String" />
                  <asp:Parameter Name="prmact" Type="String" />
                  <asp:Parameter Name="prmdecimal" Type="String" />
                  <asp:Parameter Name="prmmulcmp" Type="String" />
                  <asp:Parameter Name="prmlist" Type="String" />
                  <asp:Parameter Name="prmactlvl" Type="Int32"/>
                  <asp:Parameter Name="prmbesbact" Type="Int32" />
                  <asp:Parameter Name="prmendsbact" Type="Int32" />
                  <asp:Parameter Name="prmext" Type="String" />
                  <asp:Parameter Name="prmout" Type="String" />
                  </SelectParameters>
          </asp:ObjectDataSource>
          
          <asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>
                  fnstat:
                  <asp:Label ID="fnstatLabel" runat="server" Text='<%# Bind("fnstat") %>'></asp:Label><br />
                 
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FinancialStatement" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmGetTt" Type="String" />
                  <asp:Parameter Name="prmdscr" Type="String" />
                  <asp:Parameter Name="prmtrnsdt" Type="String" />
                  <asp:Parameter Name="prmslctrp" Type="String" />
                  <asp:Parameter Name="prmpred" Type="Int32" />
                  <asp:Parameter Name="prmprecls" Type="String" />
                  <asp:Parameter Name="prmzeroln" Type="String" />
                  <asp:Parameter Name="prmsuppze" Type="String" />
                  <asp:Parameter Name="prmact" Type="String" />
                  <asp:Parameter Name="prmdecimal" Type="String" />
                  <asp:Parameter Name="prmmulcmp" Type="String" />
                  <asp:Parameter Name="prmlist" Type="String" />
                  <asp:Parameter Name="prmactlvl" Type="Int32"/>
                  <asp:Parameter Name="prmbesbact" Type="Int32" />
                  <asp:Parameter Name="prmendsbact" Type="Int32" />
                  <asp:Parameter Name="prmext" Type="String" />
                  <asp:Parameter Name="prmout" Type="String" />
                  </SelectParameters>
                  </asp:ObjectDataSource>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

