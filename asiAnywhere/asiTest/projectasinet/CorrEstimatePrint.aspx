<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="CorrEstimatePrint" Codebehind="~/CorrEstimatePrint.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Estimate Print</title>
    
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

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_supplierscode' >   
        
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Estimate Print&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" ></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
          <asp:HiddenField ID="HiddenField1" runat="server" />
          <asp:HiddenField ID="HiddenField2" runat="server" />
          <asp:HiddenField ID="HiddenField3" runat="server" />
              
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
        <tr>
            <td colspan="4"><b><asp:CheckBox ID="PrintMftDeptNoteCheckBox" Text="Print Manufacturing Department note?" runat="server" /></b></td>
        </tr>
      
      <tr><td align="right" style="padding-right:5px" nowrap><b>From Department:</b></td>
      <td>
          <asp:TextBox ID="FrmDept_TextBox" runat="server"></asp:TextBox>
          <a> </a>
          </td>
      <td align="right" style="padding-right:5px" nowrap><b>To Department:</b></td>
      <td><asp:TextBox ID="ToDept_TextBox" runat="server"></asp:TextBox></td>
      </tr>
        
        <tr>
            <td colspan="4"><b><asp:CheckBox ID="PrintBoxDsgCheckBox" Text="Print Box design?" runat="server" /></b></td>
        </tr>
        
      
      <tr><td colspan="2">
           &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          <%-- <b>Print To:</b><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Pdf" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>--%>
           </td></tr>
           
          <tr><td colspan="4" nowrap><asp:Button ID="SubmitButton" OnClick="SubmitButton_Click" runat="server" CssClass="button" Text="Submit"></asp:Button> 
          &nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>         
                              </td></tr></table>
                              
           <asp:FormView ID="FormView1" Visible="False" runat="server" OnPreRender="FormView1_PreRender" DataSourceID="ObjectDataSource1">
              
              <ItemTemplate>
                  vCorrEstimateFile:
                  <asp:Label ID="vCorrEstimateFileLabel" runat="server" Text='<%# Bind("vCorrEstimateFile") %>'>
                  </asp:Label><br />
              </ItemTemplate>                              
               
          </asp:FormView>             
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="CorrugatedEstimatePrint" TypeName="Corrugated">
              <SelectParameters>
                  <asp:SessionParameter Name="prmUser" SessionField="appha_user_login" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="vFromDept" Type="String" />
                  <asp:Parameter Name="vToDept" Type="String" />
                  <asp:Parameter Name="vTbPrtBox" Type="String" />
                  <asp:Parameter Name="vTbPrtNote" Type="String" />
                  <asp:SessionParameter SessionField="order_corrugated_est" Name="prmEstimate" Type="String" />
                  <asp:SessionParameter SessionField="order_corrugated_formno" Name="prmFormno" Type="int32" />
                  <asp:SessionParameter SessionField="order_corrugated_blankno" Name="prmBlankno" Type="int32" />
                  <asp:SessionParameter SessionField="corr_print_line" Name="prmLine" Type="int32" />
              </SelectParameters>
          </asp:ObjectDataSource>            
    </div>
    </form>
</body>
</html>
