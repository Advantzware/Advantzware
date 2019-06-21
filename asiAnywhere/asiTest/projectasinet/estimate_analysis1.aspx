<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="estimate_analysis1" Codebehind="estimate_analysis1.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Estimate Analysis</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <%--<link href="include/tree.css" rel="stylesheet" type="text/css" />--%>
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

    function displayQtyScrDiv() {
        document.getElementById("qtyscr").style.display = "block";
        document.getElementById("btndiv").style.display = "none";
    }
               

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_supplierscode' >   
        <%--<hd:header id="Header1" runat="server"></hd:header>--%>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>                                                 
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
          <asp:HiddenField ID="HiddenField1" runat="server" />
          <asp:HiddenField ID="HiddenField2" runat="server" />
          <asp:HiddenField ID="HiddenField3" runat="server" />
          <asp:HiddenField ID="HiddenField4" runat="server" />
           <asp:HiddenField ID="HiddenField_Vendor" runat="server" />
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
                             
          <div id="btndiv" runat="server">
            <table id="pugetable" runat="server">
          <tr><td align="center">  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;    
              <asp:Button ID="Button3" CssClass="buttonM" runat="server" Width="300px" Text="Purge Existing Qty's" OnClick="PurgeQtyClick" /><br /><br /></td></tr>
              <tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Button ID="Button4" CssClass="buttonM" runat="server" Width="300px" Text="Append To Existing Qty's" OnClick="AppendQtyClick" />
          </td></tr></table>
          </div>
                                      
        
        
          <asp:FormView ID="FormView1"  runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound">            
            <InsertItemTemplate>                      
                <table>       
                    <tr>
                        <td>
                            <b><font size="2"> Selection </font></b>                          
                        </td>              
                    </tr>
                    <tr>
                        <td>
                            <b><asp:CheckBox ID="CheckBox1" runat="server" />
                            Recalc Machines' Speed, Spoil%, & Waste?</b>
                        </td>                        
                    </tr>
                    <tr>
                        <td>
                            <b><asp:CheckBox ID="CheckBox2" runat="server" />
                            Recalc Machines' MR-Hrs?</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <b><asp:CheckBox ID="CheckBox3" runat="server" />
                            Override GS&A and/or Warehouse Percentages?</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <b><asp:CheckBox ID="CheckBox4" runat="server" />
                            Drop Slitter if MSF > Minimum?</b>           
                        </td>           
                    </tr>
                </table>

                  <%--<asp:Button ID="InsertButton" CssClass="buttonM" runat="server" CausesValidation="True" OnClick="SaveBtnClick" Text="Save" />
                  &nbsp;<input type="button" name="Cancel" class="buttonM" id="close" value="Cancel" onClick="javascript:self.close()"  /> --%>
              </InsertItemTemplate>
              
          </asp:FormView>  
                      
    
        <asp:GridView ID="GridView1" Visible="false"  AllowPaging="True" runat="server" 
              AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static"  EmptyDataText="No Records Found" Width="100%" 
              BorderStyle="Dotted" CssClass="Grid">            
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />        
            
            <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <%--<asp:BoundField DataField="vDoGsa" HeaderText="vDoGsa" 
                    SortExpression="vDoGsa" />
                <asp:BoundField DataField="vDoMr" HeaderText="vDoMr" SortExpression="vDoMr" />
                <asp:BoundField DataField="vDoSpeed" HeaderText="vDoSpeed" 
                    SortExpression="vDoSpeed" />
                <asp:BoundField DataField="vDropRc" HeaderText="vDropRc" 
                    SortExpression="vDropRc" />--%>
                <asp:BoundField DataField="vFormNo" HeaderText="S#" 
                    SortExpression="vFormNo" />
                <asp:BoundField DataField="vBlankNo" HeaderText="B#" 
                    SortExpression="vBlankNo" />
                <asp:BoundField DataField="vCustNo" HeaderText="Cust#" 
                    SortExpression="vCustNo" />
                <asp:BoundField DataField="vPartNo" HeaderText="Cust Part#" 
                    SortExpression="vPartNo" />
                <asp:BoundField DataField="vBlQty" HeaderText="Req Qty" 
                    SortExpression="vBlQty" />
                <asp:BoundField DataField="vYldQty" HeaderText="Yield Qty" 
                    SortExpression="vYldQty" />
                <asp:BoundField DataField="vRelease" HeaderText="Releases" 
                    SortExpression="vRelease" />
            </Columns>
            
            </asp:GridView>
            
          <asp:Button ID="Button1" Visible="false" CssClass="buttonM" runat="server" Text="Save" OnClick="Button1_Click" />
          &nbsp;<input type="button" Visible="false" name="Cancel" runat="server" class="buttonM" id="close" value="Cancel" onClick="javascript:self.close()"  /> 
        
              
    </div>
    
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
              OldValuesParameterFormatString="original_{0}" SelectMethod="SelectDefaultValues" 
              TypeName="Corrugated">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" DefaultValue="GetDefaultValue" />                  
                  <asp:SessionParameter Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          <div runat="server" id="vendordiv" align="center" >
          <br />
          <fieldset style="width:350px">
          <table>
          <tr><td align="left"><b>Board Vendor Selection</b></td></tr>
          <tr><td>
              <asp:GridView ID="GridView2" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_selectedindex_change"
                  DataSourceID="ObjectDataSourceVendor" AllowPaging="True" AllowSorting="True" OnSorting="Gridview1_Sorting" 
            EmptyDataText="No Records Found" Width="300px" BorderStyle="Dotted" CssClass="Grid">
           <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
                  <Columns>
                   <asp:CommandField  ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" >
                    <ItemStyle  Width="10px" />
                    </asp:CommandField>  
                      <asp:BoundField DataField="key-01" HeaderText="Vender" 
                          SortExpression="key-01" />
                   
                      <asp:BoundField DataField="key-02" HeaderText="Vendor Name" 
                          SortExpression="key-02" />
                      <asp:BoundField Visible="false" DataField="rec-id" HeaderText="rec-id" 
                          SortExpression="rec-id" />
                  </Columns>
              </asp:GridView><br /><br /></td></tr>
              <tr><td align="left">
              <asp:Button ID="okButton" CssClass="button" OnClick="ok_button_click" runat="server" Text="Ok" />
              <asp:Button ID="PriceButton" runat="server" CssClass="button" OnClick="Price_Button_Click" Text="Best Price" />
              <asp:Button ID="vandercancelButton" CssClass="button" runat="server" OnClientClick="javascript:window.close();" Text="Cancel" />
              </td></tr>
              </table>
              </fieldset>
              
              <asp:ObjectDataSource ID="ObjectDataSourceVendor" runat="server" 
                  OldValuesParameterFormatString="original_{0}" SelectMethod="SelectVendor" 
                  TypeName="Corrugated">
                  <SelectParameters>
                      <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                      <asp:Parameter DefaultValue="Admin" Name="prmUser" Type="String" />
                      <asp:SessionParameter Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
                  </SelectParameters>
              </asp:ObjectDataSource>
          </div>
    </form>
</body>
</html>
