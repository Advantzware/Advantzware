<%@ Page Language="C#" AutoEventWireup="true" Inherits="corvend_lookup " Title="Vendor Information(Window)" Codebehind="corvend_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Vendor Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    
    <div>
      <asp:Panel ID="searchpanel" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="470px" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server"  width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button><br />
                  <asp:button id="Button2" runat="server"  width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" >&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                  <asp:ListItem Value="Vend">Vendor</asp:ListItem>                      
                  <asp:ListItem Value="name">Name</asp:ListItem>                      
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                     <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                   
                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    
    <div>
        &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" AllowPaging="true"  AllowSorting="true" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1"  
            EmptyDataText="No Records Found" Width="470px" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
             <asp:TemplateField>
                  <ItemStyle HorizontalAlign="Center" Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick = "javascript:top.opener.window.VendLookup('<%#DataBinder.Eval(Container,"DataItem.vVendor")%>','<%#DataBinder.Eval(Container,"DataItem.vVendName")%>','<%#DataBinder.Eval(Container,"DataItem.vAdd")%>','<%#DataBinder.Eval(Container,"DataItem.vCity")%>','<%#DataBinder.Eval(Container,"DataItem.vState")%>','<%#DataBinder.Eval(Container,"DataItem.vZip")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vVendor" HeaderText="Vendor#" ItemStyle-Wrap="false" SortExpression="vVendor" />
                <asp:BoundField DataField="vVendName" HeaderText="Name" ItemStyle-Wrap="false" SortExpression="vVendName" />
                <asp:BoundField DataField="vAdd" HeaderText="Address" ItemStyle-Wrap="false" SortExpression="vAdd" />
                <asp:BoundField DataField="vCity" HeaderText="City" ItemStyle-Wrap="false" SortExpression="vCity" />
                <asp:BoundField DataField="vState" HeaderText="State" ItemStyle-Wrap="false" SortExpression="vState" />
                <asp:BoundField DataField="vZip" HeaderText="Zip" ItemStyle-Wrap="false" SortExpression="vZip" />
                <asp:BoundField DataField="vType" HeaderText="Vendor Type" ItemStyle-Wrap="false" SortExpression="vType" />
                <asp:BoundField DataField="vBuyer" HeaderText="Buyer Code" ItemStyle-Wrap="false" SortExpression="vBuyer" /> 
                             
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="VendorLookup" TypeName="Corrugated" OnSelecting="ObjectDataSource1_Selecting">
            <SelectParameters>            
                    
                <asp:Parameter DefaultValue="" Name="prmAction" Type="String" />
                
              <asp:Parameter Name="prmUser" Type="String" />
                
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter DefaultValue="" Name="prmActive" Type="String" />
                
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
