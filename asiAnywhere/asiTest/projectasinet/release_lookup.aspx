<%@ Page Language="C#" AutoEventWireup="true" Inherits="release_lookup" Title="Release Information" Codebehind="~/release_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Release Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    
    <div>
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
   <td class="shade">
  <asp:button id="Button1" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server" AutoPostBack="true" OnSelectedIndexChanged="searchindexchanged">
                   
                      <asp:ListItem Value="release">Release#</asp:ListItem>
                      <asp:ListItem Value="customer">Customer#</asp:ListItem>
                                          
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server"> 
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
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
            
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.ReleaseLookUp('<%#DataBinder.Eval(Container,"DataItem.vRelease")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
            
                <asp:BoundField DataField="vRelease" HeaderText="Release#" 
                    SortExpression="vRelease" />
                <asp:BoundField DataField="vRelDate" HeaderText="Date Last Counted" 
                    SortExpression="vRelDate" />
                <asp:BoundField DataField="vOrdNo" HeaderText="Order#" 
                    SortExpression="vOrdNo" />
               <%-- <asp:BoundField DataField="retname" HeaderText="retname" SortExpression="retname" />--%><asp:BoundField 
                    DataField="cust-no" HeaderText="cust-no" SortExpression="Cust. #" />
                <asp:BoundField DataField="ship-id" HeaderText="Ship To ID" 
                    SortExpression="ship-id" />
                <asp:BoundField DataField="po-no" HeaderText="P.O.Num" SortExpression="po-no" />
                
                
                
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="SelectReleaseLookup" TypeName="LookUp">
            <SelectParameters>
                    
                <asp:Parameter DefaultValue="showall" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
                    
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter Name="prmComp" Type="String"  />
                <asp:Parameter Name="prmRelease" Type="Int32"  />
                 
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
