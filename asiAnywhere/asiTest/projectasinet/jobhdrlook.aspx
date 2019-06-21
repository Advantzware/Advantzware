<%@ Page Language="C#" AutoEventWireup="True" Debug="true" Codebehind="jobhdrlook.aspx.cs" Inherits="JobhdrLook" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Job Information(Open Job Only)</title>   
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" defaultbutton="Button1">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                      <asp:ListItem Value="item">Item</asp:ListItem>  
                      <asp:ListItem Value="job-no">Job#</asp:ListItem>  
                     
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                                      
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
    <div>
        <asp:GridView ID="GridView1"  AllowPaging="true" PageSize ="10" runat="server" AllowSorting="true" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
             <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.JobhdrLook('<%#DataBinder.Eval(Container,"DataItem.JOB")%>','<%#DataBinder.Eval(Container,"DataItem.JOB2")%>','<%#DataBinder.Eval(Container,"DataItem.blank-no")%>','<%#DataBinder.Eval(Container,"DataItem.form-no")%>','<%#DataBinder.Eval(Container,"DataItem.cust-no")%>','<%#DataBinder.Eval(Container,"DataItem.ord-no")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>     
                 <asp:BoundField DataField="JOB" HeaderText="Job" SortExpression="JOB" />
                 <asp:BoundField DataField="JOB2" HeaderText="" SortExpression="JOB2" />
                 <asp:BoundField DataField="i-no" HeaderText="Item#" SortExpression="i-no" />
                  <asp:BoundField DataField="estno" HeaderText="Estimate" SortExpression="estno" />                  
                  <asp:BoundField DataField="ord-no" HeaderText="Order#" SortExpression="ord-no" />
                  <asp:BoundField DataField="cust-no" HeaderText="Cust#" SortExpression="cust-no" />
                  
                                 
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectjobhdrLook" TypeName="browspo">
            <SelectParameters>               
                 <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
                 <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" /> 
                <asp:QueryStringParameter QueryStringField="item" Name="prmItem" Type="String" />
                <asp:Parameter Name="prmChar" Type="String" />    
                <asp:Parameter Name="prmJobno" Type="String" /> 
                <asp:Parameter Name="prmJob2" Type="Int32" /> 
                <asp:Parameter Name="prmSnum" Type="Int32" /> 
                <asp:Parameter Name="prmBnum" Type="Int32" /> 
                <asp:Parameter Name="prmPruom" Type="String" /> 
                <asp:Parameter Name="prmPrqtyuom" Type="String" /> 
                <asp:Parameter Name="prmOrdqty" Type="String" /> 
                <asp:Parameter Name="prmSwid" Type="Decimal" /> 
                <asp:Parameter Name="prmSlen" Type="Decimal" /> 
                <asp:Parameter Name="prmSdep" Type="Decimal" /> 
                
                <asp:Parameter Name="prmDis" Type="Decimal" /> 
                <asp:Parameter Name="prmSetup" Type="Decimal" /> 
                <asp:Parameter Name="prmConsuom" Type="String" /> 
                <asp:Parameter Name="prmCust" Type="String" /> 
                <asp:Parameter Name="prmItemType" Type="String" />
                 
                <asp:Parameter Name="prmpoNo" Type="Int32" /> 
                <asp:Parameter Name="prmLine" Type="Int32" />
                <asp:Parameter Direction="InputOutput" Name="error" Type="String" />
                
            </SelectParameters>
        </asp:ObjectDataSource>
        
       
    
    </div>
    </form>
</body>
</html>


