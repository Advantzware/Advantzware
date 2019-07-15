<%@ Page Language="C#" MasterPageFile="MasterPage5.master" Debug="true" AutoEventWireup="true" Inherits="rfqestimate" Title="Estimate for Quote" Codebehind="rfq_estimate.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">

<script type="text/javascript">
   var TargetBaseControl = null;
        
   window.onload = function()
   {
      try
      {
         //get target base control.
         TargetBaseControl = 
           document.getElementById('<%= this.GridView1.ClientID %>');
      }
      catch(err)
      {
         TargetBaseControl = null;
      }
   }
        
   function TestCheckBox()
   {              
      if(TargetBaseControl == null) return false;
      
      //get target child control.
      var TargetChildControl = "chkSelect";
            
      //get all the control of the type INPUT in the base control.
      var Inputs = TargetBaseControl.getElementsByTagName("input"); 
            
      for(var n = 0; n < Inputs.length; ++n)
         if(Inputs[n].type == 'checkbox' && 
            Inputs[n].id.indexOf(TargetChildControl,0) >= 0 && 
            Inputs[n].checked)
          return true;        
            
      alert('Select at least one checkbox!');
      return false;
   }
   
  
</script>



<div>
<%--<fieldset style="background-color:#EFF3FB; width:700px;">

<legend>Reference Information</legend>
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource1" Width="700px">
       
        <ItemTemplate>
        
           <b>RFQ#:</b>
           <asp:Label ID="aRfqNoLabel" runat="server" BackColor="PaleTurquoise" Width="143px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aRfqNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Customer#:</b>
            <asp:Label ID="aCustNoLabel" runat="server" BackColor="PaleTurquoise" Width="163px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aCustNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Requested Date:</b>
            <asp:Label ID="vRfqDtLabel" runat="server" BackColor="PaleTurquoise" Width="143px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqDt","{0:MM/dd/yyyy}") %>'></asp:Label>
        </ItemTemplate>
    </asp:FormView>
    
</fieldset>--%>
<br />
    <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:Label ID="CheckLabel" runat="server" Font-Bold="true" ForeColor="red"></asp:Label>
<fieldset style="background-color:#EFF3FB; width:700px;">
<table width="500px" > <tr ><td align="right" style="padding-right:5px"><b>Estimate Type</b></td>
<td>
    <asp:DropDownList ID="DropDownList2" runat="server">
    <asp:ListItem Text="Folding Single Item" ></asp:ListItem>
    <asp:ListItem Text="Folding Two Piece Box"></asp:ListItem>
    <asp:ListItem Text="Folding Tandem Runs"></asp:ListItem>
    <asp:ListItem Text="Folding Combination"></asp:ListItem>
    <asp:ListItem Text="Corrugated Single Item"></asp:ListItem>
    <asp:ListItem Text="Corrugated set"></asp:ListItem>
    </asp:DropDownList></td>
<td >&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <b><asp:RadioButtonList ID="RadioButtonList2" AutoPostBack="true" RepeatLayout="flow" CellSpacing="1" RepeatColumns="2" runat="server">
    <asp:ListItem Text="New Estimate"></asp:ListItem>
    <asp:ListItem Text="Update"></asp:ListItem>
    </asp:RadioButtonList></b></td></tr></table>
</fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqEstimate" TypeName="rfqs">
        <SelectParameters>
            <asp:SessionParameter SessionField="rfq_estimate_comp" Name="prmComp" Type="String" />
            <asp:Parameter Name="prmLoc" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="rfqestno" Type="Int32" />
            <asp:Parameter Name="prmEstType" Type="String" />
            <asp:Parameter Name="prmEstNew" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
<div>
    <br />
    <asp:GridView ID="GridView1" Width="700px" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" EmptyDataText="No Record Found" >
        <Columns>
        <%--<asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" >
                    <ItemStyle Width="10px" />
                </asp:CommandField>--%>
                <asp:TemplateField>
                <%--<HeaderTemplate>
<input type="checkbox" id="chkAll" name="chkAll" onclick="Check(this)" />

</HeaderTemplate>--%>

                <ItemTemplate>
                <asp:CheckBox ID="chkSelect" runat="server" />
                </ItemTemplate>
                </asp:TemplateField>
                
            <asp:BoundField DataField="RfqNo" HeaderText="Rfq#" SortExpression="RfqNo" >
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:BoundField>
             
            
            <%--<asp:BoundField DataField="RfqStock" HeaderText="Fg Item#" SortExpression="RfqStock" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>--%>
             <asp:BoundField DataField="RfqFormNo" HeaderText=" FormNo" SortExpression="RfqFormNo" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="RfqBlank" HeaderText="Blank No" SortExpression="RfqBlank" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="RfqSeq" HeaderText="#" SortExpression="RfqSeq" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="RfqStock" HeaderText="Fg Item#" SortExpression="RfqStock" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
             <asp:BoundField DataField="RfqPart" HeaderText="RfqPart" SortExpression="RfqPart" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            
            <asp:BoundField DataField="RfqName" HeaderText="Item Name" SortExpression="RfqName" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="RfqStyle" HeaderText="Style Code" SortExpression="RfqStyle" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            
            <asp:BoundField DataField="RfqProcat" HeaderText="Category" SortExpression="RfqProcatRfqProcat" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="RfqEst" HeaderText="Est#" SortExpression="RfqEst" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
           
        </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" Width="100%" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" />
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <RowStyle CssClass="shade" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqEstimate" TypeName="rfqs">
        <SelectParameters>
            <asp:SessionParameter SessionField="rfq_estimate_comp" Name="prmComp" Type="String" />
            <asp:Parameter Name="prmLoc" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="rfqestno" Type="Int32" />
           <asp:Parameter Name="prmEstType" Type="string" />
           <asp:Parameter Name="prmEstNew" Type="string" />
           <asp:Parameter Name="prmSeqList" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:Button ID="Transfertoestimate" runat="server" Text="Transfer To Estimate" CssClass="buttonM" OnClick="transfertoestimate_Click" OnClientClick="javascript:return TestCheckBox();" />
    <asp:Button ID="Button1" runat="server" Text="Transfer To Estimate" CssClass="buttonM" OnClick="transfertoestimate_Click" OnClientClick="javascript:return confirm('Estimate Already exists! Are you sure you want to transfer again');" />
   </div>
</asp:Content>
