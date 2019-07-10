using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class item_price_po_lookup2 : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {

       
        if (!Page.IsPostBack)
        {
            QtyRadioButtonList1.SelectedIndex = 0;
           
           
        }

        ObjectDataSource1.SelectParameters["prmrdqty"].DefaultValue = QtyRadioButtonList1.SelectedValue;
        string str = @"<script language=javascript>
        function Tovalue()
            {  
            var text1=document.getElementById('Text1').value;
            var text2=document.getElementById('Text2').value;
            var text3=document.getElementById('Text3').value;
            var text4=document.getElementById('Text4').value;
            var text5=document.getElementById('Text5').value;
            var text6=document.getElementById('Text6').value;
            var text7=document.getElementById('Text7').value;
            var text8=document.getElementById('Text8').value;
            var text9=document.getElementById('Text9').value;
            var text10=document.getElementById('Text10').value;
            
            var ar= new Array(text1,text2,text3,text4,text5,text6,text7,text8,text9,text10);
            window.opener.update(ar);        
            window.close(); 
            }  
           </script>";

        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "Tovalue"))
        {
            ClientScript.RegisterClientScriptBlock(this.GetType(), "Tovalue", str.ToString());
        }

        Button2.Attributes.Add("onclick", "Tovalue();");

    }
    protected void button_Click(object sender, EventArgs e)
    {
        string selectvalue = "";
        decimal totinvqty = 0;
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool ischeck = ((CheckBox)row.FindControl("chk1")).Checked;
            if (ischeck)
            {
                selectvalue = selectvalue + "Yes" + ",";  
            }
            else
                selectvalue = selectvalue + "No" + ",";
            if (ischeck)
            {
                string line = ((Label)row.FindControl("LineLabel")).Text;
                string invqty = ((Label)row.FindControl("invqtyLabel")).Text;
                string snum = ((Label)row.FindControl("snumLabel")).Text;
                string item = ((Label)row.FindControl("inoLabel")).Text;
                string act = ((Label)row.FindControl("actLabel")).Text;
                string actnum = ((Label)row.FindControl("actnameLabel")).Text;
                string amt = ((Label)row.FindControl("tamtLabel")).Text;
                Text1.Text = line;
                Text2.Text = act;
                Text3.Text = invqty;
                Text4.Text = amt;
                Text5.Text = item;
                Text6.Text = snum;
                Text7.Text = actnum;
                totinvqty = totinvqty + Convert.ToDecimal(invqty);
                Text3.Text = Convert.ToString(totinvqty);
            }
        }
       

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = selectvalue;

        if (!ClientScript.IsStartupScriptRegistered("alert"))
        {
            Page.ClientScript.RegisterStartupScript

                (this.GetType(), "alert", "Tovalue();", true);
        }
        
        
    }
    protected void Gridview1_DataBound(object sender, EventArgs e)
    {
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            string  chklabel = ((Label)row.FindControl("Label1")).Text;
            CheckBox ischeck = (CheckBox)row.FindControl("chk1");
            if (chklabel == "no")
                ischeck.Checked = false;
            else
                ischeck.Checked = true;
        }

    }
    
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
}
