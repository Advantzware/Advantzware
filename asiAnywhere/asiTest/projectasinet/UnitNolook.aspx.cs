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
using System.Text;

public partial class UnitNoLook : System.Web.UI.Page
{
    int total=0;
    string s1="";
    string s2="";
    protected void Page_Load(object sender, EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (!Page.IsPostBack)
        {
            ddlSearchOperation.SelectedIndex = 1;
            Select2.Visible = false;
        }

        string str = @"<script language=javascript>
        function Tovalue()
            { 
            var text1=document.getElementById('TextBox1').value;
            var text2=document.getElementById('TextBox2').value;
            var ar=new Array(text1,text2);
            window.opener.getvalue(ar);
            window.close();

 
            }
            </script>";
        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "Tovalue"))
        {
            Page.ClientScript.RegisterClientScriptBlock(this.GetType(), "Tovalue", str.ToString());
        }
        Select2.Attributes.Add("onclick", "Tovalue();");
        

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
        ObjectDataSource1.SelectParameters["MatType"].DefaultValue = "D";
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        Response.Redirect("UnitNoLook.aspx");
    }
    protected void Select_Click(object sender, EventArgs e)
    {
        //StringBuilder str = new StringBuilder();
        //StringBuilder str2 = new StringBuilder();
        
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow grow = GridView1.Rows[i];
            bool ischeck = ((CheckBox)grow.FindControl("chkSelect")).Checked;
            if (ischeck)
            {
                total = total + 1;
                s1 = GridView1.Rows[i].Cells[1].Text;
                //str.Append(",");
                s2=((Label)GridView1.Rows[i].FindControl("Label1")).Text;
                //str2.Append(",");
            }
        }
        if (total == 0)
        {
            TextBox1.Text = "";
            TextBox2.Text = "";
            Select.Visible = false;
            Select2.Visible = true;
        }
            if (total == 1)
            {
                //string radval = s1.Split(new char[] { ',' });
                //string radval2 = s2.Split(new char[] { ',' });
                //Session["unit_one"] = radval[0];
                TextBox1.Text = s1;
                TextBox2.Text = s2;
                Select.Visible = false;
                Select2.Visible = true;
                
            }
            if (total > 1)
            {
                Response.Write("<script>alert('You cannot select more than one record')</script>");
            }

            
    }
}
