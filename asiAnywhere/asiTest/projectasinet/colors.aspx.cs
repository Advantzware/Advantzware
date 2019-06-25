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

public partial class colorsitem : System.Web.UI.Page
{
    string strRaw = "";
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["Item_Inquiry_value_check"] != null)
        {
            LinkButton1.Visible = false;
            LinkButton2.Visible = false;
            LinkButton3.Visible = false;
        }
        else
        {
            //ImageButton iteminquiry = (ImageButton)Master.FindControl("listitem");
            //iteminquiry.Visible = false;
            HtmlGenericControl iteminquiry = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            iteminquiry.Attributes.Add("style", "display:none");
            if (Session["view_order_entry_pages"] == null)
            {
                LinkButton2.Visible = false;
            }
            if (Session["view_order_entry_pages"] != null)
            {
                LinkButton1.Visible = false;
            }
            if (Session["view_order_entry_pages_with_estimate"] != null)
            {
                LinkButton1.Visible = false;
                LinkButton2.Visible = false;
            }
            if (Session["view_order_entry_pages_with_estimate"] == null)
            {
                LinkButton3.Visible = false;
            }
        }

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource4.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Colors";
        //ImageButton colors = (ImageButton)Master.FindControl("colors");
        //colors.ImageUrl = "~/img/colors1.jpg";
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        strRaw = this.GridView1.SelectedValue.ToString();
        this.FormView1.DataBind();
    }

    protected void ObjectDataSource2_Selecting1(object sender, ObjectDataSourceSelectingEventArgs e)
    {
        e.InputParameters["prmAction"] = "Select";
        e.InputParameters["prmRawNum"] = strRaw;
    }
    protected void LinkButton2_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_entry.aspx");
    }
    protected void LinkButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_inquiry.aspx");
    }

    protected void LinkButton3_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
}
