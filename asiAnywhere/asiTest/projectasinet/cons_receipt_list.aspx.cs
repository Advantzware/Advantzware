
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class cons_receipt_list : System.Web.UI.Page
{
    

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "item_daily_receipt.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            //lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            lblComp.Text = PrmComp;
        }
        
      
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

            }


          
        }
        try
        {
            Session["Rowuser"] = UserLogin.UserName;
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);

            GridView1.SelectedIndex = Convert.ToInt32(Session["cons_recept_list_index"]);
            if (Session["cons_recept_list_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["cons_recept_list_seq"] = ((Label)GridView1.SelectedRow.FindControl("reckeylabel")).Text;
            }
            txt_seqno.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_tag.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_date.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_po.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_item.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_job.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch { }
        try
        {
            TextBox vsearch = (TextBox)FormView2.FindControl("aLineLabel");
            vsearch.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch
        { }
       

    }

   
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }



    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        
        string ss = "";
        txt_seqno.Text = ss.ToString();       
        txt_tag.Text = ss.ToString();
        txt_po.Text = ss.ToString();
        txt_item.Text = ss.ToString();
        txt_date.Text = ss.ToString();

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = txt_seqno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = txt_item.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();


        Session["cons_recept_list_seq_no"] = txt_seqno.Text.Trim();
        Session["cons_recept_list_recdate"] = txt_date.Text.Trim();
        Session["cons_recept_list_po"] = txt_po.Text.Trim();
        Session["cons_recept_list_inum"] = txt_item.Text.Trim();
        Session["cons_recept_list_tag"] = txt_tag.Text.Trim();
        Session["cons_recept_list_job"] = txt_job.Text.Trim();
    }
   


    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = txt_seqno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = txt_item.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();

        Session["cons_recept_list_seq_no"] = txt_seqno.Text.Trim();
        Session["cons_recept_list_recdate"] = txt_date.Text.Trim();
        Session["cons_recept_list_po"] = txt_po.Text.Trim();
        Session["cons_recept_list_inum"] = txt_item.Text.Trim();
        Session["cons_recept_list_tag"] = txt_tag.Text.Trim();
        Session["cons_recept_list_job"] = txt_job.Text.Trim();

    }



    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        
        Response.Redirect(sMenuURL);
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
       

    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["cons_recept_list_index"] = GridView1.SelectedIndex;
        Session["cons_recept_list_seq"] = ((Label)GridView1.SelectedRow.FindControl("reckeylabel")).Text;
    }

    protected void lnk_list_click(object sender, EventArgs e)
    {
        Response.Redirect("cons_receipt_list.aspx");
    }

    protected void lnk_view_click(object sender, EventArgs e)
    {
        Response.Redirect("view_cons_receipt.aspx");
    }

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        Session["cons_recipt_list_add_new"] = "Add";
        Response.Redirect("view_cons_receipt.aspx");
    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


}
