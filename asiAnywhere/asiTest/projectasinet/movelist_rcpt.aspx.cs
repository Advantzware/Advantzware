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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class movelist_rcpt : System.Web.UI.Page
{
    public movelist_rcpt()
    {
        //
        // TODO: Add constructor logic here
        //
    }
        

    protected void Page_Load(object sender, EventArgs e)
    {

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;

        if (!Page.IsPostBack)
        {

            if (Session["prmTag_move_list"] != null)
                txt_tag.Text = Convert.ToString(Session["prmTag_move_list"]);
            else
                txt_tag.Text = "";

        }

        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;


        //ImageButton brwsorder = (ImageButton)Master.FindControl("movelist_rcpt");
        //brwsorder.ImageUrl = "Images/list-receipts1.jpg";

        GridView1.PageIndex = Convert.ToInt32(Session["page_inder_move_list"]);
        GridView1.SelectedIndex = Convert.ToInt32(Session["move_list_grid_seqno_index_gd"]) - 1;
        try
        {
            if (Session["move_list_grid_seqno_index_gd"] == null)
            {
                GridView1.SelectedIndex = 0;
            }
            Session["move_list_seq"] = ((Label)GridView1.SelectedRow.FindControl("Labelseq")).Text;
        }
        catch
        {
            return;
        }

        
        txt_tag.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        try
        {
            TextBox vsearch = (TextBox)FormView1.FindControl("aLineLabel");
            vsearch.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch
        { }
               
        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }




        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "movelist_rcpt.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
                       

        }

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "GridSearch";        
        ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        Session["prmAction_move_list"] = "GridSearch";        
        Session["prmTag_move_list"] = null;
        Session["move_list_grid_seqno_index_gd"] = null;
        Session["move_list_grid_seqno_gd"] = null;
        GridView1.SelectedIndex = 0;

    }

    protected void btn_reset_Click(object sender, EventArgs e)
    { 
        ContentPlaceHolder ct = (ContentPlaceHolder)Master.FindControl("ContentPlaceHolder1");
        foreach (Control c in ct.Controls)
        {
            switch (c.GetType().ToString())
            {
                case "System.Web.UI.WebControls.TextBox":
                    ((TextBox)c).Text = "";
                    break;
            }
        }
        
        Session["gridsize"] = null;
        GridView1.SelectedIndex = 0;
        UserClass UserLogin = (UserClass)Session["User"];        
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "GridSelect";       
        ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        Session["prmAction_move_list"] = "GridSelect";
        Session["prmTag_move_list"] = null;
        Session["move_list_grid_seqno_index_gd"] = null;
        Session["move_list_grid_seqno_gd"] = null;
        Session["page_inder_move_list"] = 0;

        GridView1.PageIndex = Convert.ToInt32(Session["page_inder_move_list"]);
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["move_list_seq"] = ((Label)GridView1.SelectedRow.FindControl("Labelseq")).Text;
        Session["move_list_grid_seqno_index_gd"] = GridView1.SelectedIndex + 1;
        
    }

    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    protected void GridView1_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {

        GridView1.PageIndex = e.NewPageIndex;
        Session["page_inder_move_list"] = e.NewPageIndex;      
    }
   

    
}
